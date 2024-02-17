// Mx-Compiler - a compiler implementation for Mx
// Copyright (C) 2022-2024 Lau Yee-Yu
//
// This library is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

package ir

import asm.initialPhysicalRegisters
import asm.numberOfAvailablePhysicalRegisters
import asm.precolouredPhysicalRegisters
import java.util.*

fun registerAllocate(function: GlobalFunction) {
    RegisterAllocator(function).allocate()
}
abstract class Register(val name: String) {
    override fun toString(): String {
        return name
    }

    override fun equals(other: Any?): Boolean {
        if (other !is Register) {
            return false
        }
        return name == other.name
    }

    override fun hashCode(): Int {
        return name.hashCode()
    }
}
class PhysicalRegister(val id: asm.Register) : Register(id.name)
class VirtualRegister(name: String) : Register(name)

class MoveInstruction(val src: Register, val dst: Register)

class RegisterAllocator(val function: GlobalFunction) {
    private var body: List<Block> = function.body
        ?: throw InternalError("Function body is null")
    private val regNumber = numberOfAvailablePhysicalRegisters
    private val precoloured: Set<Register> =
        precolouredPhysicalRegisters(function.parameters.size)
            .map { PhysicalRegister(it) }.toSet()
    private val initial: MutableSet<Register> =
        initialPhysicalRegisters(function.parameters.size)
            .map { PhysicalRegister(it) }.toMutableSet()
    private val simplifyWorkList: MutableSet<Register> = mutableSetOf()
    private val freezeWorkList: MutableSet<Register> = mutableSetOf()
    private val spillWorkList: MutableSet<Register> = mutableSetOf()
    private val spilledNodes: MutableSet<Register> = mutableSetOf()
    private val coalescedNodes: MutableSet<Register> = mutableSetOf()
    private val colouredNodes: MutableSet<Register> = mutableSetOf()
    private val selectStack: Stack<Register> = Stack()

    // moveSets
    private val coalescedMoves: MutableSet<MoveInstruction> = mutableSetOf()
    private val constrainedMoves: MutableSet<MoveInstruction> = mutableSetOf()
    private val frozenMoves: MutableSet<MoveInstruction> = mutableSetOf()
    private val worklistMoves: MutableSet<MoveInstruction> = mutableSetOf()
    private val activeMoves: MutableSet<MoveInstruction> = mutableSetOf()

    // interference graph
    private val adjList: MutableMap<Register, MutableSet<Register>> = mutableMapOf()
    private val adjSet: MutableSet<Pair<Register, Register>> = mutableSetOf()
    private val degree: MutableMap<Register, Int> = mutableMapOf()
    private val moveList: MutableMap<Register, Set<MoveInstruction>> = mutableMapOf()
    private val alias: MutableMap<Register, Register> = mutableMapOf()
    private val colour: MutableMap<Register, Register> = mutableMapOf()

    fun allocate(): GlobalFunction {
        if (function.regAllocated) {
            return function
        }
        mainProcedure()
        return GlobalFunction(
            name = function.name,
            returnType = function.returnType,
            parameters = function.parameters,
            body = body,
            moveSafe = function.moveSafe,
            regAllocated = true,
            const = function.const,
        )
    }


    private fun mainProcedure() {
        val liveness = LiveVariableAnalysis(body)
        this.build(liveness)
        this.makeWorklist()
        do {
            if (simplifyWorkList.isNotEmpty()) {
                simplify()
            } else if (worklistMoves.isNotEmpty()) {
                coalesce()
            } else if (freezeWorkList.isNotEmpty()) {
                freeze()
            } else if (spillWorkList.isNotEmpty()) {
                selectSpill()
            }
        } while (simplifyWorkList.isNotEmpty() || worklistMoves.isNotEmpty() ||
            freezeWorkList.isNotEmpty() || spillWorkList.isNotEmpty()
        )
        assignColors()
        if (spilledNodes.isEmpty()) {
            return
        }
        rewriteProgram()
        mainProcedure()
    }

    private fun build(liveness: LiveVariableAnalysis) {
        body.forEach { block ->
            var live = liveness.blockLiveOut[block] ?: throw InternalError("LiveOut is null")
            block.statements.asReversed().forEach { inst ->
                if (inst is PackedMoveStatement) {
                    live = live subtract inst.use
                    inst.toMoveInstructions().forEach { move ->
                        moveList[move.src] = moveList[move.src]?.union(setOf(move)) ?: setOf(move)
                        moveList[move.dst] = moveList[move.dst]?.union(setOf(move)) ?: setOf(move)
                        worklistMoves.add(move)
                    }
                }
                live = live union inst.def
                inst.def.forEach { d ->
                    live.forEach { l ->
                        addEdge(d.toVirtualRegister(), l.toVirtualRegister())
                    }
                }
                live = inst.use union (live subtract inst.def)
            }
        }
    }

    private fun makeWorklist() {
        initial.forEach { n ->
            if ((degree[n] ?: 0) >= regNumber) {
                spillWorkList.add(n)
            } else if (moveRelated(n)) {
                freezeWorkList.add(n)
            } else {
                simplifyWorkList.add(n)
            }
        }
    }

    private fun simplify() {
        if (simplifyWorkList.isEmpty()) {
            return
        }
        val n = simplifyWorkList.first()
        simplifyWorkList.remove(n)
        selectStack.push(n)
        adjacent(n).forEach { m -> decrementDegree(m) }
    }

    private fun coalesce() {
        val m = worklistMoves.first()
        val src = getAlias(m.src)
        val dst = getAlias(m.dst)
        val u = if (src in precoloured) src else dst
        val v = if (src in precoloured) dst else src
        worklistMoves.remove(m)
        if (u == v) {
            coalescedMoves.add(m)
            addWorkList(u)
        } else if (v in precoloured ||Pair(u, v) in adjSet) {
            constrainedMoves.add(m)
            addWorkList(u)
            addWorkList(v)
        } else if ((u in precoloured && adjacent(v).all { w -> ok(w, v) }) ||
            (u !in precoloured && conservative(adjacent(u) union adjacent(v)))) {
            coalescedMoves.add(m)
            combine(u, v)
            addWorkList(u)
        } else {
            activeMoves.add(m)
        }
    }

    private fun freeze() {
        val u = freezeWorkList.first()
        freezeWorkList.remove(u)
        simplifyWorkList.add(u)
        freezeMoves(u)
    }

    private fun selectSpill() {
        TODO()
    }

    private fun assignColors() {
        TODO()
    }

    private fun rewriteProgram() {
        TODO()
    }

    private fun decrementDegree(m: Register) {
        val d = degree[m] ?: throw InternalError("Degree is null")
        degree[m] = d - 1
        if (d == regNumber) {
            enableMoves(adjacent(m) union setOf(m))
            spillWorkList.remove(m)
            if (moveRelated(m)) {
                freezeWorkList.add(m)
            } else {
                simplifyWorkList.add(m)
            }
        }
    }

    private fun enableMoves(nodes: Set<Register>) {
        nodes.forEach { n ->
            nodeMoves(n).filter { it in activeMoves }.forEach { m ->
                activeMoves.remove(m)
                worklistMoves.add(m)
            }
        }
    }

    private fun addEdge(u: Register, v: Register) {
        if (Pair(u, v) !in adjSet && u != v) {
            adjSet.add(Pair(u, v))
            adjSet.add(Pair(v, u))
            if (u !in precoloured) {
                adjList.getOrPut(u) { mutableSetOf() }.add(v)
                degree[u] = degree.getOrDefault(u, 0) + 1
            }
            if (v !in precoloured) {
                adjList.getOrPut(v) { mutableSetOf() }.add(u)
                degree[v] = degree.getOrDefault(v, 0) + 1
            }
        }
    }

    private fun nodeMoves(n: Register): Set<MoveInstruction> =
        moveList.getOrDefault(n, setOf()) intersect (activeMoves union worklistMoves)

    private fun moveRelated(n: Register) = nodeMoves(n).isNotEmpty()

    private fun adjacent(n: Register): Set<Register> =
        adjList.getOrDefault(n, setOf()) subtract (selectStack union coalescedNodes)

    private fun getAlias(n: Register): Register {
        val aliasOfN = alias[n] ?: return n
        return getAlias(aliasOfN)
    }

    private fun addWorkList(u: Register) {
        if (u !in precoloured && !moveRelated(u) && (degree[u] ?: 0) < regNumber) {
            freezeWorkList.remove(u)
            simplifyWorkList.add(u)
        }
    }

    private fun ok(t: Register, r: Register): Boolean =
        (degree[t] ?: 0) < regNumber || t in precoloured || Pair(t, r) in adjSet

    private fun conservative(nodes: Set<Register>): Boolean {
        val k = nodes.count { (degree[it] ?: 0) >= regNumber }
        return k < regNumber
    }

    private fun combine(u: Register, v: Register) {
        if (v in freezeWorkList) {
            freezeWorkList.remove(v)
        } else {
            spillWorkList.remove(v)
        }
        coalescedNodes.add(v)
        alias[v] = u
        moveList[u] = moveList.getOrDefault(u, setOf()) union moveList.getOrDefault(v, setOf())
        enableMoves(setOf(v))
        adjacent(v).forEach { t ->
            addEdge(t, u)
            decrementDegree(t)
        }
        if ((degree[u] ?: 0) >= regNumber && u in freezeWorkList) {
            freezeWorkList.remove(u)
            spillWorkList.add(u)
        }
    }

    private fun freezeMoves(u: Register) {
        nodeMoves(u).forEach { m ->
            val v = if (getAlias(m.src) == getAlias(u)) getAlias(m.dst) else getAlias(m.src)
            activeMoves.remove(m)
            frozenMoves.add(m)
            if (nodeMoves(v).isEmpty() && (degree[v] ?: 0) < regNumber) {
                freezeWorkList.remove(v)
                simplifyWorkList.add(v)
            }
        }
    }
}

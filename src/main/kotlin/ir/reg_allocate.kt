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

class RegisterAllocator(val function: GlobalFunction) {
    //TODO
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
    private val worklistMoves: MutableSet<Statement> = mutableSetOf()
    private val freezeWorkList: MutableSet<Register> = mutableSetOf()
    private val spillWorkList: MutableSet<Register> = mutableSetOf()
    private val spilledNodes: MutableSet<Register> = mutableSetOf()
    private val moveList: MutableMap<Register, Set<Statement>> = mutableMapOf()
    private val activeMoves: MutableSet<Register> = mutableSetOf()

    // interference graph
    private val adjList: MutableMap<Register, MutableSet<Register>> = mutableMapOf()
    private val adjSet: MutableSet<Pair<Register, Register>> = mutableSetOf()
    private val degree: MutableMap<Register, Int> = mutableMapOf()

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
                    (inst.def union inst.use).forEach { n ->
                        moveList[n.toVirtualRegister()] =
                            moveList[n.toVirtualRegister()]?.union(setOf(inst)) ?: setOf(inst)
                    }
                    worklistMoves.add(inst)
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
        TODO()
    }

    private fun coalesce() {
        TODO()
    }

    private fun freeze() {
        TODO()
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

    private fun nodeMoves(n: Register) =
        moveList.getOrDefault(n, setOf()) intersect (activeMoves union worklistMoves)

    private fun moveRelated(n: Register) = nodeMoves(n).isNotEmpty()
}

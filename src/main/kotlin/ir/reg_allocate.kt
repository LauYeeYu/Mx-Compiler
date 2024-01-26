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

fun registerAllocate(function: GlobalFunction) {
    RegisterAllocator(function).allocate()
}

class RegisterAllocator(val function: GlobalFunction) {
    //TODO
    var body: List<Block> = function.body
        ?: throw InternalError("Function body is null")
    private val simplifyWorkList: MutableList<InterferenceGraphNode> = mutableListOf()
    private val worklistMoves: MutableList<InterferenceGraphNode> = mutableListOf()
    private val freezeWorkList: MutableList<InterferenceGraphNode> = mutableListOf()
    private val spillWorkList: MutableList<InterferenceGraphNode> = mutableListOf()
    private val spilledNodes: MutableList<InterferenceGraphNode> = mutableListOf()

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
        this.build()
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

    private fun build() {
        TODO()
    }

    private fun makeWorklist() {
        TODO()
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
}

abstract class Register(val name: String)
class PhysicalRegister(val id: asm.Register) : Register(id.name)
class VirtualRegister(name: String) : Register(name)
data class InterferenceGraphNode(val register: Register)

class InterferenceGraph(
    val originalFunction: GlobalFunction,
    val realBody: List<Block>,
    val liveVariableAnalysis: LiveVariableAnalysis,
) {
    //TODO
}

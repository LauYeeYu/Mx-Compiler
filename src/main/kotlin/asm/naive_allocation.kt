// Mx-Compiler - a compiler implementation for Mx
// Copyright (C) 2022 Lau Yee-Yu
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

package asm

import kotlin.math.min

fun naiveAllocation(irRoot: ir.Root) = TranslateUnit(
    functions = irRoot.globalFunctions.filter { it.body != null }
        .map { FunctionBuilder(it).function },
    globalVariables = irRoot.variables.map { buildGlobalVariable(it) }
)

class FunctionBuilder(private val irFunction: ir.GlobalFunction) {
    val function: Function
        get() = this.toAsm()

    private val localVariableMap = mutableMapOf<String, Int>()
    private val sourceBody = irFunction.body ?: throw Exception("Function ${irFunction.name} has no body")

    private fun toAsm(): Function {
        val stackSize = ((sourceBody.sumOf { block ->
            block.statements.sumOf { it.newVariableCount }
        } + min(irFunction.parameters.size, 8) + 1) * 4 + 15) / 16 * 16
        val blockList = mutableListOf(
            Block(
                "0", mutableListOf(
                    ImmCalcInstruction(
                        op = ImmCalcInstruction.ImmCalcOp.ADDI,
                        dest = Register.SP,
                        src = Register.SP,
                        imm = ImmediateInt(-stackSize),
                    )
                )
            )
        )
        saveFunctionParameters(blockList, stackSize)
        return TODO()
    }

    private fun saveFunctionParameters(blocks: MutableList<Block>, stackSize: Int) {
        for ((index, parameter) in irFunction.parameters.withIndex()) {
            localVariableMap[parameter.name] = stackSize + index - 8
            if (index < 8) {
                blocks.last().instructions.add(
                    StoreInstruction(
                        op = when (parameter.type.size) {
                            1 -> StoreInstruction.StoreOp.SB
                            4 -> StoreInstruction.StoreOp.SW
                            else -> throw Exception("Invalid parameter size")
                        },
                        src = toRegister("a$index"),
                        offset = ImmediateInt(stackSize + index - 8),
                        base = Register.SP,
                    )
                )
            }
        }
    }
}

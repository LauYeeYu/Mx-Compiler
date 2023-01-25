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

import exceptions.AsmBuilderException
import kotlin.math.min
import ir.Root as IrRoot
import ir.GlobalFunction as IrFunction
import ir.Block as IrBlock
import ir.Statement as IrStatement

fun naiveAllocation(irRoot: IrRoot) = TranslateUnit(
    functions = irRoot.globalFunctions.filter { it.body != null }
        .map { FunctionBuilder(it).function },
    globalVariables = irRoot.variables.map { buildGlobalVariable(it) }
)

class FunctionBuilder(private val irFunction: IrFunction) {
    val function: Function
        get() = this.toAsm()

    private val localVariableMap = mutableMapOf<String, Int>()
    private val sourceBody = irFunction.body ?: throw Exception("Function ${irFunction.name} has no body")
    private var stackRemained = 0

    private fun toAsm(): Function {
        val stackSize = ((sourceBody.sumOf { block ->
            block.statements.sumOf { it.newVariableCount }
        } + min(irFunction.parameters.size, 8) + 1) * 4 + 15) / 16 * 16
        val blocks = linkedMapOf(
            "0" to Block(
                irFunction.name, mutableListOf(
                    ImmCalcInstruction(
                        op = ImmCalcInstruction.ImmCalcOp.ADDI,
                        dest = Register.SP,
                        src = Register.SP,
                        imm = ImmediateInt(-stackSize),
                    )
                )
            )
        )
        saveFunctionParameters(blocks, stackSize)
        buildBody(blocks)
        return Function(irFunction.name, blocks.values.toList())
    }

    private fun saveFunctionParameters(blocks: LinkedHashMap<String, Block>, stackSize: Int) {
        val block = blocks[irFunction.name]
            ?: throw AsmBuilderException("The first function block not found")
        for ((index, parameter) in irFunction.parameters.withIndex()) {
            val offset = stackSize + (index - 8) * 4
            localVariableMap[parameter.name] = offset
            if (index < 8) {
                block.instructions.add(
                    StoreInstruction(
                        op = when (parameter.type.size) {
                            1 -> StoreInstruction.StoreOp.SB
                            4 -> StoreInstruction.StoreOp.SW
                            else -> throw Exception("Invalid parameter size")
                        },
                        src = toRegister("a$index"),
                        offset = ImmediateInt(offset),
                        base = Register.SP,
                    )
                )
            }
        }
        stackRemained = stackSize - min(irFunction.parameters.size, 8) * 4 - 4
        // save ra
        block.instructions.add(
            StoreInstruction(
                op = StoreInstruction.StoreOp.SW,
                src = Register.RA,
                offset = ImmediateInt(stackRemained),
                base = Register.SP,
            )
        )
    }

    private fun buildBody(blocks: LinkedHashMap<String, Block>) {
        sourceBody.withIndex().forEach { (index, irBlock) ->
            if (index != 0) {
                val blockName = "${irFunction.name}.${irBlock.label}"
                blocks[blockName] = Block(blockName, mutableListOf())
            }
        }
        for ((index, irBlock) in sourceBody.withIndex()) {
            if (index == 0) {
                val block = blocks[irFunction.name]
                    ?: throw AsmBuilderException("The first function block not found")
                buildBlock(irBlock, block, blocks)
            } else {
                val blockName = "${irFunction.name}.${irBlock.label}"
                val block = blocks[blockName]
                    ?: throw AsmBuilderException("asm block $blockName not found")
                buildBlock(irBlock, block, blocks)
            }
        }
    }
    private fun buildBlock(irBlock: IrBlock, block: Block, blocks: LinkedHashMap<String, Block>) {
        irBlock.statements.forEach { statement ->
            buildInstruction(statement, block, blocks)
        }
    }

    private fun buildInstruction(statement: IrStatement, currentBlock: Block, blocks: LinkedHashMap<String, Block>) {
        //TODO
    }
}

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
import ir.IntLiteral
import ir.LocalVariable
import kotlin.math.min
import kotlin.math.max
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
    private val sourceBody = (
            irFunction.body
                ?: throw AsmBuilderException("Function ${irFunction.name} has no body")
            ) + (
            irFunction.returnBlock
                ?: throw AsmBuilderException("Function ${irFunction.name} has no return block")
            )
    private val variables = irFunction.variables
        ?: throw AsmBuilderException("Function ${irFunction.name} has no variable list")
    private val maxCallParameterSize: Int
        get() {
            var max = 0
            sourceBody.forEach { block ->
                block.statements.filterIsInstance<ir.CallStatement>()
                    .forEach { statement ->
                    max = max(max, statement.arguments.size - 8)
                }
            }
            return max
        }
    private val stackSize = ((variables.sumOf { it.newVariableCount } +
            sourceBody.sumOf { block -> block.statements.sumOf { it.newVariableCount } } +
            min(irFunction.parameters.size, 8) + maxCallParameterSize + 1) * 4 + 15) / 16 * 16
    private val raOffset = stackSize - min(irFunction.parameters.size, 8) * 4 - 4

    init { // set local variable map
        var offset = stackSize - min(irFunction.parameters.size, 8) * 4
        irFunction.parameters.forEach { functionParameter ->
            localVariableMap[functionParameter.name] = offset
            offset += 4
        }
        offset = stackSize - min(irFunction.parameters.size, 8) * 4 - 4
        val addLocalVariable = { name: String, size: Int ->
            localVariableMap[name] = offset - size * 4
            offset -= size * 4
        }
        variables.forEach { variable -> addLocalVariable(variable.property.name, 2) }
        sourceBody.forEach { block ->
            block.statements.forEach { statement ->
                when (statement) {
                    is ir.CallStatement -> if (statement.dest != null) {
                        addLocalVariable(statement.dest.name, 1)
                    }
                    is ir.LocalVariableDecl -> addLocalVariable(statement.property.name, 2)
                    is ir.LoadStatement -> addLocalVariable(statement.dest.name, 1)
                    is ir.BinaryOperationStatement -> addLocalVariable(statement.dest.name, 1)
                    is ir.IntCmpStatement -> addLocalVariable(statement.dest.name, 1)
                    is ir.GetElementPtrStatement -> addLocalVariable(statement.dest.name, 1)
                    is ir.PhiStatement -> addLocalVariable(statement.dest.name, 1)
                    else -> {}
                }
            }
        }
    }

    private fun toAsm(): Function {
        val blocks = linkedMapOf(
            irFunction.name to Block(
                irFunction.name,
                mutableListOf(
                    BinaryRegInstruction(
                        op = BinaryRegInstruction.BinaryRegOp.MV,
                        dest = Register.T0,
                        src = Register.SP,
                    ),
                )
            )
        )
        val block = blocks[irFunction.name]
            ?: throw AsmBuilderException("The first function block not found")
        // set stack
        addImmediateToRegister(block, Register.SP, Register.SP, -stackSize, Register.T1)
        saveFunctionParameters(block)
        variables.forEach { buildInstruction(it, block) }
        buildBody(blocks)
        return Function(irFunction.name, blocks.values.toList())
    }

    private fun saveFunctionParameters(block: Block) {
        for ((index, parameter) in irFunction.parameters.withIndex()) {
            val offset = stackSize + (index - 8) * 4
            if (index < 8) {
                storeRegisterToMemory(
                    block = block,
                    op = when (parameter.type.size) {
                        1 -> StoreInstruction.StoreOp.SB
                        4 -> StoreInstruction.StoreOp.SW
                        else -> throw Exception("Invalid parameter size")
                    },
                    src = toRegister("a$index"),
                    offset = offset,
                    base = Register.SP,
                )
            }
        }
        // save ra
        storeRegisterToMemory(
            block = block,
            op = StoreInstruction.StoreOp.SW,
            src = Register.RA,
            offset = raOffset,
            base = Register.SP,
        )
    }

    private fun buildBody(blocks: LinkedHashMap<String, Block>) {
        sourceBody.withIndex().forEach { (index, irBlock) ->
            if (index != 0) {
                val blockName = "${irFunction.name}.${irBlock.label}"
                blocks[blockName] = Block(blockName, mutableListOf())
            }
        }
        for ((index, irBlock) in sourceBody.zipWithNext().withIndex()) {
            if (index == 0) {
                val block = blocks[irFunction.name]
                    ?: throw AsmBuilderException("The first function block not found")
                buildBlock(irBlock.first, block, blocks, irBlock.second.label)
            } else {
                val blockName = "${irFunction.name}.${irBlock.first.label}"
                val block = blocks[blockName]
                    ?: throw AsmBuilderException("asm block $blockName not found")
                buildBlock(irBlock.first, block, blocks, irBlock.second.label)
            }
        }
        val returnBlock = sourceBody.last()
        val blockName = "${irFunction.name}.return"
        val block = blocks[blockName]
            ?: throw AsmBuilderException("asm block $blockName not found")
        buildBlock(returnBlock, block, blocks, "") // return block has no branch statement
    }

    private fun buildBlock(
        irBlock: IrBlock,
        block: Block,
        blocks: LinkedHashMap<String, Block>,
        nextLabel: String,
    ) {
        irBlock.statements.forEach { statement ->
            buildInstruction(statement, block, blocks, nextLabel)
        }
    }

    private fun buildInstruction(
        statement: IrStatement,
        currentBlock: Block,
        blocks: LinkedHashMap<String, Block>,
        nextLabel: String,
    ) {
        when (statement) {
            is ir.LocalVariableDecl -> buildInstruction(statement, currentBlock)
            is ir.CallStatement -> buildInstruction(statement, currentBlock)
            is ir.ReturnStatement -> buildInstruction(statement, currentBlock)
            is ir.BranchStatement -> buildInstruction(statement, currentBlock, nextLabel)
            is ir.LoadStatement -> buildInstruction(statement, currentBlock)
            is ir.StoreStatement -> buildInstruction(statement, currentBlock, blocks, nextLabel)
            is ir.BinaryOperationStatement -> buildInstruction(statement, currentBlock, blocks, nextLabel)
            is ir.IntCmpStatement -> buildInstruction(statement, currentBlock, blocks, nextLabel)
            is ir.GetElementPtrStatement -> buildInstruction(statement, currentBlock, blocks, nextLabel)
            is ir.PhiStatement -> buildInstruction(statement, currentBlock, blocks, nextLabel)
            else -> throw AsmBuilderException("Unexpected statement class")
        }
    }

    private fun buildInstruction(
        statement: ir.LocalVariableDecl,
        currentBlock: Block,
    ) {
        val offset = localVariableMap[statement.property.name]
            ?: throw AsmBuilderException("Local variable not found")
        addImmediateToRegister(currentBlock, Register.T1, Register.SP, offset) // pointer address
        addImmediateToRegister(currentBlock, Register.T0, Register.SP, offset + 4) // data address
        currentBlock.instructions.add(
            StoreInstruction(
                op = StoreInstruction.StoreOp.SW,
                src = Register.T0,
                offset = ImmediateInt(0),
                base = Register.T1,
            )
        )
    }

    private fun buildInstruction(
        statement: ir.CallStatement,
        currentBlock: Block,
    ) {
        for ((index, argument) in statement.arguments.withIndex()) {
            // Load data to T0
            when (argument) {
                is IntLiteral ->
                    loadImmediateToRegister(currentBlock, Register.T0, argument.value)
                is LocalVariable ->
                    loadMemoryToRegister(
                        block = currentBlock,
                        op = when (argument.type.size) {
                            1 -> LoadInstruction.LoadOp.LBU
                            4 -> LoadInstruction.LoadOp.LW
                            else -> throw Exception("Invalid parameter size")
                        },
                        dest = Register.T0,
                        offset = localVariableMap[argument.name]
                            ?: throw AsmBuilderException("Local variable not found"),
                        base = Register.SP,
                    )
                else -> throw AsmBuilderException("Unexpected argument type")
            }
            if (index < 8) {
                currentBlock.instructions.add(
                    BinaryRegInstruction(
                        op = BinaryRegInstruction.BinaryRegOp.MV,
                        dest = toRegister("a$index"),
                        src = Register.T0,
                    )
                )
            } else {
                currentBlock.instructions.add(
                    StoreInstruction(
                        op = StoreInstruction.StoreOp.SW,
                        src = Register.T0,
                        offset = ImmediateInt((index - 8) * 4),
                        base = Register.SP,
                    )
                )
            }
        }
        currentBlock.instructions.add(
            CallInstruction(ImmediateLabel(statement.function.name))
        )
        if (statement.dest != null) {
            val offset = localVariableMap[statement.dest.name]
                ?: throw AsmBuilderException("Local variable not found")
            storeRegisterToMemory(
                block = currentBlock,
                op = StoreInstruction.StoreOp.SW,
                src = Register.A0,
                offset = offset,
                base = Register.SP,
            )
        }
    }

    private fun buildInstruction(
        statement: ir.ReturnStatement,
        currentBlock: Block,
    ) {
        // Restore the stack pointer
        addImmediateToRegister(currentBlock, Register.SP, Register.SP, stackSize, Register.T1)
        // Set back the return address
        loadMemoryToRegister(
            block = currentBlock,
            op = LoadInstruction.LoadOp.LW,
            dest = Register.RA,
            offset = raOffset,
            base = Register.SP,
        )
        if (statement.value != null) {
            loadMemoryToRegister(
                block = currentBlock,
                op = when (statement.value.type.size) {
                    1 -> LoadInstruction.LoadOp.LBU
                    4 -> LoadInstruction.LoadOp.LW
                    else -> throw Exception("Invalid parameter size")
                },
                dest = Register.A0,
                offset = localVariableMap[statement.value.name]
                    ?: throw AsmBuilderException("Local variable not found"),
                base = Register.SP,
            )
        }
        currentBlock.instructions.add(ReturnInstruction())
    }

    private fun buildInstruction(
        statement: ir.BranchStatement,
        currentBlock: Block,
        nextLabel: String,
    ) {
        if (statement.condition == null) { // unconditional jump
            if (statement.trueBlockLabel != nextLabel) {
                currentBlock.instructions.add(
                    PseudoJumpInstruction(
                        PseudoJumpInstruction.JumpOp.J,
                        ImmediateLabel(statement.trueBlockLabel)
                    )
                )
            }
        } else { // conditional jump
            loadMemoryToRegister(
                block = currentBlock,
                op = LoadInstruction.LoadOp.LBU,
                dest = Register.A0,
                offset = localVariableMap[statement.condition.name]
                    ?: throw AsmBuilderException("Local variable not found"),
                base = Register.SP,
            )
            val falseBlockLabel = statement.falseBlockLabel
                ?: throw AsmBuilderException("False block label is null")
            if (statement.trueBlockLabel == nextLabel) {
                currentBlock.instructions.add(
                    BranchCompZeroInstruction(
                        BranchCompZeroInstruction.BranchCompZeroOp.BEQZ,
                        Register.A0,
                        ImmediateLabel(falseBlockLabel),
                    )
                )
            } else if (falseBlockLabel == nextLabel) {
                currentBlock.instructions.add(
                    BranchCompZeroInstruction(
                        BranchCompZeroInstruction.BranchCompZeroOp.BNEZ,
                        Register.A0,
                        ImmediateLabel(statement.trueBlockLabel),
                    )
                )
            } else {
                currentBlock.instructions.add(
                    BranchCompZeroInstruction(
                        BranchCompZeroInstruction.BranchCompZeroOp.BNEZ,
                        Register.A0,
                        ImmediateLabel(statement.trueBlockLabel),
                    )
                )
                currentBlock.instructions.add(
                    PseudoJumpInstruction(
                        PseudoJumpInstruction.JumpOp.J,
                        ImmediateLabel(falseBlockLabel)
                    )
                )
            }
        }
    }

    private fun buildInstruction(
        statement: ir.LoadStatement,
        currentBlock: Block,
    ) {
        val offset = localVariableMap[statement.dest.name]
            ?: throw AsmBuilderException("Local variable not found")
        when (statement.src) {
            is ir.GlobalVariable ->
                currentBlock.instructions.add(
                    LoadGlobalInstruction(
                        op = LoadGlobalInstruction.LoadGlobalOp.LW,
                        dest = Register.A0,
                        label = ".${statement.src.name}",
                    )
                )
            is ir.LocalVariable ->
                loadMemoryToRegister(
                    block = currentBlock,
                    op = LoadInstruction.LoadOp.LW,
                    dest = Register.A0,
                    offset = localVariableMap[statement.src.name]
                        ?: throw AsmBuilderException("Local variable not found"),
                    base = Register.SP,
                )
            else -> throw AsmBuilderException("Unexpected argument type")
        }
        loadMemoryToRegister(
            block = currentBlock,
            op = when (statement.dest.type.size) {
                1 -> LoadInstruction.LoadOp.LBU
                4 -> LoadInstruction.LoadOp.LW
                else -> throw Exception("Invalid parameter size")
            },
            dest = Register.A1,
            offset = 0,
            base = Register.A0,
        )
        storeRegisterToMemory(
            block = currentBlock,
            op = when (statement.dest.type.size) {
                1 -> StoreInstruction.StoreOp.SB
                4 -> StoreInstruction.StoreOp.SW
                else -> throw Exception("Invalid parameter size")
            },
            src = Register.T0,
            offset = offset,
            base = Register.SP,
        )
    }
}

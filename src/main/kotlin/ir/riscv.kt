// Mx-Compiler - a compiler implementation for Mx
// Copyright (C) 2023 Lau Yee-Yu
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

import asm.hasImmOp
import kotlin.random.Random

private fun withinSignedImmediateRange(immediate: Int) =
    immediate < (1 shl 11) && immediate >= -(1 shl 11)

// This function will replace the literals that cannot be used as an immediate
// operand with a variable. This is necessary because the RISC-V ISA does not
// support immediate operands for integers.
class RiscvReplaceLiteralsWithVariablesIfNecessary(val body: List<Block>) {
    private val random = Random.nextInt(0, 1 shl 30)
    private var counter = 0
    private fun nextCounter() = counter++
    fun transform() = body.map { block ->
        Block(
            block.label,
            block.statements.flatMap { statement ->
                when (statement) {
                    is AllocaStatement -> listOf(statement)
                    is AssignStatement -> listOf(statement)
                    is BinaryOperationStatement -> binaryOperationToVariables(statement)
                    is BranchStatement -> listOf(statement)
                    is CallStatement -> callToVariables(statement)
                    is GetElementPtrStatement -> getElementPtrToVariables(statement)
                    is IntCmpStatement -> intCmpToVariables(statement)
                    is LoadStatement -> listOf(statement)
                    is PackedMoveStatement -> listOf(statement)
                    is PhiStatement -> listOf(statement)
                    is ReturnStatement -> listOf(statement)
                    is StoreStatement -> storeToVariables(statement)
                    else -> throw InternalError("Unexpected statement $statement")
                }
            }.toMutableList(),
        )
    }

    private fun binaryOperationToVariables(statement: BinaryOperationStatement): List<Statement> =
        if (statement.rhs is IntLiteral &&
            withinSignedImmediateRange(statement.rhs.value) &&
            statement.op.hasImmOp) {
            val variable = LocalVariable(
                "binary_literal_${random}_${nextCounter()}",
                statement.rhs.type,
                )
            listOf(
                AssignStatement(variable, statement.rhs),
                BinaryOperationStatement(
                    statement.dest, statement.op, statement.lhs, variable,
                ),
            )
        } else {
            listOf(statement)
        }

    // Note: It might not be necessary for the first 8 arguments to be converted,
    // so we can optimize this later.
    private fun callToVariables(statement: CallStatement): List<Statement> {
        val mapping = Array<Variable?>(statement.arguments.size) { null }
        val variableList = statement.arguments.mapIndexedNotNull { index, argument ->
            if (argument is IntLiteral) {
                val variable = LocalVariable(
                    "call_literal_${random}_${nextCounter()}", argument.type,
                )
                mapping[index] = variable
                AssignStatement(variable, argument)
            } else {
                null
            }
        }
        val newCallStatement = CallStatement(
            statement.dest,
            statement.returnType,
            statement.function,
            statement.arguments.mapIndexed { index, argument ->
                mapping[index] ?: argument
            },
        )
        return variableList + newCallStatement
    }

    private fun getElementPtrToVariables(statement: GetElementPtrStatement): List<Statement> {
        val mapping = Array<Variable?>(statement.indices.size) { null }
        val variableList = statement.indices.mapIndexedNotNull { index, argument ->
            // If the offset cannot be represented as an immediate operand, we
            // need to replace it with a variable.
            // Admittedly, for bool array, we don't need to multiply the value by 4,
            // but it's not worth the effort to check for that.
            if (argument is IntLiteral &&
                withinSignedImmediateRange(argument.value * 4)) {
                val variable = LocalVariable(
                    "gep_literal_${random}_${nextCounter()}", argument.type,
                )
                mapping[index] = variable
                AssignStatement(variable, argument)
            } else {
                null
            }
        }
        val newGetElementPtrStatement = GetElementPtrStatement(
            statement.dest,
            statement.src,
            statement.srcType,
            statement.indices.mapIndexed { index, argument ->
                mapping[index] ?: argument
            },
        )
        return variableList + newGetElementPtrStatement
    }

    private fun intCmpToVariables(statement: IntCmpStatement) =
        if (statement.lhs is IntLiteral) {
            val lhs = LocalVariable(
                "icmp_literal_${random}_${nextCounter()}", statement.lhs.type,
            )
            if (statement.rhs is IntLiteral) {
                val rhs = LocalVariable(
                    "icmp_literal_${random}_${nextCounter()}",
                    statement.rhs.type,
                )
                listOf(
                    AssignStatement(lhs, statement.lhs),
                    AssignStatement(rhs, statement.rhs),
                    IntCmpStatement(statement.dest, statement.op, lhs, rhs),
                )
            } else {
                listOf(
                    AssignStatement(lhs, statement.lhs),
                    IntCmpStatement(statement.dest, statement.op, lhs, statement.rhs),
                )
            }
        } else {
            if (statement.rhs is IntLiteral) {
                val rhs = LocalVariable(
                    "icmp_literal_${random}_${nextCounter()}",
                    statement.rhs.type,
                )
                listOf(
                    AssignStatement(rhs, statement.rhs),
                    IntCmpStatement(statement.dest, statement.op, statement.lhs, rhs),
                )
            } else {
                listOf(statement)
            }
        }

    fun storeToVariables(statement: StoreStatement): List<Statement> =
        if (statement.src is IntLiteral) {
            val src = LocalVariable(
                "store_literal_${random}_${nextCounter()}", statement.src.type,
            )
            listOf(
                AssignStatement(src, statement.src),
                StoreStatement(statement.dest, src),
            )
        } else {
            listOf(statement)
        }
}


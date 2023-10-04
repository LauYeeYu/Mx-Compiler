// Mx-Compiler - a compiler implementation for Mx
// Copyright (C) 2022-2023 Lau Yee-Yu
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

import exceptions.IRBuilderException
import java.util.*

// To remove the alloca/load/store statements, and generate the real SSA form.

class MemToReg(val function: GlobalFunction) {
    private val functionBody = function.body
        ?: throw InternalError("Function definition not found")

    fun promote(): GlobalFunction {
        return GlobalFunction(
            function.name,
            function.returnType,
            function.parameters,
            memToReg(functionBody)
        )
    }

    private fun memToReg(body: List<Block>): List<Block> {
        return PhiPromotion(body).phiPromotion()
    }

}

class PhiPromotion(val body: List<Block>) {
    private val allocas: List<AllocaStatement> = body
        .flatMap { it.statements }.filterIsInstance<AllocaStatement>()
    private val variables: Map<LocalVariable, PrimitiveType> =
        allocas.associate { it.property to it.type }
    private val defsOfVariables: Map<LocalVariable, List<Block>> = variables
        .keys.associateWith { variable ->
            body.filter { block ->
                block.statements.any { statement ->
                    statement is StoreStatement && statement.dest == variable
                }
            }
        }
    private val cfg = ControlFlow(body)
    private val dominance = Dominance(body, cfg)
    private val phis: Map<Block, MutableMap<LocalVariable, PhiStatement>> =
        body.associateWith { mutableMapOf() }
    private val variableCount = variables.keys.associateWith { 0 }.toMutableMap()
    private val versions: Map<LocalVariable, Stack<Argument>> = variables.keys
        .associateWith { Stack<Argument>() }
    private val newBlock = mutableMapOf<Block, List<Statement>>()
    private val replaceMap = mutableMapOf<Variable, Argument>()

    fun phiPromotion(): List<Block> {
        // Pre-allocate the phi statements
        // phi body is left empty in this step
        defsOfVariables.forEach { (variable, defs) ->
            val type = variables[variable] ?: throw InternalError("Variable $variable not found")
            defs.forEach { defBlock ->
                dominance.dominanceFrontier[defBlock]?.forEach { frontier ->
                    preAllocatePhisRecursive(variable, type, frontier, mutableSetOf<Block>())
                }
            }
        }

        // Rename
        renameRecursive(body.first(), mutableSetOf())

        // Merge the phis
        return body.map { block ->
            val newStatements = newBlock[block] ?: throw InternalError("Block $block not found")
            val newPhis = phis[block]?.map { (_, phi) -> phi } ?: throw InternalError("Block $block not found")
            Block(
                block.label,
                (newPhis + newStatements).toMutableList(),
            )
        }
    }

    private fun preAllocatePhisRecursive(
        variable: LocalVariable, type: PrimitiveType,
        block: Block, visited: MutableSet<Block>,
    ) {
        if (visited.contains(block)) return
        visited.add(block)
        phis[block]!![variable] = PhiStatement(
            LocalVariable(
                "phi.${variable.name}.${variableCount[variable]!!}",
                type
            ), mutableListOf()
        )
        variableCount[variable] = variableCount[variable]!! + 1
        dominance.dominanceFrontier[block]?.forEach { frontier ->
            preAllocatePhisRecursive(variable, type, frontier, visited)
        }
    }

    // Replace the uses of the variable with the phi statements
    // This includes
    // 1. Push the result of phi and store to the stack
    // 2. Replace the load of the variable with the top of the stack
    // 3. Add the variable to the phi statement in the successor blocks in CFG
    // 4. Recurse through the successor blocks in the dominator tree
    // 5. Pop the result of phi and store from the stack
    private fun renameRecursive(block: Block, visited: MutableSet<Block>) {
        if (visited.contains(block)) return
        visited.add(block)

        // 1 and 2: Replace the statements
        val variableRewriteCount = variables.keys.associateWith { 0 }.toMutableMap()
        phis[block]!!.forEach { (variable, phi) ->
            versions[variable]!!.push(phi.dest)
            variableRewriteCount[variable] = variableRewriteCount[variable]!! + 1
        }
        newBlock[block] = block.statements.mapNotNull { statement ->
            when (statement) {
                is AllocaStatement -> null
                is LoadStatement -> {
                    when (statement.src) {
                        is LocalVariable -> {
                            if (statement.src in variables) {
                                val newName = versions[statement.src]!!.peek()
                                replaceMap[statement.dest] = newName
                                null
                            } else {
                                statement.replace(replaceMap)
                            }
                        }

                        is GlobalVariable -> statement
                        else -> throw InternalError("Unknown variable type")
                    }
                }

                is StoreStatement -> {
                    when (statement.dest) {
                        is LocalVariable -> {
                            if (statement.dest in variables) {
                                versions[statement.dest]!!.push(statement.src)
                                variableRewriteCount[statement.dest] =
                                    variableRewriteCount[statement.dest]!! + 1
                                null
                            } else {
                                statement.replace(replaceMap)
                            }
                        }

                        is GlobalVariable -> statement.replace(replaceMap)
                        else -> throw InternalError("Unknown variable type")
                    }
                }

                else -> statement.replace(replaceMap)
            }
        }

        // 3: Add the variable to the phi statement in the successor blocks in CFG
        cfg.successors[block]?.forEach { successor ->
            phis[successor]!!.forEach {(variable, phi) ->
                phi.incoming.add(versions[variable]!!.peek() to block.label)
            }
        }

        // 4: Recurse through the successor blocks in the dominator tree
        dominance.dominatorTree.successors[block]?.forEach { successor ->
            renameRecursive(successor, visited)
        }

        // 5: Pop the result of phi and store from the stack
        variableRewriteCount.forEach { (variable, count) ->
            repeat(count) {
                versions[variable]!!.pop()
            }
        }
    }
}

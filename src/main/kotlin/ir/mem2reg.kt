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

// To remove the alloca/load/store statements, and generate the real SSA form.

class MemToReg(val function: GlobalFunction) {
    private val functionBody = function.body
        ?: throw InternalError("Function definition not found")
    // Simple remove:
    // 1. Remove the unused alloca statements.
    // 2. remove the variable that is stored only once.
    // 3. remove the variable that is loaded and stored in the same block.
    fun simplePromotion(blocks: List<Block>): List<Block> {
        class Status {
            val stored: MutableList<Argument> = mutableListOf()
            val loaded: MutableList<Variable> = mutableListOf()
            var directUse = false
            val inBlocks = mutableSetOf<String>()
            fun unused() = stored.isEmpty() && loaded.isEmpty() && !directUse
            fun storedOnce() = stored.size == 1 && !directUse
            fun inOneBlock() = inBlocks.size == 1 && !directUse
        }
        // Build the status map
        val statusMap = mutableMapOf<Variable, Status>()
        blocks.forEach { block ->
            block.statements.forEach { statement ->
                if (statement is AllocaStatement) {
                    statusMap[statement.property] = Status()
                }
            }
        }

        // Collect the status
        blocks.forEach { block ->
            block.statements.forEach { statement ->
                when (statement) {
                    is AllocaStatement -> {}
                    is LoadStatement -> {
                        val srcStatus = statusMap[statement.src]
                        if (srcStatus != null) {
                            srcStatus.loaded.add(statement.dest)
                            srcStatus.inBlocks.add(block.label)
                        }
                    }

                    is StoreStatement -> {
                        val destStatus = statusMap[statement.dest]
                        if (destStatus != null) {
                            destStatus.stored.add(statement.src)
                            destStatus.inBlocks.add(block.label)
                        }
                    }

                    else -> {
                        statement.use.forEach { variable ->
                            val status = statusMap[variable]
                            if (status != null) {
                                status.directUse = true
                            }
                        }
                    }
                }
            }
        }

        // Remove allocas that satisfies the conditions
        val toRemove: Set<Variable> = statusMap.toList()
            .filter { (_, status) -> status.unused() }.map { it.first }.toSet()
        val toReplace: Map<Variable, Argument> = statusMap.toList()
            .filter { (_, status) -> status.storedOnce() }
            .associate { it.first to it.second.stored.first() }
        val toReplaceInBlock: Set<Variable> = statusMap.toList()
            .filter { (_, status) -> status.inOneBlock() || !status.storedOnce() }
            .map { it.first }.toSet()
        return blocks.map { block ->
            val replaceMap = mutableMapOf<Variable, Argument>() // value -> value (for statements)
            val replaceInBlockMap = mutableMapOf<Variable, Argument>() // ptr -> value
            Block(
                block.label,
                block.statements.filter { statement ->
                    when (statement) {
                        is AllocaStatement -> !toRemove.contains(statement.property) &&
                                !toReplace.containsKey(statement.property) &&
                                !toReplaceInBlock.contains(statement.property)

                        is LoadStatement -> !toRemove.contains(statement.src)

                        is StoreStatement -> !toRemove.contains(statement.dest) &&
                                !toReplace.containsKey(statement.dest)

                        else -> true
                    }
                }.map { statement ->
                    when (statement) {
                        is AllocaStatement -> statement
                        is LoadStatement -> {
                            val replace1 = toReplace[statement.src]
                            if (replace1 != null) replaceMap[statement.dest] = replace1
                            if (toReplaceInBlock.contains(statement.src)) {
                                val replace2 = replaceInBlockMap[statement.src]
                                    ?: throw IRBuilderException("Load before store")
                                replaceMap[statement.dest] = replace2
                            }
                            statement
                        }
                        is StoreStatement -> {
                            if (toReplaceInBlock.contains(statement.dest)) {
                                replaceInBlockMap[statement.dest] = statement.src
                            }
                            statement
                        }
                        else -> statement.replace(replaceMap)
                    }
                }.filter { statement ->
                    when (statement) {
                        is LoadStatement -> !toReplaceInBlock.contains(statement.src) &&
                                !toRemove.contains(statement.src)

                        is StoreStatement -> !toReplaceInBlock.contains(statement.dest)
                        else -> true
                    }
                }.toMutableList()
            )
        }
    }
}

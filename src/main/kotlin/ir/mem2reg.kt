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
    fun simplePromotion(): List<Block> {
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
        functionBody.forEach { block ->
            block.statements.forEach { statement ->
                if (statement is LocalVariableDecl) {
                    statusMap[statement.property] = Status()
                }
            }
        }

        // Collect the status
        functionBody.forEach { block ->
            block.statements.forEach { statement ->
                when (statement) {
                    is LocalVariableDecl -> {}
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
        return functionBody.map { block ->
            val replaceMap = mutableMapOf<Variable, Argument>() // value -> value (for statements)
            val replaceInBlockMap = mutableMapOf<Variable, Argument>() // ptr -> value
            Block(
                block.label,
                block.statements.filter { statement ->
                    when (statement) {
                        is LocalVariableDecl -> !toRemove.contains(statement.property) &&
                                !toReplace.containsKey(statement.property) &&
                                !toReplaceInBlock.contains(statement.property)

                        is LoadStatement -> !toRemove.contains(statement.src)

                        is StoreStatement -> !toRemove.contains(statement.dest) &&
                                !toReplace.containsKey(statement.dest)

                        else -> true
                    }
                }.map { statement ->
                    when (statement) {
                        is LocalVariableDecl -> statement
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

class DominatorTree(val function: GlobalFunction) {
    private val dfNum = mutableMapOf<Block, Int>()
    private val blocks = mutableMapOf<Int, Block>()
    private val parent = mutableMapOf<Block, Block?>()
    private val ancestor = mutableMapOf<Block, Block>()
    // bucket[i] = { j | j in dom(i) && semi(j) = i }
    private val bucket = mutableMapOf<Block, MutableSet<Block>>()
    // semi[i] = min { dfNum[j] | j in pred(i) }
    private val semi = mutableMapOf<Block, Block>()
    // idom[i] = min { j in dom(i) | semi(j) = semi(i) }
    private val idom = mutableMapOf<Block, Block>()
    // samedom[i] = min { j in dom(i) | semi(j) = semi(i) }
    private val samedom = mutableMapOf<Block, Block>()
    // best[i] = min { semi[j] | j in bucket[i] }
    private val best = mutableMapOf<Block, Block>()
    private var n = 0

    private val functionBody = function.body
        ?: throw InternalError("Function definition not found")
    private val cfg = ControlFlow(function)

    init {
        dfs(null, functionBody.first())

        // Calculate the semi-dominance nodes
        for (i in n - 1 downTo 1) {
            val block = blocks[i]!!
            val parentNode = parent[block]!!
            var semiNode = parentNode
            cfg.predecessors[block]?.forEach { pred ->
                val newSemi =
                    if (dfNum[pred]!! < i) pred
                    else semi[ancestorWithSmallestSemi(pred)]!!
                if (dfNum[newSemi]!! < dfNum[semiNode]!!) semiNode = newSemi
            }
            semi[block] = semiNode
            link(parentNode, block)
            if (bucket.containsKey(semiNode)) {
                bucket[semiNode]!!.add(block)
            } else {
                bucket[semiNode] = mutableSetOf(block)
            }
            bucket[parentNode]?.forEach { node ->
                val ancestor = ancestorWithSmallestSemi(node)
                if (semi[ancestor] == semiNode) {
                    idom[node] = parentNode
                } else {
                    samedom[node] = ancestor
                }
            }
        }

        for (i in 1 until n) {
            val block = blocks[i]!!
            if (samedom.containsKey(block)) {
                idom[block] = idom[samedom[block]!!]!!
            }
        }
    }

    private fun dfs(parent: Block?, block: Block) {
        dfNum[block] = n
        blocks[n] = block
        this.parent[block] = parent
        n++
        cfg.successors[block]!!.forEach { dfs(block, it) }
    }

    // Find the ancestor with the smallest semi-dominance node
    private fun ancestorWithSmallestSemi(block: Block): Block {
        val anc = ancestor[block]!!
        if (ancestor.containsKey(anc)) {
            val bestAnc = ancestorWithSmallestSemi(anc)
            ancestor[block] = ancestor[anc]!!
            if (dfNum[semi[bestAnc]!!]!! < dfNum[semi[best[block]]!!]!!) {
                best[block] = bestAnc
            }
        }
        return best[block]!!
    }

    private fun link(ancestor: Block, block: Block) {
        this.ancestor[block] = ancestor
        best[block] = block
    }
}

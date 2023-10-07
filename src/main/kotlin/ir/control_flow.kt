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

class ControlFlow(body: List<Block>) {
    constructor(function: GlobalFunction) : this(function.body
        ?: throw InternalError("Attempting to get the control flow graph of a function without a body"))
    val blocks: Map<String, Block> = body.associateBy { it.label }
    val successors: Map<Block, List<Block>> =
        body.associateWith { block -> block.successors.map { blocks[it]!! } }
    val predecessors: Map<Block, List<Block>> = body.associateWith { mutableListOf<Block>() }
        .also { map ->
            successors.forEach {(block, successors) ->
                successors.forEach { successor ->
                    map[successor]!!.add(block)
                }
            }
        }.mapValues { (_, predecessors) -> predecessors.toList() }
}

fun removeUnusedBlocks(body: List<Block>): List<Block> {
    val controlFlow = ControlFlow(body)
    val visited = mutableSetOf<Block>()
    fun dfs(block: Block) {
        if (block in visited) return
        visited.add(block)
        controlFlow.successors[block]?.forEach { dfs(it) }
    }
    dfs(body[0])
    return body.filter { it in visited }
}

fun removeCriticalEdges(body: List<Block>): List<Block> {
    val controlFlow = ControlFlow(body)
    fun convert(block: Block): Block {
        val phiReplaceMap = mutableMapOf<String, String>()
        if (controlFlow.predecessors[block]!!.size > 1) {
            controlFlow.predecessors[block]!!.forEach { predecessor ->
                if (controlFlow.successors[predecessor]!!.size > 1) {
                    phiReplaceMap[predecessor.label] =
                        "critical_edge_${predecessor.label}_to_${block.label}"
                }
            }
        }
        val branchReplaceMap = mutableMapOf<String, String>()
        if (controlFlow.successors[block]!!.size > 1) {
            controlFlow.successors[block]!!.forEach { successor ->
                if (controlFlow.predecessors[successor]!!.size > 1) {
                    branchReplaceMap[successor.label] =
                        "critical_edge_${block.label}_to_${successor.label}"
                }
            }
        }

        return Block(
            block.label,
            block.statements.map { statement ->
                when (statement) {
                    is PhiStatement -> statement.replaceLabel(phiReplaceMap)
                    is BranchStatement -> statement.replaceLabel(branchReplaceMap)
                    else -> statement
                }
            }.toMutableList()
        )
    }
    return body.flatMap { block ->
        val successors = controlFlow.successors[block]!!
        if (successors.size <= 1) listOf(convert(block))
        else listOf(convert(block)) + successors.mapNotNull { successor ->
            if (controlFlow.predecessors[successor]!!.size <= 1) null
            else Block(
                "critical_edge_${block.label}_to_${successor.label}",
                mutableListOf(
                    BranchStatement(successor.label)
                )
            )
        }
    }
}

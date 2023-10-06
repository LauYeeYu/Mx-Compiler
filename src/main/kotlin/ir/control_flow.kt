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
    var newBody = body
    var update = true
    while (update) {
        update = false
        val cfg = ControlFlow(newBody)
        newBody = newBody.mapNotNull { block ->
            if (cfg.predecessors[block]!!.isNotEmpty() || block.label == "entry") {
                block
            } else {
                update = true
                null
            }
        }
    }
    return newBody
}

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

import java.util.LinkedList

fun liveVariableAnalysis(root: Root) {
    root.globalFunctions.forEach {
        val body = it.body ?: return@forEach
        LiveVariableAnalysis(body)
    }
}

class LiveVariableAnalysis(body: List<Block>) {
    private val controlFlow = ControlFlow(body)
    private val blockLiveIn = body
        .associateWith { block -> block.uses.toSet() }.toMutableMap()
    private val blockLiveOut = body.associateWith { block ->
        (controlFlow.successors[block]!!.map { successor -> successor.uses }
            .reduceOrNull { acc, successorUse -> acc union successorUse }
            ?: setOf()).toSet()
    }.toMutableMap()
    val exits = body.filter { controlFlow.successors[it]!!.isEmpty() }

    // Update the liveIn and liveOut for blocks
    init {
        var changed = true
        while (changed) {
            changed = false
            val queue = LinkedList(exits)
            val visited = exits.toMutableSet()
            while (queue.isNotEmpty()) {
                val block = queue.poll() ?: throw InternalError("Queue is empty")
                val newLiveOut = (controlFlow.successors[block]!!
                    .map { successor -> blockLiveIn[successor]!! }
                    .reduceOrNull { acc, successorLiveIn -> acc.toSet() union successorLiveIn }
                    ?: setOf())

                // No need to check liveIn because it will change only when liveOut changes
                if (newLiveOut != blockLiveOut[block]) {
                    changed = true
                    blockLiveOut[block] = newLiveOut
                    blockLiveIn[block] = block.uses union (newLiveOut subtract block.defs)
                    controlFlow.predecessors[block]!!.forEach { predecessor ->
                        if (predecessor !in visited) {
                            queue.add(predecessor)
                            visited.add(predecessor)
                        }
                    }
                }
            }
        }
    }

    // Update the liveIn and liveOut for statements
    init {
        body.forEach { block ->
            val last = block.statements.last()
            last.liveOut.clear()
            last.liveOut += blockLiveOut[block]!!
            last.liveIn.clear()
            last.liveIn += last.use union (last.liveOut subtract last.def)
            block.statements.asReversed().zipWithNext().forEach { (successor, statement) ->
                statement.liveOut.clear()
                statement.liveOut += successor.liveIn
                statement.liveIn.clear()
                statement.liveIn += statement.use union (statement.liveOut subtract statement.def)
            }
        }
    }
}

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

class Dominance(val body: List<Block>, private val controlFlow: ControlFlow) {
    constructor(body: List<Block>) : this(body, ControlFlow(body))
    constructor(function: GlobalFunction) : this(
        function.body
            ?: throw InternalError("Attempting to get the dominance information of a function without a body"),
        ControlFlow(function)
    )
    val dominateSets = getDominateSet()
    val immediateDominator: Map<Block, Block?> = dominateSets.map { (block, dominateSet) ->
        block to dominateSet.reduceOrNull { immediateDom, dominatorBlock ->
            if (dominateSets[dominatorBlock]!!.size == dominateSets[block]!!.size - 1) dominatorBlock
            else immediateDom
        }
    }.toMap()
    val dominatorTree = DominatorTree(immediateDominator)
    val dominanceFrontier = getDominanceFrontier()

    private fun getDominateSet(): Map<Block, Set<Block>> {
        val bodySet = body.toSet()
        val dominateSetMap = body.associateWith { bodySet }.toMutableMap()
        var updated = true
        while (updated) {
            updated = false
            body.forEach { block ->
                val dominateSet = dominateSetMap[block]
                    ?: throw InternalError("Block $block not found")
                val predecessors = controlFlow.predecessors[block]
                    ?: throw InternalError("Block $block does not exist in the control flow graph")
                if (predecessors.isNotEmpty()) {
                    var predDomInCommon = bodySet
                    predecessors.forEach { pred ->
                        predDomInCommon = predDomInCommon intersect dominateSetMap[pred]!!.toSet()
                    }
                    val newDominateSet = predDomInCommon union setOf(block)
                    if (newDominateSet != dominateSet) {
                        updated = true
                        dominateSetMap[block] = newDominateSet
                    }
                }
            }
        }
        return dominateSetMap
    }

    private fun getDominanceFrontier(): Map<Block, Set<Block>> {
        val dominanceFrontier: MutableMap<Block, MutableSet<Block>> =
            body.associateWith { mutableSetOf<Block>() }.toMutableMap()
        body.forEach { block ->
            val predecessors = controlFlow.predecessors[block]
                ?: throw InternalError("Block $block does not exist in the control flow graph")
            predecessors.forEach { pred ->
                (dominateSets[pred]!! - (dominateSets[block]!! - block)).forEach { frontier ->
                    dominanceFrontier[frontier]!!.add(block)
                }
            }
        }
        return dominanceFrontier
    }
}

class DominatorTree(val immediateDominator: Map<Block, Block?>)

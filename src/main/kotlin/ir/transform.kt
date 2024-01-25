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

package ir

abstract class Transformer {
    open fun transform(root: Root) = Root(
        classes = root.classes,
        variables = root.variables,
        globalFunctions = root.globalFunctions.map { transformFunction(it) },
    )
    open fun transformFunction(function: GlobalFunction): GlobalFunction = function
}

class MemToRegTransformer : Transformer() {
    override fun transformFunction(function: GlobalFunction): GlobalFunction =
        when (function.body) {
            null -> function
            else -> MemToReg(function).promote()
        }
}

class MoveSafeTransformer : Transformer() {
    override fun transformFunction(function: GlobalFunction): GlobalFunction =
        when (function.body) {
            null -> function
            else -> GlobalFunction(
                name = function.name,
                parameters = function.parameters,
                returnType = function.returnType,
                body = removeCriticalEdges(function.body),
                moveSafe = true,
            )
        }
}

class RemovePhisTransformer : Transformer() {
    override fun transformFunction(function: GlobalFunction): GlobalFunction =
        when (function.body) {
            null -> function
            else -> GlobalFunction(
                name = function.name,
                parameters = function.parameters,
                returnType = function.returnType,
                body = removePhis(function.body),
                moveSafe = function.moveSafe,
            )
        }

    private fun removePhis(body: List<Block>): List<Block> {
        val cfg = ControlFlow(body)
        val moves = mutableMapOf<Block, PackedMoveStatement>()
        body.forEach { block ->
            val moveBuilder = mutableMapOf<Block, MutableList<Pair<Variable, Argument>>>()
            block.statements.filterIsInstance<PhiStatement>().forEach { statement->
                statement.incoming.forEach { (argument, blockName) ->
                    moveBuilder.getOrPut(cfg.blocks[blockName]!!) { mutableListOf() }
                        .add(statement.dest to argument)
                }
            }
            moveBuilder.forEach { (block, move) ->
                if (moves[block] == null) {
                    throw InternalError("Critical edge not removed")
                }
                moves[block] = PackedMoveStatement(move)
            }
        }
        return body.map { block ->
            Block(
                label = block.label,
                statements = block.statements.filterNot { it is PhiStatement }
                    .toMutableList().apply {
                        moves[block]?.let { add(this.size - 1, it) }
                    },
            )
        }
    }
}

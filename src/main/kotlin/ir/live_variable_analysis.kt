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

fun liveVariableAnalysis(function: GlobalFunction) {
    val blockMap = function.blockMap
    // Set successor
    blockMap.values.forEach { block ->
        block.statements.forEach { statement ->
            statement.successor.clear()
        }
    }
    blockMap.forEach { it.value.setSuccessor(blockMap) }

    // Set active in and active out
    var changed = true
    while (changed) {
        changed = false
        blockMap.values.reversed().forEach { block ->
            block.statements.reversed().forEach { statement ->
                val oldInSize = statement.liveIn.size
                val oldOutSize = statement.liveOut.size
                statement.liveIn.addAll(statement.use)
                statement.liveIn.addAll(statement.liveOut - statement.def)
                statement.successor.forEach { successor ->
                    statement.liveOut.addAll(successor.liveIn)
                }
                if (oldInSize != statement.liveIn.size || oldOutSize != statement.liveOut.size) {
                    changed = true
                }
            }
        }
    }
}

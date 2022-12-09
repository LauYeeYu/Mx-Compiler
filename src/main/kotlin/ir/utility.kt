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

fun escapeStringLiteralToIr(source: String): String {
    val sb = StringBuilder()
    for (c in source) {
        when (c) {
            '\n' -> sb.append("\\0A")
            '\\' -> sb.append("\\\\")
            '"' -> sb.append("\\22")
            else -> sb.append(c)
        }
    }
    return sb.toString()
}

fun isCompareOperator(op: ast.BinaryOperator): Boolean = when (op) {
    ast.BinaryOperator.EQUAL,
    ast.BinaryOperator.NOT_EQUAL,
    ast.BinaryOperator.LESS_THAN,
    ast.BinaryOperator.LESS_THAN_OR_EQUAL,
    ast.BinaryOperator.GREATER_THAN,
    ast.BinaryOperator.GREATER_THAN_OR_EQUAL -> true
    else -> false
}

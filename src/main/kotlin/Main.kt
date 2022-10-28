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

import ast.parse
import ast.Source
import ast.buildAst
import exceptions.MxException
import typecheck.checkAndRecord
import kotlin.system.exitProcess

fun useSystemInAsSource() {
    try {
        val source = Source("input", System.`in`.readAllBytes().decodeToString())
        val program = parse(source)
        val ast = buildAst(program)
        val environment = checkAndRecord(ast)
        if (environment.functionAlikeBindings["main"] == null) {
            throw MxException("No main function", null)
        }
    } catch (e: MxException) {
        System.err.println(e.toString())
        exitProcess(1)
    }
}

fun main(args: Array<String>) {
    useSystemInAsSource()
}

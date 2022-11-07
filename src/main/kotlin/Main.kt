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
import java.io.File
import kotlin.system.exitProcess

enum class Mode {
    NONE,
    SYNTAX_ONLY,
    IR,
    ASM,
    BINARY,
}

// Just for online judge
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

fun processSource(source: Source, mode: Mode, outputFile: String?) {
    try {
        // Syntax check (parse, build AST, and type check)
        val program = parse(source)
        val ast = buildAst(program)
        val environment = checkAndRecord(ast)
        if (mode == Mode.SYNTAX_ONLY) return
    } catch (e: MxException) {
        System.err.println(e.toString())
        exitProcess(1)
    }
}

fun main(args: Array<String>) {
    var hasInput: Boolean = false
    var mode: Mode = Mode.NONE
    val sourceList: MutableList<Source> = mutableListOf()
    var outputFileName: String? = null
    var i = 1
    while (i < args.size) {
        if (args[i] == "-h" || args[i] == "--help") {
            printHelp()
            exitProcess(0)
        } else if (args[i] == "-v" || args[i] == "--version") {
            printVersion()
            exitProcess(0)
        } else if (args[i] == "-fsyntax-only") {
            if (mode != Mode.NONE) {
                System.err.println("Multiple modes specified")
                exitProcess(1)
            }
            mode = Mode.SYNTAX_ONLY
        } else if (args[i] == "-emit-llvm") {
            if (mode != Mode.NONE) {
                System.err.println("Multiple modes specified")
                exitProcess(1)
            }
            mode = Mode.IR
        } else if (args[i] == "-S") {
            if (mode != Mode.NONE) {
                System.err.println("Multiple modes specified")
                exitProcess(1)
            }
            mode = Mode.ASM
        } else if (args[i] == "-c") {
            if (mode != Mode.NONE) {
                System.err.println("Multiple modes specified")
                exitProcess(1)
            }
            mode = Mode.BINARY
        } else if (args[i] == "-o") {
            if (i + 1 >= args.size) {
                System.err.println("No output file specified")
                exitProcess(1)
            }
            i++
            if (args[i].startsWith("-")) {
                System.err.println("No output file specified")
                exitProcess(1)
            }
            outputFileName = args[i]
        } else {
            hasInput = true
            sourceList += Source(args[i], File(args[i]).readText())
        }
        i++
    }
    if (mode == Mode.NONE) mode = Mode.BINARY
    if (!hasInput) {
        processSource(Source("input", System.`in`.readAllBytes().decodeToString()), mode, outputFileName)
    } else {
        for (source in sourceList) {
            processSource(source, mode, outputFileName)
        }
    }
}

fun printVersion() {
    println("Mx-Compiler 0.1.0")
}

fun printHelp() {
    println("Usage: mxc [options] [filename] ...")
    println("Options:")
    println("  -h, --help     Show help message")
    println("  -v, --version  Show version information")
    println("  -fsyntax-only  Only check syntax")
    println("  -emit-llvm     Check syntax and generate LLVM IR")
    println("  -S             Check syntax and generate assembly file")
    println("  -c             Check syntax and generate object file")
}

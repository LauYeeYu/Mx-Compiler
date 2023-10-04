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

import asm.naiveAllocation
import ast.parse
import ast.Source
import ast.buildAst
import exceptions.MxException
import ir.MemToRegTransformer
import ir.buildIr
import typecheck.checkAndRecord
import java.io.File
import kotlin.system.exitProcess

enum class Mode {
    NONE,
    SYNTAX_ONLY,
    IR,
    ASM,
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
        val ir = buildIr(ast)
        val naiveAsm = naiveAllocation(ir, "test.mx")
        File("output.s").writeText(naiveAsm.toString())
        // print the builtin functions
        val builtin = ir.javaClass.classLoader.getResource("builtin.s")!!.readBytes()
        File("builtin.s").writeBytes(builtin)
    } catch (e: MxException) {
        System.err.println(e.toString())
        exitProcess(1)
    }
}

fun processSource(config: Config) {
    try {
        // Syntax check (parse, build AST, and type check)
        val source = Source(config.inputFileName, config.input.readAllBytes().decodeToString())
        val program = parse(source)
        val ast = buildAst(program)
        val environment = checkAndRecord(ast)
        if (environment.functionAlikeBindings["main"] == null) {
            throw MxException("No main function", null)
        }
        if (config.compileTask == Config.CompileTask.SYNTAX) return
        val ir = buildIr(ast).transform(MemToRegTransformer())
        if (config.compileTask == Config.CompileTask.IR) {
            config.output.write(ir.toString().encodeToByteArray())
            return
        }
        val asm = naiveAllocation(ir, source.fileName)
        config.output.write(asm.toString().encodeToByteArray())
    } catch (e: MxException) {
        System.err.println(e.toString())
        exitProcess(1)
    }
}

fun main(args: Array<String>) {
    val config = Config(args)
    when (config.compileTask) {
        Config.CompileTask.HELP -> printHelp()
        Config.CompileTask.VERSION -> printVersion()
        Config.CompileTask.SYNTAX, Config.CompileTask.ASM, Config.CompileTask.IR -> processSource(config)
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
}

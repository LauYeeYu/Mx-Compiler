// Mx-Compiler - a compiler implementation for Mx
// Copyright (C) 2023 Lau Yee-Yu
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

import java.io.File
import java.io.InputStream
import java.io.OutputStream

class Config(args: Array<String>) {
    enum class CompileTask {
        SYNTAX, // With -fsyntax-only option
        IR, // With -emit-llvm option
        ASM, // With -S option
        HELP, // With -h or --help option
        VERSION; // With -v or --version option

        fun noInput()  = this == HELP || this == VERSION
        fun noOutput() = this == HELP || this == VERSION || this == SYNTAX
    }

    val compileTask: CompileTask
    val output: OutputStream
    val input: InputStream
    val inputFileName: String

    init {
        var flag: CompileTask? = null
        var outputFile: String? = null
        var inputFile: String? = null
        var streamMode = false
        var skip = false
        for (index in 0..args.lastIndex) {
            if (skip) {
                continue
            }
            when (args[index]) {
                "-fsyntax-only" -> {
                    if (flag != null) {
                        throw IllegalArgumentException("Cannot specify multiple compile tasks")
                    }
                    flag = CompileTask.SYNTAX
                }

                "-emit-llvm" -> {
                    when (flag) {
                        null, CompileTask.ASM -> flag = CompileTask.IR
                        CompileTask.SYNTAX, CompileTask.IR, CompileTask.HELP, CompileTask.VERSION ->
                            throw IllegalArgumentException("Cannot specify multiple compile tasks")
                    }
                }

                "-S" -> {
                    when (flag) {
                        null -> flag = CompileTask.ASM
                        CompileTask.SYNTAX, CompileTask.ASM, CompileTask.HELP, CompileTask.VERSION -> {
                            throw IllegalArgumentException("Cannot specify multiple compile tasks")
                        }
                        CompileTask.IR -> {}
                    }
                }

                "-o" -> {
                    if (flag != null && (flag == CompileTask.HELP || flag == CompileTask.SYNTAX)) {
                        throw IllegalArgumentException("Cannot specify output file name with output option")
                    }
                    if (index == args.lastIndex || streamMode) {
                        throw IllegalArgumentException("Output file name not specified")
                    }
                    if (outputFile != null) {
                        throw IllegalArgumentException("Cannot specify multiple output file names")
                    }
                    outputFile = args[index + 1]
                    skip = true
                }

                "-" -> {
                    if (outputFile != null) {
                        throw IllegalArgumentException("Cannot specify multiple output file names")
                    }
                    if (inputFile != null) {
                        throw IllegalArgumentException("Cannot specify multiple input file names")
                    }
                    if (streamMode) {
                        throw IllegalArgumentException("Multiple stream modes specified")
                    }
                    streamMode = true
                }

                "-h", "--help" -> {
                    if (flag != null) {
                        throw IllegalArgumentException("Cannot specify multiple compile tasks")
                    }
                    flag = CompileTask.HELP
                }

                "-v", "--version" -> {
                    if (flag != null) {
                        throw IllegalArgumentException("Cannot specify multiple compile tasks")
                    }
                    flag = CompileTask.VERSION
                }

                else -> { // Input file name
                    if (args[index].startsWith("-")) {
                        throw IllegalArgumentException("Unknown option ${args[index]}")
                    }
                    if (inputFile != null) {
                        throw IllegalArgumentException("Cannot specify multiple input file names")
                    }
                    inputFile = args[index]
                }
            }
        }
        compileTask = flag ?: throw IllegalArgumentException("No compile task specified")
        input = if (streamMode || compileTask.noInput()) {
            System.`in`
        } else {
            if (inputFile == null) {
                throw IllegalArgumentException("Input file name not specified")
            }
            val file = File(inputFile)
            if (!file.exists()) {
                throw IllegalArgumentException("Input file $inputFile does not exist")
            }
            file.inputStream()
        }
        inputFileName = inputFile ?: "<stdin>"
        output = if (streamMode || compileTask.noOutput()) {
            System.out
        } else {
            if (outputFile == null) {
                outputFile = inputFile!!.removeSuffix(".mx") + when(compileTask) {
                    CompileTask.IR -> ".ll"
                    CompileTask.ASM -> ".s"
                    else -> throw InternalError("Unreachable")
                }
            }
            File(outputFile).outputStream()
        }
    }
}
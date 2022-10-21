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

package ast

import kotlin.system.exitProcess
import org.antlr.v4.runtime.*

import MxLexer
import MxParser
import MxParser.TranslationUnitContext

class ParseResult(val source: Source, val cst: TranslationUnitContext)

fun parse(source: Source): ParseResult {
   var hasErrors = false

    class ExitListener : BaseErrorListener() {
        override fun syntaxError(
            recognizer: Recognizer<*, *>?,
            offendingSymbol: Any?,
            line: Int,
            charPositionInLine: Int,
            msg: String?,
            e: RecognitionException?,
        ) {
            System.err.println("In input: $line: $charPositionInLine (at $offendingSymbol): $msg")
            hasErrors = true
        }
    }

    val listener = ExitListener()
    val input = CharStreams.fromString(source.sourceCode)
    val lexer = MxLexer(input)
    lexer.removeErrorListeners()
    lexer.addErrorListener(listener)
    val parser = MxParser(CommonTokenStream(lexer))
    parser.removeErrorListeners()
    parser.addErrorListener(listener)
    val root = parser.translationUnit()
    if (hasErrors) {
        exitProcess(1)
    }

    return ParseResult(source, root)
}

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

import ast.AstNode
import exceptions.EnvironmentException
import exceptions.IRBuilderException

fun buildIR(astNode: AstNode): Root = IR().buildRoot(astNode)

class IR {
    fun buildRoot(astNode: AstNode): Root {
        if (astNode !is ast.TranslateUnit) {
            throw IRBuilderException("The AST node in buildRoot is not a root node")
        }
        if (astNode.environment == null) {
            throw EnvironmentException("The AST node in buildRoot has no environment")
        }
        return TODO()
    }
}

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
import ast.GlobalElement
import ast.VariablesDeclaration
import exceptions.EnvironmentException
import exceptions.IRBuilderException

fun buildIR(astNode: AstNode): Root = IR(astNode).buildRoot()

class IR(private val astNode: AstNode) {

    fun buildRoot(): Root {
        if (astNode !is ast.TranslateUnit) {
            throw IRBuilderException("The AST node in buildRoot is not a root node")
        }
        if (astNode.environment == null) {
            throw EnvironmentException("The AST node in buildRoot has no environment")
        }
        return Root(
            astNode.content.filterIsInstance<ast.Class>().map { buildClass(it) },
            buildGlobalVariableList(astNode.content),
            buildInitFunctionList(astNode.content),
            astNode.content.filterIsInstance<ast.Function>().map { buildFunction(it) },
        )
    }

    private fun buildGlobalVariableList(sourceList: List<ast.GlobalElement>): List<GlobalVariable> {
        val result = mutableListOf<GlobalVariable>()
        for (variables in sourceList) {
            if (variables is ast.VariablesDeclaration) {
                for (variable in variables.variables) {
                    result.add(buildGlobalVariable(variable, variables.type))
                }
            }
        }
        return result
    }

    private fun buildInitFunctionList(sourceList: List<ast.GlobalElement>): GlobalFunction {
        val globalInit: MutableList<Block> = mutableListOf()
        for (element in sourceList) {
            if (element is ast.VariablesDeclaration) {
                for (variable in element.variables) {
                    if (variable.body != null) {
                        //TODO
                    }
                }
            }
        }
        return GlobalFunction(
            "__global_init",
            PrimitiveType(TypeProperty.void),
            listOf(),
            globalInit,
        )
    }

    private fun buildClass(astNode: ast.Class): GlobalClass {
        if (astNode.environment == null) {
            throw EnvironmentException("The AST node in buildClass has no environment")
        }
        val memberList = mutableListOf<Type>()
        for (element in astNode.body) {
            if (element is ast.VariablesDeclaration) {
                for (variable in element.variables) {
                    memberList.add(irType(element.type))
                }
            }
        }
        return GlobalClass(astNode.name, memberList)
    }

    private fun irType(astType: ast.Type): Type {
        return when (astType) {
            is ast.VoidType   -> PrimitiveType(TypeProperty.void)
            is ast.BoolType   -> PrimitiveType(TypeProperty.i8)
            is ast.IntType    -> PrimitiveType(TypeProperty.i32)
            is ast.StringType -> PrimitiveType(TypeProperty.ptr)
            is ast.ArrayType  -> PrimitiveType(TypeProperty.ptr)
            is ast.ClassType  -> PrimitiveType(TypeProperty.ptr)
            else -> throw IRBuilderException("Unknown type in irType")
        }
    }

    private fun buildGlobalVariable(
        variable: ast.VariableDeclaration,
        astType: ast.Type
    ): GlobalVariable {
        if (variable.environment == null) {
            throw EnvironmentException("The AST node in buildGlobalVariable has no environment")
        }
        return GlobalVariable(variable.name, irType(astType))
    }

    fun buildInitFunction(astNode: ast.GlobalElement): GlobalFunction {
        return TODO()
    }

    fun buildFunction(astNode: ast.Function): GlobalFunction {
        if (astNode.environment == null) {
            throw EnvironmentException("The AST node in buildFunction has no environment")
        }
        return TODO()
    }
}

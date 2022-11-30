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

import ast.*
import exceptions.*
import typecheck.*

fun buildIR(astNode: AstNode): Root = IR(astNode).buildRoot()

class IR(private val root: AstNode, private val parent: IR? = null) {
    private var unnamedVariableCount = 0
    private var unnamedStringLiteralCount = 0
    private val globalVariableDecl = mutableListOf<GlobalDecl>()

    fun buildRoot(): Root {
        if (root !is ast.TranslateUnit) {
            throw IRBuilderException("The AST node in buildRoot is not a root node")
        }
        if (root.environment == null) {
            throw EnvironmentException("The AST node in buildRoot has no environment")
        }
        val classList = root.content.filterIsInstance<ast.Class>().map { buildClass(it) }
        val globalFunctionList = root.content.filterIsInstance<ast.Function>().map { buildFunction(it) }
        val initList = buildInitFunctionList(root.content)
        val variableList = buildGlobalVariableList(root.content)
        return Root(
            classes         = classList,
            variables       = variableList,
            initFunction    = initList,
            globalFunctions = globalFunctionList,
        )
    }

    private fun buildGlobalVariableList(sourceList: List<ast.GlobalElement>): List<GlobalDecl> {
        for (variables in sourceList) {
            if (variables is ast.VariablesDeclaration) {
                for (variable in variables.variables) {
                    globalVariableDecl.add(buildGlobalVariable(variable, variables.type))
                }
            }
        }
        return globalVariableDecl
    }

    private fun buildInitFunctionList(sourceList: List<ast.GlobalElement>): GlobalFunction {
        val globalInit: MutableList<Statement> = mutableListOf()
        for (element in sourceList) {
            if (element is ast.VariablesDeclaration) {
                for (variable in element.variables) {
                    if (variable.body != null) {
                        variableDeclInit(variable, irType(element.type), globalInit)
                    }
                }
            }
        }
        return GlobalFunction(
            "__global_init",
            PrimitiveType(TypeProperty.void),
            listOf(),
            mutableListOf(Block(0, globalInit)),
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

    private fun irType(astType: ast.Type): Type = when (astType) {
        is ast.VoidType -> PrimitiveType(TypeProperty.void)
        is ast.BoolType -> PrimitiveType(TypeProperty.i8)
        is ast.IntType -> PrimitiveType(TypeProperty.i32)
        is ast.StringType -> PrimitiveType(TypeProperty.ptr)
        is ast.ArrayType -> PrimitiveType(TypeProperty.ptr)
        is ast.ClassType -> PrimitiveType(TypeProperty.ptr)
        else -> throw IRBuilderException("Unknown type in irType")
    }

    private fun irType(internalType: MxType): Type = when (internalType) {
        is MxVoidType -> PrimitiveType(TypeProperty.void)
        is MxBoolType -> PrimitiveType(TypeProperty.i8)
        is MxIntType -> PrimitiveType(TypeProperty.i32)
        is MxStringType -> PrimitiveType(TypeProperty.ptr)
        is MxNullType -> PrimitiveType(TypeProperty.ptr)
        is MxArrayType -> PrimitiveType(TypeProperty.ptr)
        is MxClassType -> PrimitiveType(TypeProperty.ptr)
        else -> throw IRBuilderException("Unknown type in irType")
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

    private fun buildFunction(astNode: ast.Function): GlobalFunction {
        if (astNode.environment == null) {
            throw EnvironmentException("The AST node in buildFunction has no environment")
        }
        return TODO()
    }

    private fun variableDeclInit(
        variable: ast.VariableDeclaration,
        type    : Type,
        block   : MutableList<Statement>, // variable initializing statement will not have a branch
    ) {
        if (variable.body == null) return
        val returnValue = addExpression(variable.body, block)
        if (returnValue is VoidResult) {
            throw IRBuilderException("A variable is assigned with a void expression")
        }
        val binding = variable.binding ?: throw EnvironmentException("The variable has no binding")
        when (returnValue) {
            is TempVariable -> block.add(
                StoreStatement(
                    dest = when (binding.irInfo.isLocal) {
                        true -> LocalVariable(variable.name, type)
                        false -> GlobalVariable(variable.name, type)
                    },
                    src = LocalVariable(returnValue.toString(), type),
                )
            )
            is ConstExpression -> StoreImmediateStatement(
                dest = when (binding.irInfo.isLocal) {
                    true -> LocalVariable(variable.name, type)
                    false -> GlobalVariable(variable.name, type)
                },
                type = type,
                src = returnValue.value,
            )
        }
    }

    abstract class ExpressionResult
    class VoidResult : ExpressionResult()
    class ConstExpression(val value: Int) : ExpressionResult()
    class TempVariable(val name: Int) : ExpressionResult()

    // Add the expression to the block. The return value indicates the number
    // of variable to use in the block. If there is no return value, the
    // function will return -1.
    private fun addExpression(
        expr : ast.Expression,
        block: MutableList<Statement>,
    ): ExpressionResult = when (expr) {
        is ast.Object              -> addExpression(expr, block)
        is StringLiteral           -> addExpression(expr, block)
        is IntegerLiteral          -> addExpression(expr, block)
        is BooleanLiteral          -> addExpression(expr, block)
        is NullLiteral             -> addExpression(expr, block)
        is ThisLiteral             -> addExpression(expr, block)
        is MemberVariableAccess    -> addExpression(expr, block)
        is MemberFunctionAccess    -> addExpression(expr, block)
        is ArrayExpression         -> addExpression(expr, block)
        is PrefixUpdateExpression  -> addExpression(expr, block)
        is FunctionCall            -> addExpression(expr, block)
        is LambdaCall              -> addExpression(expr, block)
        is LambdaExpression        -> addExpression(expr, block)
        is NewExpression           -> addExpression(expr, block)
        is PostfixUpdateExpression -> addExpression(expr, block)
        is UnaryExpression         -> addExpression(expr, block)
        is BinaryExpression        -> addExpression(expr, block)
        is AssignExpression        -> addExpression(expr, block)
        else -> throw IRBuilderException("Unknown expression in addExpression")
    }

    private fun addExpression(expr: ast.Object,
                              block: MutableList<Statement>): ExpressionResult {
        if (expr.binding == null) {
            throw EnvironmentException("The AST node in addExpression has no binding")
        }
        val type = irType(expr.binding!!.type)
        val dest = unnamedVariableCount
        unnamedVariableCount++
        block.add(
            LoadStatement(
                dest = LocalVariable(dest.toString(), type),
                src  = when (expr.binding!!.irInfo.isLocal) {
                    true  -> LocalVariable(expr.binding!!.name, type)
                    false -> GlobalVariable(expr.binding!!.name, type)
                },
            )
        )
        return TempVariable(dest)
    }

    private fun addExpression(expr: StringLiteral,
                              block: MutableList<Statement>): ExpressionResult {
        addStringLiteral("__$unnamedStringLiteralCount", expr)
        val dest = unnamedVariableCount
        unnamedVariableCount++
        block.add(
            LoadStatement(
                dest = LocalVariable(dest.toString(), PrimitiveType(TypeProperty.ptr)),
                src  = GlobalVariable("__$unnamedStringLiteralCount", PrimitiveType(TypeProperty.ptr)),
            )
        )
        return TempVariable(block.size - 1)
    }

    private fun addExpression(
        expr: IntegerLiteral,
        block: MutableList<Statement>
    ): ExpressionResult = ConstExpression(expr.value)

    private fun addStatement(
        statement   : ast.Statement,
        blockList   : MutableList<Block>,
        currentBlock: Int,
    ) {
        when (statement) {
            is ast.BlockStatement          -> addStatement(statement, blockList, currentBlock)
            is ast.ExpressionStatement     -> addStatement(statement, blockList, currentBlock)
            is ast.BranchStatement         -> addStatement(statement, blockList, currentBlock)
            is ast.WhileStatement          -> addStatement(statement, blockList, currentBlock)
            is ast.ForExpressionStatement  -> addStatement(statement, blockList, currentBlock)
            is ast.ForDeclarationStatement -> addStatement(statement, blockList, currentBlock)
            is ast.ContinueStatement       -> addStatement(statement, blockList, currentBlock)
            is ast.BreakStatement          -> addStatement(statement, blockList, currentBlock)
            is ast.ReturnStatement         -> addStatement(statement, blockList, currentBlock)
            is ast.VariablesDeclaration    -> addStatement(statement, blockList, currentBlock)
            is ast.EmptyStatement          -> {}
            else -> throw IRBuilderException("Unknown statement in addStatement")
        }
    }

    private fun addStringLiteral(name: String, string: StringLiteral) {
        if (parent != null) {
            parent.addStringLiteral(name, string)
        } else {
            globalVariableDecl.add(StringLiteralDecl(name, string.value))
        }
    }
}

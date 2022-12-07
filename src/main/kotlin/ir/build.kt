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

fun buildIr(astNode: AstNode): Root = IR(astNode).buildRoot()

class IR(private val root: AstNode, private val parent: IR? = null) {
    private var unnamedVariableCount = 0
    private var unnamedStringLiteralCount = 0
    private val globalVariableDecl = mutableListOf<GlobalDecl>()
    private val classes = mutableMapOf<String, GlobalClass>()
    private val globalFunctions = mutableMapOf<String, GlobalFunction>()

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
        val globalInit: MutableList<Block> = mutableListOf()
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
            globalInit,
        )
    }

    private fun buildClass(astNode: ast.Class): GlobalClass {
        if (astNode.environment == null) {
            throw EnvironmentException("The AST node in buildClass has no environment")
        }
        val memberList = mutableListOf<Type>()
        val nameMap = mutableMapOf<String, Int>()
        for (element in astNode.body) {
            if (element is ast.VariablesDeclaration) {
                for (variable in element.variables) {
                    memberList.add(irType(element.type))
                    nameMap[variable.name] = memberList.size - 1
                }
            }
        }
        val returnClass = GlobalClass(ClassType(astNode.name, memberList), nameMap)
        classes[astNode.name] = returnClass
        return returnClass
    }

    private fun irType(astType: ast.Type): Type = when (astType) {
        is ast.VoidType   -> PrimitiveType(TypeProperty.void)
        is ast.BoolType   -> PrimitiveType(TypeProperty.i8)
        is ast.IntType    -> PrimitiveType(TypeProperty.i32)
        is ast.StringType -> PrimitiveType(TypeProperty.ptr)
        is ast.ArrayType  -> PrimitiveType(TypeProperty.ptr)
        is ast.ClassType  -> PrimitiveType(TypeProperty.ptr)
        else -> throw IRBuilderException("Unknown type in irType")
    }

    private fun irType(internalType: MxType): Type = when (internalType) {
        is MxVoidType   -> PrimitiveType(TypeProperty.void)
        is MxBoolType   -> PrimitiveType(TypeProperty.i8)
        is MxIntType    -> PrimitiveType(TypeProperty.i32)
        is MxStringType -> PrimitiveType(TypeProperty.ptr)
        is MxNullType   -> PrimitiveType(TypeProperty.ptr)
        is MxArrayType  -> PrimitiveType(TypeProperty.ptr)
        is MxClassType  -> PrimitiveType(TypeProperty.ptr)
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
        blocks  : MutableList<Block>, // variable initializing statement will not have a branch
    ) {
        if (variable.body == null) return
        val returnValue = addExpression(variable.body, blocks, ExpectedState.VALUE)
        if (returnValue is VoidResult) {
            throw IRBuilderException("A variable is assigned with a void expression")
        }
        val binding = variable.binding ?: throw EnvironmentException("The variable has no binding")
        val dest = when (binding.irInfo.isLocal) {
            true -> LocalVariable(variable.name, type)
            false -> GlobalVariable(variable.name, type)
        }
        blocks.last().statements.add(
            StoreStatement(
                dest = dest,
                src = when (returnValue) {
                    is TempVariable -> LocalVariable(returnValue.toString(), type)
                    is ConstExpression -> getLiteralNode(type, returnValue.value)
                    else -> throw IRBuilderException("Unknown return value in variableDeclInit")
                },
            )
        )
    }

    abstract class ExpressionResult {
        fun toArgument(): Argument = when (this) {
            is TempVariable -> LocalVariable(this.name, this.type)
            is ConstExpression -> getLiteralNode(PrimitiveType(TypeProperty.i32), this.value)
            is ExistedVariable -> this.variable
            else -> throw IRBuilderException("VoidResult cannot be converted to Argument")
        }
    }
    class VoidResult : ExpressionResult()
    class ConstExpression(val value: Int) : ExpressionResult()
    class TempVariable(val name: String, val type: Type) : ExpressionResult()
    class ExistedVariable(val variable: Variable) : ExpressionResult()

    enum class ExpectedState { PTR, VALUE }

    // Add the expression to the block. The return value indicates the number
    // of variable to use in the block. If there is no return value, the
    // function will return -1. Anyone who calls this function should remove the
    // last statement in the block if the return value is tempVariable and will
    // not be used.
    private fun addExpression(
        expr         : ast.Expression,
        blocks       : MutableList<Block>,
        expectedState: ExpectedState,
    ): ExpressionResult = when (expr) {
        is ast.Object              -> addExpression(expr, blocks)
        is StringLiteral           -> addExpression(expr, blocks)
        is IntegerLiteral          -> addExpression(expr)
        is BooleanLiteral          -> addExpression(expr)
        is NullLiteral             -> ConstExpression(0)
        is ThisLiteral             ->
            ExistedVariable(LocalVariable("__this", PrimitiveType(TypeProperty.ptr)))
        is MemberVariableAccess    -> addExpression(expr, blocks, expectedState)
        is MemberFunctionAccess    -> addExpression(expr, blocks)
        is ArrayExpression         -> addExpression(expr, blocks, expectedState)
        is PrefixUpdateExpression  -> addExpression(expr, blocks, expectedState)
        is FunctionCall            -> addExpression(expr, blocks)
        is LambdaCall              ->
            throw NotSupported("Lambda call is not supported in IRBuilder")
        is LambdaExpression        ->
            throw NotSupported("Lambda expression is not supported in IRBuilder")
        is NewExpression           -> addExpression(expr, blocks)
        is PostfixUpdateExpression -> addExpression(expr, blocks)
        is UnaryExpression         -> addExpression(expr, blocks)
        is BinaryExpression        -> addExpression(expr, blocks)
        is AssignExpression        -> addExpression(expr, blocks)
        else -> throw IRBuilderException("Unknown expression in addExpression")
    }

    private fun addExpression(expr  : ast.Object,
                              blocks: MutableList<Block>): ExpressionResult {
        if (expr.binding == null) {
            throw EnvironmentException("The AST node in addExpression has no binding")
        }
        val type = irType(expr.binding!!.type)
        return when (expr.binding!!.irInfo.isLocal) {
            true -> ExistedVariable(LocalVariable(expr.binding!!.irInfo.toString(), type))
            false -> ExistedVariable(GlobalVariable(expr.binding!!.irInfo.toString(), type))
        }
    }

    private fun addExpression(expr  : StringLiteral,
                              blocks: MutableList<Block>): ExpressionResult {
        addStringLiteral("__string_$unnamedStringLiteralCount", expr)
        val dest = unnamedVariableCount
        unnamedVariableCount++
        blocks.last().statements.add(
            LoadStatement(
                dest = LocalVariable(dest.toString(), PrimitiveType(TypeProperty.ptr)),
                src  = GlobalVariable("__string_$unnamedStringLiteralCount", PrimitiveType(TypeProperty.ptr)),
            )
        )
        return TempVariable(dest.toString(), PrimitiveType(TypeProperty.ptr))
    }

    private fun addExpression(expr: IntegerLiteral): ExpressionResult = ConstExpression(expr.value)

    private fun addExpression(expr: BooleanLiteral): ExpressionResult =
        when (expr.value) {
            true -> ConstExpression(1)
            false -> ConstExpression(0)
        }

    private fun addExpression(
        expr         : MemberVariableAccess,
        blocks       : MutableList<Block>,
        expectedState: ExpectedState,
    ): ExpressionResult {
        if (expr.resultType == null || expr.objectName.resultType == null) {
            throw EnvironmentException("The AST node in addExpression has no result type")
        }
        val type = irType(expr.resultType!!.type)
        val classType = expr.objectName.resultType!!.type
        if (classType !is MxClassType) {
            throw InternalException("The object is not a class type")
        }
        val srcType = classes[classType.name] ?: throw InternalException("The class is not found")
        val index = srcType.nameMap?.get(expr.variableName)
            ?: throw InternalException("The class has no such member")
        val source = addExpression(expr.objectName, blocks, ExpectedState.VALUE).toArgument() as? Variable
            ?: throw InternalException("The source is not a variable")
        val ptrDestName = unnamedVariableCount
        val ptrDest = LocalVariable(ptrDestName.toString(), PrimitiveType(TypeProperty.ptr))
        unnamedVariableCount++
        blocks.last().statements.add(
            GetElementPtrStatement(
                dest    = ptrDest,
                src     = source,
                srcType = srcType.classType,
                indexes = listOf<Argument>(I32Literal(0), I32Literal(index)),
            )
        )
        return when (expectedState) {
            ExpectedState.PTR -> TempVariable(ptrDestName.toString(), type)
            ExpectedState.VALUE -> {
                val valDest = unnamedVariableCount
                unnamedVariableCount++
                blocks.last().statements.add(
                    LoadStatement(
                        dest = ptrDest,
                        src  = LocalVariable(ptrDestName.toString(), type),
                    )
                )
                TempVariable(valDest.toString(), type)
            }
        }
    }

    private fun addExpression(
        expr  : MemberFunctionAccess,
        blocks: MutableList<Block>,
    ): ExpressionResult {
        if (expr.resultType == null || expr.objectName.resultType == null) {
            throw EnvironmentException("The AST node in addExpression has no result type")
        }
        val type = irType(expr.resultType!!.type)
        val classType = expr.objectName.resultType!!.type
        if (classType !is MxClassType) {
            throw InternalException("The object is not a class type")
        }
        val classPtr = addExpression(expr.objectName, blocks, ExpectedState.VALUE).toArgument() as? Variable
            ?: throw InternalException("The source is not a variable")
        val function: GlobalFunction = globalFunctions["${classType.name}.${expr.functionName}"]
            ?: throw InternalException("Cannot find find the function ${classType.name}.${expr.functionName}")
        val arguments = mutableListOf<Argument>(classPtr) + expr.arguments.map {
            addExpression(it, blocks, ExpectedState.VALUE).toArgument()
        }
        if (expr.resultType!!.type is MxVoidType) {
            blocks.last().statements.add(
                CallStatement(
                    dest       = null,
                    returnType = type,
                    function   = function,
                    arguments  = arguments,
                )
            )
            return VoidResult()
        } else {
            val dest = unnamedVariableCount
            unnamedVariableCount++
            blocks.last().statements.add(
                CallStatement(
                    dest       = LocalVariable(dest.toString(), type),
                    returnType = type,
                    function   = function,
                    arguments  = arguments,
                )
            )
            return TempVariable(dest.toString(), type)
        }
    }

    private fun addExpression(
        expr         : FunctionCall,
        blocks       : MutableList<Block>,
    ): ExpressionResult {
        if (expr.resultType == null) {
            throw EnvironmentException("The AST node in addExpression has no result type")
        }
        val type = irType(expr.resultType!!.type)
        val function: GlobalFunction = globalFunctions[expr.functionName]
            ?: throw InternalException("Cannot find find the function ${expr.functionName}")
        val arguments = expr.arguments.map {
            addExpression(it, blocks, ExpectedState.VALUE).toArgument()
        }
        if (expr.resultType!!.type is MxVoidType) {
            blocks.last().statements.add(
                CallStatement(
                    dest = null,
                    returnType = type,
                    function = function,
                    arguments = arguments,
                )
            )
            return VoidResult()
        } else {
            val dest = unnamedVariableCount
            unnamedVariableCount++
            blocks.last().statements.add(
                CallStatement(
                    dest = LocalVariable(dest.toString(), type),
                    returnType = type,
                    function = function,
                    arguments = arguments,
                )
            )
            return TempVariable(dest.toString(), type)
        }
    }

    private fun addExpression(
        expr         : ArrayExpression,
        blocks       : MutableList<Block>,
        expectedState: ExpectedState,
    ): ExpressionResult {
        if (expr.resultType == null || expr.array.resultType == null || expr.index.resultType == null) {
            throw EnvironmentException("The AST node in addExpression has no result type")
        }
        val type = irType(expr.resultType!!.type)
        val array = addExpression(expr.array, blocks, ExpectedState.VALUE).toArgument() as? Variable
            ?: throw InternalException("The array is not a variable")
        val index = addExpression(expr.index, blocks, ExpectedState.VALUE).toArgument() as? Variable
            ?: throw InternalException("The index is not a variable")
        val ptrDestName = unnamedVariableCount
        val ptrDest = LocalVariable(ptrDestName.toString(), PrimitiveType(TypeProperty.ptr))
        unnamedVariableCount++
        // add the ptr to the target
        blocks.last().statements.add(
            GetElementPtrStatement(
                dest    = ptrDest,
                src     = array,
                srcType = type,
                indexes = listOf(index),
            )
        )
        return when (expectedState) {
            ExpectedState.PTR -> TempVariable(ptrDestName.toString(), PrimitiveType(TypeProperty.ptr))
            ExpectedState.VALUE -> {
                val valueDest = unnamedVariableCount
                unnamedVariableCount++
                blocks.last().statements.add(
                    LoadStatement(
                        dest = LocalVariable(valueDest.toString(), type),
                        src  = ptrDest,
                    )
                )
                TempVariable(valueDest.toString(), type)
            }
        }
    }

    private fun addExpression(
        expr         : PrefixUpdateExpression,
        blocks       : MutableList<Block>,
        expectedState: ExpectedState,
    ): ExpressionResult {
        if (expr.resultType == null || expr.operand.resultType == null) {
            throw EnvironmentException("The AST node in addExpression has no result type")
        }
        val type = irType(expr.resultType!!.type)
        val operand = addExpression(expr.operand, blocks, expectedState).toArgument() as? Variable
            ?: throw InternalException("The operand is not a variable")
        val rhs = when (expr.operator) {
            UpdateOperator.INCREMENT -> I32Literal(1)
            UpdateOperator.DECREMENT -> I32Literal(-1)
        }
        val loadDestName = unnamedVariableCount
        val loadDest = LocalVariable(loadDestName.toString(), type)
        unnamedVariableCount++
        val addDestName = unnamedVariableCount
        val addDest = LocalVariable(addDestName.toString(), type)
        unnamedVariableCount++
        blocks.last().statements.add(LoadStatement(dest = loadDest, src = operand))
        blocks.last().statements.add(
            BinaryOperationStatement(dest = addDest, op = BinaryOperator.ADD, lhs = loadDest, rhs = rhs)
        )
        val storeDest = unnamedVariableCount
        unnamedVariableCount++
        blocks.last().statements.add(
            StoreStatement(
                dest = LocalVariable(storeDest.toString(), type),
                src = addDest,
            )
        )
        return when (expectedState) {
            ExpectedState.PTR -> ExistedVariable(operand)
            ExpectedState.VALUE -> TempVariable(addDest.toString(), type)
        }
    }

    private fun addExpression(
        expr  : PostfixUpdateExpression,
        blocks: MutableList<Block>,
    ): ExpressionResult {
        if (expr.resultType == null || expr.operand.resultType == null) {
            throw EnvironmentException("The AST node in addExpression has no result type")
        }
        val type = irType(expr.resultType!!.type)
        val operand = addExpression(expr.operand, blocks, ExpectedState.VALUE).toArgument() as? Variable
            ?: throw InternalException("The operand is not a variable")
        val rhs = when (expr.operator) {
            UpdateOperator.INCREMENT -> I32Literal(1)
            UpdateOperator.DECREMENT -> I32Literal(-1)
        }
        val loadDestName = unnamedVariableCount
        val loadDest = LocalVariable(loadDestName.toString(), type)
        unnamedVariableCount++
        val addDestName = unnamedVariableCount
        val addDest = LocalVariable(addDestName.toString(), type)
        unnamedVariableCount++
        blocks.last().statements.add(LoadStatement(dest = loadDest, src = operand))
        blocks.last().statements.add(
            BinaryOperationStatement(dest = addDest, op = BinaryOperator.ADD, lhs = loadDest, rhs = rhs)
        )
        val storeDest = unnamedVariableCount
        unnamedVariableCount++
        blocks.last().statements.add(
            StoreStatement(
                dest = LocalVariable(storeDest.toString(), type),
                src = addDest,
            )
        )
        return TempVariable(loadDestName.toString(), type)
    }

    private fun addExpression(
        expr  : NewExpression,
        blocks: MutableList<Block>,
    ): ExpressionResult {
        return TODO()
    }

    private fun addExpression(
        expr  : UnaryExpression,
        blocks: MutableList<Block>,
    ): ExpressionResult {
        return TODO()
    }

    private fun addExpression(
        expr  : BinaryExpression,
        blocks: MutableList<Block>,
    ): ExpressionResult {
        return TODO()
    }

    private fun addExpression(
        expr  : AssignExpression,
        blocks: MutableList<Block>,
    ): ExpressionResult {
        return TODO()
    }

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

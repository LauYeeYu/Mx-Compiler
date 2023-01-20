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

val emptyString: StringLiteralDecl = StringLiteralDecl("__empty_string", "")

class IR(private val root: AstNode, private val parent: IR? = null) {
    private var unnamedVariableCount = 0
    private var unnamedStringLiteralCount = 0
    private var unnamedIterator = 0
    private val globalVariableDecl = mutableListOf<GlobalDecl>()
    private val classes = mutableMapOf<String, GlobalClass>()
    private val globalFunctions = linkedMapOf<String, GlobalFunction>()

    fun buildRoot(): Root {
        if (root !is ast.TranslateUnit) {
            throw IRBuilderException("The AST node in buildRoot is not a root node")
        }
        if (root.environment == null) {
            throw EnvironmentException("The AST node in buildRoot has no environment")
        }
        val classList = root.content.filterIsInstance<ast.Class>().map { registerClass(it) }
        val globalInit = GlobalFunction(
            name = "__global_init",
            returnType = PrimitiveType(TypeProperty.VOID),
            parameters = listOf(),
            variables = mutableListOf(),
            body = mutableListOf(Block("0", mutableListOf())),
        )
        // register global functions
        globalFunctions["__global_init"] = globalInit
        globalFunctions.putAll(builtInFunctionMap)
        root.content.filterIsInstance<ast.Function>().forEach { function ->
            registerFunction(function.name, irType(function.returnType), function.bindings)
        }
        buildGlobalList(root.content)
        root.content.filterIsInstance<ast.Function>().forEach { buildFunction(it) }
        return Root(
            classes = classList,
            variables = globalVariableDecl,
            globalFunctions = globalFunctions.values.toList(),
        )
    }

    private fun registerFunction(
        name: String,
        returnIrType: PrimitiveType,
        parameters: List<Binding>,
        isMember: Boolean = false,
    ) {
        val ptrType = PrimitiveType(TypeProperty.PTR)
        val functionParameter = parameters.map { astParameter ->
            FunctionParameter(irType(astParameter.type), "${astParameter.irInfo}.param")
        }
        val variables: MutableList<LocalVariableDecl> = parameters.map { astParameter ->
            LocalVariableDecl(
                LocalVariable(
                    name = astParameter.irInfo.toString(),
                    type = ptrType,
                ),
                irType(astParameter.type),
            )
        }.toMutableList()
        val body: MutableList<Statement> = parameters.map { astParameter ->
            StoreStatement(
                dest = LocalVariable(astParameter.irInfo.toString(), ptrType),
                src = LocalVariable("${astParameter.irInfo}.param", irType(astParameter.type))
            )
        }.toMutableList()
        globalFunctions[name] = GlobalFunction(
            name = name,
            returnType = returnIrType,
            parameters = if (isMember) {
                listOf(FunctionParameter(ptrType, "__this.param")) + functionParameter
            } else {
                functionParameter
            },
            variables = if (isMember) (listOf(
                LocalVariableDecl(LocalVariable("__this", ptrType), ptrType)
            ) + variables).toMutableList() else variables,
            body = if (isMember) mutableListOf(Block("0", (listOf<Statement>(
                StoreStatement(
                    dest = LocalVariable("__this", ptrType),
                    src = LocalVariable("__this.param", ptrType),
                )
            ) + body).toMutableList())) else mutableListOf(Block("0", body)),
            returnPhi = if (returnIrType.type == TypeProperty.VOID) null
            else PhiStatement(
                LocalVariable("__return", returnIrType), mutableListOf()
            ),
        )
    }

    private fun buildGlobalList(sourceList: List<ast.GlobalElement>) {
        val globalInit = globalFunctions["__global_init"]
            ?: throw IRBuilderException("Global init function not found")
        val globalInitBlocks = globalInit.body
            ?: throw IRBuilderException("Global init function has no body")
        sourceList.filterIsInstance<ast.VariablesDeclaration>().forEach { element ->
            when (element.type) {
                is ast.BoolType, is ast.IntType -> {
                    val type = irType(element.type)
                    element.variables.forEach { variable ->
                        primitiveVariableDeclInit(variable, type, globalInit)
                    }
                }

                is ast.StringType -> for (variable in element.variables) {
                    when (variable.body) {
                        null ->
                            globalVariableDecl.add(StringLiteralDecl(variable.name, ""))

                        is StringLiteral ->
                            globalVariableDecl.add(StringLiteralDecl(variable.name, variable.body.value))

                        else -> {
                            val result = addExpression(
                                variable.body,
                                globalInit,
                                ExpectedState.VALUE,
                            ).toArgument()
                            globalVariableDecl.add(StringLiteralDecl(variable.name, ""))
                            globalInitBlocks.last().statements.add(
                                StoreStatement(
                                    GlobalVariable(variable.name, PrimitiveType(TypeProperty.PTR)),
                                    result,
                                )
                            )
                        }
                    }
                }

                is ast.ArrayType, is ast.ClassType -> {
                    val type = PrimitiveType(TypeProperty.PTR)
                    element.variables.forEach { variable ->
                        val variableProperty = GlobalVariable(variable.name, type)
                        if (variable.body != null) {
                            val result = addExpression(
                                variable.body,
                                globalInit,
                                ExpectedState.VALUE,
                            ).toArgument()
                            globalInitBlocks.last().statements.add(
                                StoreStatement(variableProperty, result)
                            )
                        }
                        globalVariableDecl.add(GlobalVariableDecl(variableProperty, 0))
                    }
                }
            }
        }
        globalInitBlocks.last().statements.add(ReturnStatement(null))
    }

    private fun registerClass(astNode: ast.Class): GlobalClass {
        if (astNode.environment == null) {
            throw EnvironmentException("The AST node in buildClass has no environment")
        }
        val memberList = mutableListOf<PrimitiveType>()
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
        astNode.body.filterIsInstance<ast.Function>().forEach { element ->
            registerFunction(
                name = "${astNode.name}.${element.name}",
                returnIrType = irType(element.returnType),
                parameters = element.bindings,
                isMember = true,
            )
        }
        registerFunction(
            name = "${astNode.name}.${astNode.name}",
            returnIrType = PrimitiveType(TypeProperty.VOID),
            parameters = listOf(),
            isMember = true,
        )
        return returnClass
    }

    private fun irType(astType: ast.Type): PrimitiveType = when (astType) {
        is ast.VoidType -> PrimitiveType(TypeProperty.VOID)
        is ast.BoolType -> PrimitiveType(TypeProperty.I1)
        is ast.IntType -> PrimitiveType(TypeProperty.I32)
        is ast.StringType -> PrimitiveType(TypeProperty.PTR)
        is ast.ArrayType -> PrimitiveType(TypeProperty.PTR)
        is ast.ClassType -> PrimitiveType(TypeProperty.PTR)
        else -> throw IRBuilderException("Unknown type in irType")
    }

    private fun irType(internalType: MxType): PrimitiveType = when (internalType) {
        is MxVoidType -> PrimitiveType(TypeProperty.VOID)
        is MxBoolType -> PrimitiveType(TypeProperty.I1)
        is MxIntType -> PrimitiveType(TypeProperty.I32)
        is MxStringType -> PrimitiveType(TypeProperty.PTR)
        is MxNullType -> PrimitiveType(TypeProperty.PTR)
        is MxArrayType -> PrimitiveType(TypeProperty.PTR)
        is MxClassType -> PrimitiveType(TypeProperty.PTR)
        else -> throw IRBuilderException("Unknown type in irType")
    }

    private fun buildFunction(astNode: ast.Function) {
        if (astNode.environment == null) {
            throw EnvironmentException("The AST node in buildFunction has no environment")
        }
        val function = globalFunctions[astNode.name]
            ?: throw IRBuilderException("Function ${astNode.name} not found")
        addStatement(astNode.body,function)
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        if (function.returnType.type == TypeProperty.VOID) {
            if (blocks.last().statements.lastOrNull() !is ReturnStatement) {
                blocks.last().statements.add(BranchStatement(null, "return", null))
            }
        } else {
            if (function.name == "main" &&
                blocks.last().statements.lastOrNull() !is ReturnStatement) {
                // main function should return 0
                blocks.last().statements.add(BranchStatement(null, "return", null))
                val returnPhi = function.returnPhi ?: throw IRBuilderException("Function has no return phi")
                returnPhi.incoming.add(Pair(I32Literal(0), blocks.last().label))
            }
        }
    }

    private fun primitiveVariableDeclInit(
        variable: ast.VariableDeclaration,
        type: Type,
        function: GlobalFunction, // variable initializing statement will not have a branch
    ) {
        val returnValue: Argument = when (variable.body) {
            null -> when (type) {
                is PrimitiveType -> when (type.type) {
                    TypeProperty.I1 -> I1Literal(0)
                    TypeProperty.I32 -> I32Literal(0)
                    else -> throw IRBuilderException("Unknown type in primitiveVariableDeclInit")
                }

                else -> throw IRBuilderException("Unknown type in primitiveVariableDeclInit")
            }

            else -> addExpression(variable.body, function, ExpectedState.VALUE).toArgument()
        }
        val irVariable = GlobalVariable(variable.name, type)
        when (returnValue) {
            is IntLiteral ->
                globalVariableDecl.add(GlobalVariableDecl(irVariable, returnValue.value))

            is Variable -> {
                globalVariableDecl.add(GlobalVariableDecl(irVariable, 0))
                val blocks = function.body ?: throw IRBuilderException("Function has no body")
                blocks.last().statements.add(
                    StoreStatement(dest = irVariable, src = returnValue)
                )
            }

            else -> throw IRBuilderException("Unknown type in primitiveVariableDeclInit")
        }

    }

    abstract class ExpressionResult {
        fun toArgument(): Argument = when (this) {
            is ConstExpression -> getLiteralNode(this.value, this.type)
            is IrVariable -> this.variable
            else -> throw IRBuilderException("VoidResult cannot be converted to Argument")
        }
    }

    class VoidResult : ExpressionResult()
    class ConstExpression(val value: Int, val type: Type) : ExpressionResult()
    class IrVariable(val variable: Variable) : ExpressionResult()

    enum class ExpectedState { PTR, VALUE }

    // Add the expression to the block. The return value indicates the number
    // of variable to use in the block. If there is no return value, the
    // function will return -1. Anyone who calls this function should remove the
    // last statement in the block if the return value is tempVariable and will
    // not be used.
    private fun addExpression(
        expr: ast.Expression,
        function: GlobalFunction,
        expectedState: ExpectedState,
    ): ExpressionResult = when (expr) {
        is ast.Object -> addExpression(expr, function, expectedState)
        is StringLiteral -> addExpression(expr, function)
        is IntegerLiteral -> addExpression(expr)
        is BooleanLiteral -> addExpression(expr)
        is NullLiteral -> ConstExpression(0, PrimitiveType(TypeProperty.PTR))
        is ThisLiteral ->
            IrVariable(LocalVariable("__this", PrimitiveType(TypeProperty.PTR)))

        is MemberVariableAccess -> addExpression(expr, function, expectedState)
        is MemberFunctionAccess -> addExpression(expr, function)
        is ArrayExpression -> addExpression(expr, function, expectedState)
        is PrefixUpdateExpression -> addExpression(expr, function, expectedState)
        is FunctionCall -> addExpression(expr, function)
        is LambdaCall ->
            throw NotSupported("Lambda call is not supported in IRBuilder")

        is LambdaExpression ->
            throw NotSupported("Lambda expression is not supported in IRBuilder")

        is NewExpression -> addExpression(expr, function)
        is PostfixUpdateExpression -> addExpression(expr, function)
        is UnaryExpression -> addExpression(expr, function)
        is BinaryExpression -> addExpression(expr, function)
        is AssignExpression -> addExpression(expr, function)
        else -> throw IRBuilderException("Unknown expression in addExpression")
    }

    private fun addExpression(
        expr: ast.Object,
        function: GlobalFunction,
        expectedState: ExpectedState
    ): ExpressionResult {
        val binding = expr.binding ?: throw IRBuilderException("Object has no binding")
        val type = irType(binding.type)
        if (binding.fromClass == null) {
            val srcVariable = when (binding.irInfo.isLocal) {
                true -> LocalVariable(binding.irInfo.toString(), PrimitiveType(TypeProperty.PTR))
                false -> GlobalVariable(binding.irInfo.toString(), PrimitiveType(TypeProperty.PTR))
            }
            return when (expectedState) {
                ExpectedState.PTR -> IrVariable(srcVariable)
                ExpectedState.VALUE -> {
                    val destName = unnamedVariableCount
                    unnamedVariableCount++
                    val dest = LocalVariable(destName.toString(), type)
                    val blocks = function.body ?: throw IRBuilderException("Function has no body")
                    blocks.last().statements.add(LoadStatement(dest = dest, src = srcVariable))
                    IrVariable(dest)
                }
            }
        } else {
            val srcClass = classes[binding.fromClass.name]
                ?: throw IRBuilderException("Class ${binding.fromClass.name} not found")
            val index = srcClass.nameMap[expr.name]
                ?: throw IRBuilderException("Cannot find the index of the member")
            val ptr = LocalVariable(unnamedVariableCount.toString(), PrimitiveType(TypeProperty.PTR))
            unnamedVariableCount++
            val blocks = function.body ?: throw IRBuilderException("Function has no body")
            blocks.last().statements.add(
                GetElementPtrStatement(
                    dest = ptr,
                    src = LocalVariable("__this", PrimitiveType(TypeProperty.PTR)),
                    srcType = srcClass.classType,
                    indexes = listOf(I32Literal(0), I32Literal(index)),
                )
            )
            return when (expectedState) {
                ExpectedState.PTR -> IrVariable(ptr)
                ExpectedState.VALUE -> {
                    val destName = unnamedVariableCount
                    unnamedVariableCount++
                    val dest = LocalVariable(destName.toString(), type)
                    blocks.last().statements.add(LoadStatement(dest = dest, src = ptr))
                    IrVariable(dest)
                }
            }
        }
    }

    private fun addExpression(
        expr: StringLiteral,
        function: GlobalFunction
    ): ExpressionResult {
        addStringLiteral("__string_$unnamedStringLiteralCount", expr)
        val destName = unnamedVariableCount
        unnamedVariableCount++
        val dest = LocalVariable(destName.toString(), PrimitiveType(TypeProperty.PTR))
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        blocks.last().statements.add(
            LoadStatement(
                dest = dest,
                src = GlobalVariable("__string_$unnamedStringLiteralCount", PrimitiveType(TypeProperty.PTR)),
            )
        )
        return IrVariable(dest)
    }

    private fun addExpression(expr: IntegerLiteral): ExpressionResult =
        ConstExpression(expr.value, PrimitiveType(TypeProperty.I32))

    private fun addExpression(expr: BooleanLiteral): ExpressionResult =
        when (expr.value) {
            true -> ConstExpression(1, PrimitiveType(TypeProperty.I1))
            false -> ConstExpression(0, PrimitiveType(TypeProperty.I1))
        }

    private fun addExpression(
        expr: MemberVariableAccess,
        function: GlobalFunction,
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
        val index = srcType.nameMap[expr.variableName]
            ?: throw InternalException("The class has no such member")
        val source = addExpression(expr.objectName, function, ExpectedState.VALUE).toArgument() as? Variable
            ?: throw InternalException("The source is not a variable")
        val ptrDestName = unnamedVariableCount
        val ptrDest = LocalVariable(ptrDestName.toString(), PrimitiveType(TypeProperty.PTR))
        unnamedVariableCount++
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        blocks.last().statements.add(
            GetElementPtrStatement(
                dest = ptrDest,
                src = source,
                srcType = srcType.classType,
                indexes = listOf<Argument>(I32Literal(0), I32Literal(index)),
            )
        )
        return when (expectedState) {
            ExpectedState.PTR -> IrVariable(ptrDest)
            ExpectedState.VALUE -> {
                val valDestName = unnamedVariableCount
                unnamedVariableCount++
                val valDest = LocalVariable(valDestName.toString(), type)
                blocks.last().statements.add(LoadStatement(dest = valDest, src = ptrDest))
                IrVariable(valDest)
            }
        }
    }

    private fun addExpression(
        expr: MemberFunctionAccess,
        function: GlobalFunction,
    ): ExpressionResult {
        if (expr.resultType == null || expr.objectName.resultType == null) {
            throw EnvironmentException("The AST node in addExpression has no result type")
        }
        val type = irType(expr.resultType!!.type)
        val classType = expr.objectName.resultType!!.type
        if (classType !is MxClassType) {
            throw InternalException("The object is not a class type")
        }
        val classPtr = addExpression(expr.objectName, function, ExpectedState.VALUE).toArgument() as? Variable
            ?: throw InternalException("The source is not a variable")
        val calledFunction: GlobalFunction = globalFunctions["${classType.name}.${expr.functionName}"]
            ?: throw InternalException("Cannot find find the function ${classType.name}.${expr.functionName}")
        val arguments = mutableListOf<Argument>(classPtr) + expr.arguments.map {
            addExpression(it, function, ExpectedState.VALUE).toArgument()
        }
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        return if (expr.resultType!!.type is MxVoidType) {
            blocks.last().statements.add(CallStatement(null, type, calledFunction, arguments))
            VoidResult()
        } else {
            val destName = unnamedVariableCount
            unnamedVariableCount++
            val dest = LocalVariable(destName.toString(), type)
            blocks.last().statements.add(CallStatement(dest, type, calledFunction, arguments))
            IrVariable(dest)
        }
    }

    private fun addExpression(
        expr: FunctionCall,
        function: GlobalFunction,
    ): ExpressionResult {
        if (expr.resultType == null) {
            throw EnvironmentException("The AST node in addExpression has no result type")
        }
        val type = irType(expr.resultType!!.type)
        val calledFunction: GlobalFunction = globalFunctions[expr.functionName]
            ?: throw InternalException("Cannot find find the function ${expr.functionName}")
        val arguments = expr.arguments.map {
            addExpression(it, function, ExpectedState.VALUE).toArgument()
        }
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        return if (expr.resultType!!.type is MxVoidType) {
            blocks.last().statements.add(CallStatement(null, type, calledFunction, arguments))
            VoidResult()
        } else {
            val destName = unnamedVariableCount
            unnamedVariableCount++
            val dest = LocalVariable(destName.toString(), type)
            blocks.last().statements.add(CallStatement(dest, type, calledFunction, arguments))
            IrVariable(dest)
        }
    }

    private fun addExpression(
        expr: ArrayExpression,
        function: GlobalFunction,
        expectedState: ExpectedState,
    ): ExpressionResult {
        if (expr.resultType == null || expr.array.resultType == null || expr.index.resultType == null) {
            throw EnvironmentException("The AST node in addExpression has no result type")
        }
        val type = irType(expr.resultType!!.type)
        val array = addExpression(expr.array, function, ExpectedState.VALUE).toArgument() as? Variable
            ?: throw InternalException("The array is not a variable")
        val index = addExpression(expr.index, function, ExpectedState.VALUE).toArgument()
        val ptrDestName = unnamedVariableCount
        val ptrDest = LocalVariable(ptrDestName.toString(), PrimitiveType(TypeProperty.PTR))
        unnamedVariableCount++
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        // add the ptr to the target
        blocks.last().statements.add(
            GetElementPtrStatement(
                dest = ptrDest,
                src = array,
                srcType = type,
                indexes = listOf(index),
            )
        )
        return when (expectedState) {
            ExpectedState.PTR -> IrVariable(ptrDest)
            ExpectedState.VALUE -> {
                val valueDestName = unnamedVariableCount
                unnamedVariableCount++
                val valueDest = LocalVariable(valueDestName.toString(), type)
                blocks.last().statements.add(
                    LoadStatement(dest = valueDest, src = ptrDest)
                )
                IrVariable(valueDest)
            }
        }
    }

    private fun addExpression(
        expr: PrefixUpdateExpression,
        function: GlobalFunction,
        expectedState: ExpectedState,
    ): ExpressionResult {
        if (expr.resultType == null || expr.operand.resultType == null) {
            throw EnvironmentException("The AST node in addExpression has no result type")
        }
        val type = irType(expr.resultType!!.type)
        val operand = addExpression(expr.operand, function, expectedState).toArgument() as? Variable
            ?: throw InternalException("The operand is not a variable")
        val rhs = when (expr.operator) {
            UpdateOperator.INCREMENT -> I32Literal(1)
            UpdateOperator.DECREMENT -> I32Literal(-1)
        }
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        val addSrc = when (expectedState) {
            ExpectedState.VALUE -> {
                val loadDestName = unnamedVariableCount
                val loadDest = LocalVariable(loadDestName.toString(), type)
                unnamedVariableCount++
                blocks.last().statements.add(LoadStatement(dest = loadDest, src = operand))
                loadDest
            }

            ExpectedState.PTR -> operand
        }
        val addDestName = unnamedVariableCount
        val addDest = LocalVariable(addDestName.toString(), type)
        unnamedVariableCount++
        blocks.last().statements.add(
            BinaryOperationStatement(dest = addDest, op = BinaryOperator.ADD, lhs = addSrc, rhs = rhs)
        )
        val storeDest = unnamedVariableCount
        unnamedVariableCount++
        blocks.last().statements.add(
            StoreStatement(
                dest = LocalVariable(storeDest.toString(), type),
                src = addDest,
            )
        )
        return IrVariable(addDest)
    }

    private fun addExpression(
        expr: PostfixUpdateExpression,
        function: GlobalFunction,
    ): ExpressionResult {
        if (expr.resultType == null || expr.operand.resultType == null) {
            throw EnvironmentException("The AST node in addExpression has no result type")
        }
        val type = irType(expr.resultType!!.type)
        val operand = addExpression(expr.operand, function, ExpectedState.VALUE).toArgument() as? Variable
            ?: throw InternalException("The operand is not a variable")
        val rhs = when (expr.operator) {
            UpdateOperator.INCREMENT -> I32Literal(1)
            UpdateOperator.DECREMENT -> I32Literal(-1)
        }
        val addDestName = unnamedVariableCount
        val addDest = LocalVariable(addDestName.toString(), type)
        unnamedVariableCount++
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        blocks.last().statements.add(
            BinaryOperationStatement(dest = addDest, op = BinaryOperator.ADD, lhs = operand, rhs = rhs)
        )
        val storeDest = unnamedVariableCount
        unnamedVariableCount++
        blocks.last().statements.add(
            StoreStatement(
                dest = LocalVariable(storeDest.toString(), type),
                src = addDest,
            )
        )
        return IrVariable(operand)
    }

    private fun addExpression(
        expr: NewExpression,
        function: GlobalFunction,
    ): ExpressionResult {
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        if (expr.dimension == 0) { // New class
            return IrVariable(addNewClass(blocks, (expr.type as ast.ClassType)))
        }

        // New array
        val arguments = expr.arguments.map { addExpression(it, function, ExpectedState.VALUE).toArgument() }
        val iterators = mutableListOf<LocalVariable>()
        for (i in 0 until expr.arguments.size) {
            iterators.add(LocalVariable("__iterator_$unnamedIterator", PrimitiveType(TypeProperty.PTR)))
            unnamedIterator++
        }
        function.variables?.addAll(iterators.map { LocalVariableDecl(it, PrimitiveType(TypeProperty.I32)) })
            ?: throw InternalException("Function has no variable list")
        val array = addNewExpressionLoop(blocks, arguments, iterators, expr.dimension, 0, expr.type)
        return IrVariable(array)
    }

    private fun addNewExpressionLoop(
        blocks: MutableList<Block>,
        arguments: List<Argument>,
        iterators: List<LocalVariable>,
        dimension: Int,
        index: Int,
        type: ast.Type,
    ): LocalVariable {
        val array = LocalVariable(unnamedVariableCount.toString(), PrimitiveType(TypeProperty.PTR))
        unnamedVariableCount++
        blocks.last().statements.add(
            CallStatement(
                dest = array,
                returnType = PrimitiveType(TypeProperty.VOID),
                function = if (dimension == 1) {
                    when (type) {
                        is ast.IntType -> globalFunctions["__newIntArray"]
                            ?: throw InternalException("Function __newIntArray not found")

                        is ast.BoolType -> globalFunctions["__newBoolArray"]
                            ?: throw InternalException("Function __newBoolArray not found")

                        else -> globalFunctions["__newPtrArray"]
                            ?: throw InternalException("Function __newPtrArray not found")
                    }
                } else {
                    globalFunctions["__newPtrArray"]
                        ?: throw InternalException("Function __newPtrArray not found")
                },
                arguments = listOf(arguments[0]),
            )
        )
        // Set the initial number of the iterator, i.e. __iterator__i = 0
        blocks.last().statements.add(StoreStatement(dest = iterators[index], src = I32Literal(0)))
        val loopConditionLabel = blocks.size
        val loopStartLabel = loopConditionLabel + 1
        // Jump to the loop condition
        blocks.last().statements.add(
            BranchStatement(
                condition = null,
                trueBlockLabel = loopConditionLabel.toString(),
                falseBlockLabel = null
            )
        )

        // Add the loop condition
        val iteratorValue = LocalVariable(unnamedVariableCount.toString(), PrimitiveType(TypeProperty.I32))
        unnamedVariableCount++
        val compareResult = LocalVariable(unnamedVariableCount.toString(), PrimitiveType(TypeProperty.I1))
        unnamedVariableCount++
        blocks.add(
            Block(
                loopConditionLabel.toString(),
                mutableListOf(
                    LoadStatement(dest = iteratorValue, src = iterators[index]),
                    IntCmpStatement(
                        dest = compareResult,
                        op = IntCmpOperator.SLT,
                        lhs = iteratorValue,
                        rhs = arguments[index],
                    ),
                    // Branch statement cannot be added yet, since the end label is unknown
                )
            )
        )

        // Add the loop body
        val position = LocalVariable(unnamedVariableCount.toString(), PrimitiveType(TypeProperty.PTR))
        unnamedVariableCount++
        blocks.add(
            Block(
                loopStartLabel.toString(),
                mutableListOf(
                    GetElementPtrStatement(
                        dest = position,
                        src = array,
                        srcType = PrimitiveType(TypeProperty.PTR),
                        indexes = listOf(iteratorValue),
                    )
                )
            )
        )
        if (index + 1 == arguments.size) {
            if (dimension == 1) {
                when (type) {
                    is ast.ClassType -> {
                        val newClass = addNewClass(blocks, type)
                        blocks[loopStartLabel].statements.add(
                            StoreStatement(dest = position, src = newClass)
                        )
                    }

                    is ast.StringType -> {
                        val newString = LocalVariable(unnamedVariableCount.toString(), PrimitiveType(TypeProperty.PTR))
                        unnamedVariableCount++
                        blocks[loopStartLabel].statements.add(
                            CallStatement(
                                dest = newString,
                                returnType = PrimitiveType(TypeProperty.VOID),
                                function = globalFunctions["string.string"]
                                    ?: throw InternalException("Cannot find malloc in builtin function"),
                                arguments = listOf(I32Literal(ptrSize)),
                            )
                        )
                        blocks[loopStartLabel].statements.add(
                            StoreStatement(dest = position, src = newString)
                        )
                    }
                }
            } else {
                val newArray = LocalVariable(unnamedVariableCount.toString(), PrimitiveType(TypeProperty.PTR))
                unnamedVariableCount++
                blocks[loopStartLabel].statements.add(
                    CallStatement(
                        dest = newArray,
                        returnType = PrimitiveType(TypeProperty.VOID),
                        function = globalFunctions["__newPtrArray"]
                            ?: throw InternalException("Function __newPtrArray not found"),
                        arguments = listOf(arguments[index + 1]),
                    )
                )
                blocks[loopStartLabel].statements.add(
                    StoreStatement(dest = position, src = newArray)
                )
            }
        } else {
            val newArray = addNewExpressionLoop(blocks, arguments, iterators, dimension - 1, index + 1, type)
            blocks[loopStartLabel].statements.add(
                StoreStatement(dest = position, src = newArray)
            )
        }

        // Add the loop increment
        val incrementIndex = blocks.size
        val endBlockIndex = incrementIndex + 1
        blocks[loopConditionLabel].statements.add(
            BranchStatement(
                condition = compareResult,
                trueBlockLabel = loopStartLabel.toString(),
                falseBlockLabel = endBlockIndex.toString(),
            )
        )
        val iteratorOld = LocalVariable(unnamedVariableCount.toString(), PrimitiveType(TypeProperty.I32))
        unnamedVariableCount++
        val iteratorNew = LocalVariable(unnamedVariableCount.toString(), PrimitiveType(TypeProperty.I32))
        unnamedVariableCount++
        blocks.add(
            Block(
                incrementIndex.toString(),
                mutableListOf(
                    LoadStatement(dest = iteratorOld, src = iterators[index]),
                    BinaryOperationStatement(
                        dest = iteratorNew,
                        op = BinaryOperator.ADD,
                        lhs = iteratorOld,
                        rhs = I32Literal(1),
                    ),
                    StoreStatement(dest = iterators[index], src = iteratorNew),
                    BranchStatement(
                        condition = null,
                        trueBlockLabel = loopConditionLabel.toString(),
                        falseBlockLabel = null,
                    ),
                )
            )
        )
        blocks.add(Block(endBlockIndex.toString(), mutableListOf()))
        return array
    }

    private fun addNewClass(
        blocks: MutableList<Block>,
        type: ast.ClassType,
    ): LocalVariable {
        val dest = LocalVariable(unnamedVariableCount.toString(), PrimitiveType(TypeProperty.PTR))
        unnamedVariableCount++
        val className = type.name
        blocks.last().statements.add(
            CallStatement(
                dest = dest,
                returnType = PrimitiveType(TypeProperty.PTR),
                function = globalFunctions["malloc"]
                    ?: throw InternalException("Cannot find malloc in builtin function"),
                arguments = listOf(I32Literal(ptrSize)),
            )
        )
        blocks.last().statements.add(
            CallStatement(
                dest = null,
                returnType = PrimitiveType(TypeProperty.VOID),
                function = globalFunctions["$className.$className"]
                    ?: throw InternalException("Cannot find constructor"),
                arguments = listOf(dest),
            )
        )
        return dest
    }

    private fun addExpression(
        expr: UnaryExpression,
        function: GlobalFunction,
    ): ExpressionResult {
        if (expr.resultType == null || expr.operand.resultType == null) {
            throw EnvironmentException("The AST node in addExpression has no result type")
        }
        val type = irType(expr.resultType!!.type)
        val operand = addExpression(expr.operand, function, ExpectedState.VALUE).toArgument()
        if (operand is IntLiteral) {
            return when (expr.operator) {
                UnaryOperator.NEGATIVE ->
                    ConstExpression(-operand.value, PrimitiveType(TypeProperty.I32))

                UnaryOperator.POSITIVE ->
                    ConstExpression(operand.value, PrimitiveType(TypeProperty.I32))

                UnaryOperator.BITWISE_NOT ->
                    ConstExpression(operand.value.inv(), PrimitiveType(TypeProperty.I32))

                UnaryOperator.LOGICAL_NOT ->
                    ConstExpression(if (operand.value == 0) 1 else 0, PrimitiveType(TypeProperty.I1))
            }
        }
        if (expr.operator == UnaryOperator.POSITIVE) return IrVariable(operand as Variable)

        val destName = unnamedVariableCount
        val dest = LocalVariable(destName.toString(), type)
        unnamedVariableCount++
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        when (expr.operator) {
            UnaryOperator.NEGATIVE -> {
                blocks.last().statements.add(
                    BinaryOperationStatement(
                        dest = dest,
                        op = BinaryOperator.SUB,
                        lhs = I32Literal(0),
                        rhs = operand,
                    )
                )
            }

            UnaryOperator.BITWISE_NOT -> {
                blocks.last().statements.add(
                    BinaryOperationStatement(
                        dest = dest,
                        op = BinaryOperator.XOR,
                        lhs = operand,
                        rhs = I32Literal(-1),
                    )
                )
            }

            UnaryOperator.LOGICAL_NOT -> {
                blocks.last().statements.add(
                    BinaryOperationStatement(
                        dest = dest,
                        op = BinaryOperator.XOR,
                        lhs = operand,
                        rhs = I1Literal(0),
                    )
                )
            }

            else -> throw InternalException("Unexpected unary operator")
        }
        return IrVariable(dest)
    }

    private fun addExpression(
        expr: BinaryExpression,
        function: GlobalFunction,
    ): ExpressionResult = when (expr.left.resultType?.type) {
        is MxStringType -> addStringBinaryExpression(expr.left, expr.right, expr.operator, function)
        is MxIntType -> when (expr.operator) {
            ast.BinaryOperator.LOGICAL_AND, ast.BinaryOperator.LOGICAL_OR -> {
                addBinaryLogicExpression(expr.left, expr.right, expr.operator, function)
            }

            else -> {
                val srcType = expr.resultType?.type
                    ?: throw EnvironmentException("The AST node in addExpression has no result type")
                val type = irType(srcType)
                addBinaryArithmeticExpression(expr.left, expr.right, expr.operator, type, function)
            }
        }

        null -> throw EnvironmentException("The AST node in addExpression has no result type")
        else -> {
            val srcType = expr.resultType?.type
                ?: throw EnvironmentException("The AST node in addExpression has no result type")
            val type = irType(srcType)
            addBinaryArithmeticExpression(expr.left, expr.right, expr.operator, type, function)
        } // should be ptr only
    }

    private fun addExpression(
        expr: AssignExpression,
        function: GlobalFunction,
    ): ExpressionResult {
        if (expr.resultType == null || expr.left.resultType == null || expr.right.resultType == null) {
            throw EnvironmentException("The AST node in addExpression has no result type")
        }
        val destPtr = addExpression(expr.left, function, ExpectedState.PTR).toArgument() as? Variable
            ?: throw InternalException("The left side of the assignment is not a variable")
        val src = addExpression(expr.right, function, ExpectedState.VALUE)
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        blocks.last().statements.add(StoreStatement(destPtr, src.toArgument()))
        return src
    }

    private fun addBinaryLogicExpression(
        lhs: Expression,
        rhs: Expression,
        operator: ast.BinaryOperator,
        function: GlobalFunction,
    ): ExpressionResult {
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        val lhsResult = addExpression(lhs, function, ExpectedState.VALUE).toArgument()
        val lhsResultBlockIndex = blocks.last().label
        val lhsNext = blocks.size.toString()
        val lhsResultBlock = blocks.last()
        when (lhsResult) {
            is Variable -> {
                blocks.add(Block(lhsNext, mutableListOf()))
                when (val rhsResult = addExpression(rhs, function, ExpectedState.VALUE).toArgument()) {
                    is Variable -> {
                        val rhsResultBlockIndex = blocks.last().label
                        val rhsResultBlock = blocks.last()
                        val rhsNext = blocks.size.toString()
                        lhsResultBlock.statements.add(
                            when (operator) {
                                ast.BinaryOperator.LOGICAL_AND -> BranchStatement(
                                    condition = lhsResult,
                                    trueBlockLabel = lhsNext,
                                    falseBlockLabel = rhsNext,
                                )

                                ast.BinaryOperator.LOGICAL_OR -> BranchStatement(
                                    condition = lhsResult,
                                    trueBlockLabel = rhsNext,
                                    falseBlockLabel = lhsNext,
                                )

                                else -> throw InternalException("Unexpected binary operator")
                            }
                        )
                        rhsResultBlock.statements.add(
                            BranchStatement(
                                condition = null,
                                trueBlockLabel = rhsNext,
                                falseBlockLabel = null
                            )
                        )

                        val dest = LocalVariable(
                            unnamedVariableCount.toString(),
                            PrimitiveType(TypeProperty.I1)
                        )
                        unnamedVariableCount++
                        blocks.add(
                            Block(
                                label = rhsNext,
                                statements = mutableListOf(
                                    PhiStatement(
                                        dest = dest,
                                        incoming = when (operator) {
                                            ast.BinaryOperator.LOGICAL_AND -> mutableListOf(
                                                Pair(I1Literal(0), lhsResultBlockIndex),
                                                Pair(rhsResult, rhsResultBlockIndex),
                                            )

                                            ast.BinaryOperator.LOGICAL_OR -> mutableListOf(
                                                Pair(I1Literal(1), lhsResultBlockIndex),
                                                Pair(rhsResult, rhsResultBlockIndex),
                                            )

                                            else -> throw InternalException("Unexpected binary operator")
                                        },
                                    )
                                )
                            )
                        )
                        return IrVariable(dest)
                    }

                    is IntLiteral -> {
                        return if (operator == ast.BinaryOperator.LOGICAL_AND && rhsResult.value == 0) {
                            blocks.removeAt(blocks.lastIndex)
                            ConstExpression(0, PrimitiveType(TypeProperty.I1))
                        } else if (operator == ast.BinaryOperator.LOGICAL_OR && rhsResult.value == 1) {
                            blocks.removeAt(blocks.lastIndex)
                            ConstExpression(1, PrimitiveType(TypeProperty.I1))
                        } else {
                            blocks.removeAt(blocks.lastIndex)
                            IrVariable(lhsResult)
                        }
                    }

                    else -> throw InternalException("Unexpected argument type")
                }

            }

            is IntLiteral -> {
                return if (operator == ast.BinaryOperator.LOGICAL_AND && lhsResult.value == 0) {
                    ConstExpression(0, PrimitiveType(TypeProperty.I1))
                } else if (operator == ast.BinaryOperator.LOGICAL_OR && lhsResult.value == 1) {
                    ConstExpression(1, PrimitiveType(TypeProperty.I1))
                } else {
                    addExpression(rhs, function, ExpectedState.VALUE)
                }
            }

            else -> throw InternalException("Unexpected argument type")
        }
    }

    private fun addBinaryArithmeticExpression(
        lhs: Expression,
        rhs: Expression,
        operator: ast.BinaryOperator,
        type: Type,
        function: GlobalFunction,
    ): ExpressionResult {
        val lhsResult = addExpression(lhs, function, ExpectedState.VALUE).toArgument()
        val rhsResult = addExpression(rhs, function, ExpectedState.VALUE).toArgument()
        when (lhs.resultType?.type) {
            null -> throw EnvironmentException("The AST node in addExpression has no result type")
            is MxIntType -> return addIntBinaryExpression(lhsResult, rhsResult, type, operator, function)
            else -> {
                if (lhsResult is IntLiteral && rhsResult is IntLiteral) {
                    return when (operator) {
                        ast.BinaryOperator.EQUAL ->
                            ConstExpression(
                                if (lhsResult.value == rhsResult.value) 1 else 0,
                                PrimitiveType(TypeProperty.I1)
                            )

                        ast.BinaryOperator.NOT_EQUAL ->
                            ConstExpression(
                                if (lhsResult.value != rhsResult.value) 1 else 0,
                                PrimitiveType(TypeProperty.I1)
                            )

                        else -> throw InternalException("Unexpected binary operator")
                    }
                } else {
                    return addCompareExpression(lhsResult, rhsResult, operator, function)
                }
            }
        }
    }

    // Add integer compare expression. Please note that if both argument is
    // constant, you should not this expression.
    private fun addCompareExpression(
        lhs: Argument,
        rhs: Argument,
        operator: ast.BinaryOperator,
        function: GlobalFunction,
    ): ExpressionResult {
        val dest = LocalVariable(unnamedVariableCount.toString(), PrimitiveType(TypeProperty.I1))
        unnamedVariableCount++
        val irOperator = when (operator) {
            ast.BinaryOperator.LESS_THAN -> IntCmpOperator.SLT
            ast.BinaryOperator.LESS_THAN_OR_EQUAL -> IntCmpOperator.SLE
            ast.BinaryOperator.GREATER_THAN -> IntCmpOperator.SGT
            ast.BinaryOperator.GREATER_THAN_OR_EQUAL -> IntCmpOperator.SGE
            ast.BinaryOperator.EQUAL -> IntCmpOperator.EQ
            ast.BinaryOperator.NOT_EQUAL -> IntCmpOperator.NE
            else -> throw InternalException("Unexpected binary compare operator")
        }
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        blocks.last().statements.add(IntCmpStatement(dest, irOperator, lhs, rhs))
        return IrVariable(dest)
    }

    private fun addStringBinaryExpression(
        lhs: Expression,
        rhs: Expression,
        operator: ast.BinaryOperator,
        function: GlobalFunction,
    ): ExpressionResult {
        if (lhs.resultType == null || rhs.resultType == null) {
            throw EnvironmentException("The AST node in addExpression has no result type")
        }
        val destType = when (operator) {
            ast.BinaryOperator.ADD -> PrimitiveType(TypeProperty.PTR)
            ast.BinaryOperator.LESS_THAN, ast.BinaryOperator.LESS_THAN_OR_EQUAL,
            ast.BinaryOperator.GREATER_THAN, ast.BinaryOperator.GREATER_THAN_OR_EQUAL,
            ast.BinaryOperator.EQUAL, ast.BinaryOperator.NOT_EQUAL -> PrimitiveType(TypeProperty.I1)

            else -> throw InternalException("The AST node in addExpression has an unsupported type")
        }
        val dest = LocalVariable(unnamedVariableCount.toString(), destType)
        unnamedVariableCount++
        val lhsArg = addExpression(lhs, function, ExpectedState.VALUE).toArgument()
        val rhsArg = addExpression(rhs, function, ExpectedState.VALUE).toArgument()
        when (operator) {
            ast.BinaryOperator.ADD -> CallStatement(
                dest = dest,
                returnType = PrimitiveType(TypeProperty.PTR),
                function = builtInFunctionMap["string.add"]
                    ?: throw InternalException("The built-in function string.add is not found"),
                arguments = listOf(lhsArg, rhsArg),
            )

            ast.BinaryOperator.LESS_THAN -> CallStatement(
                dest = dest,
                returnType = PrimitiveType(TypeProperty.I1),
                function = builtInFunctionMap["string.less"]
                    ?: throw InternalException("The built-in function string.lessThan is not found"),
                arguments = listOf(lhsArg, rhsArg),
            )

            ast.BinaryOperator.LESS_THAN_OR_EQUAL -> CallStatement(
                dest = dest,
                returnType = PrimitiveType(TypeProperty.I1),
                function = builtInFunctionMap["string.lessOrEqual"]
                    ?: throw InternalException("The built-in function string.lessThanOrEqual is not found"),
                arguments = listOf(lhsArg, rhsArg),
            )

            ast.BinaryOperator.GREATER_THAN -> CallStatement(
                dest = dest,
                returnType = PrimitiveType(TypeProperty.I1),
                function = builtInFunctionMap["string.greater"]
                    ?: throw InternalException("The built-in function string.greaterThan is not found"),
                arguments = listOf(lhsArg, rhsArg),
            )

            ast.BinaryOperator.GREATER_THAN_OR_EQUAL -> CallStatement(
                dest = dest,
                returnType = PrimitiveType(TypeProperty.I1),
                function = builtInFunctionMap["string.greaterOrEqual"]
                    ?: throw InternalException("The built-in function string.greaterThanOrEqual is not found"),
                arguments = listOf(lhsArg, rhsArg),
            )

            ast.BinaryOperator.EQUAL -> CallStatement(
                dest = dest,
                returnType = PrimitiveType(TypeProperty.I1),
                function = builtInFunctionMap["string.equal"]
                    ?: throw InternalException("The built-in function string.equal is not found"),
                arguments = listOf(lhsArg, rhsArg),
            )

            ast.BinaryOperator.NOT_EQUAL -> CallStatement(
                dest = dest,
                returnType = PrimitiveType(TypeProperty.I1),
                function = builtInFunctionMap["string.notEqual"]
                    ?: throw InternalException("The built-in function string.notEqual is not found"),
                arguments = listOf(lhsArg, rhsArg),
            )

            else -> throw InternalException("The AST node in addExpression has an unsupported type")
        }
        return IrVariable(dest)
    }

    private fun addIntBinaryExpression(
        lhsResult: Argument,
        rhsResult: Argument,
        type: Type,
        operator: ast.BinaryOperator,
        function: GlobalFunction,
    ): ExpressionResult {
        if (lhsResult is IntLiteral && rhsResult is IntLiteral) {
            return when (operator) {
                ast.BinaryOperator.ADD ->
                    ConstExpression(lhsResult.value + rhsResult.value, type)

                ast.BinaryOperator.SUB ->
                    ConstExpression(lhsResult.value - rhsResult.value, type)

                ast.BinaryOperator.MUL ->
                    ConstExpression(lhsResult.value * rhsResult.value, type)

                ast.BinaryOperator.DIV ->
                    ConstExpression(lhsResult.value / rhsResult.value, type)

                ast.BinaryOperator.MOD ->
                    ConstExpression(lhsResult.value % rhsResult.value, type)

                ast.BinaryOperator.BITWISE_AND ->
                    ConstExpression(lhsResult.value and rhsResult.value, type)

                ast.BinaryOperator.BITWISE_OR ->
                    ConstExpression(lhsResult.value or rhsResult.value, type)

                ast.BinaryOperator.BITWISE_XOR ->
                    ConstExpression(lhsResult.value xor rhsResult.value, type)

                ast.BinaryOperator.LEFT_SHIFT ->
                    ConstExpression(lhsResult.value shl rhsResult.value, type)

                ast.BinaryOperator.RIGHT_SHIFT ->
                    ConstExpression(lhsResult.value shr rhsResult.value, type)

                ast.BinaryOperator.LESS_THAN ->
                    ConstExpression(
                        if (lhsResult.value < rhsResult.value) 1 else 0,
                        PrimitiveType(TypeProperty.I1)
                    )

                ast.BinaryOperator.LESS_THAN_OR_EQUAL ->
                    ConstExpression(
                        if (lhsResult.value <= rhsResult.value) 1 else 0,
                        PrimitiveType(TypeProperty.I1)
                    )

                ast.BinaryOperator.GREATER_THAN ->
                    ConstExpression(
                        if (lhsResult.value > rhsResult.value) 1 else 0,
                        PrimitiveType(TypeProperty.I1)
                    )

                ast.BinaryOperator.GREATER_THAN_OR_EQUAL ->
                    ConstExpression(
                        if (lhsResult.value >= rhsResult.value) 1 else 0,
                        PrimitiveType(TypeProperty.I1)
                    )

                ast.BinaryOperator.EQUAL ->
                    ConstExpression(
                        if (lhsResult.value == rhsResult.value) 1 else 0,
                        PrimitiveType(TypeProperty.I1)
                    )

                ast.BinaryOperator.NOT_EQUAL ->
                    ConstExpression(
                        if (lhsResult.value != rhsResult.value) 1 else 0,
                        PrimitiveType(TypeProperty.I1)
                    )

                else -> throw InternalException("Unexpected binary operator")
            }
        } else {
            if (isCompareOperator(operator)) {
                return addCompareExpression(lhsResult, rhsResult, operator, function)
            }
            val dest = LocalVariable(unnamedVariableCount.toString(), type)
            unnamedVariableCount++
            val blocks = function.body ?: throw IRBuilderException("Function has no body")
            blocks.last().statements.add(
                BinaryOperationStatement(
                    dest,
                    when (operator) {
                        ast.BinaryOperator.ADD -> BinaryOperator.ADD
                        ast.BinaryOperator.SUB -> BinaryOperator.SUB
                        ast.BinaryOperator.MUL -> BinaryOperator.MUL
                        ast.BinaryOperator.DIV -> BinaryOperator.SDIV
                        ast.BinaryOperator.MOD -> BinaryOperator.SREM
                        ast.BinaryOperator.BITWISE_AND -> BinaryOperator.AND
                        ast.BinaryOperator.BITWISE_OR -> BinaryOperator.OR
                        ast.BinaryOperator.BITWISE_XOR -> BinaryOperator.XOR
                        ast.BinaryOperator.LEFT_SHIFT -> BinaryOperator.SHL
                        ast.BinaryOperator.RIGHT_SHIFT -> BinaryOperator.ASHR
                        else -> throw InternalException("Unexpected binary operator")
                    },
                    lhsResult,
                    rhsResult,
                )
            )
            return IrVariable(dest)
        }
    }

    private fun addStatement(statement: ast.Statement, function: GlobalFunction) {
        when (statement) {
            is ast.BlockStatement -> addStatement(statement, function)
            is ast.ExpressionStatement -> addStatement(statement, function)
            is ast.BranchStatement -> addStatement(statement, function)
            is ast.WhileStatement -> addStatement(statement, function)
            is ast.ForExpressionStatement -> addStatement(statement, function)
            is ast.ForDeclarationStatement -> addStatement(statement, function)
            is ast.ContinueStatement -> addStatement(statement, function)
            is ast.BreakStatement -> addStatement(statement, function)
            is ast.ReturnStatement -> addStatement(statement, function)
            is ast.VariablesDeclaration -> addStatement(statement, function)
            is ast.EmptyStatement -> {}
            else -> throw IRBuilderException("Unknown statement in addStatement")
        }
    }

    private fun addStatement(statement: ast.BlockStatement, function: GlobalFunction) {
        for (stmt in statement.statements) {
            addStatement(stmt, function)
            if (stmt is ast.ReturnStatement) return // skip the part after return
        }
    }

    private fun addStatement(statement: ast.ExpressionStatement, function: GlobalFunction) {
        addExpression(statement.expression, function, ExpectedState.VALUE)
    }

    private fun addStatement(statement: ast.ReturnStatement, function: GlobalFunction) {
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        if (statement.expression != null) {
            val returnValue = addExpression(statement.expression, function, ExpectedState.VALUE).toArgument()
            blocks.last().statements.add(BranchStatement(null, "return", null))
            val returnPhi = function.returnPhi ?: throw IRBuilderException("Non-void function has no return phi")
            returnPhi.incoming.add(Pair(returnValue, blocks.last().label))
        } else {
            blocks.last().statements.add(BranchStatement(null, "return", null))
        }
    }

    private fun addStatement(statement: ast.VariablesDeclaration, function: GlobalFunction) {
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        val variableList = function.variables ?: throw IRBuilderException("Function has no variable list")
        val type = irType(statement.type)
        val ptrType = PrimitiveType(TypeProperty.PTR)
        for (variable in statement.variables) {
            val binding = variable.binding ?: throw IRBuilderException("Variable has no binding")
            val dest = LocalVariable(binding.irInfo.toString(), ptrType)
            variableList.add(LocalVariableDecl(dest, type))
            if (variable.body != null) {
                val initializer = addExpression(variable.body, function, ExpectedState.VALUE).toArgument()
                blocks.last().statements.add(StoreStatement(dest, initializer))
            } else if (statement.type is ast.StringType) {
                blocks.last().statements.add(StoreStatement(dest, GlobalVariable("__empty_string", ptrType)))
            }
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

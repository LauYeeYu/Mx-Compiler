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
val voidType = PrimitiveType(TypeProperty.VOID)
val ptrType = PrimitiveType(TypeProperty.PTR)
val i32Type = PrimitiveType(TypeProperty.I32)
val i1Type = PrimitiveType(TypeProperty.I1)

class IR(private val root: AstNode) {
    private var unnamedVariableCount = 1
    private var unnamedStringLiteralCount = 0
    private var unnamedIterator = 0
    private var branchCount = 0
    private var loopCount = 0
    private var currentLoopCount = 0
    private var currentLoopHasStep = false
    private val globalVariableDecl = mutableListOf<GlobalDecl>()
    private val classes = mutableMapOf<String, GlobalClass>()
    private val globalFunctions = linkedMapOf<String, GlobalFunctionBuilder>()

    fun buildRoot(): Root {
        if (root !is ast.TranslateUnit) {
            throw IRBuilderException("The AST node in buildRoot is not a root node")
        }
        if (root.environment == null) {
            throw EnvironmentException("The AST node in buildRoot has no environment")
        }
        val classList = root.content.filterIsInstance<ast.Class>().map { registerClass(it) }
        val globalInit = GlobalFunctionBuilder(
            name = "__global_init",
            returnType = voidType,
            parameters = listOf(),
            variables = mutableListOf(),
            body = mutableListOf(Block("entry", mutableListOf())),
        )
        // register global functions
        globalFunctions["__global_init"] = globalInit
        globalFunctions.putAll(builtInFunctionMap)
        root.content.filterIsInstance<ast.Function>().forEach { function ->
            registerFunction(function.name, irType(function.returnType), function.bindings)
        }
        buildGlobalList(root.content)
        root.content.filterIsInstance<ast.Function>().forEach { buildFunction(it) }
        root.content.filterIsInstance<ast.Class>().forEach { buildClass(it) }
        return Root(
            classes = classList,
            variables = globalVariableDecl,
            globalFunctions = globalFunctions.values.toList().map { it.toGlobalFunction() },
        )
    }

    private fun registerFunction(
        name: String,
        returnIrType: PrimitiveType,
        parameters: List<Binding>,
        isMember: Boolean = false,
    ) {
        val functionParameter = parameters.map { astParameter ->
            FunctionParameter(irType(astParameter.type), "${astParameter.irInfo}.param")
        }
        val variables: MutableList<AllocaStatement> = parameters.map { astParameter ->
            AllocaStatement(
                LocalVariable(astParameter.irInfo.toString(), ptrType),
                irType(astParameter.type),
            )
        }.toMutableList()
        val body: MutableList<Statement> = parameters.map { astParameter ->
            val type = irType(astParameter.type)
            StoreStatement(
                dest = LocalVariable(astParameter.irInfo.toString(), ptrType),
                src = LocalVariable("${astParameter.irInfo}.param", type),
            )
        }.toMutableList()
        globalFunctions[name] = GlobalFunctionBuilder(
            name = name,
            returnType = returnIrType,
            parameters = if (isMember) {
                listOf(FunctionParameter(ptrType, "__this.param")) + functionParameter
            } else {
                functionParameter
            },
            variables = if (isMember) (listOf(
                AllocaStatement(LocalVariable("__this", ptrType), ptrType)
            ) + variables).toMutableList() else variables,
            body = if (isMember) mutableListOf(Block("entry", (listOf<Statement>(
                StoreStatement(
                    dest = LocalVariable("__this", ptrType),
                    src = LocalVariable("__this.param", ptrType),
                )
            ) + body).toMutableList())) else mutableListOf(Block("entry", body)),
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
        globalVariableDecl.add(emptyString)
        sourceList.filterIsInstance<ast.VariablesDeclaration>().forEach { element ->
            when (element.type) {
                is ast.BoolType, is ast.IntType -> {
                    val type = irType(element.type)
                    element.variables.forEach { variable ->
                        primitiveVariableDeclInit(variable, type, globalInit)
                    }
                }

                is ast.StringType -> for (variable in element.variables) {
                    val binding = variable.binding ?: throw IRBuilderException("Variable has no binding")
                    val name = binding.irInfo.toString()
                    when (variable.body) {
                        null ->
                            globalVariableDecl.add(
                                GlobalVariableDecl(
                                    GlobalVariable(name, ptrType),
                                    ptrType,
                                    GlobalVariable("__empty_string", ptrType),
                                )
                            )

                        is StringLiteral -> {
                            globalVariableDecl.add(StringLiteralDecl("$name.str", variable.body.value))
                            globalVariableDecl.add(
                                GlobalVariableDecl(
                                    GlobalVariable(name, ptrType),
                                    ptrType,
                                    GlobalVariable("$name.str", ptrType),
                                )
                            )
                        }

                        else -> {
                            val result = addExpression(
                                variable.body,
                                globalInit,
                                ExpectedState.VALUE,
                            ).toArgument()
                            globalVariableDecl.add(
                                GlobalVariableDecl(
                                    GlobalVariable(name, ptrType),
                                    ptrType,
                                    GlobalVariable("__empty_string", ptrType),
                                )
                            )
                            globalInitBlocks.last().statements.add(
                                StoreStatement(
                                    GlobalVariable(name, ptrType),
                                    result,
                                )
                            )
                        }
                    }
                }

                is ast.ArrayType, is ast.ClassType -> {
                    val type = ptrType
                    element.variables.forEach { variable ->
                        val binding = variable.binding ?: throw IRBuilderException("Variable has no binding")
                        val name = binding.irInfo.toString()
                        val variableProperty = GlobalVariable(name, type)
                        if (variable.body != null) {
                            val result = addExpression(
                                variable.body,
                                globalInit,
                                ExpectedState.VALUE,
                            ).toArgument()
                            globalInitBlocks.last().statements.add(
                                StoreStatement(dest = variableProperty, src = result)
                            )
                        }
                        globalVariableDecl.add(GlobalVariableDecl(variableProperty, ptrType, NullLiteral()))
                    }
                }
            }
        }
        globalInitBlocks.last().statements.add(BranchStatement("return"))
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
            returnIrType = voidType,
            parameters = listOf(),
            isMember = true,
        )
        return returnClass
    }

    private fun irType(astType: ast.Type): PrimitiveType = when (astType) {
        is ast.VoidType -> voidType
        is ast.BoolType -> i1Type
        is ast.IntType -> i32Type
        is ast.StringType -> ptrType
        is ast.ArrayType -> ptrType
        is ast.ClassType -> ptrType
        else -> throw IRBuilderException("Unknown type in irType")
    }

    private fun irType(internalType: MxType): PrimitiveType = when (internalType) {
        is MxVoidType -> voidType
        is MxBoolType -> i1Type
        is MxIntType -> i32Type
        is MxStringType -> ptrType
        is MxNullType -> ptrType
        is MxArrayType -> ptrType
        is MxClassType -> ptrType
        else -> throw IRBuilderException("Unknown type in irType")
    }

    private fun buildFunction(astNode: ast.Function) {
        if (astNode.environment == null) {
            throw EnvironmentException("The AST node in buildFunction has no environment")
        }
        val function = globalFunctions[astNode.name]
            ?: throw IRBuilderException("Function ${astNode.name} not found")
        unnamedVariableCount = 1
        if (astNode.name == "main") {
            val blocks = function.body ?: throw IRBuilderException("Function has no body")
            blocks.last().statements.add(
                CallStatement(
                    dest = null,
                    returnType = voidType,
                    function = globalFunctions["__global_init"]?.toGlobalFunctionDecl()
                        ?: throw IRBuilderException("Function __global_init not found"),
                    arguments = listOf(),
                )
            )
        }
        addStatement(astNode.body,function)
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        if (function.returnType.type == TypeProperty.VOID) {
            if (blocks.last().statements.lastOrNull() !is BranchStatement) {
                blocks.last().statements.add(BranchStatement("return"))
            }
        } else {
            if (function.name == "main" &&
                blocks.last().statements.lastOrNull() !is BranchStatement) {
                // main function should return 0
                blocks.last().statements.add(BranchStatement("return"))
                val returnPhi = function.returnPhi ?: throw IRBuilderException("Function has no return phi")
                returnPhi.incoming.add(Pair(I32Literal(0), blocks.last().label))
            }
        }
    }

    // Build class functions and constructor
    private fun buildClass(astNode: ast.Class) {
        if (astNode.environment == null) {
            throw EnvironmentException("The AST node in buildClass has no environment")
        }
        var hasConstructor = false
        for (node in astNode.body) {
            if (node is ast.Function) {
                val function = globalFunctions["${astNode.name}.${node.name}"]
                    ?: throw IRBuilderException("Function ${astNode.name}.${node.name} not found")
                unnamedVariableCount = 1
                addStatement(node.body, function)
                val blocks = function.body ?: throw IRBuilderException("Function has no body")
                if (function.returnType.type == TypeProperty.VOID) {
                    if (blocks.last().statements.lastOrNull() !is ReturnStatement) {
                        blocks.last().statements.add(BranchStatement("return"))
                    }
                }
            } else if (node is ast.Constructor) {
                hasConstructor = true
                buildClassConstructor(astNode, node)
            }
        }
        if (!hasConstructor) buildClassConstructor(astNode, null)
    }

    private fun buildClassConstructor(classNode: ast.Class, constructor: ast.Constructor?) {
        val function = globalFunctions["${classNode.name}.${classNode.name}"]
            ?: throw IRBuilderException("Function ${classNode.name}.${classNode.name} not found")
        unnamedVariableCount = 1

        // Add variable initialization
        val classIrNode: GlobalClass = classes[classNode.name]
            ?: throw IRBuilderException("Class ${classNode.name} not found")
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        val thisPtr = getThisPtr(function)
        classNode.body.filterIsInstance<ast.VariablesDeclaration>().forEach { node ->
            node.variables.forEach { variable ->
                val ptr = LocalVariable("__${variable.name}.ptr", ptrType)
                val index = classIrNode.nameMap[variable.name]
                    ?: throw IRBuilderException("Member ${variable.name} not found")
                val ptrStatement = GetElementPtrStatement(
                    dest = ptr,
                    src = thisPtr,
                    srcType = classIrNode.classType,
                    indices = listOf(I32Literal(0), I32Literal(index)),
                )
                if (variable.body != null) {
                    val value = addExpression(variable.body, function, ExpectedState.VALUE).toArgument()
                    blocks.last().statements.add(ptrStatement)
                    blocks.last().statements.add(StoreStatement(dest = ptr, src = value))
                } else if (node.type is ast.ClassType || node.type is ast.ArrayType) {
                    blocks.last().statements.add(ptrStatement)
                    blocks.last().statements.add(StoreStatement(dest = ptr, src = I32Literal(0)))
                } else if (node.type is ast.StringType) {
                    val emptyString = GlobalVariable("__empty_string", ptrType)
                    blocks.last().statements.add(ptrStatement)
                    blocks.last().statements.add(StoreStatement(dest = ptr, src = emptyString))
                }
            }
        }

        // Add constructor body
        if (constructor != null) addStatement(constructor.body, function)
        if (blocks.last().statements.lastOrNull() !is ReturnStatement) {
            blocks.last().statements.add(BranchStatement("return"))
        }
    }

    private fun primitiveVariableDeclInit(
        variable: ast.VariableDeclaration,
        type: PrimitiveType,
        function: GlobalFunctionBuilder, // variable initializing statement will not have a branch
    ) {
        val binding = variable.binding ?: throw IRBuilderException("Variable has no binding")
        val name = binding.irInfo.toString()
        val returnValue: Argument = when (variable.body) {
            null -> when (type.type) {
                TypeProperty.I1 -> I1Literal(0)
                TypeProperty.I32 -> I32Literal(0)
                else -> throw IRBuilderException("Unknown type in primitiveVariableDeclInit")
            }

            else -> addExpression(variable.body, function, ExpectedState.VALUE).toArgument()
        }

        val irVariable = GlobalVariable(name, ptrType)
        when (returnValue) {
            is IntLiteral ->
                globalVariableDecl.add(GlobalVariableDecl(irVariable, type, I32Literal(returnValue.value)))

            is Variable -> {
                globalVariableDecl.add(GlobalVariableDecl(irVariable, type, I32Literal(0)))
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
        function: GlobalFunctionBuilder,
        expectedState: ExpectedState,
    ): ExpressionResult = when (expr) {
        is ast.Object -> addExpression(expr, function, expectedState)
        is StringLiteral -> addExpression(expr)
        is IntegerLiteral -> addExpression(expr)
        is BooleanLiteral -> addExpression(expr)
        is ast.NullLiteral -> ConstExpression(0, ptrType)
        is ThisLiteral -> IrVariable(getThisPtr(function))
        is MemberVariableAccess -> addExpression(expr, function, expectedState)
        is MemberFunctionAccess -> addExpression(expr, function)
        is ArrayExpression -> addExpression(expr, function, expectedState)
        is PrefixUpdateExpression -> addUpdateExpression(expr, function, expectedState)
        is FunctionCall -> addExpression(expr, function)
        is LambdaCall ->
            throw NotSupported("Lambda call is not supported in IRBuilder")

        is LambdaExpression ->
            throw NotSupported("Lambda expression is not supported in IRBuilder")

        is NewExpression -> addExpression(expr, function)
        is PostfixUpdateExpression -> addUpdateExpression(expr, function, ExpectedState.VALUE)
        is UnaryExpression -> addExpression(expr, function)
        is BinaryExpression -> addExpression(expr, function)
        is AssignExpression -> addExpression(expr, function)
        else -> throw IRBuilderException("Unknown expression in addExpression")
    }

    private fun getThisPtr(function: GlobalFunctionBuilder): LocalVariable {
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        val thisPtr = LocalVariable("__this.val.${unnamedVariableCount++}", ptrType)
        blocks.last().statements.add(
            LoadStatement(dest = thisPtr, src = LocalVariable("__this", ptrType))
        )
        return thisPtr
    }

    private fun addExpression(
        expr: ast.Object,
        function: GlobalFunctionBuilder,
        expectedState: ExpectedState,
    ): ExpressionResult {
        val binding = expr.binding ?: throw IRBuilderException("Object has no binding")
        val type = irType(binding.type)
        if (binding.fromClass == null) {
            val srcVariable: Variable = when (binding.irInfo.isLocal) {
                true -> LocalVariable(binding.irInfo.toString(), ptrType)
                false -> GlobalVariable(binding.irInfo.toString(), ptrType)
            }
            return when (expectedState) {
                ExpectedState.PTR -> IrVariable(srcVariable)
                ExpectedState.VALUE -> {
                    val dest = LocalVariable("${binding.irInfo}.val.${unnamedVariableCount++}", type)
                    val blocks = function.body ?: throw IRBuilderException("Function has no body")
                    blocks.last().statements.add(LoadStatement(dest = dest, src = srcVariable))
                    IrVariable(dest)
                }
            }
        } else { // member variable
            val srcClass = classes[binding.fromClass.name]
                ?: throw IRBuilderException("Class ${binding.fromClass.name} not found")
            val index = srcClass.nameMap[expr.name]
                ?: throw IRBuilderException("Cannot find the index of the member")
            val thisPtr = getThisPtr(function)
            val ptr = LocalVariable(
                "${srcClass.classType.name}.${expr.name}.ptr.${unnamedVariableCount++}",
                ptrType,
            )
            val blocks = function.body ?: throw IRBuilderException("Function has no body")
            blocks.last().statements.add(
                GetElementPtrStatement(
                    dest = ptr,
                    src = thisPtr,
                    srcType = srcClass.classType,
                    indices = listOf(I32Literal(0), I32Literal(index)),
                )
            )
            return when (expectedState) {
                ExpectedState.PTR -> IrVariable(ptr)
                ExpectedState.VALUE -> {
                    val dest = LocalVariable(
                        "${srcClass.classType.name}.${expr.name}.val.${unnamedVariableCount++}",
                        type,
                    )
                    blocks.last().statements.add(LoadStatement(dest = dest, src = ptr))
                    IrVariable(dest)
                }
            }
        }
    }

    private fun addExpression(expr: StringLiteral): ExpressionResult {
        val name = "__string_$unnamedStringLiteralCount"
        globalVariableDecl.add(StringLiteralDecl(name, expr.value))
        val variable = GlobalVariable(name, PrimitiveType(TypeProperty.ARRAY))
        unnamedStringLiteralCount++
        return IrVariable(variable)
    }

    private fun addExpression(expr: IntegerLiteral): ExpressionResult =
        ConstExpression(expr.value, i32Type)

    private fun addExpression(expr: BooleanLiteral): ExpressionResult =
        when (expr.value) {
            true -> ConstExpression(1, i1Type)
            false -> ConstExpression(0, i1Type)
        }

    private fun addExpression(
        expr: MemberVariableAccess,
        function: GlobalFunctionBuilder,
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
        val ptrDest = LocalVariable(
            "${classType.name}.${expr.variableName}.ptr.${unnamedVariableCount++}",
            ptrType,
        )
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        blocks.last().statements.add(
            GetElementPtrStatement(
                dest = ptrDest,
                src = source,
                srcType = srcType.classType,
                indices = listOf<Argument>(I32Literal(0), I32Literal(index)),
            )
        )
        return when (expectedState) {
            ExpectedState.PTR -> IrVariable(ptrDest)
            ExpectedState.VALUE -> {
                val valDest = LocalVariable(
                    "${classType.name}.${expr.variableName}.val.${unnamedVariableCount++}",
                    type,
                )
                blocks.last().statements.add(LoadStatement(dest = valDest, src = ptrDest))
                IrVariable(valDest)
            }
        }
    }

    private fun addExpression(
        expr: MemberFunctionAccess,
        function: GlobalFunctionBuilder,
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
        val calledFunction: GlobalFunction =
            globalFunctions["${classType.name}.${expr.functionName}"]?.toGlobalFunctionDecl()
            ?: throw InternalException("Cannot find find the function ${classType.name}.${expr.functionName}")
        val arguments = mutableListOf<Argument>(classPtr) + expr.arguments.map {
            addExpression(it, function, ExpectedState.VALUE).toArgument()
        }
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        return if (expr.resultType!!.type is MxVoidType) {
            blocks.last().statements.add(CallStatement(null, type, calledFunction, arguments))
            VoidResult()
        } else {
            val dest = LocalVariable(
                "${classType.name}.${expr.functionName}.ret.${unnamedVariableCount++}",
                type,
            )
            blocks.last().statements.add(CallStatement(dest, type, calledFunction, arguments))
            IrVariable(dest)
        }
    }

    private fun addExpression(
        expr: FunctionCall,
        function: GlobalFunctionBuilder,
    ): ExpressionResult {
        if (expr.resultType == null) {
            throw EnvironmentException("The AST node in addExpression has no result type")
        }
        val type = irType(expr.resultType!!.type)
        val fromClass = expr.fromClass
        val calledFunction: GlobalFunction = if (fromClass != null) {
            globalFunctions["${fromClass.name}.${expr.functionName}"]?.toGlobalFunctionDecl()
                ?: throw InternalException("Cannot find find the function ${fromClass.name}.${expr.functionName}")
        } else {
            globalFunctions[expr.functionName]?.toGlobalFunctionDecl()
                ?: throw InternalException("Cannot find find the function ${expr.functionName}")
        }
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        val thisPointer = if (fromClass != null) {
            val thisPtr = LocalVariable("__this.val.${unnamedVariableCount++}", ptrType)
            blocks.last().statements.add(
                LoadStatement(
                    dest = thisPtr,
                    src = LocalVariable("__this", ptrType),
                )
            )
            thisPtr
        } else {
            null
        }
        val arguments: List<Argument> = if (thisPointer != null) {
            listOf<Argument>(thisPointer) + expr.arguments.map {
                addExpression(it, function, ExpectedState.VALUE).toArgument()
            }
        } else {
            expr.arguments.map {
                addExpression(it, function, ExpectedState.VALUE).toArgument()
            }
        }
        return if (expr.resultType!!.type is MxVoidType) {
            blocks.last().statements.add(CallStatement(null, type, calledFunction, arguments))
            VoidResult()
        } else {
            val dest = LocalVariable("${calledFunction.name}.ret.${unnamedVariableCount++}", type)
            blocks.last().statements.add(CallStatement(dest, type, calledFunction, arguments))
            IrVariable(dest)
        }
    }

    private fun addExpression(
        expr: ArrayExpression,
        function: GlobalFunctionBuilder,
        expectedState: ExpectedState,
    ): ExpressionResult {
        if (expr.resultType == null || expr.array.resultType == null || expr.index.resultType == null) {
            throw EnvironmentException("The AST node in addExpression has no result type")
        }
        val type = irType(expr.resultType!!.type)
        val array = addExpression(expr.array, function, ExpectedState.VALUE).toArgument() as? Variable
            ?: throw InternalException("The array is not a variable")
        val index = addExpression(expr.index, function, ExpectedState.VALUE).toArgument()
        val ptrDest = LocalVariable("__ptr.${unnamedVariableCount++}", ptrType)
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        // add the ptr to the target
        blocks.last().statements.add(
            GetElementPtrStatement(
                dest = ptrDest,
                src = array,
                srcType = type,
                indices = listOf(index),
            )
        )
        return when (expectedState) {
            ExpectedState.PTR -> IrVariable(ptrDest)
            ExpectedState.VALUE -> {
                val valueDest = LocalVariable("__val.${unnamedVariableCount++}", type)
                blocks.last().statements.add(
                    LoadStatement(dest = valueDest, src = ptrDest)
                )
                IrVariable(valueDest)
            }
        }
    }

    private fun addUpdateExpression(
        expr: UpdateExpression,
        function: GlobalFunctionBuilder,
        expectedState: ExpectedState,
    ): ExpressionResult {
        if (expr.resultType == null || expr.operand.resultType == null) {
            throw EnvironmentException("The AST node in addExpression has no result type")
        }
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        val type = irType(expr.resultType!!.type)
        val operand = addExpression(expr.operand, function, ExpectedState.PTR).toArgument() as? Variable
            ?: throw InternalException("The operand is not a variable")
        val rhs = when (expr.operator) {
            UpdateOperator.INCREMENT -> I32Literal(1)
            UpdateOperator.DECREMENT -> I32Literal(-1)
        }
        // load the value
        val value = LocalVariable("__update.old.${unnamedVariableCount++}", type)
        blocks.last().statements.add(LoadStatement(dest = value, src = operand))
        // update the value
        val addDest = LocalVariable("__update.new.${unnamedVariableCount++}", type)
        blocks.last().statements.add(
            BinaryOperationStatement(dest = addDest, op = BinaryOperator.ADD, lhs = value, rhs = rhs)
        )
        // store the value
        blocks.last().statements.add(
            StoreStatement(
                dest = operand,
                src = addDest,
            )
        )
        return when (expectedState) {
            ExpectedState.PTR -> IrVariable(operand)
            ExpectedState.VALUE -> when (expr) {
                is PostfixUpdateExpression -> IrVariable(value)
                is PrefixUpdateExpression -> IrVariable(addDest)
                else -> throw InternalException("Unknown update expression")
            }
        }
    }

    private fun addExpression(
        expr: NewExpression,
        function: GlobalFunctionBuilder,
    ): ExpressionResult {
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        if (expr.dimension == 0) { // New class
            return IrVariable(addNewClass(blocks, (expr.type as ast.ClassType)))
        }

        // New array
        val arguments = expr.arguments.map { addExpression(it, function, ExpectedState.VALUE).toArgument() }
        val iterators = mutableListOf<LocalVariable>()
        val size = if (expr.dimension == expr.arguments.size &&
            (expr.type is ast.IntType || expr.type is ast.BoolType)){
                expr.arguments.size - 1
            } else {
                expr.arguments.size
            }
        for (i in 0 until size) {
            iterators.add(LocalVariable("__iterator_$unnamedIterator", ptrType))
            unnamedIterator++
        }
        function.variables?.addAll(iterators.map { AllocaStatement(it, i32Type) })
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
        val array = LocalVariable("__array.${unnamedVariableCount++}", ptrType)
        blocks.last().statements.add(
            CallStatement(
                dest = array,
                returnType = ptrType,
                function = if (dimension == 1) {
                    when (type) {
                        is ast.IntType -> globalFunctions["__newIntArray"]?.toGlobalFunctionDecl()
                            ?: throw InternalException("Function __newIntArray not found")

                        is ast.BoolType -> globalFunctions["__newBoolArray"]?.toGlobalFunctionDecl()
                            ?: throw InternalException("Function __newBoolArray not found")

                        else -> globalFunctions["__newPtrArray"]?.toGlobalFunctionDecl()
                            ?: throw InternalException("Function __newPtrArray not found")
                    }
                } else {
                    globalFunctions["__newPtrArray"]?.toGlobalFunctionDecl()
                        ?: throw InternalException("Function __newPtrArray not found")
                },
                arguments = listOf(arguments[0]),
            )
        )

        // When the variable is int or bool, we don't need to initialize the array
        if (index + 1 == arguments.size &&
            (dimension != 1 || type !is ast.StringType)) {
            return array
        }
        // Set the initial number of the iterator, i.e. __iterator__i = 0
        blocks.last().statements.add(
            StoreStatement(
                dest = iterators[index],
                src = I32Literal(0),
            )
        )
        val loopConditionLabel = "new_loop_condition_${blocks.size}"
        val loopStartLabel = "new_loop_start_${blocks.size + 1}"
        // Jump to the loop condition
        blocks.last().statements.add(
            BranchStatement(
                condition = null,
                trueBlockLabel = loopConditionLabel,
                falseBlockLabel = null
            )
        )

        // Add the loop condition
        val iteratorValue = LocalVariable(
            "__iter.val.${unnamedVariableCount++}",
            i32Type,
        )
        val compareResult = LocalVariable(
            "__iter.comp.${unnamedVariableCount++}",
            i1Type,
        )
        blocks.add(
            Block(
                loopConditionLabel,
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
        val loopConditionBlock = blocks.last()

        // Add the loop body
        val position = LocalVariable("__new.ptr.${unnamedVariableCount++}", ptrType)
        blocks.add(
            Block(
                loopStartLabel,
                mutableListOf(
                    GetElementPtrStatement(
                        dest = position,
                        src = array,
                        srcType = ptrType,
                        indices = listOf(iteratorValue),
                    )
                )
            )
        )
        val loopStartBlock = blocks.last()
        if (index + 1 == arguments.size) {
            if (dimension == 1) {
                when (type) {
                    is ast.StringType -> {
                        loopStartBlock.statements.add(
                            StoreStatement(
                                position,
                                GlobalVariable("__empty_string", ptrType))
                        )
                    }
                    else -> throw InternalException("Unknown type")
                }
            }
        } else {
            val newArray = addNewExpressionLoop(blocks, arguments, iterators, dimension - 1, index + 1, type)
            loopStartBlock.statements.add(StoreStatement(dest = position, src = newArray))
        }

        // Add the loop increment
        val incrementLabel = "new_increment_${blocks.size}"
        val endBlockLabel = "new_end_${blocks.size + 1}"
        loopStartBlock.statements.add(BranchStatement(incrementLabel))
        loopConditionBlock.statements.add(
            BranchStatement(
                condition = compareResult,
                trueBlockLabel = loopStartLabel,
                falseBlockLabel = endBlockLabel,
            )
        )
        val iteratorOld = LocalVariable("__iter.old.${unnamedVariableCount++}", i32Type)
        val iteratorNew = LocalVariable("__iter.new.${unnamedVariableCount++}", i32Type)
        blocks.add(
            Block(
                incrementLabel,
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
                        trueBlockLabel = loopConditionLabel,
                        falseBlockLabel = null,
                    ),
                )
            )
        )
        blocks.add(Block(endBlockLabel, mutableListOf()))
        return array
    }

    private fun addNewClass(
        blocks: MutableList<Block>,
        type: ast.ClassType,
    ): LocalVariable {
        val dest = LocalVariable("class.ptr.${unnamedVariableCount++}", ptrType)
        val className = type.name
        val classInfo = classes[className]
            ?: throw EnvironmentException("Cannot find class $className")
        blocks.last().statements.add(
            CallStatement(
                dest = dest,
                returnType = ptrType,
                function = globalFunctions["malloc"]?.toGlobalFunctionDecl()
                    ?: throw InternalException("Cannot find malloc in builtin function"),
                arguments = listOf(I32Literal(classInfo.classType.memberList.size * 4)),
            )
        )
        blocks.last().statements.add(
            CallStatement(
                dest = null,
                returnType = voidType,
                function = globalFunctions["$className.$className"]?.toGlobalFunctionDecl()
                    ?: throw InternalException("Cannot find constructor"),
                arguments = listOf(dest),
            )
        )
        return dest
    }

    private fun addExpression(
        expr: UnaryExpression,
        function: GlobalFunctionBuilder,
    ): ExpressionResult {
        if (expr.resultType == null || expr.operand.resultType == null) {
            throw EnvironmentException("The AST node in addExpression has no result type")
        }
        val type = irType(expr.resultType!!.type)
        val operand = addExpression(expr.operand, function, ExpectedState.VALUE).toArgument()
        if (operand is IntLiteral) {
            return when (expr.operator) {
                UnaryOperator.NEGATIVE ->
                    ConstExpression(-operand.value, i32Type)

                UnaryOperator.POSITIVE ->
                    ConstExpression(operand.value, i32Type)

                UnaryOperator.BITWISE_NOT ->
                    ConstExpression(operand.value.inv(), i32Type)

                UnaryOperator.LOGICAL_NOT ->
                    ConstExpression(if (operand.value == 0) 1 else 0, i1Type)
            }
        }
        if (expr.operator == UnaryOperator.POSITIVE) return IrVariable(operand as Variable)

        val dest = LocalVariable("__unary.${unnamedVariableCount++}", type)
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
                        rhs = I1Literal(1),
                    )
                )
            }

            else -> throw InternalException("Unexpected unary operator")
        }
        return IrVariable(dest)
    }

    private fun addExpression(
        expr: BinaryExpression,
        function: GlobalFunctionBuilder,
    ): ExpressionResult = when (expr.left.resultType?.type) {
        is MxStringType ->
            addStringBinaryExpression(expr.left, expr.right, expr.operator, function)

        is MxBoolType -> when (expr.operator) {
            ast.BinaryOperator.LOGICAL_OR, ast.BinaryOperator.LOGICAL_AND ->
                addBinaryLogicExpression(expr.left, expr.right, expr.operator, function)
            else -> {
                val srcType = expr.resultType?.type
                    ?: throw EnvironmentException("The AST node in addExpression has no result type")
                val type = irType(srcType)
                addBinaryArithmeticExpression(expr.left, expr.right, expr.operator, type, function)
            }
        }

        is MxIntType -> {
            val srcType = expr.resultType?.type
                ?: throw EnvironmentException("The AST node in addExpression has no result type")
            val type = irType(srcType)
            addBinaryArithmeticExpression(expr.left, expr.right, expr.operator, type, function)
        }

        null -> throw EnvironmentException("The AST node in addExpression has no result type")
        else -> { // should be ptr only
            val srcType = expr.resultType?.type
                ?: throw EnvironmentException("The AST node in addExpression has no result type")
            val type = irType(srcType)
            addBinaryArithmeticExpression(expr.left, expr.right, expr.operator, type, function)
        }
    }

    private fun addExpression(
        expr: AssignExpression,
        function: GlobalFunctionBuilder,
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
        function: GlobalFunctionBuilder,
    ): ExpressionResult {
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        val lhsResult = addExpression(lhs, function, ExpectedState.VALUE).toArgument()
        val lhsResultBlockIndex = blocks.last().label
        val lhsNext = "logic_${blocks.size}"
        val lhsResultBlock = blocks.last()
        when (lhsResult) {
            is Variable -> {
                blocks.add(Block(lhsNext, mutableListOf()))
                when (val rhsResult = addExpression(rhs, function, ExpectedState.VALUE).toArgument()) {
                    is Variable -> {
                        val rhsResultBlockIndex = blocks.last().label
                        val rhsResultBlock = blocks.last()
                        val rhsNext = "logic_${blocks.size}"
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
                            "__logical.${unnamedVariableCount++}",
                            i1Type,
                        )
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
                            ConstExpression(0, i1Type)
                        } else if (operator == ast.BinaryOperator.LOGICAL_OR && rhsResult.value == 1) {
                            blocks.removeAt(blocks.lastIndex)
                            ConstExpression(1, i1Type)
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
                    ConstExpression(0, i1Type)
                } else if (operator == ast.BinaryOperator.LOGICAL_OR && lhsResult.value == 1) {
                    ConstExpression(1, i1Type)
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
        type: PrimitiveType,
        function: GlobalFunctionBuilder,
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
                                i1Type
                            )

                        ast.BinaryOperator.NOT_EQUAL ->
                            ConstExpression(
                                if (lhsResult.value != rhsResult.value) 1 else 0,
                                i1Type
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
        function: GlobalFunctionBuilder,
    ): ExpressionResult {
        val dest = LocalVariable("__comp.${unnamedVariableCount++}", i1Type)
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
        function: GlobalFunctionBuilder,
    ): ExpressionResult {
        if (lhs.resultType == null || rhs.resultType == null) {
            throw EnvironmentException("The AST node in addExpression has no result type")
        }
        val destType = when (operator) {
            ast.BinaryOperator.ADD -> ptrType
            ast.BinaryOperator.LESS_THAN, ast.BinaryOperator.LESS_THAN_OR_EQUAL,
            ast.BinaryOperator.GREATER_THAN, ast.BinaryOperator.GREATER_THAN_OR_EQUAL,
            ast.BinaryOperator.EQUAL, ast.BinaryOperator.NOT_EQUAL -> i1Type

            else -> throw InternalException("The AST node in addExpression has an unsupported type")
        }
        val lhsArg = addExpression(lhs, function, ExpectedState.VALUE).toArgument()
        val rhsArg = addExpression(rhs, function, ExpectedState.VALUE).toArgument()
        val dest = LocalVariable("__binary.${unnamedVariableCount++}", destType)
        val statement = when (operator) {
            ast.BinaryOperator.ADD -> CallStatement(
                dest = dest,
                returnType = ptrType,
                function = builtInFunctionMap["string.add"]?.toGlobalFunctionDecl()
                    ?: throw InternalException("The built-in function string.add is not found"),
                arguments = listOf(lhsArg, rhsArg),
            )

            ast.BinaryOperator.LESS_THAN -> CallStatement(
                dest = dest,
                returnType = i1Type,
                function = builtInFunctionMap["string.less"]?.toGlobalFunctionDecl()
                    ?: throw InternalException("The built-in function string.lessThan is not found"),
                arguments = listOf(lhsArg, rhsArg),
            )

            ast.BinaryOperator.LESS_THAN_OR_EQUAL -> CallStatement(
                dest = dest,
                returnType = i1Type,
                function = builtInFunctionMap["string.lessOrEqual"]?.toGlobalFunctionDecl()
                    ?: throw InternalException("The built-in function string.lessThanOrEqual is not found"),
                arguments = listOf(lhsArg, rhsArg),
            )

            ast.BinaryOperator.GREATER_THAN -> CallStatement(
                dest = dest,
                returnType = i1Type,
                function = builtInFunctionMap["string.greater"]?.toGlobalFunctionDecl()
                    ?: throw InternalException("The built-in function string.greaterThan is not found"),
                arguments = listOf(lhsArg, rhsArg),
            )

            ast.BinaryOperator.GREATER_THAN_OR_EQUAL -> CallStatement(
                dest = dest,
                returnType = i1Type,
                function = builtInFunctionMap["string.greaterOrEqual"]?.toGlobalFunctionDecl()
                    ?: throw InternalException("The built-in function string.greaterThanOrEqual is not found"),
                arguments = listOf(lhsArg, rhsArg),
            )

            ast.BinaryOperator.EQUAL -> CallStatement(
                dest = dest,
                returnType = i1Type,
                function = builtInFunctionMap["string.equal"]?.toGlobalFunctionDecl()
                    ?: throw InternalException("The built-in function string.equal is not found"),
                arguments = listOf(lhsArg, rhsArg),
            )

            ast.BinaryOperator.NOT_EQUAL -> CallStatement(
                dest = dest,
                returnType = i1Type,
                function = builtInFunctionMap["string.notEqual"]?.toGlobalFunctionDecl()
                    ?: throw InternalException("The built-in function string.notEqual is not found"),
                arguments = listOf(lhsArg, rhsArg),
            )

            else -> throw InternalException("The AST node in addExpression has an unsupported type")
        }
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        blocks.last().statements.add(statement)
        return IrVariable(dest)
    }

    private fun addIntBinaryExpression(
        lhsResult: Argument,
        rhsResult: Argument,
        type: PrimitiveType,
        operator: ast.BinaryOperator,
        function: GlobalFunctionBuilder,
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
                        i1Type
                    )

                ast.BinaryOperator.LESS_THAN_OR_EQUAL ->
                    ConstExpression(
                        if (lhsResult.value <= rhsResult.value) 1 else 0,
                        i1Type
                    )

                ast.BinaryOperator.GREATER_THAN ->
                    ConstExpression(
                        if (lhsResult.value > rhsResult.value) 1 else 0,
                        i1Type
                    )

                ast.BinaryOperator.GREATER_THAN_OR_EQUAL ->
                    ConstExpression(
                        if (lhsResult.value >= rhsResult.value) 1 else 0,
                        i1Type
                    )

                ast.BinaryOperator.EQUAL ->
                    ConstExpression(
                        if (lhsResult.value == rhsResult.value) 1 else 0,
                        i1Type
                    )

                ast.BinaryOperator.NOT_EQUAL ->
                    ConstExpression(
                        if (lhsResult.value != rhsResult.value) 1 else 0,
                        i1Type
                    )

                else -> throw InternalException("Unexpected binary operator")
            }
        } else {
            if (isCompareOperator(operator)) {
                return addCompareExpression(lhsResult, rhsResult, operator, function)
            }
            val dest = LocalVariable("__binary.${unnamedVariableCount++}", type)
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

    // The return value indicates whether the control flow terminates after this statement
    private fun addStatement(statement: ast.Statement, function: GlobalFunctionBuilder): Boolean =
        when (statement) {
            is ast.BlockStatement -> addStatement(statement, function)
            is ast.ExpressionStatement -> addStatement(statement, function)
            is ast.BranchStatement -> addStatement(statement, function)
            is ast.WhileStatement -> addLoopStatement(statement, function)
            is ast.ForExpressionStatement -> addLoopStatement(statement, function)
            is ast.ForDeclarationStatement -> addLoopStatement(statement, function)
            is ast.ContinueStatement -> addContinueStatement(function)
            is ast.BreakStatement -> addBreakStatement(function)
            is ast.ReturnStatement -> addStatement(statement, function)
            is ast.VariablesDeclaration -> addStatement(statement, function)
            is ast.EmptyStatement -> false
            else -> throw IRBuilderException("Unknown statement in addStatement")
        }

    private fun addStatement(statement: ast.BlockStatement, function: GlobalFunctionBuilder): Boolean {
        for (stmt in statement.statements) {
            val hasReturn = addStatement(stmt, function)
            if (hasReturn) return true // skip the part after return
        }
        return false
    }

    private fun addStatement(statement: ast.ExpressionStatement, function: GlobalFunctionBuilder): Boolean {
        addExpression(statement.expression, function, ExpectedState.VALUE)
        return false
    }

    private fun addStatement(statement: ast.BranchStatement, function: GlobalFunctionBuilder): Boolean {
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        val condition = addExpression(statement.condition, function, ExpectedState.VALUE).toArgument()
        if (condition is IntLiteral) {
            if (condition.value == 1) {
                return addStatement(statement.trueBranch, function)
            } else if (statement.falseBranch != null) {
                return addStatement(statement.falseBranch, function)
            }
        }
        // condition is a variable
        val trueBlockLabel = "true_$branchCount"
        val falseBlockLabel = "false_$branchCount"
        val endBlockLabel = "condition_end_$branchCount"
        branchCount++
        blocks.last().statements.add(
            BranchStatement(
                condition,
                trueBlockLabel,
                if (statement.falseBranch != null) falseBlockLabel else endBlockLabel
            )
        )
        blocks.add(Block(trueBlockLabel, mutableListOf()))
        val trueHasReturn = addStatement(statement.trueBranch, function)
        if (!trueHasReturn) blocks.last().statements.add(BranchStatement(endBlockLabel))
        if (statement.falseBranch != null) {
            blocks.add(Block(falseBlockLabel, mutableListOf()))
            val falseHasReturn = addStatement(statement.falseBranch, function)
            if (!falseHasReturn) blocks.last().statements.add(BranchStatement(endBlockLabel))
            if (trueHasReturn && falseHasReturn) return true
        }
        blocks.add(Block(endBlockLabel, mutableListOf()))
        return false
    }

    private fun addLoopStatement(statement: ast.LoopStatement, function: GlobalFunctionBuilder): Boolean {
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        val oldLoopCount = currentLoopCount
        val oldLoopHasStep = currentLoopHasStep
        currentLoopCount = loopCount
        val step = when (statement) {
            is ForExpressionStatement -> statement.step
            is ForDeclarationStatement -> statement.step
            else -> null
        }
        currentLoopHasStep = step != null
        val conditionBlockLabel = "loop_condition_$loopCount"
        val bodyBlockLabel = "loop_body_$loopCount"
        val endBlockLabel = "loop_end_$loopCount"
        val stepBlockLabel = if (currentLoopHasStep) "step_$loopCount" else conditionBlockLabel
        loopCount++
        // Generate the initialization
        when (statement) {
            is ForExpressionStatement -> if (statement.init != null) {
                addExpression(statement.init, function, ExpectedState.VALUE)
            }

            is ForDeclarationStatement -> addStatement(statement.init, function)
            else -> {}
        }
        blocks.last().statements.add(BranchStatement(conditionBlockLabel))

        // Generate the condition
        blocks.add(Block(conditionBlockLabel, mutableListOf()))
        val condition = when (statement) {
            is ForStatement -> when (statement.condition) {
                null -> I1Literal(1)
                else -> addExpression(statement.condition, function, ExpectedState.VALUE).toArgument()
            }
            is WhileStatement -> addExpression(statement.condition, function, ExpectedState.VALUE).toArgument()
            else -> throw IRBuilderException("Unknown loop statement")
        }
        if (condition is IntLiteral) {
            if (condition.value == 1) {
                blocks.last().statements.add(BranchStatement(bodyBlockLabel))
            } else {
                blocks.removeAt(blocks.lastIndex)
                blocks.last().statements.removeAt(blocks.last().statements.lastIndex)
                currentLoopCount = oldLoopCount
                currentLoopHasStep = oldLoopHasStep
                return false
            }
        } else {
            blocks.last().statements.add(BranchStatement(condition, bodyBlockLabel, endBlockLabel))
        }

        // Generate the body
        blocks.add(Block(bodyBlockLabel, mutableListOf()))
        addStatement(statement.body, function)
        // To avoid two branch statements caused by break or continue
        if (blocks.last().statements.lastOrNull() !is BranchStatement) {
            blocks.last().statements.add(BranchStatement(stepBlockLabel))
        }
        // Generate the step
        if (step != null) {
            blocks.add(Block(stepBlockLabel, mutableListOf()))
            addExpression(step, function, ExpectedState.VALUE)
            blocks.last().statements.add(BranchStatement(conditionBlockLabel))
        }
        blocks.add(Block(endBlockLabel, mutableListOf()))
        currentLoopCount = oldLoopCount
        currentLoopHasStep = oldLoopHasStep
        return false
    }

    private fun addContinueStatement(function: GlobalFunctionBuilder): Boolean {
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        val branchLabel = if (currentLoopHasStep) "step_$currentLoopCount" else "loop_condition_$currentLoopCount"
        blocks.last().statements.add(BranchStatement(branchLabel))
        return true
    }

    private fun addBreakStatement(function: GlobalFunctionBuilder): Boolean {
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        blocks.last().statements.add(BranchStatement("loop_end_$currentLoopCount"))
        return true
    }

    private fun addStatement(statement: ast.ReturnStatement, function: GlobalFunctionBuilder): Boolean {
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        if (statement.expression != null) {
            val returnValue = addExpression(statement.expression, function, ExpectedState.VALUE).toArgument()
            blocks.last().statements.add(BranchStatement("return"))
            val returnPhi = function.returnPhi ?: throw IRBuilderException("Non-void function has no return phi")
            returnPhi.incoming.add(Pair(returnValue, blocks.last().label))
        } else {
            blocks.last().statements.add(BranchStatement("return"))
        }
        return true
    }

    private fun addStatement(statement: ast.VariablesDeclaration, function: GlobalFunctionBuilder): Boolean {
        val blocks = function.body ?: throw IRBuilderException("Function has no body")
        val variableList = function.variables ?: throw IRBuilderException("Function has no variable list")
        val type = irType(statement.type)
        for (variable in statement.variables) {
            val binding = variable.binding ?: throw IRBuilderException("Variable has no binding")
            val dest = LocalVariable(binding.irInfo.toString(), ptrType)
            variableList.add(AllocaStatement(dest, type))
            if (variable.body != null) {
                val initializer = addExpression(variable.body, function, ExpectedState.VALUE).toArgument()
                blocks.last().statements.add(StoreStatement(dest, initializer))
            } else if (statement.type is ast.StringType) {
                blocks.last().statements.add(StoreStatement(dest, GlobalVariable("__empty_string", ptrType)))
            }
        }
        return false
    }
}

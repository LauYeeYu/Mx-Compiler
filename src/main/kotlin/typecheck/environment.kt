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

package typecheck

import ast.*
import exceptions.InternalException
import exceptions.SemanticException

class IrInfo(
    private val name: String,
    val count: Int,
    val isLocal: Boolean,
) {
    override fun toString(): String {
        if (count == 0) return name
        return "$name.$count"
    }
}

fun checkAndRecord(ast: AstNode): GlobalEnvironmentRecord {
    if (ast !is TranslateUnit) {
        throw SemanticException("Expected a TranslateUnit, got ${ast::class.simpleName}", ast.ctx)
    }
    val rootEnvironment = GlobalEnvironmentRecord().checkAndRecord(ast)
    ast.environment = rootEnvironment
    return rootEnvironment
}

class Binding(
    val ctx: SourceContext?,
    val name: String,
    val type: MxType,
    val irInfo: IrInfo,
    val fromClass: ast.Class? = null,
)

open class EnvironmentRecord(protected val parent: EnvironmentRecord?) {
    // Check whether there is a variable or class with the given name
    fun findVariableAlike(name: String): Binding? =
        variableAlikeBindings[name] ?: parent?.findVariableAlike(name)

    // Check whether there is a function or class with the given name
    fun findFunctionAlike(name: String): Binding? =
        functionAlikeBindings[name] ?: parent?.findFunctionAlike(name)

    private fun findClass(name: String): Binding? =
        classBindings[name] ?: parent?.findClass(name)

    open fun inClass(): Boolean =
        throw InternalException("cannot call inClass() on a base EnvironmentRecord")

    open fun inLoop(): Boolean =
        throw InternalException("cannot call inLoop() on a base EnvironmentRecord")

    open fun functionReturnType(): MxType? =
        throw InternalException("cannot call FunctionReturnType() on a base EnvironmentRecord")

    open fun thisType(): MxType =
        throw InternalException("cannot call thisType() on a base EnvironmentRecord")

    private fun commitReturn(returnType: MxType) {
        if (hasReturn && referredReturnType != returnType) {
            throw SemanticException("return type mismatch", null)
        }
        referredReturnType = returnType
        hasReturn = true
    }
    
    fun getType(type: Type, ctx: SourceContext?): MxType {
        var returnType = MxType(null)
        when (type) {
            is PrimitiveType -> {
                when (type) {
                    is IntType -> returnType = MxIntType()
                    is BoolType -> returnType = MxBoolType()
                    is StringType -> returnType =
                        findClass("string")?.type ?:
                        throw InternalException("cannot find class string")
                    is VoidType -> returnType = MxVoidType()
                }
            }
            is ArrayType -> {
                val elementType = getType(type.type, ctx)
                returnType = MxArrayType(elementType, type.dimension)
            }
            is ClassType -> {
                val className = type.name
                val classBinding = findClass(className)
                    ?: throw SemanticException("Class $className not found", ctx)
                if (classBinding.type !is MxClassType) {
                    throw SemanticException("Expected a class, got ${classBinding.type}", ctx)
                }
                returnType = classBinding.type
            }
            else -> throw SemanticException("Unknown type ${type::class.simpleName}", ctx)
        }
        return returnType
    }

    protected fun recordFunction(node: ast.Function) {
        val binding = findFunctionAlike(node.name)
            ?: throw InternalException("Function ${node.name} not found")
        val functionEnvironmentRecord = FunctionEnvironmentRecord(
            this,
            node.parameters.map {
                Binding(
                    it.ctx,
                    it.name,
                    getType(it.type, it.ctx),
                    IrInfo(
                        it.name,
                        when (findVariableAlike(it.name)) {
                            null -> 0
                            else -> findVariableAlike(it.name)!!.irInfo.count + 1
                        },
                        true,
                    ),
                )
            },
            getType(node.returnType, node.ctx),
        )
        node.bindings.addAll(functionEnvironmentRecord.parameters)
        for (statement in node.body.statements) {
            functionEnvironmentRecord.checkAndRecord(statement)
        }
        val funReturnType = getType(node.returnType, node.ctx)
        if (node.name == "main") {
            if (node.parameters.isNotEmpty()) {
                throw SemanticException("main function should not have parameters", node.ctx)
            }
            if (funReturnType !is MxIntType) {
                throw SemanticException("main function should return int", node.ctx)
            }
            if (functionEnvironmentRecord.hasReturn &&
                functionEnvironmentRecord.referredReturnType !is MxIntType) {
                throw SemanticException("main function should return int", node.ctx)
            } else {
                hasReturn = true
                referredReturnType = MxIntType()
            }
        } else if (funReturnType is MxVoidType) {
            if (functionEnvironmentRecord.referredReturnType != null &&
                functionEnvironmentRecord.referredReturnType !is MxVoidType) {
                throw SemanticException(
                    "Function ${node.name} should not return a value",
                    node.ctx,
                )
            }
        } else {
            if (functionEnvironmentRecord.referredReturnType != funReturnType) {
                throw SemanticException(
                    "Function ${node.name} should return a value of type $funReturnType",
                    node.ctx,
                )
            }
        }
        binding.type.environment = functionEnvironmentRecord
        node.environment = functionEnvironmentRecord
    }

    protected fun recordClass(node: ast.Class) {
        val binding = classBindings[node.name]
            ?: throw InternalException("Class ${node.name} is not found")
        node.environment = (binding.type.environment as ClassEnvironmentRecord).checkAndRecord(node)
    }

    protected fun recordVariable(node: VariablesDeclaration): List<Binding> {
        val type = getType(node.type, node.ctx)
        if (!isValidVariableType(type)) {
            throw SemanticException("Variable type cannot be $type", node.ctx)
        }
        val variableBindings: MutableList<Binding> = mutableListOf()
        for (variable in node.variables) {
            // the name can conflict with other variables out of this scope
            val binding = variableAlikeBindings[variable.name]
            if (binding != null) {
                throw SemanticException(
                    "Variable ${variable.name} is already defined in ${binding.ctx?.loc}",
                    variable.ctx,
                )
            }
            if (variable.body != null &&
                checkType(variable.body, this, node.ctx).type != type) {
                throw SemanticException(
                    "Expected type $type, got ${checkType(variable.body, this, node.ctx).type}",
                    variable.ctx,
                )
            }
            val variableCount: Int = when (val shallowBinding = findVariableAlike(variable.name)) {
                null -> 0
                else -> shallowBinding.irInfo.count + 1
            }
            val variableBinding = Binding(
                variable.ctx,
                variable.name,
                type,
                IrInfo(variable.name, variableCount, parent != null),
            )
            variableAlikeBindings[variable.name] = variableBinding
            variable.binding = variableBinding
            variableBindings.add(variableBinding)
        }
        return variableBindings
    }

    open fun checkAndRecord(root: Statement): EnvironmentRecord {
        when (root) {
            is BlockStatement -> {
                val newEnvironment = BlockEnvironmentRecord(this)
                for (statement in root.statements) {
                    newEnvironment.checkAndRecord(statement)
                }
                if (newEnvironment.referredReturnType != null) {
                    commitReturn(newEnvironment.referredReturnType!!)
                }
                root.environment = newEnvironment
            }
            is VariablesDeclaration -> recordVariable(root)
            is ExpressionStatement -> checkType(root.expression, this, root.ctx)
            is BranchStatement -> {
                if (checkType(root.condition, this, root.ctx).type !is MxBoolType) {
                    throw SemanticException("Expected a bool type", root.condition.ctx)
                }
                if (root.falseBranch != null) {
                    val trueBranchEnvironment = BlockEnvironmentRecord(this).checkAndRecord(root.trueBranch)
                    val falseBranchEnvironment = BlockEnvironmentRecord(this).checkAndRecord(root.falseBranch)
                    root.trueBranch.environment = trueBranchEnvironment
                    root.falseBranch.environment = falseBranchEnvironment
                    if (trueBranchEnvironment.hasReturn && falseBranchEnvironment.hasReturn) {
                        if (trueBranchEnvironment.referredReturnType != falseBranchEnvironment.referredReturnType) {
                            throw SemanticException(
                                "Expected same return type, got ${trueBranchEnvironment.referredReturnType} and ${falseBranchEnvironment.referredReturnType}",
                                root.ctx,
                            )
                        }
                        commitReturn(trueBranchEnvironment.referredReturnType!!)
                    }
                    subEnvironments.add(trueBranchEnvironment)
                    subEnvironments.add(falseBranchEnvironment)
                } else {
                    val trueBranchEnvironment = BlockEnvironmentRecord(this).checkAndRecord(root.trueBranch)
                    root.trueBranch.environment = trueBranchEnvironment
                    subEnvironments.add(trueBranchEnvironment)
                }
            }

            is WhileStatement -> {
                if (checkType(root.condition, this, root.ctx).type !is MxBoolType) {
                    throw SemanticException("Expected a bool type", root.condition.ctx)
                }
                root.environment = BlockEnvironmentRecord(this, listOf(), true).checkAndRecord(root.body)
            }

            is ForExpressionStatement -> {
                if (root.init != null) {
                    checkType(root.init, this, root.ctx)
                }
                if (root.condition != null &&
                    (checkType(root.condition, this, root.ctx).type !is MxBoolType)) {
                    throw SemanticException("Expected a bool type", root.condition.ctx)
                }
                if (root.step != null) {
                    checkType(root.step, this, root.ctx)
                }
                val forEnvironment = BlockEnvironmentRecord(this, listOf(), true).checkAndRecord(root.body)
                root.environment = forEnvironment
                subEnvironments.add(forEnvironment)
            }
            is ForDeclarationStatement -> {
                recordVariable(root.init)
                if (root.condition != null &&
                    (checkType(root.condition, this, root.ctx).type !is MxBoolType)) {
                    throw SemanticException("Expected a bool type", root.condition.ctx)
                }
                if (root.step != null) {
                    checkType(root.step, this, root.ctx)
                }
                val forEnvironment = BlockEnvironmentRecord(this, listOf(), true).checkAndRecord(root.body)
                root.environment = forEnvironment
                subEnvironments.add(forEnvironment)
            }
            is ContinueStatement -> {
                if (!inLoop()) {
                    throw SemanticException("Continue statement not in a loop", root.ctx)
                }
            }
            is BreakStatement -> {
                if (!inLoop()) {
                    throw SemanticException("Break statement not in a loop", root.ctx)
                }
            }
            is ReturnStatement -> {
                if (root.expression != null) {
                    commitReturn(checkType(root.expression, this, root.ctx).type)
                } else {
                    commitReturn(MxVoidType())
                }
            }
        }
        return this
    }

    val variableAlikeBindings: HashMap<String, Binding> = HashMap()
    val functionAlikeBindings: HashMap<String, Binding> = HashMap()
    val classBindings: HashMap<String, Binding> = HashMap()
    var hasReturn = false
    var referredReturnType: MxType? = null
    private val subEnvironments: MutableList<EnvironmentRecord> = mutableListOf()
}

class ClassEnvironmentRecord(
    parent: EnvironmentRecord?,
    private val className: String
) : EnvironmentRecord(parent) {
    override fun inClass() = true
    override fun inLoop() = false
    override fun functionReturnType() =
        throw InternalException("Class does not have a return type")

    override fun thisType() = MxClassType(className, this)

    fun registerClassElement(root: ast.Class): ClassEnvironmentRecord {
        for (classElement in root.body) {
            when (classElement) {
                is ast.VariablesDeclaration -> {
                    for (variable in classElement.variables) {
                        val binding = variableAlikeBindings[variable.name]
                        if (binding != null) {
                            throw SemanticException(
                                "Variable ${variable.name} is already defined in ${binding.ctx?.loc}",
                                variable.ctx,
                            )
                        }
                        val count = when (val shallowBinding = findVariableAlike(variable.name)) {
                            null -> 0
                            else -> shallowBinding.irInfo.count + 1
                        }
                        val newBinding = Binding(
                            variable.ctx,
                            variable.name,
                            getType(classElement.type, classElement.ctx),
                            IrInfo(variable.name, count, true),
                            root,
                        )
                        variableAlikeBindings[variable.name] = newBinding
                        variable.binding = newBinding
                    }
                }

                is ast.Function -> {
                    if (classElement.name == className) {
                        throw SemanticException(
                            "A member function cannot be the same name of class constructor",
                            classElement.ctx,
                        )
                    }
                    val binding = functionAlikeBindings[classElement.name]
                    if (binding != null) {
                        throw SemanticException(
                            "Function ${classElement.name} is already defined in ${binding.ctx?.loc}",
                            classElement.ctx,
                        )
                    }
                    functionAlikeBindings[classElement.name] = Binding(
                        classElement.ctx,
                        classElement.name,
                        MxFunctionType(
                            getType(classElement.returnType, classElement.ctx),
                            classElement.parameters.map { getType(it.type, it.ctx) },
                            null,
                        ),
                        IrInfo("$className.${classElement.name}", 0, false),
                    )
                }

                else -> {}
            }
        }
        return this
    }

    fun checkAndRecord(root: ast.Class): ClassEnvironmentRecord {
        for (classElement in root.body) {
            when (classElement) {
                is ast.VariablesDeclaration -> {
                    val type = getType(classElement.type, classElement.ctx)
                    for (variable in classElement.variables) {
                        if (variable.body != null &&
                            checkType(variable.body, this, classElement.ctx).type != type) {
                            throw SemanticException(
                                "Expected type $type, got ${checkType(variable.body, this, classElement.ctx)}",
                                variable.ctx,
                            )
                        }
                    }
                }
                is ast.Function -> recordFunction(classElement)
                is ast.Constructor -> recordConstructor(classElement)
            }
        }
        return this
    }

    // An ugly implementation of the built-in method for String
    fun loadStringBuiltin(stringType: MxStringType): ClassEnvironmentRecord {
        functionAlikeBindings["length"] = Binding(
            null,
            "length",
            MxFunctionType(MxIntType(), listOf(), null),
            IrInfo("String.length", 0, false),
        )
        functionAlikeBindings["substring"] = Binding(
            null,
            "substring",
            MxFunctionType(stringType, listOf(MxIntType(), MxIntType()), null),
            IrInfo("String.substring", 0, false),
        )
        functionAlikeBindings["parseInt"] = Binding(
            null,
            "parseInt",
            MxFunctionType(MxIntType(), listOf(), null),
            IrInfo("String.parseInt", 0, false),
        )
        functionAlikeBindings["ord"] = Binding(
            null,
            "ord",
            MxFunctionType(MxIntType(), listOf(MxIntType()), null),
            IrInfo("String.ord", 0, false),
        )
        return this
    }

    // An ugly implementation of the built-in method for Array
    fun loadArrayBuiltin(): ClassEnvironmentRecord {
        functionAlikeBindings["size"] = Binding(
            null,
            "size",
            MxFunctionType(MxIntType(), listOf(), null),
            IrInfo("Array.size", 0, false),
        )
        return this
    }

    private fun recordConstructor(node: ast.Constructor) {
        if (node.name != className) {
            throw SemanticException("Constructor name must be the same as class name", node.ctx)
        }
        val environmentRecord = FunctionEnvironmentRecord(
                this,
                listOf(),
                MxVoidType(),
            ).checkAndRecord(node.body) as FunctionEnvironmentRecord
        if (environmentRecord.hasReturn && environmentRecord.referredReturnType !is MxVoidType) {
            throw SemanticException("Constructor do not a return statement", node.ctx)
        }
        functionAlikeBindings[node.name] = Binding(
            node.ctx,
            node.name,
            MxFunctionType(MxVoidType(), listOf(), environmentRecord),
            IrInfo(className + node.name, 0, false),
        )
    }

    private val thisType = MxClassType(className, null)
}

class FunctionEnvironmentRecord(
    parent: EnvironmentRecord?,
    val parameters: List<Binding>,
    var returnType: MxType
) : EnvironmentRecord(parent) {
    override fun inClass(): Boolean = when (parent) {
        null -> false
        else -> parent.inClass()
    }

    override fun inLoop(): Boolean = false

    override fun functionReturnType(): MxType = returnType

    override fun thisType(): MxType = when (parent) {
        null -> throw InternalException("Function does not have a this type")
        else -> parent.thisType()
    }

    init {
        for (variableBinding in parameters) {
            variableAlikeBindings[variableBinding.name] = variableBinding
        }
    }
}

class GlobalEnvironmentRecord : EnvironmentRecord(null) {
    override fun inClass() = false
    override fun inLoop() = false

    override fun thisType(): MxType =
        throw InternalException("Global environment does not have a this type")

    init {
        val stringType = MxStringType()
        val stringBinding = Binding(
            null,
            "string",
            stringType,
            IrInfo("string", 0, false),
        )
        functionAlikeBindings["string"] = stringBinding
        variableAlikeBindings["string"] = stringBinding
        classBindings["string"] = stringBinding
        loadBuiltinFunctions(stringType)
    }

    private fun loadBuiltinFunctions(stringType: MxStringType) {
        functionAlikeBindings["print"] = Binding(
            null,
            "print",
            MxFunctionType(MxVoidType(), listOf(stringType), null),
            IrInfo("print", 0, false),
        )
        functionAlikeBindings["println"] = Binding(
            null,
            "println",
            MxFunctionType(MxVoidType(), listOf(stringType), null),
            IrInfo("println", 0, false),
        )
        functionAlikeBindings["printInt"] = Binding(
            null,
            "printInt",
            MxFunctionType(MxVoidType(), listOf(MxIntType()), null),
            IrInfo("printInt", 0, false),
        )
        functionAlikeBindings["printlnInt"] = Binding(
            null,
            "printlnInt",
            MxFunctionType(MxVoidType(), listOf(MxIntType()), null),
            IrInfo("printlnInt", 0, false),
        )
        functionAlikeBindings["getString"] = Binding(
            null,
            "getString",
            MxFunctionType(stringType, listOf(), null),
            IrInfo("getString", 0, false),
        )
        functionAlikeBindings["getInt"] = Binding(
            null,
            "getInt",
            MxFunctionType(MxIntType(), listOf(), null),
            IrInfo("getInt", 0, false),
        )
        functionAlikeBindings["toString"] = Binding(
            null,
            "toString",
            MxFunctionType(stringType, listOf(MxIntType()), null),
            IrInfo("toString", 0, false),
        )
    }

    private fun registerSymbols(root: TranslateUnit) {
        // register all classes
        for (element in root.content) {
            if (element is ast.Class) {
                val classBinding = Binding(
                    element.ctx,
                    element.name,
                    MxClassType(
                        element.name,
                        null,
                    ),
                    IrInfo(element.name, 0, false),
                )
                if (variableAlikeBindings.containsKey(element.name)) {
                    throw SemanticException("Duplicate class name", element.ctx)
                }
                classBindings[element.name] = classBinding
                variableAlikeBindings[element.name] = classBinding
            }
        }

        // register all functions
        for (element in root.content) {
            if (element is ast.Function) {
                if (findFunctionAlike(element.name) != null) {
                    throw SemanticException(
                        "Function ${element.name} is already defined",
                        element.ctx,
                    )
                }
                functionAlikeBindings[element.name] = Binding(
                    element.ctx,
                    element.name,
                    MxFunctionType(
                        getType(element.returnType, element.ctx),
                        element.parameters.map { getType(it.type, it.ctx) },
                        null,
                    ),
                    IrInfo(element.name, 0, false),
                )
            } else if (element is ast.Class) {
                val classBinding = classBindings[element.name]
                if (classBinding == null) {
                    throw InternalException("Class ${element.name} is not registered")
                } else {
                    classBinding.type.environment =
                        ClassEnvironmentRecord(this, element.name).registerClassElement(element)
                }
            }
        }
    }

    fun checkAndRecord(root: TranslateUnit): GlobalEnvironmentRecord {
        registerSymbols(root)
        for (node in root.content) {
            when (node) {
                is ast.Function -> recordFunction(node)
                is ast.Class -> recordClass(node)
                is ast.VariablesDeclaration -> recordVariable(node)
            }
        }
        return this
    }
}

class BlockEnvironmentRecord(parent: EnvironmentRecord?) : EnvironmentRecord(parent) {
    override fun inClass(): Boolean = when (parent) {
        null -> false
        else -> parent.inClass()
    }

    override fun inLoop(): Boolean = when (inLoop) {
        true -> true
        else -> when (parent) {
            null -> false
            else -> parent.inLoop()
        }
    }
    override fun functionReturnType(): MxType? = when (parent) {
        null -> throw InternalException("Block does not have a return type")
        else -> parent.functionReturnType()
    }

    override fun thisType(): MxType = when (parent) {
        null -> throw InternalException("Block does not have a this type")
        else -> parent.thisType()
    }

    private var inLoop = false

    constructor(parent: EnvironmentRecord?,
                variableBindings: List<Binding>,
                inLoop: Boolean) : this(parent) {
        for (variableBinding in variableBindings) {
            variableAlikeBindings[variableBinding.name] = variableBinding
        }
        this.inLoop = inLoop
    }
}

var lambdaCount = 0

fun buildLambdaBinding(parent: EnvironmentRecord, expression: LambdaExpression): Binding {
    val lambdaParent = when (expression.captureReference) {
        true -> parent
        false -> null
    }
    val functionEnvironmentRecord = FunctionEnvironmentRecord(
        lambdaParent,
        expression.parameters.map {
            Binding(
                it.ctx,
                it.name,
                parent.getType(it.type, it.ctx),
                IrInfo(it.name,
                    when (parent.findVariableAlike(it.name)) {
                        null -> 0
                        else -> parent.findVariableAlike(it.name)!!.irInfo.count + 1
                    },
                    true),
            )
        },
        MxType(null), // need to replace later
    )

    for (statement in expression.body.statements) {
        functionEnvironmentRecord.checkAndRecord(statement)
    }
    if (functionEnvironmentRecord.referredReturnType == null ||
        functionEnvironmentRecord.referredReturnType is MxVoidType) {
        functionEnvironmentRecord.returnType = MxVoidType()
    } else {
        functionEnvironmentRecord.returnType = functionEnvironmentRecord.referredReturnType!!
    }
    ++lambdaCount
    return Binding(
        expression.ctx,
        "lambda$lambdaCount",
        MxFunctionType(
            functionEnvironmentRecord.returnType,
            functionEnvironmentRecord.parameters.map { it.type },
            functionEnvironmentRecord,
        ),
        IrInfo("lambda$lambdaCount", 0, true),
    )
}

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

fun checkAndRecord(ast: AstNode): GlobalEnvironmentRecord {
    if (ast !is TranslateUnit) {
        throw SemanticException("Expected a TranslateUnit, got ${ast::class.simpleName}", ast.ctx)
    }
    return GlobalEnvironmentRecord().checkAndRecord(ast)
}

class Binding(
    val ctx: SourceContext?,
    val name: String,
    val type: MxType,
)

open class EnvironmentRecord(protected val parent: EnvironmentRecord?) {
    // Check whether there is a variable or class with the given name
    fun findVariableAlike(name: String): Binding? =
        variableAlikeBindings[name] ?: parent?.findVariableAlike(name)

    // Check whether there is a function or class with the given name
    fun findFunctionAlike(name: String): Binding? =
        functionAlikeBindings[name] ?: parent?.findFunctionAlike(name)

    fun findClass(name: String): Binding? =
        classBindings[name] ?: parent?.findClass(name)

    open fun inClass(): Boolean =
        throw InternalException("cannot call inClass() on a base EnvironmentRecord")

    open fun inLoop(): Boolean =
        throw InternalException("cannot call inLoop() on a base EnvironmentRecord")

    open fun functionReturnType(): MxType? =
        throw InternalException("cannot call FunctionReturnType() on a base EnvironmentRecord")

    open fun thisType(): MxType =
        throw InternalException("cannot call thisType() on a base EnvironmentRecord")
    
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
                val classBinding = findVariableAlike(className)
                if (classBinding == null) {
                    throw SemanticException("Class $className not found", ctx)
                }
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
        if (binding != null) {
            throw SemanticException(
                "Function ${node.name} is already defined in ${binding.ctx?.loc}",
                node.ctx,
            )
        }
        val functionEnvironmentRecord = FunctionEnvironmentRecord(
            this,
            node.parameters.map {
                Binding(it.ctx, it.name, getType(it.type, it.ctx))
            },
            getType(node.returnType, node.ctx),
        )
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
            if (hasReturn && functionEnvironmentRecord.referredReturnType !is MxIntType) {
                throw SemanticException("main function should return int", node.ctx)
            } else {
                hasReturn = true
                referredReturnType = MxIntType()
            }
        } else if (funReturnType is MxVoidType) {
            if (functionEnvironmentRecord.referredReturnType !is MxVoidType) {
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
        functionAlikeBindings[node.name] = Binding(
            node.ctx,
            node.name,
            MxFunctionType(
                funReturnType,
                node.parameters.map { getType(it.type, it.ctx) },
                functionEnvironmentRecord,
            ),
        )
    }

    protected fun recordClass(node: ast.Class) {
        val binding = findVariableAlike(node.name)
        if (binding != null) {
            throw SemanticException(
                "Class ${node.name} is already defined in ${binding.ctx?.loc}",
                node.ctx,
            )
        }
        val newBinding = Binding(
            node.ctx,
            node.name,
            MxClassType(
                node.name,
                ClassEnvironmentRecord(this, node.name).checkAndRecord(node),
            ),
        )

        variableAlikeBindings[node.name] = newBinding
        functionAlikeBindings[node.name] = newBinding
        classBindings[node.name] = newBinding
    }

    protected open fun recordVariable(node: VariablesDeclaration): List<Binding> {
        val type = getType(node.type, node.ctx)
        val variableBindings: MutableList<Binding> = mutableListOf()
        for (variable in node.variables) {
            // the name can conflict with other variables out of this scope
            val binding = findVariableAlike(variable.name)
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
             val variableBinding = Binding(
                variable.ctx,
                variable.name,
                type,
            )
            variableAlikeBindings[variable.name] = variableBinding
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
                    if (statement is ReturnStatement) {
                        hasReturn = true
                        referredReturnType = newEnvironment.referredReturnType
                    }
                }
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
                    if (trueBranchEnvironment.hasReturn && falseBranchEnvironment.hasReturn &&
                        trueBranchEnvironment.referredReturnType != falseBranchEnvironment.referredReturnType) {
                        hasReturn = true
                        referredReturnType = trueBranchEnvironment.referredReturnType
                    }
                    subEnvironments.add(trueBranchEnvironment)
                    subEnvironments.add(falseBranchEnvironment)
                } else {
                    subEnvironments.add(BlockEnvironmentRecord(this).checkAndRecord(root.trueBranch))
                }
            }

            is WhileStatement -> {
                if (checkType(root.condition, this, root.ctx).type !is MxBoolType) {
                    throw SemanticException("Expected a bool type", root.condition.ctx)
                }
                BlockEnvironmentRecord(this, listOf(), true).checkAndRecord(root.body)
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
                subEnvironments.add(
                    BlockEnvironmentRecord(this, listOf(), true).checkAndRecord(root.body)
                )
            }
            is ForDeclarationStatement -> {
                val variableList = recordVariable(root.init)
                if (root.condition != null &&
                    (checkType(root.condition, this, root.ctx).type !is MxBoolType)) {
                    throw SemanticException("Expected a bool type", root.condition.ctx)
                }
                if (root.step != null) {
                    checkType(root.step, this, root.ctx)
                }
                subEnvironments.add(
                    BlockEnvironmentRecord(this, variableList, true).checkAndRecord(root.body)
                )
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
                    referredReturnType = checkType(root.expression, this, root.ctx).type
                    hasReturn = true
                } else {
                    referredReturnType = MxVoidType()
                }
            }
        }
        return this
    }

    var variableAlikeBindings: HashMap<String, Binding> = HashMap()
    var functionAlikeBindings: HashMap<String, Binding> = HashMap()
    var classBindings: HashMap<String, Binding> = HashMap()
    protected var hasReturn = false
    var referredReturnType: MxType? = null
    protected val subEnvironments: MutableList<EnvironmentRecord> = mutableListOf()
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

    fun checkAndRecord(root: ast.Class): ClassEnvironmentRecord {
        for (classElement in root.body) {
            when (classElement) {
                is ast.VariablesDeclaration -> {
                    for (variable in classElement.variables) {
                        val binding = findVariableAlike(variable.name)
                        if (binding != null) {
                            throw SemanticException(
                                "Variable ${variable.name} is already defined in ${binding.ctx?.loc}",
                                variable.ctx,
                            )
                        }
                        variableAlikeBindings[variable.name] = Binding(
                            variable.ctx,
                            variable.name,
                            getType(classElement.type, classElement.ctx),
                        )
                    }
                }
                is ast.Function -> {
                    val binding = findFunctionAlike(classElement.name)
                    if (binding != null) {
                        throw SemanticException(
                            "Function ${classElement.name} is already defined in ${binding.ctx?.loc}",
                            classElement.ctx,
                        )
                    }
                    functionAlikeBindings[classElement.name] = Binding(
                        classElement.ctx,
                        classElement.name,
                        getType(classElement.returnType, classElement.ctx),
                    )
                }
                else -> {}
            }
        }
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
                is ast.Function -> {
                    functionAlikeBindings[classElement.name]?.type?.environment =
                    FunctionEnvironmentRecord(this,
                        classElement.parameters.map {
                            Binding(it.ctx, it.name, getType(it.type, it.ctx))
                        },
                        getType(classElement.returnType, classElement.ctx),
                    ).checkAndRecord(classElement.body) as FunctionEnvironmentRecord
                }
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
        )
        functionAlikeBindings["substring"] = Binding(
            null,
            "substring",
            MxFunctionType(stringType, listOf(MxIntType(), MxIntType()), null),
        )
        functionAlikeBindings["parseInt"] = Binding(
            null,
            "parseInt",
            MxFunctionType(MxIntType(), listOf(), null),
        )
        functionAlikeBindings["ord"] = Binding(
            null,
            "ord",
            MxFunctionType(MxIntType(), listOf(MxIntType()), null),
        )
        return this
    }

    // An ugly implementation of the built-in method for Array
    fun loadArrayBuiltin(): ClassEnvironmentRecord {
        functionAlikeBindings["size"] = Binding(
            null,
            "size",
            MxFunctionType(MxIntType(), listOf(), null),
        )
        return this
    }

    private fun recordConstructor(node: ast.Constructor) {
        if (node.name != className) {
            throw SemanticException("Constructor name must be the same as class name", node.ctx)
        }
        functionAlikeBindings[node.name] = Binding(
            node.ctx,
            node.name,
            MxFunctionType(
                thisType,
                listOf(),
                FunctionEnvironmentRecord(this, listOf(), thisType)
                    .checkAndRecord(node.body) as FunctionEnvironmentRecord
            ),
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
        )
        functionAlikeBindings["println"] = Binding(
            null,
            "println",
            MxFunctionType(MxVoidType(), listOf(stringType), null),
        )
        functionAlikeBindings["printInt"] = Binding(
            null,
            "printInt",
            MxFunctionType(MxVoidType(), listOf(MxIntType()), null),
        )
        functionAlikeBindings["printlnInt"] = Binding(
            null,
            "printlnInt",
            MxFunctionType(MxVoidType(), listOf(MxIntType()), null),
        )
        functionAlikeBindings["getString"] = Binding(
            null,
            "getString",
            MxFunctionType(stringType, listOf(), null),
        )
        functionAlikeBindings["getInt"] = Binding(
            null,
            "getInt",
            MxFunctionType(MxIntType(), listOf(), null),
        )
        functionAlikeBindings["toString"] = Binding(
            null,
            "toString",
            MxFunctionType(stringType, listOf(MxIntType()), null),
        )
    }

    fun checkAndRecord(root: TranslateUnit): GlobalEnvironmentRecord {
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

    override fun inLoop(): Boolean = inLoop
    override fun functionReturnType(): MxType? = when (parent) {
        null -> throw InternalException("Block does not have a return type")
        else -> parent.functionReturnType()
    }

    override fun thisType(): MxType = when (parent) {
        null -> throw InternalException("Block does not have a this type")
        else -> parent.thisType()
    }

    constructor(parent: EnvironmentRecord?,
                variableBindings: List<Binding>,
                inLoop: Boolean) : this(parent) {
        for (variableBinding in variableBindings) {
            variableAlikeBindings[variableBinding.name] = variableBinding
        }
        this.inLoop = inLoop
    }

    private var inLoop = false
}

fun buildLambdaBinding(parent: EnvironmentRecord, expression: LambdaExpression): Binding {
    val lambdaParent = when (expression.captureReference) {
        true -> parent
        false -> null
    }
    val functionEnvironmentRecord = FunctionEnvironmentRecord(
        lambdaParent,
        expression.parameters.map {
            Binding(it.ctx, it.name, parent.getType(it.type, it.ctx))
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
    return Binding(
        expression.ctx,
        "lambda",
        MxFunctionType(
            functionEnvironmentRecord.returnType,
            functionEnvironmentRecord.parameters.map { it.type},
            functionEnvironmentRecord,
        ),
    )
}
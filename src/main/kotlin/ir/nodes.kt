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

import exceptions.IRBuilderException
import exceptions.InternalException

class Root(
    val classes: List<GlobalClass>,
    val variables: List<GlobalDecl>,
    val globalFunctions: List<GlobalFunction>,
) {
    override fun toString(): String {
        return classes.joinToString("\n") + "\n\n" +
                variables.joinToString("\n") + "\n\n" +
                globalFunctions.joinToString("\n")
    }

    fun transform(transformer: Transformer) = transformer.transform(this)
}

class GlobalClass(
    val classType: ClassType,
    val nameMap: Map<String, Int>,
) {
    override fun toString() =
        "%class.${classType.name} = type { ${classType.memberList.joinToString(", ")} }"
}

sealed interface GlobalDecl
abstract class Argument (
    val name: String,
    val type: PrimitiveType,
) {
    fun replace(replaceMap: MutableMap<Variable, Argument>): Argument = if (this is Variable) {
        replaceMap[this] ?: this
    } else {
        this
    }
}

abstract class Variable(name: String, type: PrimitiveType) : Argument(name, type) {
    override fun equals(other: Any?): Boolean {
        if (other is Variable) return name == other.name
        return false
    }

    override fun hashCode(): Int {
        return name.hashCode()
    }

    fun variableName(): String {
        return name.substring(1)
    }
}

class GlobalVariable(
    variableName: String,
    type: PrimitiveType,
) : Variable("@$variableName", type) {
    override fun toString() = when (type.type) {
        TypeProperty.VOID -> "void"
        else -> "$type $name"
    }
}

class LocalVariable(
    variableName: String,
    type: PrimitiveType,
) : Variable("%$variableName", type) {
    override fun toString() = when (type.type) {
        TypeProperty.VOID -> "void"
        else -> "$type $name"
    }
}

abstract class IntLiteral(
    val value: Int,
    type: PrimitiveType
) : Argument(if (type.type == TypeProperty.PTR) "null" else value.toString(), type)

fun getLiteralNode(value: Int, type: Type): IntLiteral {
    return when (type) {
        is PrimitiveType -> when (type.type) {
            TypeProperty.I1 -> I1Literal(value)
            TypeProperty.I8 -> I8Literal(value)
            TypeProperty.I32 -> I32Literal(value)
            TypeProperty.PTR -> NullLiteral()
            else -> throw InternalException("Unsupported type for literal: $type")
        }

        else -> throw InternalException("Unsupported type for literal: $type")
    }
}

class I1Literal(
    value: Int
) : IntLiteral(value, PrimitiveType(TypeProperty.I1)) {
    override fun toString() = if (value == 0) "i1 0" else "i1 1"
}

class I8Literal(
    value: Int
) : IntLiteral(value, PrimitiveType(TypeProperty.I8)) {
    override fun toString() = "i8 $value"
}

class I32Literal(
    value: Int
) : IntLiteral(value, PrimitiveType(TypeProperty.I32)) {
    override fun toString() = "i32 $value"
}

// Actually used only for null
class NullLiteral : IntLiteral(0, PrimitiveType(TypeProperty.PTR)) {
    override fun toString() = "ptr null"
}

class GlobalVariableDecl(
    val property: GlobalVariable,
    val type: PrimitiveType,
    val initValue: Argument, // Every global variable must have an initial value
) : GlobalDecl {
    override fun toString() = when (type.type) {
        TypeProperty.PTR -> "${property.name} = global $type ${initValue.name}"
        else -> "${property.name} = global $initValue"
    }
}

class AllocaStatement(
    val property: LocalVariable,
    val type: PrimitiveType,
) : Statement(
    newVariableCount = 2,
    def = setOf(property),
    use = setOf(),
) {
    override fun toString() =
        "${property.name} = alloca $type"

    override fun replace(map: MutableMap<Variable, Argument>): Statement = this
}

class GlobalFunction(
    val name: String, // without @
    val returnType: PrimitiveType,
    val parameters: List<FunctionParameter>,
    val body: List<Block>? = null,
    val moveSafe: Boolean = false, // This indicates that this function doesn't have a critical edge
    val const: Boolean = false,
) {
    val blockMap: LinkedHashMap<String, Block>
        get() {
            val body = body ?: return linkedMapOf()
            val blockMap = LinkedHashMap<String, Block>()
            for (block in body) {
                blockMap[block.label] = block
            }
            return blockMap
        }

    override fun toString(): String= when (body) {
        null -> "declare $returnType @$name(${parameters.joinToString(", ")})\n"
        else -> "define $returnType @$name(${parameters.joinToString(", ")}) {\n" +
                body.joinToString("\n") + "\n}\n"
    }
}

class GlobalFunctionBuilder(
    val name: String, // without @
    val returnType: PrimitiveType,
    val parameters: List<FunctionParameter>,
    val variables: MutableList<AllocaStatement>? = null,
    val body: MutableList<Block>? = null,
    val returnPhi: PhiStatement? = null,
    val const: Boolean = false,
    // `const` indicates that this function won't change any variable,
    // and always return the same value when given the same arguments.
) {
    fun toGlobalFunction() = GlobalFunction(name, returnType, parameters, mergedBody, const)
    fun toGlobalFunctionDecl() = GlobalFunction(name, returnType, parameters, null, const)

    private val mergedBody: List<Block>?
        get() {
            val body = body ?: return null
            val newBody = mutableListOf<Block>()
            val variables = variables
                ?: throw InternalException("A function definition has no variable list")
            val returnBlock = returnBlock
                ?: throw InternalException("A function definition has no return block")
            for ((index, block) in body.withIndex()) {
                if (index == 0) {
                    newBody.add(Block("entry", mutableListOf()))
                    newBody[0].statements.addAll(variables)
                    newBody[0].statements.addAll(block.statements)
                } else {
                    newBody.add(block)
                }
            }
            newBody.add(returnBlock)
            return newBody
        }
    private val returnVariable: LocalVariable
        get() = LocalVariable("__return", returnType)
    private val returnBlock: Block?
        get() = if (body == null) null
        else if (returnType.type == TypeProperty.VOID) {
                Block("return", mutableListOf(ReturnStatement()))
        } else {
                val returnPhiStatement = returnPhi ?: throw InternalException("Return phi statement not found")
                Block("return", mutableListOf(returnPhiStatement, ReturnStatement(returnVariable)))
        }
}

class FunctionParameter(
    val type: PrimitiveType,
    val name: String,
) {
    override fun toString() = "$type %$name"
}

class Block(
    val label: String,
    val statements: MutableList<Statement>,
) {
    private val statementsString get() =
        statements.joinToString("\n  ", "  ")
    override fun toString() = "$label:\n$statementsString"

    val successors: Set<String>
        get() {
            val last = statements.lastOrNull()
            return if (last is BranchStatement) {
                if (last.falseBlockLabel != null) {
                    setOf(last.trueBlockLabel, last.falseBlockLabel)
                } else {
                    setOf(last.trueBlockLabel)
                }
            } else {
                setOf()
            }
        }

    fun setSuccessor(blockMap: Map<String, Block>) {
        for ((current, next) in statements.zipWithNext()) {
            current.successor.add(next)
        }
        val last = statements.lastOrNull() ?: return
        if (last is BranchStatement) {
            val trueStatement = blockMap[last.trueBlockLabel]?.statements?.first()
                ?: throw InternalException("Block not found: ${last.trueBlockLabel}")
            last.successor.add(trueStatement)
            if (last.falseBlockLabel != null) {
                val falseStatement = blockMap[last.falseBlockLabel]?.statements?.first()
                    ?: throw InternalException("Block not found: ${last.falseBlockLabel}")
                last.successor.add(falseStatement)
            }
        }
    }
}

abstract class Statement(
    val newVariableCount: Int,
    val def: Set<Variable>,
    val use: Set<Variable>,
) {
    val successor: MutableSet<Statement> = mutableSetOf()
    val liveIn: MutableSet<Variable> = mutableSetOf()
    val liveOut: MutableSet<Variable> = mutableSetOf()

    abstract fun replace(map: MutableMap<Variable, Argument>): Statement
}

class CallStatement(
    val dest: Variable?,
    val returnType: PrimitiveType,
    val function: GlobalFunction,
    val arguments: List<Argument>,
) : Statement(
    newVariableCount = if (returnType.type != TypeProperty.VOID) 1 else 0,
    def = if (returnType.type != TypeProperty.VOID) setOf(dest!!) else setOf(),
    use = arguments.filterIsInstance<Variable>().toSet(),
) {
    override fun toString() = when (returnType.type) {
        TypeProperty.VOID -> "call void @${function.name}(${arguments.joinToString(", ")})"
        else -> "${dest?.name} = call ${returnType.type} @${function.name}(${arguments.joinToString(", ")})"
    }

    override fun replace(map: MutableMap<Variable, Argument>): Statement {
        val newArguments = arguments.map { argument -> argument.replace(map) }
        return CallStatement(dest, returnType, function, newArguments)
    }
}

class ReturnStatement(
    val value: Argument? = null,
) : Statement(
    newVariableCount = 0,
    def = setOf(),
    use = if (value == null || value !is Variable) setOf() else setOf(value),
) {
    override fun toString() = when (value) {
        null -> "ret void"
        else -> "ret $value"
    }

    override fun replace(map: MutableMap<Variable, Argument>) =
        ReturnStatement(value?.replace(map))
}

class BranchStatement(
    val condition: Argument?, // null if unconditional
    val trueBlockLabel: String,
    val falseBlockLabel: String?, // null if unconditional
) : Statement(
    newVariableCount = 0,
    def = setOf(),
    use = if (condition == null || condition !is Variable) setOf() else setOf(condition),
) {
    override fun toString() = when (falseBlockLabel) {
        null -> "br label %$trueBlockLabel"
        else -> "br $condition, label %$trueBlockLabel, label %$falseBlockLabel"
    }

    // Unconditional branch
    constructor(branchLabel: String) : this(null, branchLabel, null)

    override fun replace(map: MutableMap<Variable, Argument>) =
        BranchStatement(condition?.replace(map), trueBlockLabel, falseBlockLabel)

    fun replaceLabel(map: MutableMap<String, String>) = BranchStatement(
        condition,
        if (trueBlockLabel in map) map[trueBlockLabel]!! else trueBlockLabel,
        if (falseBlockLabel != null && falseBlockLabel in map) map[falseBlockLabel]!! else falseBlockLabel,
    )
}

class LoadStatement(
    val dest: LocalVariable,
    val src: Variable,
) : Statement(
    newVariableCount = 1,
    def = setOf(dest),
    use = setOf(src),
) {
    override fun toString() =
        "${dest.name} = load ${dest.type}, $src"

    override fun replace(map: MutableMap<Variable, Argument>): Statement =
        LoadStatement(
            dest,
            src.replace(map) as? Variable
                ?: throw InternalException("Load statement source is not a variable")
        )
}

class StoreStatement(
    val dest: Variable,
    val src : Argument,
) : Statement(
    newVariableCount = 0,
    def = setOf(),
    use = if (src is Variable) setOf(dest, src) else setOf(dest),
) {
    override fun toString() = when (src) {
        is Variable -> "store $src, $dest"
        else -> "store $src, $dest"
    }

    override fun replace(map: MutableMap<Variable, Argument>): Statement =
        StoreStatement(dest, src.replace(map))
}

class BinaryOperationStatement(
    val dest: LocalVariable,
    val op  : BinaryOperator,
    val lhs : Argument,
    val rhs : Argument,
) : Statement(
    newVariableCount = 1,
    def = setOf(dest),
    use = listOf(lhs, rhs).filterIsInstance<Variable>().toSet(),
) {
    override fun toString() =
        "${dest.name} = $op ${lhs.type} ${lhs.name}, ${rhs.name}"

    override fun replace(map: MutableMap<Variable, Argument>): Statement =
        BinaryOperationStatement(dest, op, lhs.replace(map), rhs.replace(map))
}

class IntCmpStatement(
    val dest: LocalVariable,
    val op  : IntCmpOperator,
    val lhs : Argument,
    val rhs : Argument,
) : Statement(
    newVariableCount = 1,
    def = setOf(dest),
    use = listOf(lhs, rhs).filterIsInstance<Variable>().toSet(),
) {
    override fun toString() =
        "${dest.name} = icmp $op ${lhs.type} ${lhs.name}, ${rhs.name}"

    override fun replace(map: MutableMap<Variable, Argument>): Statement =
        IntCmpStatement(dest, op, lhs.replace(map), rhs.replace(map))
}

class GetElementPtrStatement(
    val dest   : LocalVariable,
    val src    : Variable,
    val srcType: Type,
    val indices: List<Argument>,
) : Statement(
    newVariableCount = 1,
    def = setOf(dest),
    use = listOf(src, *indices.toTypedArray()).filterIsInstance<Variable>().toSet(),
) {
    override fun toString(): String {
        val stringBuilder = StringBuilder("${dest.name} = getelementptr inbounds ${srcType}, ptr ${src.name}")
        for (index in indices) {
            stringBuilder.append(", $index")
        }
        return stringBuilder.toString()
    }

    override fun replace(map: MutableMap<Variable, Argument>): Statement {
        val newSrc = src.replace(map) as? Variable
            ?: throw IRBuilderException("Src of GEP cannot be replaced with non-variable")
        val newIndices = indices.map { index -> index.replace(map) }
        return GetElementPtrStatement(dest, newSrc, srcType, newIndices)
    }
}

class PhiStatement(
    val dest: LocalVariable,
    val incoming: MutableList<Pair<Argument, String>>,
) : Statement(
    newVariableCount = 1,
    def = setOf(dest),
    use = incoming.map { it.first }.filterIsInstance<Variable>().toSet(),
) {
    override fun toString(): String {
        var returnString = "${dest.name} = phi ${dest.type} "
        var count = 0
        for ((value, label) in incoming) {
            returnString += "[${value.name}, %$label]"
            if (count != incoming.size - 1) {
                returnString += ", "
            }
            count++
        }
        return returnString
    }

    override fun replace(map: MutableMap<Variable, Argument>): Statement {
        val newIncoming = incoming.map { (value, label) -> value.replace(map) to label }
        return PhiStatement(dest, newIncoming.toMutableList())
    }

    fun replaceLabel(map: MutableMap<String, String>) = PhiStatement(
        dest,
        incoming.map { (value, label) ->
            if (label in map) value to map[label]!!
            else value to label
        }.toMutableList(),
    )
}

class PackedMoveStatement(
    val movePairs: List<Pair<Variable, Argument>>,
) : Statement(
    newVariableCount = 0,
    def = movePairs.map { it.first }.toSet(),
    use = movePairs.map { it.second }.filterIsInstance<Variable>().toSet(),
) {
    override fun toString(): String = throw InternalError("Packed move statement is an internal statement")

    override fun replace(map: MutableMap<Variable, Argument>): Statement {
        val newMovePairs = movePairs.map { (variable, value) ->
            variable to value.replace(map)
        }
        return PackedMoveStatement(newMovePairs)
    }
}

class AssignStatement(
    val dest: Variable,
    val src : Argument,
) : Statement(
    newVariableCount = 1,
    def = setOf(dest),
    use = if (src is Variable) setOf(src) else setOf(),
) {
    override fun toString() = throw InternalError("Assign statement is an internal statement")

    override fun replace(map: MutableMap<Variable, Argument>): Statement =
        AssignStatement(dest, src.replace(map))
}

class StringLiteralDecl(
    val name: String,
    val content: String,
) : GlobalDecl {
    override fun toString(): String {
        return "@$name = private unnamed_addr constant " +
                "[${content.length + 1} x i8] " +
                "c\"${escapeStringLiteralToIr(content)}\\00\""
    }
}

enum class BinaryOperator {
    ADD,
    SUB,
    MUL,
    SDIV,
    SREM,
    SHL,
    ASHR,
    AND,
    OR,
    XOR;

    override fun toString() : String = when (this) {
        ADD  -> "add"
        SUB  -> "sub"
        MUL  -> "mul"
        SDIV -> "sdiv"
        SREM -> "srem"
        SHL  -> "shl"
        ASHR -> "ashr"
        AND  -> "and"
        OR   -> "or"
        XOR  -> "xor"
    }
}

enum class IntCmpOperator {
    EQ,
    NE,
    SLT,
    SGT,
    SLE,
    SGE;

    override fun toString() : String = when (this) {
        EQ  -> "eq"
        NE  -> "ne"
        SLT -> "slt"
        SGT -> "sgt"
        SLE -> "sle"
        SGE -> "sge"
    }
}

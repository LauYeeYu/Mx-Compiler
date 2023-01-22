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

import exceptions.InternalException

const val alignValue: Int = 0

class Root(
    val classes: List<GlobalClass>,
    val variables: List<GlobalDecl>,
    val globalFunctions: List<GlobalFunction>,
) {
    override fun toString(): String {
        return classes.joinToString("\n") + "\n" +
                variables.joinToString("\n") + "\n" +
                globalFunctions.joinToString("\n")
    }
}

class GlobalClass(
    val classType: ClassType,
    val nameMap: Map<String, Int>,
) {
    override fun toString() =
        "%class.${classType.name} = type { ${classType.memberList.joinToString(", ")} }"

    val size: Int
        get() = classType.size
}

sealed interface GlobalDecl
abstract class Argument (
    val name: String,
    val type: PrimitiveType,
)

abstract class Variable(name: String, type: PrimitiveType) : Argument(name, type)

class GlobalVariable(name: String, type: PrimitiveType) : Variable(name, type) {
    override fun toString() = when (type.type) {
        TypeProperty.VOID -> "void"
        else -> "$type @$name"
    }
}

class LocalVariable(name: String, type: PrimitiveType) : Variable(name, type) {
    override fun toString() = when (type.type) {
        TypeProperty.VOID -> "void"
        else -> "$type %$name"
    }
}

abstract class IntLiteral(val value: Int, type: PrimitiveType) : Argument(value.toString(), type)

fun getLiteralNode(value: Int, type: Type): IntLiteral {
    return when (type) {
        is PrimitiveType -> when (type.type) {
            TypeProperty.I1 -> I1Literal(value)
            TypeProperty.I8 -> I8Literal(value)
            TypeProperty.I32 -> I32Literal(value)
            TypeProperty.PTR -> PtrLiteral(value)
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
class PtrLiteral(
    value: Int
) : IntLiteral(value, PrimitiveType(TypeProperty.PTR)) {
    override fun toString() = "ptr $value"
}

class GlobalVariableDecl(
    val property: GlobalVariable,
    val initValue: Int, // Every global variable must have an initial value
) : GlobalDecl {
    override fun toString() = when (property.type.type) {
        TypeProperty.PTR -> "@${property.name} = global ${property.type} null, align $alignValue"
        else -> "@${property.name} = global ${property.type} $initValue, align $alignValue"
    }
}

class LocalVariableDecl(val property: LocalVariable, val type: PrimitiveType) : Statement() {
    override fun toString() =
        "%${property.name} = alloca $type, align $alignValue"
}

class GlobalFunction(
    val name: String, // without @
    val returnType: PrimitiveType,
    val parameters: List<FunctionParameter>,
    val variables: MutableList<LocalVariableDecl>? = null,
    val body: MutableList<Block>? = null,
    val returnPhi: PhiStatement? = null,
    val const: Boolean = false,
    // `const` indicates that this function won't change any variable,
    // and always return the same value when given the same arguments.
) {
    val returnVariable: LocalVariable
        get() = LocalVariable("__return", returnType)
    val returnBlock: Block?
        get() = if (body == null) null
        else if (returnType.type == TypeProperty.VOID) {
                Block("return", mutableListOf(ReturnStatement()))
        } else {
                val returnPhiStatement = returnPhi ?: throw InternalException("Return phi statement not found")
                Block("return", mutableListOf(returnPhiStatement, ReturnStatement(returnVariable)))
        }

    private val returnBlockString: String
        get() {
            val returnBlock = returnBlock ?: return ""
            return "$returnBlock\n"
        }

    private val variablesString: String
        get() {
            val variables = variables
                ?: throw InternalException("A function definition has no variable list")
            val stringBuilder = StringBuilder()
            if (variables.isNotEmpty()) {
                for (variable in variables) {
                    stringBuilder.append("${variable}\n")
                }
            }
            return stringBuilder.toString()
        }

    private val bodyString: String
        get() {
            val body = body ?: return ""
            val stringBuilder = StringBuilder()
            if (body.isNotEmpty()) {
                for (i in body.indices) {
                    if (i == 0) stringBuilder.append("${body[0].statementsString}\n")
                    else stringBuilder.append("${body[i]}\n")
                }
            }
            return stringBuilder.toString()
        }

    override fun toString() = when (body) {
        null -> "declare $returnType @$name(${parameters.joinToString(", ")})"
        else -> "define $returnType @$name(${parameters.joinToString(", ")}) {\n" +
                variablesString + bodyString + "\n" + returnBlockString + "}"
    }
}

class FunctionParameter(
    val type: Type,
    val name: String,
) {
    override fun toString() = "$type %$name"
}

class Block(
    val label: String,
    val statements: MutableList<Statement>,
) {
    val statementsString get() = statements.joinToString("\n")
    override fun toString() = "$label:\n$statementsString"
}

abstract class Statement

class CallStatement(
    val dest: Variable?,
    val returnType: Type,
    val function: GlobalFunction,
    val arguments: List<Argument>,
) : Statement() {
    override fun toString() = when (returnType) {
        is PrimitiveType -> when (returnType.type) {
            TypeProperty.VOID -> "call void @${function.name}(${arguments.joinToString(", ")})"
            else -> "%${dest?.name} = call ${returnType.type} @${function.name}(${arguments.joinToString(", ")})"
        }
        else -> "%${dest?.name} = call $returnType @${function.name}(${arguments.joinToString(", ")})"
    }
}

class ReturnStatement(
    val value: Variable? = null,
) : Statement() {
    override fun toString() = when (value) {
        null -> "ret void"
        else -> "ret $value"
    }
}

class BranchStatement(
    val condition: Argument?, // null if unconditional
    val trueBlockLabel: String,
    val falseBlockLabel: String?, // null if unconditional
) : Statement() {
    override fun toString() = when (falseBlockLabel) {
        null -> "br label %$trueBlockLabel"
        else -> "br i1 $condition, label %$trueBlockLabel, label %$falseBlockLabel"
    }

    // Unconditional branch
    constructor(branchLabel: String) : this(null, branchLabel, null)
}

class LoadStatement(
    val dest: LocalVariable,
    val src: Argument,
) : Statement() {
    override fun toString() =
        "${dest.name} = load ${dest.type}, ptr $src, align $alignValue"
}

class StoreStatement(
    val dest: Variable,
    val src : Argument,
) : Statement() {
    override fun toString() = when (src) {
        is Variable -> "store $src, ptr $dest, align $alignValue"
        else -> "store $src, ptr $dest, align $alignValue"
    }
}

class BinaryOperationStatement(
    val dest: LocalVariable,
    val op  : BinaryOperator,
    val lhs : Argument,
    val rhs : Argument,
) : Statement() {
    override fun toString() =
        "${dest.name} = $op ${lhs.type} ${lhs.name}, ${rhs.name}"
}

class IntCmpStatement(
    val dest: LocalVariable,
    val op  : IntCmpOperator,
    val lhs : Argument,
    val rhs : Argument,
) : Statement() {
    override fun toString() =
        "${dest.name} = icmp $op ${lhs.type} ${lhs.name}, ${rhs.name}"
}

class GetElementPtrStatement(
    val dest   : LocalVariable,
    val src    : Variable,
    val srcType: Type,
    val indexes: List<Argument>,
) : Statement() {
    override fun toString(): String {
        var returnString = when (src) {
            // Note that the offset is i32 (Maybe changed in future)
            is GlobalVariable -> "${dest.name} = getelementptr ${srcType}, ptr @${src.name}"
            is LocalVariable  -> "${dest.name} = getelementptr ${srcType}, ptr %${src.name}"
            else              -> throw InternalError("IR: Unknown variable type")
        }
        for (index in indexes) {
            returnString += ", ptr $index"
        }
        return returnString
    }
}

class PhiStatement(
    val dest: LocalVariable,
    val incoming: MutableList<Pair<Argument, String>>,
) : Statement() {
    override fun toString(): String {
        var returnString = "${dest.name} = phi ${dest.type}"
        var count = 0
        for ((value, label) in incoming) {
            returnString += "[$value, %$label]"
            if (count != incoming.size - 1) {
                returnString += ", "
            }
            count++
        }
        return returnString
    }
}

class StringLiteralDecl(
    val name: String,
    val content: String,
) : GlobalDecl {
    override fun toString(): String {
        return "@$name = private unnamed_addr constant " +
                "[${content.length + 1} x i8] " +
                "c\"${escapeStringLiteralToIr(content)}\\00\", " +
                "align $alignValue"
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

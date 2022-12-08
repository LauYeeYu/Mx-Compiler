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
const val ptrSize: Int = 4

class Root(
    val classes: List<GlobalClass>,
    val variables: List<GlobalDecl>,
    val initFunction: GlobalFunction,
    val globalFunctions: List<GlobalFunction>,
) {
    override fun toString(): String {
        return classes.joinToString("\n") + "\n" +
                variables.joinToString("\n") + "\n" +
                initFunction + "\n" +
                globalFunctions.joinToString("\n")
    }
}

class GlobalClass(
    val classType: ClassType,
    val nameMap: Map<String, Int>,
) {
    override fun toString(): String {
        return "%class.${classType.name} = type { ${classType.memberList.joinToString(" ")} }"
    }
}

sealed interface GlobalDecl
abstract class Argument (
    val name: String,
    val type: Type,
)

abstract class Variable(
    name: String,
    type: Type,
) : Argument(name, type)

class GlobalVariable(
    name: String,
    type: Type,
) : Variable(name, type), GlobalDecl {
    override fun toString(): String = when (type) {
        is PrimitiveType -> when (type.type) {
            TypeProperty.VOID -> "void"
            else              -> "$type @$name"
        }
        else                  -> "$type @$name"
    }
}

class LocalVariable(
    name: String,
    type: Type,
) : Variable(name, type) {
    override fun toString(): String = when (type) {
        is PrimitiveType -> when (type.type) {
            TypeProperty.VOID -> "void"
            else -> "$type %$name"
        }
        else -> "$type %$name"
    }
}

abstract class IntLiteral(
    val value: Int,
    type: Type
) : Argument(value.toString(), type)

fun getLiteralNode(type: Type, value: Int): IntLiteral {
    return when (type) {
        is PrimitiveType -> when (type.type) {
            TypeProperty.I1  -> I1Literal(value)
            TypeProperty.I8  -> I8Literal(value)
            TypeProperty.I32 -> I32Literal(value)
            else -> throw InternalException("Unsupported type for literal: $type")
        }
        else -> throw InternalException("Unsupported type for literal: $type")
    }
}

class I1Literal(
    value: Int
) : IntLiteral(value, PrimitiveType(TypeProperty.I1)) {
    override fun toString(): String = if (value == 0) "i1 0" else "i1 1"
}

class I8Literal(
    value: Int
) : IntLiteral(value, PrimitiveType(TypeProperty.I8)) {
    override fun toString(): String = "i8 $value"
}

class I32Literal(
    value: Int
) : IntLiteral(value, PrimitiveType(TypeProperty.I32)) {
    override fun toString(): String = "i32 $value"
}

class GlobalVariableDecl(
    val property: GlobalVariable,
    val initValue: Int, // Every global variable must have an initial value
) {
    override fun toString(): String {
        return "@${property.name} = ${property.type} $initValue, align $alignValue"
    }
}

class LocalVariableDecl(
    val property: GlobalVariable,
    val initValue: Int, // Every global variable must have an initial value
) : Statement() {
    override fun toString(): String {
        return "%${property.name} = ${property.type} $initValue, align $alignValue"
    }
}

class GlobalFunction(
    val name: String, // without @
    val returnType: Type,
    val parameters: List<FunctionParameter>,
    val body: MutableList<Block>,
) {
    override fun toString(): String {
        return "define $returnType @$name(${parameters.joinToString(", ")}) {\n" +
                body.joinToString("\n") + "\n" +
                "}"
    }
}

enum class TypeProperty {
    I32,
    I8,
    I1,
    PTR,
    VOID;

    override fun toString() = when (this) {
        I32  -> "i32"
        I8   -> "i8"
        I1   -> "i1"
        PTR  -> "ptr"
        VOID -> "void"
    }

    val size: Int // the amount of memory needed to store this type
        get() = when (this) {
            I32  -> 4
            I8   -> 1
            I1   -> 1
            PTR  -> ptrSize
            VOID -> 0
        }
}

abstract class Type

class PrimitiveType(
    val type: TypeProperty,
) : Type() {
    override fun toString() = type.toString()
}

class ClassType(
    val name: String,
    val memberList: List<Type>,
) : Type() {
    override fun toString() = "%class.$name"
    fun structure() = "{ ${memberList.joinToString(" ")} }"

    fun declare(): String {
        return "%class.$name = type ${structure()}"
    }
}

class ArrayType(
    val elementType: PrimitiveType,
    val size: Int,
) : Type() {
    override fun toString() = "[$size x $elementType]"
}

class FunctionParameter(
    val type: Type,
    val name: String,
) {
    override fun toString(): String {
        return "$type %$name"
    }
}

class Block(
    val label: Int,
    val statements: MutableList<Statement>,
) {
    override fun toString(): String {
        return "$label:\n" + statements.joinToString("\n")
    }
}

abstract class Statement

class CallStatement(
    val dest: Variable?,
    val returnType: Type,
    val function: GlobalFunction,
    val arguments: List<Argument>,
) : Statement() {
    override fun toString(): String = when (returnType) {
        is PrimitiveType -> when (returnType.type) {
            TypeProperty.VOID -> "call void @${function.name}(${arguments.joinToString(", ")})"
            else -> "%${dest?.name} = call ${returnType.type} @${function.name}(${arguments.joinToString(", ")})"
        }
        else -> "%${dest?.name} = call $returnType @${function.name}(${arguments.joinToString(", ")})"
    }
}

class ReturnStatement(
    val value: Argument,
) : Statement() {
    override fun toString(): String {
        return "ret $value"
    }
}

class BranchStatement(
    val condition: Argument?, // null if unconditional
    val trueBlockLabel: Int,
    val falseBlockLabel: Int?, // null if unconditional
) : Statement() {
    override fun toString(): String {
        return when (falseBlockLabel) {
            null -> "br label %$trueBlockLabel"
            else -> "br $condition, label %$trueBlockLabel, label %$falseBlockLabel"
        }
    }
}

class LoadStatement(
    val dest: LocalVariable,
    val src: Argument,
) : Statement() {
    override fun toString(): String {
        return "${dest.name} = load ${dest.type}, ptr $src, align $alignValue"
    }
}

class StoreStatement(
    val dest: Variable,
    val src : Argument,
) : Statement() {
    override fun toString(): String {
        return "store ${dest.type} $src, ptr $dest, align $alignValue"
    }
}

class BinaryOperationStatement(
    val dest: LocalVariable,
    val op  : BinaryOperator,
    val lhs : Argument,
    val rhs : Argument,
) : Statement() {
    override fun toString(): String {
        return "${dest.name} = $op ${lhs.type} ${lhs.name}, ${rhs.name}"
    }
}

class IntCmpStatement(
    val dest: LocalVariable,
    val op  : IntCmpOperator,
    val lhs : Variable,
    val rhs : Argument,
) : Statement() {
    override fun toString(): String {
        return "${dest.name} = icmp $op ${lhs.type} ${lhs.name}, ${rhs.name}"
    }
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

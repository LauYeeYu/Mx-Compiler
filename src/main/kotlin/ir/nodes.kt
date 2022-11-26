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

const val alignValue: Int = 0
const val ptrSize: Int = 4

class Root(
    val classes: List<GlobalClass>,
    val variables: List<GlobalVariable>,
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
    val name: String,
    val memberList: List<Type>,
) {
    override fun toString(): String {
        return "%class.$name = type { ${memberList.joinToString(" ")} }"
    }
}

abstract class Variable(
    val name: String,
    val type: Type,
)

class GlobalVariable(
    name: String,
    type: Type,
) : Variable(name, type) {
    override fun toString(): String = when (type) {
        is PrimitiveType -> when (type.type) {
            TypeProperty.void -> "void"
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
            TypeProperty.void -> "void"
            else -> "$type %$name"
        }
        else -> "$type %$name"
    }
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
    val body: List<Block>,
) {
    override fun toString(): String {
        return "define $returnType @$name(${parameters.joinToString(", ")}) {\n" +
                body.joinToString("\n") + "\n" +
                "}"
    }
}

enum class TypeProperty {
    i32,
    i8,
    ptr,
    void;

    override fun toString() = when (this) {
        i32  -> "i32"
        i8   -> "i8"
        ptr  -> "ptr"
        void -> "void"
    }

    val size: Int
        get() = when (this) {
            i32  -> 4
            i8   -> 1
            ptr  -> ptrSize
            void -> 0
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

    fun declare(): String {
        return "%class.$name = type { ${memberList.joinToString(", ")} }"
    }
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
    val statements: List<Statement>,
) {
    override fun toString(): String {
        return "$label:\n" + statements.joinToString("\n")
    }
}

abstract class Statement

class CallStatement(
    val dest: Variable,
    val returnType: Type,
    val function: GlobalFunction,
    val parameters: List<Variable>,
) : Statement() {
    override fun toString(): String = when (returnType) {
        is PrimitiveType -> when (returnType.type) {
            TypeProperty.void -> "call void ${function.name}(${parameters.joinToString(", ")})"
            else -> "%${dest.name} = call ${returnType.type} @${function.name}(${parameters.joinToString(", ")})"
        }
        else -> "%${dest.name} = call $returnType @${function.name}(${parameters.joinToString(", ")})"
    }
}

class ReturnStatement(
    val value: Variable,
) : Statement() {
    override fun toString(): String {
        return "ret $value"
    }
}

class BranchStatement(
    val condition: Variable?, // null if unconditional
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
    val src: Variable,
) : Statement() {
    override fun toString(): String {
        return "${dest.name} = load ${dest.type}, ptr $src, align $alignValue"
    }
}

class StoreStatement(
    val dest: Variable,
    val src: Variable,
) : Statement() {
    override fun toString(): String {
        return "store $src, ptr $dest, align $alignValue"
    }
}

class BinaryOperationStatement(
    val dest: LocalVariable,
    val op: BinaryOperator,
    val lhs: Variable,
    val rhs: Variable,
) : Statement() {
    override fun toString(): String {
        return "${dest.name} = $op ${lhs.type} ${lhs.name}, ${rhs.name}"
    }
}

class IntCmpStatement(
    val dest: LocalVariable,
    val op: IntCmpOperator,
    val lhs: Variable,
    val rhs: Variable,
) : Statement() {
    override fun toString(): String {
        return "${dest.name} = icmp $op ${lhs.type} ${lhs.name}, ${rhs.name}"
    }
}

class GetElementPtrStatement(
    val dest: LocalVariable,
    val src: Variable,
    val index: Int,
) : Statement() {
    override fun toString(): String = when (src) {
        // Note that the offset is i32 (Maybe changed in future)
        is GlobalVariable -> "${dest.name} = getelementptr ${dest.type}, ptr @${src.name}, i32 $index"
        is LocalVariable  -> "${dest.name} = getelementptr ${dest.type}, ptr %${src.name}, i32 $index"
        else              -> throw InternalError("IR: Unknown variable type")
    }
}

enum class BinaryOperator {
    add,
    sub,
    mul,
    sdiv,
    srem,
    shl,
    ashr,
    and,
    or,
    xor;

    override fun toString() : String = when (this) {
        add  -> "add"
        sub  -> "sub"
        mul  -> "mul"
        sdiv -> "sdiv"
        srem -> "srem"
        shl  -> "shl"
        ashr -> "ashr"
        and  -> "and"
        or   -> "or"
        xor  -> "xor"
    }
}

enum class IntCmpOperator {
    eq,
    ne,
    slt,
    sgt,
    sle,
    sge;

    override fun toString() : String = when (this) {
        eq  -> "eq"
        ne  -> "ne"
        slt -> "slt"
        sgt -> "sgt"
        sle -> "sle"
        sge -> "sge"
    }
}

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

open class MxType(var environment: EnvironmentRecord?)

open class MxPrimitiveType(environment: EnvironmentRecord?) : MxType(environment)

class MxVoidType : MxPrimitiveType(null) {
    override fun toString() = "void"

    override fun equals(other: Any?): Boolean = when (other) {
        is MxVoidType -> true
        else -> false
    }

    override fun hashCode(): Int {
        return javaClass.hashCode()
    }
}

class MxBoolType : MxPrimitiveType(null) {
    override fun toString() = "bool"

    override fun equals(other: Any?): Boolean = when (other) {
        is MxBoolType -> true
        else -> false
    }

    override fun hashCode(): Int {
        return javaClass.hashCode()
    }
}

class MxIntType : MxPrimitiveType(null) {
    override fun toString() = "int"

    override fun equals(other: Any?): Boolean = when (other) {
        is MxIntType -> true
        else -> false
    }

    override fun hashCode(): Int {
        return javaClass.hashCode()
    }
}

class MxStringType : MxClassType(
    "string",
    null
) {
    init {
        environment = ClassEnvironmentRecord(null, "string").loadStringBuiltin(this)
    }
    override fun toString() = "string"

    override fun equals(other: Any?): Boolean = when (other) {
        is MxStringType -> true
        else -> false
    }

    override fun hashCode(): Int {
        return javaClass.hashCode()
    }
}

class MxNullType : MxPrimitiveType(null) {
    override fun toString() = "null"

    override fun equals(other: Any?) = when (other) {
        is MxNullType -> true
        is MxClassType -> true
        is MxArrayType -> true
        else -> false
    }

    override fun hashCode(): Int {
        return javaClass.hashCode()
    }
}

class MxArrayType(
    val elementType: MxType,
    val dimension: Int
    ) : MxType(ClassEnvironmentRecord(null, "array").loadArrayBuiltin()) {
    override fun toString() = "$elementType" + "[]".repeat(dimension)

    override fun equals(other: Any?): Boolean = when (other) {
        is MxArrayType -> elementType == other.elementType && dimension == other.dimension
        is MxNullType -> true
        else -> false
    }

    override fun hashCode(): Int {
        return javaClass.hashCode() + elementType.hashCode() + dimension
    }
}

open class MxClassType(
    val name: String,
    environment: ClassEnvironmentRecord?,
) : MxType(environment) {
    override fun toString() = name

    override fun equals(other: Any?): Boolean = when (other) {
        is MxClassType -> name == other.name
        is MxNullType -> true
        else -> false
    }

    override fun hashCode(): Int {
        return javaClass.hashCode() + name.hashCode()
    }
}

class MxFunctionType(
    val returnType: MxType,
    val parameterTypes: List<MxType>,
    val fromClass: ast.Class?,
    environment: FunctionEnvironmentRecord?,
) : MxType(environment) {
    constructor(
        returnType: MxType,
        parameterTypes: List<MxType>,
        environment: FunctionEnvironmentRecord?,
    ) : this(returnType, parameterTypes, null, environment)
    override fun toString() = "$returnType(${
        parameterTypes.joinToString(", ") { it.toString() }
    })"
    override fun equals(other: Any?): Boolean = when (other) {
        is MxFunctionType -> returnType == other.returnType && parameterTypes == other.parameterTypes
        else -> false
    }

    override fun hashCode(): Int {
        return javaClass.hashCode() + returnType.hashCode() + parameterTypes.hashCode()
    }
}

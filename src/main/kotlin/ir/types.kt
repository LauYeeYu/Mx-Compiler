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

const val ptrSize: Int = 4
enum class TypeProperty {
    I32,
    I8,
    I1,
    PTR,
    VOID,
    SIZE_T;

    override fun toString() = when (this) {
        I32    -> "i32"
        I8     -> "i8"
        I1     -> "i1"
        PTR    -> "ptr"
        VOID   -> "void"
        SIZE_T -> "i${ptrSize * 8}"
    }

    val size: Int // the amount of memory needed to store this type
        get() = when (this) {
            I32    -> 4
            I8     -> 1
            I1     -> 1
            PTR    -> ptrSize
            VOID   -> 0
            SIZE_T -> ptrSize
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

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

val builtInFunction = listOf<GlobalFunction>(
    GlobalFunction(
        name = "print",
        returnType = PrimitiveType(TypeProperty.VOID),
        parameters = listOf(FunctionParameter(PrimitiveType(TypeProperty.PTR), "__str")),
    ),
    GlobalFunction(
        name = "println",
        returnType = PrimitiveType(TypeProperty.VOID),
        parameters = listOf(FunctionParameter(PrimitiveType(TypeProperty.PTR), "__str")),
    ),
    GlobalFunction(
        name = "printInt",
        returnType = PrimitiveType(TypeProperty.VOID),
        parameters = listOf(FunctionParameter(PrimitiveType(TypeProperty.I32), "__n")),
    ),
    GlobalFunction(
        name = "printlnInt",
        returnType = PrimitiveType(TypeProperty.VOID),
        parameters = listOf(FunctionParameter(PrimitiveType(TypeProperty.I32), "__n")),
    ),
    GlobalFunction(
        name = "getString",
        returnType = PrimitiveType(TypeProperty.PTR),
        parameters = listOf(),
    ),
    GlobalFunction(
        name = "getInt",
        returnType = PrimitiveType(TypeProperty.I32),
        parameters = listOf(),
    ),
    GlobalFunction(
        name = "toString",
        returnType = PrimitiveType(TypeProperty.PTR),
        parameters = listOf(FunctionParameter(PrimitiveType(TypeProperty.I32), "__i")),
    ),
    GlobalFunction(
        name = "string.length",
        returnType = PrimitiveType(TypeProperty.I32),
        parameters = listOf(FunctionParameter(PrimitiveType(TypeProperty.PTR), "__this")),
    ),
    GlobalFunction(
        name = "string.substring",
        returnType = PrimitiveType(TypeProperty.PTR),
        parameters = listOf(
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "__this"),
            FunctionParameter(PrimitiveType(TypeProperty.I32), "left"),
            FunctionParameter(PrimitiveType(TypeProperty.I32), "right"),
        ),
    ),
    GlobalFunction(
        name = "string.parseInt",
        returnType = PrimitiveType(TypeProperty.I32),
        parameters = listOf(FunctionParameter(PrimitiveType(TypeProperty.PTR), "__this")),
    ),
    GlobalFunction(
        name = "string.ord",
        returnType = PrimitiveType(TypeProperty.I32),
        parameters = listOf(
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "__this"),
            FunctionParameter(PrimitiveType(TypeProperty.I32), "pos"),
        ),
    ),
    GlobalFunction(
        name = "string.add",
        returnType = PrimitiveType(TypeProperty.PTR),
        parameters = listOf(
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str1"),
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str2"),
        ),
    ),
    GlobalFunction(
        name = "string.equal",
        returnType = PrimitiveType(TypeProperty.I1),
        parameters = listOf(
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str1"),
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str2"),
        ),
    ),
    GlobalFunction(
        name = "string.notEqual",
        returnType = PrimitiveType(TypeProperty.I1),
        parameters = listOf(
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str1"),
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str2"),
        ),
    ),
    GlobalFunction(
        name = "string.less",
        returnType = PrimitiveType(TypeProperty.I1),
        parameters = listOf(
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str1"),
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str2"),
        ),
    ),
    GlobalFunction(
        name = "string.lessOrEqual",
        returnType = PrimitiveType(TypeProperty.I1),
        parameters = listOf(
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str1"),
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str2"),
        ),
    ),
    GlobalFunction(
        name = "string.greater",
        returnType = PrimitiveType(TypeProperty.I1),
        parameters = listOf(
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str1"),
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str2"),
        ),
    ),
    GlobalFunction(
        name = "string.greaterOrEqual",
        returnType = PrimitiveType(TypeProperty.I1),
        parameters = listOf(
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str1"),
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str2"),
        ),
    ),
    GlobalFunction(
        name = "array.size",
        returnType = PrimitiveType(TypeProperty.I32),
        parameters = listOf(FunctionParameter(PrimitiveType(TypeProperty.PTR), "__this")),
    ),
)

val builtInFunctionMap: Map<String, GlobalFunction> = builtInFunction.associateBy { it.name }

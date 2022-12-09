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

val builtInFunction = listOf(
    GlobalFunction(
        name = "print",
        returnType = PrimitiveType(TypeProperty.VOID),
        parameters = listOf(FunctionParameter(PrimitiveType(TypeProperty.PTR), "str")),
        const = true,
    ),
    GlobalFunction(
        name = "println",
        returnType = PrimitiveType(TypeProperty.VOID),
        parameters = listOf(FunctionParameter(PrimitiveType(TypeProperty.PTR), "str")),
        const = true,
    ),
    GlobalFunction(
        name = "printInt",
        returnType = PrimitiveType(TypeProperty.VOID),
        parameters = listOf(FunctionParameter(PrimitiveType(TypeProperty.I32), "n")),
        const = true,
    ),
    GlobalFunction(
        name = "printlnInt",
        returnType = PrimitiveType(TypeProperty.VOID),
        parameters = listOf(FunctionParameter(PrimitiveType(TypeProperty.I32), "__n")),
        const = true,
    ),
    GlobalFunction(
        name = "getString",
        returnType = PrimitiveType(TypeProperty.PTR),
        parameters = listOf(),
        const = false,
    ),
    GlobalFunction(
        name = "getInt",
        returnType = PrimitiveType(TypeProperty.I32),
        parameters = listOf(),
        const = false,
    ),
    GlobalFunction(
        name = "toString",
        returnType = PrimitiveType(TypeProperty.PTR),
        parameters = listOf(FunctionParameter(PrimitiveType(TypeProperty.I32), "n")),
        const = true,
    ),
    GlobalFunction(
        name = "string.length",
        returnType = PrimitiveType(TypeProperty.I32),
        parameters = listOf(FunctionParameter(PrimitiveType(TypeProperty.PTR), "__this")),
        const = true,
    ),
    GlobalFunction(
        name = "string.substring",
        returnType = PrimitiveType(TypeProperty.PTR),
        parameters = listOf(
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "__this"),
            FunctionParameter(PrimitiveType(TypeProperty.I32), "left"),
            FunctionParameter(PrimitiveType(TypeProperty.I32), "right"),
        ),
        const = true,
    ),
    GlobalFunction(
        name = "string.parseInt",
        returnType = PrimitiveType(TypeProperty.I32),
        parameters = listOf(FunctionParameter(PrimitiveType(TypeProperty.PTR), "__this")),
        const = true,
    ),
    GlobalFunction(
        name = "string.ord",
        returnType = PrimitiveType(TypeProperty.I32),
        parameters = listOf(
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "__this"),
            FunctionParameter(PrimitiveType(TypeProperty.I32), "pos"),
        ),
        const = true,
    ),
    GlobalFunction(
        name = "string.add",
        returnType = PrimitiveType(TypeProperty.PTR),
        parameters = listOf(
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str1"),
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str2"),
        ),
        const = false,
    ),
    GlobalFunction(
        name = "string.equal",
        returnType = PrimitiveType(TypeProperty.I1),
        parameters = listOf(
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str1"),
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str2"),
        ),
        const = true,
    ),
    GlobalFunction(
        name = "string.notEqual",
        returnType = PrimitiveType(TypeProperty.I1),
        parameters = listOf(
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str1"),
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str2"),
        ),
        const = true,
    ),
    GlobalFunction(
        name = "string.less",
        returnType = PrimitiveType(TypeProperty.I1),
        parameters = listOf(
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str1"),
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str2"),
        ),
        const = true,
    ),
    GlobalFunction(
        name = "string.lessOrEqual",
        returnType = PrimitiveType(TypeProperty.I1),
        parameters = listOf(
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str1"),
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str2"),
        ),
        const = true,
    ),
    GlobalFunction(
        name = "string.greater",
        returnType = PrimitiveType(TypeProperty.I1),
        parameters = listOf(
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str1"),
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str2"),
        ),
        const = true,
    ),
    GlobalFunction(
        name = "string.greaterOrEqual",
        returnType = PrimitiveType(TypeProperty.I1),
        parameters = listOf(
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str1"),
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str2"),
        ),
        const = true,
    ),
    GlobalFunction(
        name = "array.size",
        returnType = PrimitiveType(TypeProperty.I32),
        parameters = listOf(FunctionParameter(PrimitiveType(TypeProperty.PTR), "__this")),
        const = true,
    ),
)

val builtInFunctionMap: Map<String, GlobalFunction> = builtInFunction.associateBy { it.name }

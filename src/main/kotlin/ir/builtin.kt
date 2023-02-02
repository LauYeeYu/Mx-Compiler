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
    GlobalFunctionBuilder(
        name = "print",
        returnType = PrimitiveType(TypeProperty.VOID),
        parameters = listOf(FunctionParameter(PrimitiveType(TypeProperty.PTR), "str")),
        const = true,
    ),
    GlobalFunctionBuilder(
        name = "println",
        returnType = PrimitiveType(TypeProperty.VOID),
        parameters = listOf(FunctionParameter(PrimitiveType(TypeProperty.PTR), "str")),
        const = true,
    ),
    GlobalFunctionBuilder(
        name = "printInt",
        returnType = PrimitiveType(TypeProperty.VOID),
        parameters = listOf(FunctionParameter(PrimitiveType(TypeProperty.I32), "n")),
        const = true,
    ),
    GlobalFunctionBuilder(
        name = "printlnInt",
        returnType = PrimitiveType(TypeProperty.VOID),
        parameters = listOf(FunctionParameter(PrimitiveType(TypeProperty.I32), "__n")),
        const = true,
    ),
    GlobalFunctionBuilder(
        name = "getString",
        returnType = PrimitiveType(TypeProperty.PTR),
        parameters = listOf(),
        const = false,
    ),
    GlobalFunctionBuilder(
        name = "getInt",
        returnType = PrimitiveType(TypeProperty.I32),
        parameters = listOf(),
        const = false,
    ),
    GlobalFunctionBuilder(
        name = "toString",
        returnType = PrimitiveType(TypeProperty.PTR),
        parameters = listOf(FunctionParameter(PrimitiveType(TypeProperty.I32), "n")),
        const = true,
    ),
    GlobalFunctionBuilder(
        name = "string.string",
        returnType = PrimitiveType(TypeProperty.PTR),
        parameters = listOf(),
        const = false,
    ),
    GlobalFunctionBuilder(
        name = "string.length",
        returnType = PrimitiveType(TypeProperty.I32),
        parameters = listOf(FunctionParameter(PrimitiveType(TypeProperty.PTR), "__this")),
        const = true,
    ),
    GlobalFunctionBuilder(
        name = "string.substring",
        returnType = PrimitiveType(TypeProperty.PTR),
        parameters = listOf(
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "__this"),
            FunctionParameter(PrimitiveType(TypeProperty.I32), "left"),
            FunctionParameter(PrimitiveType(TypeProperty.I32), "right"),
        ),
        const = true,
    ),
    GlobalFunctionBuilder(
        name = "string.parseInt",
        returnType = PrimitiveType(TypeProperty.I32),
        parameters = listOf(FunctionParameter(PrimitiveType(TypeProperty.PTR), "__this")),
        const = true,
    ),
    GlobalFunctionBuilder(
        name = "string.ord",
        returnType = PrimitiveType(TypeProperty.I32),
        parameters = listOf(
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "__this"),
            FunctionParameter(PrimitiveType(TypeProperty.I32), "pos"),
        ),
        const = true,
    ),
    GlobalFunctionBuilder(
        name = "string.add",
        returnType = PrimitiveType(TypeProperty.PTR),
        parameters = listOf(
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str1"),
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str2"),
        ),
        const = false,
    ),
    GlobalFunctionBuilder(
        name = "string.equal",
        returnType = PrimitiveType(TypeProperty.I1),
        parameters = listOf(
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str1"),
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str2"),
        ),
        const = true,
    ),
    GlobalFunctionBuilder(
        name = "string.notEqual",
        returnType = PrimitiveType(TypeProperty.I1),
        parameters = listOf(
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str1"),
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str2"),
        ),
        const = true,
    ),
    GlobalFunctionBuilder(
        name = "string.less",
        returnType = PrimitiveType(TypeProperty.I1),
        parameters = listOf(
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str1"),
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str2"),
        ),
        const = true,
    ),
    GlobalFunctionBuilder(
        name = "string.lessOrEqual",
        returnType = PrimitiveType(TypeProperty.I1),
        parameters = listOf(
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str1"),
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str2"),
        ),
        const = true,
    ),
    GlobalFunctionBuilder(
        name = "string.greater",
        returnType = PrimitiveType(TypeProperty.I1),
        parameters = listOf(
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str1"),
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str2"),
        ),
        const = true,
    ),
    GlobalFunctionBuilder(
        name = "string.greaterOrEqual",
        returnType = PrimitiveType(TypeProperty.I1),
        parameters = listOf(
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str1"),
            FunctionParameter(PrimitiveType(TypeProperty.PTR), "str2"),
        ),
        const = true,
    ),
    GlobalFunctionBuilder(
        name = "__array.size",
        returnType = PrimitiveType(TypeProperty.I32),
        parameters = listOf(FunctionParameter(PrimitiveType(TypeProperty.PTR), "__this")),
        const = true,
    ),
    GlobalFunctionBuilder(
        name = "malloc",
        returnType = PrimitiveType(TypeProperty.PTR),
        parameters = listOf(FunctionParameter(PrimitiveType(TypeProperty.SIZE_T), "n")),
        const = false,
    ),
    GlobalFunctionBuilder(
        name = "__newPtrArray",
        returnType = PrimitiveType(TypeProperty.PTR),
        parameters = listOf(FunctionParameter(PrimitiveType(TypeProperty.I32), "size")),
        const = false,
    ),
    GlobalFunctionBuilder(
        name = "__newIntArray",
        returnType = PrimitiveType(TypeProperty.PTR),
        parameters = listOf(FunctionParameter(PrimitiveType(TypeProperty.I32), "size")),
        const = false,
    ),
    GlobalFunctionBuilder(
        name = "__newBoolArray",
        returnType = PrimitiveType(TypeProperty.PTR),
        parameters = listOf(FunctionParameter(PrimitiveType(TypeProperty.I32), "size")),
        const = false,
    ),
)

val builtInFunctionMap: Map<String, GlobalFunctionBuilder> = builtInFunction.associateBy { it.name }

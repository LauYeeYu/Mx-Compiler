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

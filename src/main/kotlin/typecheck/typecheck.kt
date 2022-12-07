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

import ast.*
import exceptions.*

enum class Status {
    LVALUE,
    RVALUE,
}

class TypeProperty(
    val type: MxType,
    val status: Status,
)

fun checkType(expression: Expression,
              environmentRecord: EnvironmentRecord,
              ctx: SourceContext?) =
    when (expression) {
        is Object -> checkType(expression, environmentRecord, ctx)
        is Literal -> checkType(expression, environmentRecord, ctx)
        is MemberFunctionAccess -> checkType(expression, environmentRecord, ctx)
        is MemberVariableAccess -> checkType(expression, environmentRecord, ctx)
        is ArrayExpression -> checkType(expression, environmentRecord, ctx)
        is PrefixUpdateExpression -> checkType(expression, environmentRecord, ctx)
        is FunctionCall -> checkType(expression, environmentRecord, ctx)
        is LambdaCall -> checkType(expression, environmentRecord, ctx)
        is LambdaExpression -> checkType(expression, environmentRecord, ctx)
        is NewExpression -> checkType(expression, environmentRecord, ctx)
        is PostfixUpdateExpression -> checkType(expression, environmentRecord, ctx)
        is UnaryExpression -> checkType(expression, environmentRecord, ctx)
        is BinaryExpression -> checkType(expression, environmentRecord, ctx)
        is AssignExpression -> checkType(expression, environmentRecord, ctx)
        else -> throw InternalException("Unknown expression type")
    }

fun checkType(expression: Object,
              environmentRecord: EnvironmentRecord,
              ctx: SourceContext?): TypeProperty {
    when (environmentRecord.findVariableAlike(expression.name)) {
        null -> throw MxException("Undefined variable ${expression.name}", ctx)
        else -> {
            expression.binding = environmentRecord.findVariableAlike(expression.name)
            expression.resultType = TypeProperty(
                environmentRecord.findVariableAlike(expression.name)!!.type,
                Status.LVALUE,
            )
            return expression.resultType!!
        }
    }
}

fun checkType(expression: Literal,
              environmentRecord: EnvironmentRecord,
              ctx: SourceContext?): TypeProperty {
    expression.resultType = when (expression) {
        is IntegerLiteral -> TypeProperty(MxIntType(), Status.RVALUE)
        is StringLiteral -> TypeProperty(MxStringType(), Status.RVALUE)
        is BooleanLiteral -> TypeProperty(MxBoolType(), Status.RVALUE)
        is NullLiteral -> TypeProperty(MxNullType(), Status.RVALUE)
        is ThisLiteral -> {
            when (environmentRecord.inClass()) {
                true -> TypeProperty(environmentRecord.thisType(), Status.RVALUE)
                else -> throw MxException("Cannot use 'this' outside of a class", expression.ctx)
            }
        }
        else -> throw MxException("Unknown literal type", expression.ctx)
    }
    return expression.resultType!!
}

fun checkType(expression: MemberVariableAccess,
              environmentRecord: EnvironmentRecord,
              ctx: SourceContext?): TypeProperty {
    val objectProperty = checkType(expression.objectName, environmentRecord, ctx)
    if (objectProperty.type !is MxClassType) {
        throw TypeMismatchException(
            "Cannot access member variable of non-class type ${objectProperty.type}",
            expression.ctx
        )
    }
    val memberVariable: Binding? =
        objectProperty.type.environment?.variableAlikeBindings?.get(expression.variableName)
    if (memberVariable == null) {
        throw ContextException("Cannot find member variable", expression.ctx)
    }
    expression.resultType = TypeProperty(memberVariable.type, Status.LVALUE)
    return expression.resultType!!
}

fun checkType(expression: MemberFunctionAccess,
              environmentRecord: EnvironmentRecord,
              ctx: SourceContext?): TypeProperty {
    val objectProperty = checkType(expression.objectName, environmentRecord, ctx)
    if (objectProperty.type !is MxClassType &&
        objectProperty.type !is MxArrayType) {
        throw TypeMismatchException("Cannot access member function of non-class type", expression.ctx)
    }
    val memberFunction: Binding? =
        objectProperty.type.environment?.functionAlikeBindings?.get(expression.functionName)
    if (memberFunction == null) {
        throw ContextException("Cannot find member function", expression.ctx)
    }
    expression.resultType = TypeProperty((memberFunction.type as MxFunctionType).returnType, Status.RVALUE)
    return expression.resultType!!
}

fun checkType(expression: ArrayExpression,
              environmentRecord: EnvironmentRecord,
              ctx: SourceContext?): TypeProperty {
    val arrayProperty = checkType(expression.array, environmentRecord, ctx)
    if (arrayProperty.type !is MxArrayType) {
        throw TypeMismatchException("Cannot access array element of non-array type", expression.ctx)
    }
    val indexProperty = checkType(expression.index, environmentRecord, ctx)
    if (indexProperty.type !is MxIntType) {
        throw TypeMismatchException("Array index must be integer", expression.ctx)
    }
    val returnType: MxType =
        if (arrayProperty.type.dimension == 1) {
            arrayProperty.type.elementType
        } else {
            MxArrayType(
                arrayProperty.type.elementType,
                arrayProperty.type.dimension - 1
            )
        }
    expression.resultType = TypeProperty(returnType, Status.LVALUE)
    return expression.resultType!!
}

fun checkType(expression: PrefixUpdateExpression,
              environmentRecord: EnvironmentRecord,
              ctx: SourceContext?): TypeProperty {
    val objectProperty = checkType(expression.operand, environmentRecord, ctx)
    if (objectProperty.type !is MxIntType) {
        throw TypeMismatchException("Cannot update non-integer type", expression.ctx)
    }
    if (objectProperty.status == Status.RVALUE) {
        throw ValueCategoryException("Cannot update rvalue", expression.ctx)
    }
    expression.resultType = TypeProperty(MxIntType(), Status.LVALUE)
    return expression.resultType!!
}

fun checkType(expression: PostfixUpdateExpression,
              environmentRecord: EnvironmentRecord,
              ctx: SourceContext?): TypeProperty {
    val objectProperty = checkType(expression.operand, environmentRecord, ctx)
    if (objectProperty.type !is MxIntType) {
        throw TypeMismatchException("Cannot update non-integer type", expression.ctx)
    }
    if (objectProperty.status == Status.RVALUE) {
        throw ValueCategoryException("Cannot update rvalue", expression.ctx)
    }
    expression.resultType = TypeProperty(MxIntType(), Status.RVALUE)
    return expression.resultType!!
}

fun checkType(expression: FunctionCall,
              environmentRecord: EnvironmentRecord,
              ctx: SourceContext?): TypeProperty {
    val functionBinding: Binding? =
        environmentRecord.findFunctionAlike(expression.functionName)
    if (functionBinding == null) {
        throw ContextException("Cannot find function", expression.ctx)
    }
    if (expression.arguments.size !=
        (functionBinding.type as MxFunctionType).parameterTypes.size) {
        throw ContextException("Argument number mismatch", expression.ctx)
    }
    for (i in 0 until expression.arguments.size) {
        if (checkType(expression.arguments[i], environmentRecord, ctx).type !=
            functionBinding.type.parameterTypes[i]) {
            throw TypeMismatchException("Argument type mismatch", expression.ctx)
        }
    }
    expression.resultType = TypeProperty(functionBinding.type.returnType, Status.RVALUE)
    return expression.resultType!!
}

fun checkType(expression: LambdaCall,
              environmentRecord: EnvironmentRecord,
              ctx: SourceContext?): TypeProperty {
    val lambdaProperty = checkType(expression.lambda, environmentRecord, ctx)
    if (expression.arguments.size !=
        (lambdaProperty.type.environment as FunctionEnvironmentRecord).parameters.size) {
        throw ContextException("Argument number mismatch", expression.ctx)
    }
    for (i in 0 until expression.arguments.size) {
        if (checkType(expression.arguments[i], environmentRecord, ctx).type !=
            (lambdaProperty.type.environment as FunctionEnvironmentRecord).parameters[i].type) {
            throw TypeMismatchException("Argument type mismatch", expression.ctx)
        }
    }
    expression.resultType = TypeProperty(
        (lambdaProperty.type.environment as FunctionEnvironmentRecord).returnType,
        when ((lambdaProperty.type.environment as FunctionEnvironmentRecord).returnType) {
            is MxPrimitiveType -> Status.RVALUE
            else -> Status.LVALUE
        }
    )
    return expression.resultType!!
}

fun checkType(expression: LambdaExpression,
              environmentRecord: EnvironmentRecord,
              ctx: SourceContext?): TypeProperty {
    expression.resultType = TypeProperty(
        buildLambdaBinding(environmentRecord, expression).type,
        Status.RVALUE
    )
    return expression.resultType!!
}

fun checkType(expression: NewExpression,
              environmentRecord: EnvironmentRecord,
              ctx: SourceContext?): TypeProperty {
    for (dimensionArgument in expression.arguments) {
        val dimensionProperty = checkType(dimensionArgument, environmentRecord, ctx)
        if (dimensionProperty.type !is MxIntType) {
            throw TypeMismatchException("Array dimension must be integer", expression.ctx)
        }
    }
    expression.resultType = TypeProperty(
        when (expression.dimension) {
            0 -> environmentRecord.getType(expression.type, ctx)
            else -> MxArrayType(
                environmentRecord.getType(expression.type, null),
                expression.dimension
            )
        },
        Status.RVALUE,
    )
    return expression.resultType!!
}

fun checkType(expression: UnaryExpression,
              environmentRecord: EnvironmentRecord,
              ctx: SourceContext?): TypeProperty {
    expression.resultType = TypeProperty(
        getUnaryExpressionReturn(
            expression.operator,
            checkType(expression.operand, environmentRecord, ctx).type,
            ctx,
        ),
        Status.RVALUE,
    )
    return expression.resultType!!
}

fun checkType(expression: BinaryExpression,
              environmentRecord: EnvironmentRecord,
              ctx: SourceContext?): TypeProperty {
    expression.resultType = TypeProperty(
        getBinaryExpressionReturn(
            checkType(expression.left, environmentRecord, ctx).type,
            expression.operator,
            checkType(expression.right, environmentRecord, ctx).type,
            ctx,
        ),
        Status.RVALUE,
    )
    return expression.resultType!!
}

fun checkType(expression: AssignExpression,
              environmentRecord: EnvironmentRecord,
              ctx: SourceContext?): TypeProperty {
    val leftProperty = checkType(expression.left, environmentRecord, ctx)
    val rightProperty = checkType(expression.right, environmentRecord, ctx)
    if (leftProperty.type != rightProperty.type) {
        throw TypeMismatchException(
            "Assign type mismatch, expecting ${leftProperty.type}, but got ${rightProperty.type}",
            expression.ctx,
        )
    }
    if (leftProperty.status == Status.RVALUE) {
        throw ValueCategoryException("Cannot assign to rvalue", expression.ctx)
    }
    expression.resultType = TypeProperty(leftProperty.type, Status.RVALUE)
    return expression.resultType!!
}

fun getUnaryExpressionReturn(operator: UnaryOperator,
                             inputType: MxType,
                             ctx: SourceContext?): MxType =
    when (operator) {
        UnaryOperator.NEGATIVE -> {
            if (inputType !is MxIntType) {
                throw TypeMismatchException("Cannot negate non-integer type", ctx)
            }
            MxIntType()
        }
        UnaryOperator.POSITIVE -> {
            if (inputType !is MxIntType) {
                throw TypeMismatchException("Cannot negate non-boolean type", ctx)
            }
            MxIntType()
        }
        UnaryOperator.BITWISE_NOT -> {
            if (inputType !is MxIntType) {
                throw TypeMismatchException("Cannot bitwise negate non-integer type", ctx)
            }
            MxIntType()
        }
        UnaryOperator.LOGICAL_NOT -> {
            if (inputType !is MxBoolType) {
                throw TypeMismatchException("Cannot logical negate non-boolean type", ctx)
            }
            MxBoolType()
        }
    }

fun getBinaryExpressionReturn(lhsType: MxType,
                              operator: BinaryOperator,
                              rhsType: MxType,
                              ctx: SourceContext?): MxType =
    when (operator) {
        BinaryOperator.ADD -> {
            if (lhsType != rhsType || lhsType !is MxStringType && lhsType !is MxIntType) {
                throw TypeMismatchException("Cannot add $lhsType with $rhsType", ctx)
            }
            lhsType
        }
        BinaryOperator.SUB -> {
            if (lhsType !is MxIntType || rhsType !is MxIntType) {
                throw TypeMismatchException("Cannot subtract non-integer type", ctx)
            }
            MxIntType()
        }
        BinaryOperator.MUL -> {
            if (lhsType !is MxIntType || rhsType !is MxIntType) {
                throw TypeMismatchException("Cannot multiply non-integer type", ctx)
            }
            MxIntType()
        }
        BinaryOperator.DIV -> {
            if (lhsType !is MxIntType || rhsType !is MxIntType) {
                throw TypeMismatchException("Cannot divide non-integer type", ctx)
            }
            MxIntType()
        }
        BinaryOperator.MOD -> {
            if (lhsType !is MxIntType || rhsType !is MxIntType) {
                throw TypeMismatchException("Cannot modulo non-integer type", ctx)
            }
            MxIntType()
        }
        BinaryOperator.BITWISE_AND -> {
            if (lhsType !is MxIntType || rhsType !is MxIntType) {
                throw TypeMismatchException("Cannot bitwise and non-integer type", ctx)
            }
            MxIntType()
        }
        BinaryOperator.BITWISE_OR -> {
            if (lhsType !is MxIntType || rhsType !is MxIntType) {
                throw TypeMismatchException("Cannot bitwise or non-integer type", ctx)
            }
            MxIntType()
        }
        BinaryOperator.BITWISE_XOR -> {
            if (lhsType !is MxIntType || rhsType !is MxIntType) {
                throw TypeMismatchException("Cannot bitwise xor non-integer type", ctx)
            }
            MxIntType()
        }
        BinaryOperator.LEFT_SHIFT -> {
            if (lhsType !is MxIntType || rhsType !is MxIntType) {
                throw TypeMismatchException("Cannot bitwise left shift non-integer type", ctx)
            }
            MxIntType()
        }
        BinaryOperator.RIGHT_SHIFT -> {
            if (lhsType !is MxIntType || rhsType !is MxIntType) {
                throw TypeMismatchException("Cannot bitwise right shift non-integer type", ctx)
            }
            MxIntType()
        }
        BinaryOperator.LESS_THAN -> {
            if (lhsType != rhsType || lhsType !is MxIntType && lhsType !is MxStringType) {
                throw TypeMismatchException("Cannot compare $lhsType < $rhsType", ctx)
            }
            MxBoolType()
        }
        BinaryOperator.LESS_THAN_OR_EQUAL -> {
            if (lhsType != rhsType || lhsType !is MxIntType && lhsType !is MxStringType) {
                throw TypeMismatchException("Cannot compare $lhsType <= $rhsType", ctx)
            }
            MxBoolType()
        }
        BinaryOperator.GREATER_THAN -> {
            if (lhsType != rhsType || lhsType !is MxIntType && lhsType !is MxStringType) {
                throw TypeMismatchException("Cannot compare $lhsType > $rhsType", ctx)
            }
            MxBoolType()
        }
        BinaryOperator.GREATER_THAN_OR_EQUAL -> {
            if (lhsType != rhsType || lhsType !is MxIntType && lhsType !is MxStringType) {
                throw TypeMismatchException("Cannot compare $lhsType >= $rhsType" , ctx)
            }
            MxBoolType()
        }
        BinaryOperator.EQUAL -> {
            if (lhsType != rhsType) {
                throw TypeMismatchException("Cannot compare $lhsType == $rhsType", ctx)
            }
            MxBoolType()
        }
        BinaryOperator.NOT_EQUAL -> {
            if (lhsType != rhsType) {
                throw TypeMismatchException("Cannot compare non-integer type", ctx)
            }
            MxBoolType()
        }
        BinaryOperator.LOGICAL_AND -> {
            if (lhsType !is MxBoolType || rhsType !is MxBoolType) {
                throw TypeMismatchException("Cannot logical and non-boolean type", ctx)
            }
            MxBoolType()
        }
        BinaryOperator.LOGICAL_OR -> {
            if (lhsType !is MxBoolType || rhsType !is MxBoolType) {
                throw TypeMismatchException("Cannot logical or non-boolean type", ctx)
            }
            MxBoolType()
        }
    }

fun isValidVariableType(type: MxType): Boolean =
    when (type) {
        is MxIntType -> true
        is MxBoolType -> true
        is MxStringType -> true
        is MxClassType -> true
        is MxArrayType -> isValidVariableType(type.elementType)
        else -> false
    }

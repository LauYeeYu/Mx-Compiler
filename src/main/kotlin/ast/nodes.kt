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

package ast

import typecheck.EnvironmentRecord

abstract class AstNode(val ctx: SourceContext) {
    var environment: EnvironmentRecord? = null
}

class TranslateUnit(
    ctx: SourceContext,
    val content: List<GlobalElement>,
) : AstNode(ctx)

sealed interface GlobalElement

// Nodes for Functions
class Function(
    ctx: SourceContext,
    val returnType: Type,
    val name: String,
    val parameters: List<FunctionParameter>,
    val body: BlockStatement,
) : AstNode(ctx), GlobalElement, ClassElement

class FunctionParameter(
    ctx: SourceContext,
    val type: Type,
    val name: String,
) : AstNode(ctx)

// Nodes for Classes
class Class(
    ctx: SourceContext,
    val name: String,
    val body: List<ClassElement>,
) : AstNode(ctx), GlobalElement

sealed interface ClassElement

class Constructor(
    ctx: SourceContext,
    val name: String,
    val body: BlockStatement,
) : AstNode(ctx), ClassElement

class VariablesDeclaration(
    ctx: SourceContext,
    val type: Type,
    val variables: List<VariableDeclaration>,
) : Statement(ctx), ClassElement, GlobalElement

class VariableDeclaration(
    ctx: SourceContext,
    val name: String,
    val body: Expression?,
) : AstNode(ctx)

// Nodes for Statements
open class Statement(ctx: SourceContext) : AstNode(ctx)

class BlockStatement(
    ctx: SourceContext,
    val statements: List<Statement>,
) : Statement(ctx)

class ExpressionStatement(
    ctx: SourceContext,
    val expression: Expression,
) : Statement(ctx)

class BranchStatement(
    ctx: SourceContext,
    val condition: Expression,
    val trueBranch: Statement,
    val falseBranch: Statement?,
) : Statement(ctx)

open class LoopStatement(
    ctx: SourceContext,
    val body: Statement,
) : Statement(ctx)

class WhileStatement(
    ctx: SourceContext,
    val condition: Expression,
    body: Statement,
) : LoopStatement(ctx, body)

class ForExpressionStatement(
    ctx: SourceContext,
    val init: Expression?,
    val condition: Expression?,
    val step: Expression?,
    body: Statement,
) : LoopStatement(ctx, body)

class ForDeclarationStatement(
    ctx: SourceContext,
    val init: VariablesDeclaration,
    val condition: Expression?,
    val step: Expression?,
    body: Statement,
) : LoopStatement(ctx, body)

open class ControlFlowStatement(ctx: SourceContext) : Statement(ctx)

class ContinueStatement(ctx: SourceContext) : ControlFlowStatement(ctx)
class BreakStatement(ctx: SourceContext) : ControlFlowStatement(ctx)
class ReturnStatement(ctx: SourceContext, val expression: Expression?) : ControlFlowStatement(ctx)

class EmptyStatement(ctx: SourceContext) : Statement(ctx)

// Enumerate class for operators
enum class UpdateOperator {
    INCREMENT, // ++
    DECREMENT, // --
}

enum class BinaryOperator {
    ADD,                   // +
    SUB,                   // -
    MUL,                   // *
    DIV,                   // /
    MOD,                   // %
    BITWISE_AND,           // &
    BITWISE_OR,            // |
    BITWISE_XOR,           // ^
    LEFT_SHIFT,            // <<
    RIGHT_SHIFT,           // >>
    LESS_THAN,             // <
    LESS_THAN_OR_EQUAL,    // <=
    GREATER_THAN,          // >
    GREATER_THAN_OR_EQUAL, // >=
    EQUAL,                 // ==
    NOT_EQUAL,             // !=
    LOGICAL_AND,           // &&
    LOGICAL_OR,            // ||
}

enum class UnaryOperator {
    BITWISE_NOT, // ~
    LOGICAL_NOT, // !
    POSITIVE,    // +
    NEGATIVE,    // -
}

// Nodes for Expressions
open class Expression(ctx: SourceContext) : AstNode(ctx)

class Object(ctx: SourceContext, val name: String) : Expression(ctx)

open class Literal(ctx: SourceContext) : Expression(ctx)

class StringLiteral(ctx: SourceContext, val value: String) : Literal(ctx)
class IntegerLiteral(ctx: SourceContext, val value: Int) : Literal(ctx)
class BooleanLiteral(ctx: SourceContext, val value: Boolean) : Literal(ctx)
class NullLiteral(ctx: SourceContext) : Literal(ctx)
class ThisLiteral(ctx: SourceContext) : Literal(ctx)

class MemberVariableAccess(
    ctx: SourceContext,
    val objectName: Expression,
    val variableName: String,
) : Expression(ctx)

class MemberFunctionAccess(
    ctx: SourceContext,
    val objectName: Expression,
    val functionName: String,
    val arguments: List<Expression>,
) : Expression(ctx)

class ArrayExpression(
    ctx: SourceContext,
    val array: Expression,
    val index: Expression,
) : Expression(ctx)

class PrefixUpdateExpression(
    ctx: SourceContext,
    val operator: UpdateOperator,
    val operand: Expression,
) : Expression(ctx)

class FunctionCall(
    ctx: SourceContext,
    val functionName: String,
    val arguments: List<Expression>,
) : Expression(ctx)

class LambdaCall(
    ctx: SourceContext,
    val lambda: LambdaExpression,
    val arguments: List<Expression>,
) : Expression(ctx)

class LambdaExpression(
    ctx: SourceContext,
    val captureReference: Boolean,
    val parameters: List<FunctionParameter>,
    val body: BlockStatement,
) : Expression(ctx)

class NewExpression(
    ctx: SourceContext,
    val type: Type, // Classes and primitive types only
    val arguments: List<Expression>,
    val dimension: Int,
) : Expression(ctx)

class PostfixUpdateExpression(
    ctx: SourceContext,
    val operator: UpdateOperator,
    val operand: Expression,
) : Expression(ctx)

class UnaryExpression(
    ctx: SourceContext,
    val operator: UnaryOperator,
    val operand: Expression,
) : Expression(ctx)

class BinaryExpression(
    ctx: SourceContext,
    val operator: BinaryOperator,
    val left: Expression,
    val right: Expression,
) : Expression(ctx)

class AssignExpression(
    ctx: SourceContext,
    val left: Expression,
    val right: Expression,
) : Expression(ctx)

// Nodes for types
open class Type(ctx: SourceContext) : AstNode(ctx)

open class PrimitiveType(ctx: SourceContext) : Type(ctx)

class VoidType  (ctx: SourceContext) : PrimitiveType(ctx)
class BoolType  (ctx: SourceContext) : PrimitiveType(ctx)
class IntType   (ctx: SourceContext) : PrimitiveType(ctx)
class StringType(ctx: SourceContext) : PrimitiveType(ctx)

class ArrayType(
    ctx: SourceContext,
    val type: Type,
    val dimension: Int,
) : Type(ctx)

class ClassType(
    ctx: SourceContext,
    val name: String,
) : Type(ctx)

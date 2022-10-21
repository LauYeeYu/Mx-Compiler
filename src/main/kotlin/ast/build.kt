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

import MxParser.*
import exceptions.MxException
import org.antlr.v4.runtime.ParserRuleContext

fun buildAst(parseResult: ParseResult) = Ast(parseResult).build()

fun unEscapeString(str: String): String {
    val sb = StringBuilder()
    var i = 0
    while (i < str.length) {
        if (str[i] == '\\') {
            when (str[i]) {
                'n' -> sb.append('\n')
                '\\' -> sb.append('\\')
                '"' -> sb.append('"')
                else -> throw MxException("Invalid escape sequence: ${str[i + 1]}")
            }
            i += 2
        } else {
            sb.append(str[i])
            i++
        }
    }
    return sb.toString()
}

class Ast(private val parseResult: ParseResult) {
    val ParserRuleContext.ctx: SourceContext
        get() = SourceContext(parseResult.source, this)

    fun build() = buildNode(parseResult.cst)

    private fun buildNode(input: TranslationUnitContext) = TranslateUnit(
        input.ctx,
        input.declaration().map { buildNode(it) }
    )

    private fun buildNode(input: DeclarationContext): GlobalElement =
        when (input) {
            is ClassDeclarContext -> buildNode(input)
            is FunctionDeclarContext -> buildNode(input)
            is GlobalVariableDeclarContext -> buildNode(input)
            else -> throw MxException("Unknown declaration type")
        }

    private fun buildNode(input: FunctionDeclarContext) =
        Function(
            input.ctx,
            buildNode(input.functionDeclaration().typename()),
            input.functionDeclaration().identifier().text,
            when (input.functionDeclaration().functionDeclParamList()) {
                null -> listOf()
                else -> input.functionDeclaration()
                    .functionDeclParamList()
                    .functionDeclParam()
                    .map {
                        FunctionParameter(
                            it.ctx,
                            buildNode(it.typename()),
                            it.identifier().text,
                        )
                    }
            },
            buildNode(input.functionDeclaration().body),
        )

    private fun buildNode(input: ClassDeclarContext) = Class(
        input.ctx,
        input.classDeclaration().identifier().text,
        input.classDeclaration().classComponents().map { buildNode(it) }
    )

    private fun buildNode(input: ClassComponentsContext): ClassElement =
        when (input) {
            is ClassMemberDeclarContext -> buildNode(input)
            is ClassMethodDeclarContext -> buildNode(input)
            is ConstructorDeclarContext -> buildNode(input)
            else -> throw MxException("Unknown class component type")
        }

    private fun buildNode(input: ClassMemberDeclarContext) =
        buildNode(input.variableDeclaration())

    private fun buildNode(input: ClassMethodDeclarContext) =
        Function(
            input.ctx,
            buildNode(input.functionDeclaration().typename()),
            input.functionDeclaration().identifier().text,
            input.functionDeclaration().functionDeclParamList().functionDeclParam().map {
                FunctionParameter(
                    input.functionDeclaration().functionDeclParamList().ctx,
                    buildNode(it.typename()),
                    it.identifier().text,
                )
            },
            buildNode(input.functionDeclaration().body),
        )

    private fun buildNode(input: ConstructorDeclarContext) =
        Constructor(
            input.ctx,
            input.constructorDeclaration().identifier().text,
            buildNode(input.constructorDeclaration().body),
        )

    private fun buildNode(input: InitDeclaratorContext) =
        VariableDeclaration(
            input.ctx,
            input.identifier().text,
            buildNode(input.expression()),
        )

    private fun buildNode(input: GlobalVariableDeclarContext) =
        buildNode(input.variableDeclaration())

    // Statements
    private fun buildNode(input: StatementContext): Statement =
        when (input) {
            is BlockStmtContext -> buildNode(input)
            is VarDeclarStmtContext -> buildNode(input)
            is ExpressionStmtContext -> buildNode(input)
            is BranchStmtContext -> buildNode(input)
            is LoopStmtContext -> buildNode(input)
            is CtrlFlowStmtContext -> buildNode(input)
            is EmptyStmtContext -> buildNode(input)
            else -> throw MxException("Unknown statement type")
        }

    private fun buildNode(input: BlockStatementContext) =
        BlockStatement(input.ctx, input.statement().map { buildNode(it) })

    private fun buildNode(input: BlockStmtContext) = buildNode(input.blockStatement())

    private fun buildNode(input: VarDeclarStmtContext) = buildNode(input.variableDeclaration())

    private fun buildNode(input: VariableDeclarationContext) =
        VariablesDeclaration(
            input.ctx,
            buildNode(input.typename()),
            input.initDeclarator().map { buildNode(it) }
        )

    private fun buildNode(input: ExpressionStmtContext) =
        ExpressionStatement(
            input.ctx,
            buildNode(input.expressionStatement().expression())
        )

    private fun buildNode(input: BranchStmtContext) =
        BranchStatement(
            input.ctx,
            buildNode(input.branchStatement().condition),
            buildNode(input.branchStatement().ifStatement),
            buildNode(input.branchStatement().elseStatement),
        )

    private fun buildNode(input: LoopStmtContext): LoopStatement =
        when (input.loopStatement()) {
            is WhileLoopContext -> buildNode(input.loopStatement() as WhileLoopContext)
            is DeclForLoopContext -> buildNode(input.loopStatement() as DeclForLoopContext)
            is ExprForLoopContext -> buildNode(input.loopStatement() as ExprForLoopContext)
            else -> throw MxException("Unknown loop statement type")
        }

    private fun buildNode(input: WhileLoopContext) =
        WhileStatement(
            input.ctx,
            buildNode(input.condition),
            buildNode(input.body),
        )

    private fun buildNode(input: DeclForLoopContext) =
        ForDeclarationStatement(
            input.ctx,
            buildNode(input.init),
            buildNode(input.condition),
            buildNode(input.step),
            buildNode(input.body),
        )

    private fun buildNode(input: ExprForLoopContext) =
        ForExpressionStatement(
            input.ctx,
            buildNode(input.init),
            buildNode(input.condition),
            buildNode(input.step),
            buildNode(input.body),
        )

    private fun buildNode(input: CtrlFlowStmtContext): ControlFlowStatement =
        when (input.controlFlowStatement()) {
            is ReturnStmtContext ->
                buildNode((input.controlFlowStatement() as ReturnStmtContext).returnStatement())

            is BreakStmtContext ->
                buildNode((input.controlFlowStatement() as BreakStmtContext).breakStatement())

            is ContinueStmtContext ->
                buildNode((input.controlFlowStatement() as ContinueStmtContext).continueStatement())

            else -> throw MxException("Unknown control flow statement type")
        }

    private fun buildNode(input: ReturnStatementContext) =
        ReturnStatement(input.ctx, buildNode(input.expression()))

    private fun buildNode(input: BreakStatementContext) = BreakStatement(input.ctx)

    private fun buildNode(input: ContinueStatementContext) = ContinueStatement(input.ctx)

    private fun buildNode(input: EmptyStmtContext) = EmptyStatement(input.ctx)

    // Type
    private fun buildNode(input: TypenameContext): Type = when (input) {
        is PrimitiveTypeContext -> buildNode(input.primitiveTypename())
        is ArrayTypeContext -> buildNode(input)
        is ClassTypeContext -> buildNode(input)
        else -> throw MxException("Unknown type")
    }

    private fun buildNode(input: PrimitiveTypenameContext): PrimitiveType =
        when (input) {
            is IntTypeContext -> IntType(input.ctx)
            is BoolTypeContext -> BoolType(input.ctx)
            is StringTypeContext -> StringType(input.ctx)
            is VoidTypeContext -> VoidType(input.ctx)
            else -> throw MxException("Unknown primitive type")
        }

    private fun buildNode(input: ArrayTypeContext): ArrayType {
        var type = input.typename()
        var dimension = 1
        while (type is ArrayTypeContext) {
            type = type.typename()
            dimension++
        }
        return ArrayType(input.ctx, buildNode(type), dimension)
    }

    private fun buildNode(input: ClassTypeContext) =
        ClassType(input.ctx, input.identifier().text)

    private fun buildNode(input: ExpressionContext): Expression =
        when (input) {
            is LhsExprContext -> buildNode(input.lhsExpression())
            is LambdaExprContext -> buildNode(input.lambdaExpression())
            is NewExprContext -> buildNode(input.newExpression())
            is PostfixUpdateExprContext -> buildNode(input)
            is UnaryExprContext -> buildNode(input)
            is BinaryExprContext -> buildNode(input)
            is AssignExprContext -> buildNode(input)
            else -> throw MxException("Unknown expression type")
        }

    private fun buildNode(input: LhsExpressionContext): Expression =
        when (input) {
            is IdentifierExprContext -> buildNode(input)
            is LiteralExprContext -> buildNode(input)
            is ParenthesesExprContext -> buildNode(input.expression())
            is MemberVariableAccessExprContext -> buildNode(input)
            is MemberFunctionAccessExprContext -> buildNode(input)
            is ArrayExprContext -> buildNode(input)
            is PrefixUpdateExprContext -> buildNode(input)
            is FunCallExprContext -> buildNode(input)
            is LambdaCallExprContext -> buildNode(input)
            else -> throw MxException("Unknown lhs expression type")
        }

    private fun buildNode(input: IdentifierExprContext) =
        Object(input.ctx, input.identifier().Identifier().text)

    private fun buildNode(input: LiteralExprContext): Literal =
        when (input.literalExpression()) {
            is LiteralStringContext -> StringLiteral(
                input.ctx,
                unEscapeString(
                    (input.literalExpression() as LiteralStringContext).stringLiteral().StringLiteral().text
                )
            )

            is LiteralThisContext -> ThisLiteral(input.ctx)
            is LiteralLogicContext -> BooleanLiteral(
                input.ctx,
                (input.literalExpression() as LiteralLogicContext).logicLiteral().True() != null
            )

            is LiteralNullContext -> NullLiteral(input.ctx)
            is LiteralNumberContext -> IntegerLiteral(
                input.ctx,
                (input.literalExpression() as LiteralNumberContext).numberLiteral().IntegerLiteral().text.toInt()
            )

            else -> throw MxException("Unknown literal type")
        }

    private fun buildNode(input: MemberVariableAccessExprContext) =
        MemberVariableAccess(
            input.ctx,
            buildNode(input.lhsExpression()),
            input.identifier().Identifier().text
        )

    private fun buildNode(input: MemberFunctionAccessExprContext) =
        MemberFunctionAccess(
            input.ctx,
            buildNode(input.lhsExpression()),
            input.identifier().Identifier().text,
            when (input.functionCallArgList()) {
                null -> listOf()
                else -> input.functionCallArgList().expression().map { buildNode(it) }
            }
        )

    private fun buildNode(input: ArrayExprContext) =
        ArrayExpression(
            input.ctx,
            buildNode(input.lhsExpression()),
            buildNode(input.expression())
        )

    private fun buildNode(input: PrefixUpdateExprContext) =
        PrefixUpdateExpression(
            input.ctx,
            when (input.op.text) {
                "++" -> UpdateOperator.INCREMENT
                "--" -> UpdateOperator.DECREMENT
                else -> throw MxException("Unknown prefix update operator")
            },
            buildNode(input.lhsExpression())
        )

    private fun buildNode(input: FunCallExprContext) =
        FunctionCall(
            input.ctx,
            input.identifier().Identifier().text,
            when (input.functionCallArgList()) {
                null -> listOf()
                else -> input.functionCallArgList().expression().map { buildNode(it) }
            }
        )

    private fun buildNode(input: LambdaCallExprContext) =
        LambdaCall(
            input.ctx,
            buildNode(input.lambdaExpression()),
            when (input.functionCallArgList()) {
                null -> listOf()
                else -> input.functionCallArgList().expression().map { buildNode(it) }
            }
        )

    private fun buildNode(input: LambdaExpressionContext) =
        LambdaExpression(
            input.ctx,
            input.capture != null,
            when (input.functionDeclParamList()) {
                null -> listOf()
                else -> input.functionDeclParamList().functionDeclParam().map {
                    FunctionParameter(
                        it.ctx,
                        buildNode(it.typename()),
                        it.identifier().text,
                    )
                }
            },
            buildNode(input.body)
        )

    private fun buildNode(input: NewExpressionContext): NewExpression =
        when (input.newTypename()) {
            is NewClassContext ->
                buildNode(input.newTypename() as NewClassContext)
            is NewClassArrayContext ->
                buildNode(input.newTypename() as NewClassArrayContext)
            is NewPrimitiveArrayContext ->
                buildNode(input.newTypename() as NewPrimitiveArrayContext)
            else -> throw MxException("Unknown new expression type")
        }

    private fun buildNode(input: NewClassContext) =
        NewExpression(
            input.ctx,
            ClassType(input.ctx, input.identifier().text),
            listOf(),
            0
        )

    private fun buildNode(input: NewClassArrayContext) =
        NewExpression(
            input.ctx,
            ClassType(input.ctx, input.identifier().text),
            input.newArrayExprCount().map { buildNode(it.expression()) },
            input.newArrayExprCount().size +
                    when (input.newArrayEmptyCount()) {
                null -> 0
                else -> input.newArrayEmptyCount().size
            }
        )

    private fun buildNode(input: NewPrimitiveArrayContext) =
        NewExpression(
            input.ctx,
            buildNode(input.primitiveTypename()),
            input.newArrayExprCount().map { buildNode(it.expression()) },
            input.newArrayExprCount().size +
                    when (input.newArrayEmptyCount()) {
                null -> 0
                else -> input.newArrayEmptyCount().size
            }
        )

    private fun buildNode(input: PostfixUpdateExprContext) =
        PostfixUpdateExpression(
            input.ctx,
            when (input.op.text) {
                "++" -> UpdateOperator.INCREMENT
                "--" -> UpdateOperator.DECREMENT
                else -> throw MxException("Unknown postfix update operator")
            },
            buildNode(input.lhsExpression())
        )

    private fun buildNode(input: UnaryExprContext) =
        UnaryExpression(
            input.ctx,
            when (input.op.text) {
                "-" -> UnaryOperator.NEGATIVE
                "!" -> UnaryOperator.LOGICAL_NOT
                "~" -> UnaryOperator.BITWISE_NOT
                else -> throw MxException("Unknown unary operator")
            },
            buildNode(input.expression())
        )

    private fun buildNode(input: BinaryExprContext) =
        BinaryExpression(
            input.ctx,
            when (input.op.text) {
                "*" -> BinaryOperator.MUL
                "/" -> BinaryOperator.DIV
                "%" -> BinaryOperator.MOD
                "+" -> BinaryOperator.ADD
                "-" -> BinaryOperator.SUB
                "<<" -> BinaryOperator.LEFT_SHIFT
                ">>" -> BinaryOperator.RIGHT_SHIFT
                "<" -> BinaryOperator.LESS_THAN
                "<=" -> BinaryOperator.LESS_THAN_OR_EQUAL
                ">" -> BinaryOperator.GREATER_THAN
                ">=" -> BinaryOperator.GREATER_THAN_OR_EQUAL
                "==" -> BinaryOperator.EQUAL
                "!=" -> BinaryOperator.NOT_EQUAL
                "&" -> BinaryOperator.BITWISE_AND
                "^" -> BinaryOperator.BITWISE_XOR
                "|" -> BinaryOperator.BITWISE_OR
                "&&" -> BinaryOperator.LOGICAL_AND
                "||" -> BinaryOperator.LOGICAL_OR
                else -> throw MxException("Unknown binary operator")
            },
            buildNode(input.l),
            buildNode(input.r)
        )

    private fun buildNode(input: AssignExprContext) =
        AssignExpression(
            input.ctx,
            buildNode(input.l),
            buildNode(input.r)
        )
}

parser grammar MxParser;

options {
  tokenVocab=MxLexer;
}

translationUnit: declaration* EOF;

declaration
    : functionDeclaration # FunctionDeclar
    | classDeclaration    # ClassDeclar
    | variableDeclaration # GlobalVariableDeclar
    ;

// Function
functionDeclaration: typename identifier '(' functionDeclParamList? ')' body=blockStatement;
functionDeclParamList: (functionDeclParam ',')* functionDeclParam;
functionDeclParam: typename identifier;
functionCallArgList: (expression ',')* expression;

// Class
classDeclaration: 'class' identifier '{' classComponents* '}' ';';

classComponents
    : variableDeclaration    # ClassMemberDeclar
    | functionDeclaration    # ClassMethodDeclar
    | constructorDeclaration # ConstructorDeclar
    ;

constructorDeclaration: identifier '(' ')' body=blockStatement;

// statements
statement
    : blockStatement       # BlockStmt
    | variableDeclaration  # VarDeclarStmt
    | expressionStatement  # ExpressionStmt
    | branchStatement      # BranchStmt
    | loopStatement        # LoopStmt
    | controlFlowStatement # CtrlFlowStmt
    | emptyStatement       # EmptyStmt
    ;

blockStatement: '{' statement* '}';

variableDeclaration: typename (initDeclarator ',')* initDeclarator ';';
initDeclarator: identifier ('=' initializer=expression)?;

expressionStatement: expression ';';

branchStatement: If '(' condition=expression ')' ifStatement=statement (Else elseStatement=statement)?;

loopStatement
    : While '(' condition=expression ')' body=statement                                              # WhileLoop
    | For '(' init=variableDeclaration condition=expression? ';' step=expression? ')' body=statement # DeclForLoop
    | For '(' init=expression? ';'     condition=expression? ';' step=expression? ')' body=statement # ExprForLoop
    ;

controlFlowStatement
    : continueStatement # ContinueStmt
    | breakStatement    # BreakStmt
    | returnStatement   # ReturnStmt
    ;
continueStatement: Continue           ';';
breakStatement:    Break              ';';
returnStatement:   Return expression? ';';

emptyStatement: ';';

// Components
typename
    : primitiveTypename # PrimitiveType
    | identifier        # ClassType
    | typename '[' ']'  # ArrayType
    ;

newTypename
    : identifier                                               # NewClass
    | identifier newArrayExprCount+ newArrayEmptyCount*        # NewClassArray
    | primitiveTypename newArrayExprCount+ newArrayEmptyCount* # NewPrimitiveArray
    ;

newArrayExprCount: '[' expression ']';
newArrayEmptyCount: '[' ']';

identifier: Identifier;

stringLiteral: StringLiteral;
thisLiteral:   'this';
logicLiteral:  'true' | 'false';
nullLiteral:   'null';
numberLiteral: IntegerLiteral;

// Expressions
expression
    : lhsExpression                                                        # LhsExpr
    | lambdaExpression                                                     # LambdaExpr
    | newExpression                                                        # NewExpr
    | lhsExpression ('++' | '--')                                          # PostfixUpdateExpr
    | op=('!' | '-' | '~')                                    r=expression # UnaryExpr
    | l=expression op=('*' | '/' | '%')                       r=expression # BinaryExpr
    | l=expression op=('+' | '-')                             r=expression # BinaryExpr
    | l=expression op=('<<' | '>>')                           r=expression # BinaryExpr
    | l=expression op=('<' | '>' | '<=' | '>=' | '==' | '!=') r=expression # BinaryExpr
    | l=expression op='&'                                     r=expression # BinaryExpr
    | l=expression op='|'                                     r=expression # BinaryExpr
    | l=expression op='^'                                     r=expression # BinaryExpr
    | l=expression op='&&'                                    r=expression # BinaryExpr
    | l=expression op='||'                                    r=expression # BinaryExpr
    | l=lhsExpression '='                                     r=expression # AssignExpr
    ;

newExpression: 'new' newTypename;

literalExpression
    : stringLiteral # LiteralString
    | thisLiteral   # LiteralThis
    | logicLiteral  # LiteralLogic
    | nullLiteral   # LiteralNull
    | numberLiteral # LiteralNumber
    ;

lhsExpression
    : identifier                                                      # IdentifierExpr
    | literalExpression                                               # LiteralExpr
    | '(' expression ')'                                              # ParenthesesExpr
    | lhsExpression '.' variable=identifier                           # MemberVariableAccessExpr
    | lhsExpression '.' method=identifier'(' functionCallArgList? ')' # MemberFunctionAccessExpr
    | lhsExpression '[' expression ']'                                # ArrayExpr
    | (op='++' | op='--') lhsExpression                               # PrefixUpdateExpr
    | identifier '(' functionCallArgList? ')'                         # FunCallExpr
    | lambdaExpression '(' functionCallArgList? ')'                   # LambdaCallExpr
    ;

lambdaExpression: '[' capture='&'? ']' '(' functionDeclParamList ')' '->' body=blockStatement;

primitiveTypename
    : Void   # VoidType
    | Bool   # BoolType
    | Int    # IntType
    | String # StringType
    ;

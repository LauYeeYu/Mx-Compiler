parser grammar MxParser;

options {
  tokenVocab=MxLexer;
}

translationUnit: declarationSequence? EOF;

declarationSequence: declaration+;

declaration
    : functionDeclaration # FunctionDeclar
    | classDeclaration    # ClassDeclar
    | variableDeclaration # GlobalVariableDeclar
    ;

// Function
functionDeclaration: typename identifier '(' functionDeclParamList? ')' body=blockStatement;
functionDeclParamList: (functionDeclParam ',')* functionDeclParam;
functionDeclParam: typename identifier;
functionCallParamList: (expression ',')* expression;

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
    : continueStatement
    | breakStatement
    | returnStatement
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
    : identifier                                         # NewClass
    | identifier ('[' expression ']')+ ('[' ']')*        # NewClassArray
    | primitiveTypename ('[' expression ']')+ ('[' ']')* # NewPrimitiveArray
    ;

identifier: Identifier;

stringLiteral: StringLiteral;
thisLiteral:   'this';
logicLiteral:  'true' | 'false';
nullLiteral:   'null';
numberLiteral: IntegerLiteral;

// Expressions
expression
    : lvalueExpression                                                     # LvalueExpr
    | lambdaExpression                                                     # LambdaExpr
    | newExpression                                                        # NewExpr
    | lvalueExpression ('++' | '--')                                       # PostfixUpdateExpr
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
    | l=lvalueExpression '='                                  r=expression # AssignExpr
    ;

newExpression: 'new' newTypename ('(' ')')?;

literalExpression
    : stringLiteral
    | thisLiteral
    | logicLiteral
    | nullLiteral
    | numberLiteral
    ;

lvalueExpression
    : identifier                                                                                 # IdentifierExpr
    | literalExpression                                                                          # LiteralExpr
    | '(' expression ')'                                                                         # ParenthesesExpr
    | lvalueExpression '.' (function=identifier | data=identifier'(' functionCallParamList? ')') # MemberAccessExpr
    | lvalueExpression '[' expression ']'                                                        # ArrayExpr
    | ('++' | '--') lvalueExpression                                                             # PrefixUpdateExpr
    | (identifier | lambdaExpression) '(' functionCallParamList? ')'                             # FunCallExpr
    ;

lambdaExpression: '[' capture='&'? ']' '(' functionDeclParamList ')' '->' ;

primitiveTypename
    : Void
    | Bool
    | Int
    | String
    ;

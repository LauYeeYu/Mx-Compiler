# AST

An AST is an Abstract Syntax Tree. It is a tree composed of AST nodes.
Each AST node has a source context (`ctx`) and some members that contains
the essential information of the AST tree. Block statement nodes, function
nodes, class nodes and the root node also have an environment (`environment`).

- [Operators](#operators)
  - [Update Operators](#update-operators)
  - [Unary Operators](#unary-operators)
  - [Binary Operators](#binary-operators)
- [Interfaces](#interfaces)
  - [Global Element](#global-element)
  - [Class Element](#class-element)
- [Translation Unit](#translation-unit)
- [Function](#function)
- [Function Parameter](#function-parameter)
- [Class](#class)
- [Constructor](#constructor)
- [Multiple Variables Declaration](#multiple-variables-declaration)
- [Variable Declaration](#variable-declaration)
- [Statement](#statement)
  - [Block Statement](#block-statement)
  - [Expression Statement](#expression-statement)
  - [Branch Statement](#branch-statement)
  - [Loop Statement](#loop-statement)
    - [While Statement](#while-statement)
    - [For Expression Statement](#for-expression-statement)
    - [For Declaration Statement](#for-declaration-statement)
  - [Control Flow Statement](#control-flow-statement)
    - [Continue Statement](#continue-statement)
    - [Break Statement](#break-statement)
    - [Return Statement](#return-statement)
  - [Empty Statement](#empty-statement)
  - [Expression](#expression)
    - [Object](#object)
    - [Literal](#literal)
      - [String Literal](#string-literal)
      - [Integer Literal](#integer-literal)
      - [Boolean Literal](#boolean-literal)
      - [Null Literal](#null-literal)
      - [This Literal](#this-literal)
    - [Member Variable Access](#member-variable-access)
    - [Member Function Access](#member-function-access)
    - [Array Expression](#array-expression)
    - [Prefix Update Expression](#prefix-update-expression)
    - [Function Call](#function-call)
    - [Lambda Call](#lambda-call)
    - [Lambda Expression](#lambda-expression)
    - [New Expression](#new-expression)
    - [Postfix Update Expression](#postfix-update-expression)
    - [Unary Expression](#unary-expression)
    - [Binary Expression](#binary-expression)
    - [Assignment Expression](#assignment-expression) 
  - [Type](#type)
    - [Primitive Type](#primitive-type)
      - [Void Type](#void-type)
      - [Bool Type](#bool-type)
      - [Int Type](#int-type)
      - [String Type](#string-type)
    - [Array Type](#array-type)
    - [Class Type](#class-type)

## Operators

### Update Operators
An update operator has only one operand.
- `++` (postfix increment)
- `--` (postfix decrement)

### Unary Operators
A unary operator has only one operand, except
[update operators](#update-operators).
- `~` (bitwise not)
- `!` (logical not)
- `-` (negative)
- `+` (positive)

### Binary Operators
A binary operator has two operands.
- `+` (addition)
- `-` (subtraction)
- `*` (multiplication)
- `/` (division)
- `%` (modulus)
- `<<` (left shift)
- `>>` (right shift)
- `&` (bitwise and)
- `|` (bitwise or)
- `^` (bitwise xor)
- `&&` (logical and)
- `||` (logical or)
- `==` (equal)
- `!=` (not equal)
- `<` (less than)
- `<=` (less than or equal)
- `>` (greater than)
- `>=` (greater than or equal)

## Interfaces

### Global Element
`GlobalElement` is the interface of global elements.

The following classes implement `GlobalElement`:
- [Function](#function)
- [Class](#class)
- [Variable(s) Declaration](#multiple-variables-declaration)

### Class Element
`ClassElement` is the interface of class elements.

The following classes implement `ClassElement`:
- [Function](#function)
- [Variable(s) Declaration](#multiple-variables-declaration)
- [Constructor](#constructor)

## Translation Unit
`TranslationUnit` is the top node for a source file with a list of
[global elements](#global-element).

## Function
`Function` is the class for function with a [return type](#type),
a function name (a `String`), a list of [parameters](#function-parameter)
and a body [(block statement)](#block-statement).

## Function Parameter
`FunctionParameter` is pair of a [type](#type) and a variable name (a `String`).

## Class
`Class` is the class for class with a class name (a `String`),
and a list of [class elements](#class-element).

## Constructor
`Constructor` is the class for constructor with a class name (the same
as the class name), and a body [(block statement)](#block-statement).

## Multiple Variables Declaration
`MultipleVariablesDeclaration` is the class with a list of
[variable declarations](#variable-declaration) and a [type](#type).

## Variable Declaration
`VariableDeclaration` is the class for variable declaration with a
variable name (a `String`) and an optional
[initializing expression](#expression).
 
## Statement
There are several kinds of statements:
- [Block Statement](#block-statement)
- [Expression Statement](#expression-statement)
- [Branch Statement](#branch-statement)
- [Loop Statement](#loop-statement)
  - [While Statement](#while-statement)
  - [For Expression Statement](#for-expression-statement)
  - [For Declaration Statement](#for-declaration-statement)
- [Control Flow Statement](#control-flow-statement)
    - [Continue Statement](#continue-statement)
    - [Break Statement](#break-statement)
  - [Return Statement](#return-statement)
- [Empty Statement](#empty-statement)

### Block Statement
`BlockStatement` is the class for block statement with a list of
[statements](#statement).

### Expression Statement
`ExpressionStatement` is the class for expression statement with an
[expression](#expression).

### Branch Statement
`BranchStatement` is the class for branch statement with a condition
[expression](#expression), a [true statement](#statement), and an optional
[false statement](#statement).

### Loop Statement
There are three kinds of loop statements:
- [While Statement](#while-statement)
- [For Expression Statement](#for-expression-statement)
- [For Declaration Statement](#for-declaration-statement)

#### While Statement
`WhileStatement` is the class for while statement with a
[condition](#expression) and a [body](#statement).

#### For Expression Statement
`ForExpressionStatement` is the class with an optional
[initializing expression](#expression), an optional [condition](#expression),
an optional [step expression](#expression), and a [body](#statement).

#### For Declaration Statement
`ForDeclarationStatement` is the class with an optional
[initializing declaration](#multiple-variables-declaration),
an optional [condition](#expression), an optional [step expression](#expression),
and a [body](#statement).

### Control Flow Statement
There are three kinds of control flow statements:
- [Continue Statement](#continue-statement)
- [Break Statement](#break-statement)
- [Return Statement](#return-statement)

#### Continue Statement
`ContinueStatement` is the class for continue statement.

#### Break Statement
`BreakStatement` is the class for break statement.

#### Return Statement
`ReturnStatement` is the class for return statement with an optional
[return expression](#expression).

### Empty Statement
`EmptyStatement` is the class for empty statement.

## Expression
There are several kinds of expressions:
- [variable](#object)
- [Literal](#literal)
  - [String Literal](#string-literal)
  - [Integer Literal](#integer-literal)
  - [Boolean Literal](#boolean-literal)
  - [Null Literal](#null-literal)
  - [This Literal](#this-literal)
- [Member Variable Access](#member-variable-access)
- [Member Function Access](#member-function-access)
- [Array Expression](#array-expression)
- [Prefix Update Expression](#prefix-update-expression)
- [Function Call](#function-call)
- [Lambda Call](#lambda-call)
- [Lambda Expression](#lambda-expression)
- [New Expression](#new-expression)
- [Postfix Update Expression](#postfix-update-expression)
- [Unary Expression](#unary-expression)
- [Binary Expression](#binary-expression)
- [Assignment Expression](#assignment-expression)

### Object
`Object` is the class for variable with a variable name (a `String`).

### Literal
`Literal` is the interface of literals.

There are several kinds of literals:
- [String Literal](#string-literal)
- [Integer Literal](#integer-literal)
- [Boolean Literal](#boolean-literal)
- [Null Literal](#null-literal)
- [This Literal](#this-literal)

#### String Literal
`StringLiteral` is the class for string literal with a string value (a `String`).

#### Integer Literal
`IntegerLiteral` is the class for integer literal with an integer value (an `int`).

#### Boolean Literal
`BooleanLiteral` is the class for boolean literal with a boolean value (a `boolean`).

#### Null Literal
`NullLiteral` is the class for null literal.

#### This Literal
`ThisLiteral` is the class for this literal (only available in class).

### Member Variable Access
`MemberVariableAccess` is the class for member variable access with an
[object expression](#expression) and a variable name (a `String`).

### Member Function Access
`MemberFunctionAccess` is the class for member function access with an
[object expression](#expression), a function name (a `String`) and a list of
[expressions](#expression).

### Array Expression
`ArrayExpression` is the class for array expression with an
[object name](#expression) and an [index expression](#expression).

### Prefix Update Expression
`PrefixUpdateExpression` is the class for prefix update expression with an
[operand expression](#expression) and an [operator](#update-operators).

### Function Call
`FunctionCall` is the class for function call with a function name (a
`String`) and a list of [arguments](#expression).

### Lambda Call
`LambdaCall` is the class for lambda call with a
[lambda expression](#lambda-expression), and a list of [arguments](#expression).

### Lambda Expression
`LambdaExpression` is the class for lambda expression with an optional
capture mark, a list of [parameters](#function-parameter) and a
[body](#block-statement).

### New Expression
`NewExpression` is the class for new expression with [type](#type), a
list of [arguments](#expression), and a dimension (a integer).

### Postfix Update Expression
`PostfixUpdateExpression` is the class for postfix update expression with an
[operand expression](#expression) and an [operator](#update-operators).

### Unary Expression
`UnaryExpression` is the class for unary expression with an
[operand expression](#expression) and an [operator](#unary-operators).

### Binary Expression
`BinaryExpression` is the class for binary expression with a
[left expression](#expression), a [right expression](#expression), and an
[operator](#binary-operators).

### Assignment Expression
`AssignmentExpression` is the class for assignment expression with a
[left expression](#expression), a [right expression](#expression).

## Type
There are several kinds of types:
- [Primitive Type](#primitive-type)
  - [Void Type](#void-type)
  - [Bool Type](#bool-type)
  - [Int Type](#int-type)
  - [String Type](#string-type)
- [Array Type](#array-type)
- [Class Type](#class-type)

### Primitive Type
`PrimitiveType` is the class for primitive types.

There are four kinds of primitive types:
- [Void Type](#void-type)
- [Bool Type](#bool-type)
- [Int Type](#int-type)
- [String Type](#string-type)

#### Void Type
`VoidType` is the class for void type.

#### Bool Type
`BoolType` is the class for bool type.

#### Int Type
`IntType` is the class for int type.

#### String Type
`StringType` is the class for string type.

### Array Type
`ArrayType` is the class for array type with a [base type](#type) and a
dimension (an integer).

### Class Type
`ClassType` is the class for class type with a class name (a string).

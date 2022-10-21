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

lexer grammar MxLexer;

// Comments
LineComment: '//' ~[\r\n\u2028\u2029]* -> channel(HIDDEN);
BlockComment: '/*' .*? '*/' -> channel(HIDDEN);

// Keywords
Void:     'void';
Bool:     'bool';
Int:      'int';
String:   'string';
New:      'new';
Class:    'class';
Null:     'null';
True:     'true';
False:    'false';
This:     'this';
If:       'if';
Else:     'else';
For:      'for';
While:    'while';
Break:    'break';
Continue: 'continue';
Return:   'return';

// Operators
OpArrow: '->';
OpIncre: '++';
OpDecre: '--';
OpAnd: '&&';
OpOr: '||';
OpNot: '!';
OpShr: '>>';
OpShl: '<<';
OpBitAnd: '&';
OpBitOr: '|';
OpBitXor: '^';
OpBitCompl: '~';
OpAdd: '+';
OPSub: '-';
OPMul: '*';
OpDiv: '/';
OpMod: '%';
OpGeq: '>=';
OpLeq: '<=';
OpGt: '>';
OpLt: '<';
OpNeq: '!=';
OpEq: '==';
OpAssign: '=';
OpMemberAccess: '.';
LBracket: '[';
RBracket: ']';
LParenthese: '(';
RParenthese: ')';
LBrace: '{';
RBrace: '}';
SemiColon: ';';
Comma: ',';

// fragments
fragment Digit: [0-9];
fragment Symbol: [!"#$%&'()*+,\-./:;<=>?@[\]^_`{|}~];
fragment DigitExceptZero: [1-9];
fragment Letter: [a-zA-Z];
fragment IdentifierCharacter: [a-zA-Z0-9_];
fragment EscapeCharacter
    : 'n'
    | '\\'
    | '"';
fragment StringCharacter
    : ~["\\\n\r\u2028\u2029]
    | '\\' EscapeCharacter;

// Identifier
Identifier : Letter IdentifierCharacter*;

// Literals
LogicalLiteral: True | False;
IntegerLiteral: ('0' | DigitExceptZero Digit*);
StringLiteral: '"' StringCharacter* '"';
NullLiteral: Null;

Whitespace
  :(' '
  | '\t'
  | '\u000B'
  | '\u000C'
  | '\u00A0'
  ) -> channel(HIDDEN);

NewLine
  :('\r'
  | '\n'
  | '\u2028'
  | '\u2029'
  ) -> channel(HIDDEN);

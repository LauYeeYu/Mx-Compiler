lexer grammar MxLexer;

SingleLineComment: '//' ~[\r\n\u2028\u2029]* -> channel(HIDDEN);

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

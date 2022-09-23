parser grammar MxParser;

options {
  tokenVocab=MxLexer;
}

translationUnit: declarationSequence? EOF;

declarationSequence: declaration+;

declaration: ;

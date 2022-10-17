# Mx Compiler

This is an assignment of SJTU ACM class, a compiler for a language called
Mx*. For more information about this assignment, click
[here](https://github.com/ACMClassCourses/Compiler-Design-Implementation).

此爲上海交通大學 ACM 班編譯器大作業。此編譯器將 Mx* 編譯到
RV32-I。如需獲取更多關於此大作業的内容，點此檢視[詳情](https://github.com/ACMClassCourses/Compiler-Design-Implementation)。

*This project is still in development.*

*此項目仍在開發階段。*

## Overview

This project compiles the code written by Mx* (the grammar of Mx* is
defined [here](https://github.com/ACMClassCourses/Compiler-Design-Implementation)),
into binary code in RISC-V 32I.

此項目將 Mx* 語言（語法定義在[此處](https://github.com/ACMClassCourses/Compiler-Design-Implementation)）編譯到
RISC-V 32I 平臺。

## Development Progress 開發進度

### Semantic
- grammar file for Lexer and Parser (Finished)
- use the parser and lexer to perform a grammar check (Todo)

### Code generating

### Register Allocation

## Implementation-Defined Behaviour 實現定義行爲

### Comments 註釋

Block comments are supported.

支援塊註釋。

```c++
/*
 * block coment
 */
/* block comment */
```

### UTF-8 Character UTF-8 字符

- `\u2028` and `\u2029` are also regarded as new line character.

  `\u2028` and `\u2029` 亦被當作換行符。
- All UTF-8 character except `\u2028` and `\u2029` can be used in a string.

  字符串支援除 `\u2028` and `\u2029` 外的所有 UTF-8 字符。
- All UTF-8 character except `\u2028` and `\u2029` can be used in line comments.

  行註釋支援除 `\u2028` and `\u2029` 外的所有 UTF-8 字符。
- All UTF-8 character can be used in the block comment.

  行註釋支援所有 UTF-8 字符。

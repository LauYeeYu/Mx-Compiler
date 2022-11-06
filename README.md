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
- build abstract syntax tree from concrete syntax tree (Finished)
- build symbol table (Finished)
- type check (Finished)

### Code Generating
- Convert the AST to IR (Developing)
- Generate RISC-V assembly code (Not started)

### Register Allocation

## Implementation-Defined Behaviour 實現定義行爲

### Method of Literals 字面量內建方法

Method of Literals is supported and performs as the ordinary method call.

字面量內建方法被支持，其行爲與普通方法相同。

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

## License 許可證

Mx-Compiler - a compiler implementation for [Mx](https://github.com/ACMClassCourses/Compiler-Design-Implementation).

Copyright (C) 2022  Lau Yee-Yu

This library is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

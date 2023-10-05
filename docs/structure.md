# Mx Compiler

The Mx Compiler can check the code and generate the RISC-V assembly code.

## Order of Compilation

### Lex and Parse
This part uses [ANTLR project](https://www.antlr.org/) to generate the lexer
and parser. The parser will finally generate the concrete syntax tree (CST).

### Build the Abstract Syntax Tree (AST)
Why we need AST?

- the CST has a lot of useless elements (like the `;`);
- the CST generate by antlr cannot be modified;
- the CST is not easy to traverse, some elements is too deep.

Therefore, we build an AST from the CST. The AST is easy to traverse and
can be more simple.

For more details about AST, see [the document for AST](ast.md).

### Build the Symbol Table
The symbol table is built from the AST. The symbol table is used to check
the type of variables and functions for each scope. Bindings (defined in
`kotlin/typecheck/environment.kt`) is used to implement this.

### Type Check
The program checks the type of expressions and statements in the AST, with
the help of the symbol table.

This procedure will scan the AST three times. The first time is to register
all classes. The second time is to register all functions, including member
functions and non-member functions. The third time is to check the type of
expressions and statements.

### Convert the AST to IR
The AST is converted to [LLVM IR](https://llvm.org/docs/LangRef.html), with
the help of the symbol table.

For more details about IR, see [the document for IR](ir.md).

### Promotion

#### Memory to Register Promotion Pass

See [the document for memory to register promotion](mem2reg.md).

### Generate RISC-V Assembly Code
The IR is converted to RISC-V assembly code.

For more details about the assembly code, see
[the document for assembly](asm.md).

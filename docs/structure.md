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
We build the symbol table from the AST. The symbol table is used to check
the type of variables and functions for each scope. Bindings (defined in
`kotlin/typecheck/environment.kt`) is used to implement this.

### Type Check
We check the type of expressions and statements in the AST, with the help
of the symbol table.

### Convert the AST to IR
We convert the AST to [LLVM IR](https://llvm.org/docs/LangRef.html), with
the help of the symbol table.

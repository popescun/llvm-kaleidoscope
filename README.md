# Implementing a language with LLVM
## References
- [My First Language Frontend with LLVM](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html#my-first-language-frontend-with-llvm-tutorial)

## Changes
- there are no globals, code is better structured in components for `lexer`, `parser`, `AST expressions`, `IR code generator` and `JIT`
- follow Google C++ coding guideline
- use visitor for generating IR code
- use comma separated argument list in function prototypes and calls

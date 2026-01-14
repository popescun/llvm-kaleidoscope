# Implementing a language with LLVM
## References
- [My First Language Frontend with LLVM](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html#my-first-language-frontend-with-llvm-tutorial)

## Changes
- there are no globals, code is better structured in components for `lexer`, `parser`, `AST expressions`, `IR code generator` and `JIT`
- follow Google C++ coding guideline
- use visitor for generating IR code
- use comma separated argument list in function prototypes and calls
- AST expression tree is stored in a map
- changed body delimiters in `for .. in <body> ;` to braces `{..}`    
- fixed for-loop that behaved like do..while, now following expression is a legit one:
```cpp
  def mandelhelp(xmin, xmax, xstep, ymin, ymax, ystep)
    for y = ymin, y < ymax, ystep {
      for x = xmin, x < xmax, xstep {
        printdensity(mandelconverge(x,y));
      }
      putch(10);
    }
```

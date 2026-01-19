# Implementing a language with LLVM
## References
- [My First Language Frontend with LLVM](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html#my-first-language-frontend-with-llvm-tutorial)

## Changes
- there are no globals, code is better structured in components for `lexer`, `parser`, `AST expressions`, `IR code generator` and `JIT`
- follow Google C++ coding guideline
- use visitor for generating IR code
- use comma separated argument list in function prototypes and calls
- AST expression tree is stored in a map
- can load source files written in `toy` language. Added `library.toy` file that contains user
    defined operators discussed in [chpater 6](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl06.html),
    including the `mandelbrot` example.
- changed body delimiters in `for .. in <body> ;` to braces `{..}`    
- fixed for-loop that behaved like `do..while` and allow multiple expressions in the body.
  Now following expression is a legit one:
```cpp
  def mandelhelp(xmin, xmax, xstep, ymin, ymax, ystep)
    for y = ymin, y < ymax, ystep {
      for x = xmin, x < xmax, xstep {
        printdensity(mandelconverge(x,y));
      }
      putch(10);
    }
```
- `toy` compiler executable can run in two modes
  - `JIT` mode. Just run it without any cli options.
    - `compilation` mode. Run it as:
      ```bash
      ./toy compile
      ```
      This will run the main loop where code can be generated.
      On `toy>exit` all generated code is dumped to `output.o` object file.
      In `test/test_linking` there's an example how the object file is linked to the main program.
      Notice that extern functions used in toy program, like `putch`, need to be exported from the application runtime. 
      The `test_linking` shows this case.
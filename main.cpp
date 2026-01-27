#include <iostream>

#include "jit.hpp"

#include <vector>

int main(int argc, char *argv[]) {

  const std::vector<std::string> args(argv, argv + argc);

  if (args.size() == 1) {
    std::cout << "jit mode...\n";
    // create Jit and run
    toy::Jit::create()->run();
  } else if (args[1] == "compile") {
    std::cout << "compile mode...\n";
    toy::ParserAST().run().compile();
  } else if (args[1] == "debug") {
    // std::cout << "debug mode...\n";
    auto jit = toy::Jit::create();
    jit->parser_ast_->debug().run();
  } else {
    throw std::runtime_error("Invalid argument(s)");
  }

  return 0;
}

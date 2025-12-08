#include <iostream>
#include <ostream>

#include "jit.hpp"

int main() {
  auto jit = toy::Jit::create();
  jit->run();
  return 0;
}

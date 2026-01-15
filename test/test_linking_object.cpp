#include <iomanip>
#include <iostream>

extern "C" {
  double average(double, double);
}

// todo: extern functions are not found by expected name in
// todo: the object file, so the linking is failing
int main(int argc, char *argv[]) {
  std::cout << "average " << average(3.0, 4.0) << std::endl;
  return 0;
}

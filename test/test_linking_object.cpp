#include <iomanip>
#include <iostream>

extern "C" {
double mandel(double, double, double, double);
double fib(double);
}

// extern functions, like `putch`, need to be exported from this application
// runtime
#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

extern "C" DLLEXPORT double putch(double x) {
  fputc(static_cast<char>(x), stderr);
  return 0;
}

int main(int argc, char *argv[]) {
  // mandel(-2.3, -1.3, 0.05, 0.07);
  fib(10);
  return 0;
}

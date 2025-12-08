//
// Created by Nicolae Popescu on 08/12/2025.
//

#include <cstdint>
#include <cstdio>

namespace toy {
void log_error(const char *what, std::int32_t row, std::int32_t col) {
  if (row >= 0) {
    fprintf(stderr, "error: %s at row %d, col %d\n", what, row, col);
  } else {
    fprintf(stderr, "error: %s", what);
  }
}

void log_error_prototype(const char *what, std::int32_t row, std::int32_t col) {
  log_error(what, row, col);
}
} // namespace toy
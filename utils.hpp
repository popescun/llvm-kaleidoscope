//
// Created by Nicolae Popescu on 08/12/2025.
//

#ifndef TOY_UTILS_HPP
#define TOY_UTILS_HPP
#include <cstdint>

namespace toy {
void log_error(const char *what, std::int32_t row, std::int32_t col);
void log_error_prototype(const char *what, std::int32_t row, std::int32_t col);
} // namespace toy

#endif // TOY_UTILS_HPP
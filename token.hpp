//
// Created by Nicolae Popescu on 03/12/2025.
//

#ifndef TOY_TOKEN_HPP
#define TOY_TOKEN_HPP

#include <cstdint>
#include <cstdio>

namespace toy {
using Token = std::int8_t;

enum class ReservedToken : Token {
  token_unknown = -100,
  token_eof = EOF, // -1
  // keywords
  token_function_definition = -2,
  token_external_function = -3,
  // primary, standalone or start of an expression
  token_identifier = -4,
  token_number = -5,
  token_if = -6,
  // punctuators
  token_leading_parenthesis = '(',
  token_trailing_parenthesis = ')',
  token_whitespace = ' ',
  token_comma = ',',
  token_semicolon = ';',
  // binary operations
  token_operator_assignment = '=',
  token_operator_add = '+',
  token_operator_subtract = '-',
  token_operator_multiply = '*',
  token_operator_less = '<',
  // helper tokens for user defined operators in the language standard library!
  token_binary = -7,
  token_unary = -8,
  // input
  token_new_line = '\n',
  token_carriage_return = '\r',
  // comment
  token_comment = '#',
  token_dot = '.',
  // flow control
  token_then = -10,
  token_else = -11,
  token_for = -12,
  token_in = -13,
  // exit program
  token_exit = -14
};

#define STRINGIFY(s) #s
#define keyword_token(t) constexpr auto keyword_token_##t{STRINGIFY(t)};

// clang-format off
  keyword_token(def)
  keyword_token(extern)
  keyword_token(if)
  keyword_token(then)
  keyword_token(else);
  keyword_token(for);
  keyword_token(in);
  // helper keywords for user defined operators in the language standard library!
  // From doc: Many languages aspire to being able to implement their standard
  // runtime library in the language itself. In Kaleidoscope, we can implement
  // significant parts of the language in the library!
  keyword_token(binary);
  keyword_token(unary);

} // namespace toy

  // clang-format on

#endif // TOY_TOKEN_HPP
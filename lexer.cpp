//
// Created by Nicolae Popescu on 11/11/2025.
//

#include "lexer.hpp"

namespace toy {

void Lexer::next_token() {
  auto get_char = [this]() -> Token {
    const auto c = static_cast<Token>(std::getchar());
    if (c == to_token(ReservedToken::token_new_line)) {
      col_ = 0;
      ++row_;
    } else {
      ++col_;
    }
    return c;
  };

  // skip any whitespace
  while (isspace(next_token_)) {
    next_token_ = get_char();
  }

  // keywords and identifier: [a-zA-Z][a-zA-Z0-9]*
  if (isalpha(next_token_)) {
    identifier_ = next_token_;
    while (isalnum(next_token_ = get_char())) {
      identifier_ += next_token_;
    }

    // todo: use defines or so fo keywords
    if (identifier_ == keyword_token_def) {
      current_token_ = to_token(ReservedToken::token_function_definition);
      return;
    }

    if (identifier_ == "exit") {
      current_token_ = to_token(ReservedToken::token_exit);
      return;
    }

    if (identifier_ == keyword_token_def) {
      current_token_ = to_token(ReservedToken::token_function_definition);
      return;
    }

    if (identifier_ == keyword_token_extern) {
      current_token_ = to_token(ReservedToken::token_external_function);
      return;
    }

    if (identifier_ == keyword_token_if) {
      current_token_ = to_token(ReservedToken::token_if);
      return;
    }

    if (identifier_ == keyword_token_then) {
      current_token_ = to_token(ReservedToken::token_then);
      return;
    }

    if (identifier_ == keyword_token_else) {
      current_token_ = to_token(ReservedToken::token_else);
      return;
    }

    if (identifier_ == keyword_token_for) {
      current_token_ = to_token(ReservedToken::token_for);
      return;
    }

    if (identifier_ == keyword_token_in) {
      current_token_ = to_token(ReservedToken::token_in);
      return;
    }

    if (identifier_ == keyword_token_unary) {
      current_token_ = to_token(ReservedToken::token_unary);
      return;
    }

    if (identifier_ == keyword_token_binary) {
      current_token_ = to_token(ReservedToken::token_binary);
      return;
    }

    // otherwise is an identifier
    current_token_ = to_token(ReservedToken::token_identifier);
    return;
  }

  // number: [0-9.]+
  if (isdigit(next_token_) ||
      next_token_ == to_token(ReservedToken::token_dot)) {
    std::string number;
    do {
      number += next_token_;
      next_token_ = get_char();
    } while (isdigit(next_token_) ||
             next_token_ == to_token(ReservedToken::token_dot));
    number_value_ = strtod(number.c_str(), nullptr);
    current_token_ = to_token(ReservedToken::token_number);
    return;
  }

  // comment until end of line
  if (next_token_ == to_token(ReservedToken::token_comment)) {
    do {
      next_token_ = get_char();
    } while (next_token_ != to_token(ReservedToken::token_eof) &&
             next_token_ != to_token(ReservedToken::token_new_line) &&
             next_token_ != to_token(ReservedToken::token_carriage_return));

    if (next_token_ != to_token(ReservedToken::token_eof)) {
      next_token();
      return;
    }
  }

  // check for end of file. Don't eat the EOF.
  if (next_token_ == to_token(ReservedToken::token_eof)) {
    current_token_ = to_token(ReservedToken::token_eof);
    return;
  }

  if (next_token_ == to_token(ReservedToken::token_number)) {
    ++row_;
    col_ = 0;
  }

  // otherwise, just return the character as its ascii value
  current_token_ = next_token_;
  next_token_ = get_char();
}

std::int8_t Lexer::get_current_token_precedence() {
  if (!isascii(current_token_)) {
    return -1;
  }

  // make sure it's a predefined binary operation
  const auto &token_precedence = binary_op_precedence_[current_token_];
  if (token_precedence <= 0) {
    return -1;
  }
  return token_precedence;
}

} // namespace toy
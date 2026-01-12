//
// Created by Nicolae Popescu on 11/11/2025.
//

#include "lexer.hpp"

#include <assert.h>

#include "utils.hpp"

#include <fstream>

namespace toy {

Lexer::Lexer() {
  // file_ = std::ifstream{"library.toy"};
  file_ = std::ifstream{"library_test.toy"};
  if (!file_.is_open()) {
    log_error("file library.toy not found", -1, 0);
  }
}

void Lexer::next_token() {
  auto get_char = [this]() -> Token {
    std::filebuf *file_buffer{nullptr};
    if (file_.is_open()) {
      file_buffer = file_.rdbuf();
    }

    // fprintf(stderr, "eof: %d\n, next token:%c\n", file_buffer->sgetc() ==
    // EOF, (char)next_token_);
    auto c = static_cast<Token>(file_buffer ? file_buffer->sbumpc()
                                            : std::getchar());
    if (file_buffer && file_buffer->sgetc() == EOF) {
      // c = EOF;
      file_.close();
      file_buffer = nullptr;
    }

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

    // if (identifier_ == keyword_token_in) {
    //   current_token_ = to_token(ReservedToken::token_in);
    //   return;
    // }

    if (identifier_ == keyword_token_unary) {
      current_token_ = to_token(ReservedToken::token_unary);
      return;
    }

    if (identifier_ == keyword_token_binary) {
      current_token_ = to_token(ReservedToken::token_binary);
      return;
    }

    if (identifier_ == keyword_token_var) {
      current_token_ = to_token(ReservedToken::token_var);
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
    } while (next_token_ != static_cast<Token>(EOF) &&
             next_token_ != to_token(ReservedToken::token_new_line) &&
             next_token_ != to_token(ReservedToken::token_carriage_return));

    if (next_token_ != to_token(ReservedToken::token_eof)) {
      current_token_ = next_token_;
      return;
    }
  }

  // check for end of file. Don't eat the EOF.
  if (next_token_ == static_cast<Token>(EOF)) {
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
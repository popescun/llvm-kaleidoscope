//
// Created by Nicolae Popescu on 11/11/2025.
//

#ifndef TOY_LEXER_HPP
#define TOY_LEXER_HPP

#include "token.hpp"

#include <fstream>
#include <map>
#include <string>

namespace toy {

/**
 * The lexer returns tokens [0-255] if it is an unknown character, otherwise
 * one of these for known things.
 */
struct Lexer {
  /**
   * Constructor;
   */
  Lexer();
  /**
   * Update current token and next character token from standard input.
   *
   * Reads another token, parse it and updates `current_token_` with its
   * results. It also updates next character token `next_token_` without parsing
   * it.
   *
   *
   * @return current token
   */
  void next_token();

  /**
   * Helper function to get next token.
   *
   * @return Token
   */
  Token get_next_token() {
    next_token();
    return current_token_;
  }

  /**
   * Get the precedence of the pending binary operator token.
   * @return token precedence
   */
  std::int8_t get_current_token_precedence();

  // conversion helper
  static Token to_token(ReservedToken token) {
    return static_cast<Token>(token);
  }

  // conversion helper
  static ReservedToken to_reserved_token(Token token) {
    return static_cast<ReservedToken>(token);
  }

  // next character(unparsed) token
  Token next_token_{to_token(ReservedToken::token_whitespace)};
  // current parsed token the lexer is looking at
  Token current_token_{to_token(ReservedToken::token_unknown)};
  // filled in if token_identifier
  std::string identifier_{"none"};
  // filled in if token_number
  double number_value_{0};
  // todo: should move to the AST parser?
  using BinaryOperationPrecedence = std::map<Token, Token>;
  // this holds the precedence for each binary operator token that is defined.
  // Install standard binary operators, first has the lowest precedence.
  // Range: 0..100
  BinaryOperationPrecedence binary_op_precedence_ = {
      {'m', -1}, {'=', 2},  {'<', 10}, {'+', 20},
      {'-', 20}, {'*', 40}, {'M', 101}};
  // track where current token position
  std::int32_t row_{0};
  std::int32_t col_{0};
  std::ifstream file_;
};

} // namespace toy

#endif // TOY_LEXER_HPP
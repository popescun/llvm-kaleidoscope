//
// Created by Nicolae Popescu on 03/12/2025.
//

#ifndef TOY_AST_EXPRESSIONS_HPP
#define TOY_AST_EXPRESSIONS_HPP

#include "token.hpp"

#include <llvm/IR/Function.h>

#include <map>
#include <memory>
#include <string>
#include <vector>

namespace toy {

struct ParserAST;

// AST types

struct IRCode {
  virtual ~IRCode() = default;
  virtual llvm::Value *generate_IR_code() = 0;
};

/**
 *  Base struct for all expression nodes.
 */
struct ExpressionAST : IRCode {
  ~ExpressionAST() override = default;
};

/**
 * Expression struct for numeric literals like "1.0".
 */
struct NumberExpressionAST final : ExpressionAST {
  explicit NumberExpressionAST(ParserAST &parser_ast, double value);
  llvm::Value *generate_IR_code() override;

  double value_;
  ParserAST &parser_ast_;
};

/**
 * Expression struct for referencing a variable, like "a".
 */
struct VariableExpressionAST final : ExpressionAST {
  explicit VariableExpressionAST(ParserAST &parser_ast, std::string name);
  llvm::Value *generate_IR_code() override;

  std::string name_;
  ParserAST &parser_ast_;
};

/**
 * Expression struct for a binary operator like "+".
 */
struct BinaryExpressionAST final : ExpressionAST {
  explicit BinaryExpressionAST(ParserAST &parser_ast, Token op,
                               std::unique_ptr<ExpressionAST> lhs,
                               std::unique_ptr<ExpressionAST> rhs);
  llvm::Value *generate_IR_code() override;

  ReservedToken operator_;
  std::unique_ptr<ExpressionAST> lhs_, rhs_;
  ParserAST &parser_ast_;
};

struct UnaryExpressionAST final : public ExpressionAST {
  UnaryExpressionAST(ParserAST &parser_ast, std::uint8_t operation_code,
                     std::unique_ptr<ExpressionAST> operand);

  llvm::Value *generate_IR_code() override;

  ParserAST &parser_ast_;
  std::uint8_t operation_code_;
  std::unique_ptr<ExpressionAST> operand_;
};

/**
 * Expression struct for function calls.
 */
struct CallExpressionAST final : ExpressionAST {
  explicit CallExpressionAST(
      ParserAST &parser_ast, std::string callee,
      std::vector<std::unique_ptr<ExpressionAST>> arguments);

  llvm::Value *generate_IR_code() override;

  std::string callee_;
  std::vector<std::unique_ptr<ExpressionAST>> arguments_;
  ParserAST &parser_ast_;
};

/**
 * This struct represents the "prototype" for a function, which captures its
 * name, and its argument names (thus implicitly the number of arguments the
 * function takes).
 */
struct FunctionPrototypeAST : IRCode {
  FunctionPrototypeAST(ParserAST &parser_ast, std::string name,
                       std::vector<std::string> arguments_names,
                       bool is_operator = false,
                       std::uint8_t binary_operator_precedence = 0);

  // Note that llvm `Function` is a `Value` sub-class.
  llvm::Value *generate_IR_code() override;

  [[nodiscard]] bool is_unary_operator() const;
  [[nodiscard]] bool is_binary_operator() const;
  [[nodiscard]] char get_operator_name() const;
  [[nodiscard]] std::uint8_t get_binary_operator_precedence() const;

  std::string name_;
  std::vector<std::string> arguments_names_;
  bool is_operator_;
  std::uint8_t binary_operator_precedence_;
  ParserAST &parser_ast_;
};

/**
 * This struct represents a function definition itself.
 */
struct FunctionDefinitionAST : IRCode {
  FunctionDefinitionAST(ParserAST &parser_ast,
                        std::unique_ptr<FunctionPrototypeAST> prototype,
                        std::unique_ptr<ExpressionAST> body);

  llvm::Value *generate_IR_code() override;

  std::unique_ptr<FunctionPrototypeAST> prototype_;
  std::string prototype_name_{"none"};
  std::unique_ptr<ExpressionAST> body_;
  ParserAST &parser_ast_;
};

struct IfExpressionAST : public ExpressionAST {
  IfExpressionAST(ParserAST &parser_ast, std::unique_ptr<ExpressionAST> _if,
                  std::unique_ptr<ExpressionAST> _then,
                  std::unique_ptr<ExpressionAST> _else);

  llvm::Value *generate_IR_code() override;

  ParserAST &parser_ast_;
  std::unique_ptr<ExpressionAST> if_, then_, else_;
};

struct ForExpressionAST : public ExpressionAST {
  ForExpressionAST(ParserAST &parser_ast, std::string variable_name,
                   std::unique_ptr<ExpressionAST> start,
                   std::unique_ptr<ExpressionAST> end,
                   std::unique_ptr<ExpressionAST> step,
                   std::unique_ptr<ExpressionAST> body);

  llvm::Value *generate_IR_code() override;

  ParserAST &parser_ast_;
  std::string variable_name_;
  // start_ is variable value,
  // end_ is condition expression
  std::unique_ptr<ExpressionAST> start_, end_, step_, body_;
};

} // namespace toy

#endif // TOY_AST_EXPRESSIONS_HPP
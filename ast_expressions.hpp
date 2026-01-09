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

enum class ExpressionTypeAST : std::uint8_t {
  Unknown,
  NumberExpressionAST,
  VariableExpressionAST,
  VarExpressionAST,
  BinaryExpressionAST,
  UnaryExpressionAST,
  CallExpressionAST,
  FunctionPrototypeAST,
  FunctionDefinitionAST,
  IfExpressionAST,
  ForExpressionAST,
};

using IdExpressionAST = std::size_t;
inline IdExpressionAST InvalidIdExpressionAST{0};

/**
 *  Base struct for all expression nodes.
 */
struct ExpressionAST : IRCode {
  ExpressionAST();
  ~ExpressionAST() override = default;

  IdExpressionAST id_{0};
  IdExpressionAST parent_id_{0};
  ExpressionTypeAST type_{ExpressionTypeAST::Unknown};
};

/**
 * Expression struct for numeric literals like "1.0".
 */
struct NumberExpressionAST final : public ExpressionAST {
  explicit NumberExpressionAST(ParserAST &parser_ast, double value);
  llvm::Value *generate_IR_code() override;

  ParserAST &parser_ast_;
  double value_;
};

// todo: rename it as it refers more to the function parameters?
/**
 * Expression struct for referencing a variable, like "a".
 */
struct VariableExpressionAST final : ExpressionAST {
  explicit VariableExpressionAST(ParserAST &parser_ast, std::string name);
  llvm::Value *generate_IR_code() override;

  ParserAST &parser_ast_;
  std::string name_;
};

// todo: rename it VariableASTExpression
/**
 * Expression class for var/in
 */
struct VarExpressionAST final : ExpressionAST {
  VarExpressionAST(
      ParserAST &parser_ast,
      std::vector<std::pair<std::string, IdExpressionAST>> variables,
      IdExpressionAST body);
  llvm::Value *generate_IR_code() override;

  ParserAST &parser_ast_;
  std::vector<std::pair<std::string, IdExpressionAST>> variables_;
  IdExpressionAST body_;
};

/**
 * Expression struct for a binary operator like "+".
 */
struct BinaryExpressionAST final : ExpressionAST {
  explicit BinaryExpressionAST(ParserAST &parser_ast, Token op,
                               IdExpressionAST lhs, IdExpressionAST rhs);
  llvm::Value *generate_IR_code() override;

  ParserAST &parser_ast_;
  ReservedToken operator_;
  IdExpressionAST lhs_, rhs_;
};

struct UnaryExpressionAST final : ExpressionAST {
  UnaryExpressionAST(ParserAST &parser_ast, std::uint8_t operation_code,
                     IdExpressionAST operand);

  llvm::Value *generate_IR_code() override;

  ParserAST &parser_ast_;
  std::uint8_t operation_code_;
  IdExpressionAST operand_;
};

/**
 * Expression struct for function calls.
 */
struct CallExpressionAST final : ExpressionAST {
  explicit CallExpressionAST(ParserAST &parser_ast, std::string callee,
                             std::vector<IdExpressionAST> arguments);

  llvm::Value *generate_IR_code() override;

  ParserAST &parser_ast_;
  std::string callee_;
  std::vector<IdExpressionAST> arguments_;
};

/**
 * This struct represents the "prototype" for a function, which captures its
 * name, and its argument names (thus implicitly the number of arguments the
 * function takes).
 */
struct FunctionPrototypeAST : ExpressionAST {
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

  ParserAST &parser_ast_;
  std::string name_;
  std::vector<std::string> arguments_names_;
  bool is_operator_;
  std::uint8_t binary_operator_precedence_;
};

/**
 * This struct represents a function definition itself.
 */
struct FunctionDefinitionAST : ExpressionAST {
  FunctionDefinitionAST(ParserAST &parser_ast, IdExpressionAST prototype,
                        IdExpressionAST body);

  llvm::Value *generate_IR_code() override;

  ParserAST &parser_ast_;
  IdExpressionAST prototype_;
  std::string prototype_name_{"none"};
  IdExpressionAST body_;
};

struct IfExpressionAST : public ExpressionAST {
  IfExpressionAST(ParserAST &parser_ast, IdExpressionAST _if,
                  IdExpressionAST _then, IdExpressionAST _else);

  llvm::Value *generate_IR_code() override;

  ParserAST &parser_ast_;
  IdExpressionAST if_;
  IdExpressionAST then_;
  IdExpressionAST else_;
};

struct ForExpressionAST : public ExpressionAST {
  ForExpressionAST(ParserAST &parser_ast, std::string variable_name,
                   IdExpressionAST start, IdExpressionAST end,
                   IdExpressionAST step, IdExpressionAST body);

  llvm::Value *generate_IR_code() override;

  ParserAST &parser_ast_;
  std::string variable_name_;
  // start_ is variable value,
  // end_ is condition expression
  IdExpressionAST start_, end_, step_, body_;
};

struct ExpressionASTMap {
  ExpressionASTMap();
  static ExpressionASTMap &instance();
  [[nodiscard]] const std::unique_ptr<ExpressionAST> &get(std::size_t id) const;
  [[nodiscard]] IdExpressionAST
  store(std::unique_ptr<ExpressionAST> expression);
  void remove(std::size_t id);

  std::unordered_map<IdExpressionAST, std::unique_ptr<ExpressionAST>> ast_map_;
  std::size_t size_{1000};
};

#define GET_EXPRESSION_FROM_MAP(id) ExpressionASTMap::instance().get(id)
#define GENERATE_IR_CODE(id) GET_EXPRESSION_FROM_MAP(id)->generate_IR_code()
#define STORE_EXPRESSION_IN_MAP(expression)                                    \
  ExpressionASTMap::instance().store(std::move(expression))

} // namespace toy

#endif // TOY_AST_EXPRESSIONS_HPP
//
// Created by Nicolae Popescu on 03/12/2025.
//

#include "ast_expressions.hpp"
#include "ir_code_generator.hpp"

#include <utility>
#include <variant>

#include "ast_parser.hpp"

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>

namespace toy {

namespace {
auto next_expression_id() {
  static std::size_t id{0};
  return ++id;
}
} // namespace

using namespace llvm;
using namespace llvm::orc;

ExpressionAST::ExpressionAST() : id_{next_expression_id()} {}

NumberExpressionAST::NumberExpressionAST(ParserAST &parser_ast, double value)
    : parser_ast_{parser_ast}, value_{value} {
  type_ = ExpressionTypeAST::NumberExpressionAST;
}

Value *NumberExpressionAST::generate_IR_code() {
  return std::visit(IRCodeGenerator{parser_ast_}, ExpressionASTVariant{*this});
}

VariableExpressionAST::VariableExpressionAST(ParserAST &parser_ast,
                                             std::string name)
    : parser_ast_{parser_ast}, name_{std::move(name)} {
  type_ = ExpressionTypeAST::VariableExpressionAST;
}

Value *VariableExpressionAST::generate_IR_code() {
  return std::visit(IRCodeGenerator{parser_ast_}, ExpressionASTVariant{*this});
}

VarExpressionAST::VarExpressionAST(
    ParserAST &parser_ast,
    std::vector<std::pair<std::string, IdExpressionAST>> variables,
    IdExpressionAST body)
    : parser_ast_(parser_ast), variables_(std::move(variables)), body_(body) {
  type_ = ExpressionTypeAST::VariableExpressionAST;
}

Value *VarExpressionAST::generate_IR_code() {
  return std::visit(IRCodeGenerator{parser_ast_},
                    ExpressionASTVariant{std::move(*this)});
}

BinaryExpressionAST::BinaryExpressionAST(ParserAST &parser_ast, Token op,
                                         IdExpressionAST lhs,
                                         IdExpressionAST rhs)
    : parser_ast_{parser_ast}, operator_{op}, lhs_{lhs}, rhs_{rhs} {
  type_ = ExpressionTypeAST::BinaryExpressionAST;
}

Value *BinaryExpressionAST::generate_IR_code() {
  // todo: check if moving to the variant is robust, and does not leave anything
  // dangling?
  return std::visit(IRCodeGenerator{parser_ast_},
                    ExpressionASTVariant{std::move(*this)});
}

UnaryExpressionAST::UnaryExpressionAST(ParserAST &parser_ast,
                                       std::uint8_t operation_code,
                                       IdExpressionAST operand)
    : parser_ast_{parser_ast}, operation_code_{operation_code},
      operand_{operand} {
  type_ = ExpressionTypeAST::UnaryExpressionAST;
}

Value *UnaryExpressionAST::generate_IR_code() {
  return std::visit(IRCodeGenerator{parser_ast_},
                    ExpressionASTVariant{std::move(*this)});
}

CallExpressionAST::CallExpressionAST(ParserAST &parser_ast, std::string callee,
                                     std::vector<IdExpressionAST> arguments)
    : parser_ast_{parser_ast}, callee_{std::move(callee)},
      arguments_{std::move(arguments)} {
  type_ = ExpressionTypeAST::CallExpressionAST;
}

Value *CallExpressionAST::generate_IR_code() {
  return std::visit(IRCodeGenerator{parser_ast_},
                    ExpressionASTVariant{std::move(*this)});
}

FunctionPrototypeAST::FunctionPrototypeAST(
    ParserAST &parser_ast, std::string name,
    std::vector<std::string> arguments_names, bool is_operator,
    std::uint8_t binary_operator_precedence)
    : parser_ast_{parser_ast}, name_{std::move(name)},
      arguments_names_{std::move(arguments_names)}, is_operator_{is_operator},
      binary_operator_precedence_{binary_operator_precedence} {
  type_ = ExpressionTypeAST::FunctionPrototypeAST;
}

Value *FunctionPrototypeAST::generate_IR_code() {
  return std::visit(IRCodeGenerator{parser_ast_}, ExpressionASTVariant{*this});
}

bool FunctionPrototypeAST::is_unary_operator() const {
  return is_operator_ && arguments_names_.size() == 1;
}

bool FunctionPrototypeAST::is_binary_operator() const {
  return is_operator_ && arguments_names_.size() == 2;
}

char FunctionPrototypeAST::get_operator_name() const {
  assert(is_unary_operator() || is_binary_operator());
  return name_[name_.size() - 1];
}

std::uint8_t FunctionPrototypeAST::get_binary_operator_precedence() const {
  return binary_operator_precedence_;
}

FunctionDefinitionAST::FunctionDefinitionAST(ParserAST &parser_ast,
                                             IdExpressionAST prototype,
                                             IdExpressionAST body)
    : parser_ast_{parser_ast}, prototype_{prototype}, body_{body} {
  // set prototype name
  const auto &expression_ast = GET_EXPRESSION_FROM_MAP(prototype);
  assert(expression_ast->type_ == ExpressionTypeAST::FunctionPrototypeAST);
  const auto *prototype_ast =
      dynamic_cast<FunctionPrototypeAST *>(expression_ast.get());
  prototype_name_ = prototype_ast->name_;
  type_ = ExpressionTypeAST::FunctionDefinitionAST;
}

Value *FunctionDefinitionAST::generate_IR_code() {
  return std::visit(IRCodeGenerator{parser_ast_},
                    ExpressionASTVariant{std::move(*this)});
}

IfExpressionAST::IfExpressionAST(ParserAST &parser_ast, IdExpressionAST _if,
                                 IdExpressionAST _then, IdExpressionAST _else)
    : parser_ast_{parser_ast}, if_{_if}, then_{_then}, else_{_else} {
  type_ = ExpressionTypeAST::IfExpressionAST;
}

Value *IfExpressionAST::generate_IR_code() {
  return std::visit(IRCodeGenerator{parser_ast_},
                    ExpressionASTVariant{std::move(*this)});
}

ForExpressionAST::ForExpressionAST(ParserAST &parser_ast,
                                   std::string variable_name,
                                   IdExpressionAST start, IdExpressionAST end,
                                   IdExpressionAST step, IdExpressionAST body)
    : parser_ast_{parser_ast}, variable_name_{std::move(variable_name)},
      start_{start}, end_{end}, step_{step}, body_{body} {
  type_ = ExpressionTypeAST::ForExpressionAST;
}

Value *ForExpressionAST::generate_IR_code() {
  return std::visit(IRCodeGenerator{parser_ast_},
                    ExpressionASTVariant{std::move(*this)});
}

ExpressionASTMap::ExpressionASTMap() = default;

ExpressionASTMap &ExpressionASTMap::instance() {
  static ExpressionASTMap instance_;
  return instance_;
}

const std::unique_ptr<ExpressionAST> &
ExpressionASTMap::get(std::size_t id) const {
  const auto it = ast_map_.find(id);
  assert(it != ast_map_.end());
  return it->second;
}

IdExpressionAST
ExpressionASTMap::store(std::unique_ptr<ExpressionAST> expression) {
  const auto id = expression->id_;
  ast_map_[id] = std::move(expression);
  return id;
}

void ExpressionASTMap::remove(std::size_t id) {
  const auto it = ast_map_.find(id);
  assert(it != ast_map_.end());
  ast_map_.erase(it);
}

} // namespace toy

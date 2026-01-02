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
  static std::uint8_t id{0};
  return ++id;
}
} // namespace

using namespace llvm;
using namespace llvm::orc;

ExpressionAST::ExpressionAST() : id_{next_expression_id()} {}

NumberExpressionAST::NumberExpressionAST(ParserAST &parser_ast, double value)
    : value_{value}, parser_ast_{parser_ast} {}

Value *NumberExpressionAST::generate_IR_code() {
  return std::visit(IRCodeGenerator{parser_ast_}, ExpressionASTVariant{*this});
}

VariableExpressionAST::VariableExpressionAST(ParserAST &parser_ast,
                                             std::string name)
    : parser_ast_{parser_ast}, name_{std::move(name)} {}

Value *VariableExpressionAST::generate_IR_code() {
  return std::visit(IRCodeGenerator{parser_ast_}, ExpressionASTVariant{*this});
}

VarExpressionAST::VarExpressionAST(
    ParserAST &parser_ast,
    std::vector<std::pair<std::string, std::unique_ptr<ExpressionAST>>>
        variables,
    std::unique_ptr<ExpressionAST> body)
    : parser_ast_(parser_ast), variables_(std::move(variables)),
      body_(std::move(body)) {}

Value *VarExpressionAST::generate_IR_code() {
  return std::visit(IRCodeGenerator{parser_ast_},
                    ExpressionASTVariant{std::move(*this)});
}

BinaryExpressionAST::BinaryExpressionAST(ParserAST &parser_ast, Token op,
                                         std::unique_ptr<ExpressionAST> lhs,
                                         std::unique_ptr<ExpressionAST> rhs)
    : parser_ast_{parser_ast}, operator_{op}, lhs_{std::move(lhs)},
      rhs_{std::move(rhs)} {}
Value *BinaryExpressionAST::generate_IR_code() {
  // todo: check if moving to the variant is robust, and does not leave anything
  // dangling?
  return std::visit(IRCodeGenerator{parser_ast_},
                    ExpressionASTVariant{std::move(*this)});
}

UnaryExpressionAST::UnaryExpressionAST(ParserAST &parser_ast,
                                       std::uint8_t operation_code,
                                       std::unique_ptr<ExpressionAST> operand)
    : parser_ast_{parser_ast}, operation_code_{operation_code},
      operand_{std::move(operand)} {}

Value *UnaryExpressionAST::generate_IR_code() {
  return std::visit(IRCodeGenerator{parser_ast_},
                    ExpressionASTVariant{std::move(*this)});
}

CallExpressionAST::CallExpressionAST(
    ParserAST &parser_ast, std::string callee,
    std::vector<std::unique_ptr<ExpressionAST>> arguments)
    : parser_ast_{parser_ast}, callee_{std::move(callee)},
      arguments_{std::move(arguments)} {}

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
      binary_operator_precedence_{binary_operator_precedence} {}

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

FunctionDefinitionAST::FunctionDefinitionAST(
    ParserAST &parser_ast, std::unique_ptr<FunctionPrototypeAST> prototype,
    ExpressionAST *body)
    : parser_ast_{parser_ast}, prototype_{std::move(prototype)},
      prototype_name_(prototype_->name_), body_{body} {}

Value *FunctionDefinitionAST::generate_IR_code() {
  return std::visit(IRCodeGenerator{parser_ast_},
                    ExpressionASTVariant{std::move(*this)});
}

IfExpressionAST::IfExpressionAST(ParserAST &parser_ast,
                                 std::unique_ptr<ExpressionAST> _if,
                                 ExpressionAST &_then, ExpressionAST *_else,
                                 bool has_else)
    : parser_ast_{parser_ast}, if_{std::move(_if)}, then_{_then}, else_{_else},
      has_else_{has_else} {}

Value *IfExpressionAST::generate_IR_code() {
  return std::visit(IRCodeGenerator{parser_ast_},
                    ExpressionASTVariant{std::move(*this)});
}

ForExpressionAST::ForExpressionAST(ParserAST &parser_ast,
                                   std::string variable_name,
                                   std::unique_ptr<ExpressionAST> start,
                                   std::unique_ptr<ExpressionAST> end,
                                   std::unique_ptr<ExpressionAST> step,
                                   ExpressionAST &body)
    : parser_ast_{parser_ast}, variable_name_{std::move(variable_name)},
      start_{std::move(start)}, end_{std::move(end)}, step_{std::move(step)},
      body_{body} {}

Value *ForExpressionAST::generate_IR_code() {
  return std::visit(IRCodeGenerator{parser_ast_},
                    ExpressionASTVariant{std::move(*this)});
}

ExpressionASTPool::ExpressionASTPool() { pool_.reserve(size_); }

ExpressionASTPool &ExpressionASTPool::instance() {
  static ExpressionASTPool instance_;
  return instance_;
}

const std::unique_ptr<ExpressionAST> &
ExpressionASTPool::get(std::size_t id) const {
  auto it = std::ranges::find_if(
      pool_, [id](const auto &item) { return item->id_ == id; });

  assert(it != pool_.end());

  return *it;
}

} // namespace toy

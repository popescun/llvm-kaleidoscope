//
// Created by Nicolae Popescu on 03/12/2025.
//

#ifndef TOY_IR_CODE_GENERATOR_HPP
#define TOY_IR_CODE_GENERATOR_HPP

#include "ast_parser.hpp"

#include <llvm/IR/Value.h>

#include <variant>

namespace toy {

using ExpressionASTVariant =
    std::variant<NumberExpressionAST, VariableExpressionAST,
                 BinaryExpressionAST, UnaryExpressionAST, CallExpressionAST,
                 FunctionPrototypeAST, FunctionDefinitionAST, IfExpressionAST,
                 ForExpressionAST>;

/**
 * IR code generator as function object.
 *
 * This Functor may be used in the visitor pattern along
 *  with std::variant & std::visit.
 *
 */
struct IRCodeGenerator {
  ParserAST &parser_ast_;

  explicit IRCodeGenerator(ParserAST &parser_ast);

  llvm::Value *operator()(const NumberExpressionAST &expression) const;
  llvm::Value *operator()(const VariableExpressionAST &expression) const;
  llvm::Value *operator()(const BinaryExpressionAST &expression) const;
  llvm::Value *operator()(const UnaryExpressionAST &expression) const;
  llvm::Value *operator()(const CallExpressionAST &expression) const;
  llvm::Value *operator()(const FunctionPrototypeAST &expression) const;
  llvm::Value *operator()(const FunctionDefinitionAST &expression) const;
  llvm::Value *operator()(const IfExpressionAST &expression) const;
  llvm::Value *operator()(const ForExpressionAST &expression) const;
};

} // namespace toy

#endif // TOY_IR_CODE_GENERATOR_HPP
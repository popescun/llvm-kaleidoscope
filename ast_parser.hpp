//
// Created by Nicolae Popescu on 11/11/2025.
//

#ifndef TOY_AST_PARSER_HPP
#define TOY_AST_PARSER_HPP

#include "ast_expressions.hpp"
#include "lexer.hpp"

#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/Analysis/LoopAnalysisManager.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassInstrumentation.h>
#include <llvm/IR/PassManager.h>
#include <llvm/IR/Value.h>
#include <llvm/Passes/StandardInstrumentations.h>

#include <map>
#include <memory>
#include <string>

/**
 * Abstract Syntax Tree (aka Parse Tree).
 */

namespace toy {

struct Jit;

/**
 * AST parser as single-tone.
 */
struct ParserAST {
  /**
   * Constructor.
   */
  explicit ParserAST(Jit &jit);

  void init();

  /**
   * Parse a number expression with syntax:
   *   numberexpr ::= number
   *
   * @return a `NumberExpressionAST`
   */
  std::unique_ptr<ExpressionAST> parse_number_expression();

  /**
   * Parse a parenthesis expression with syntax:
   *  parenexpr ::= '(' expression ')'
   *
   * @return an ExpressionAST`
   */
  std::unique_ptr<ExpressionAST> parse_parenthesis_expression();

  /**
   * Parse an identifier expression with syntax:
   *  identifierexpr
   *    ::= identifier // simple variable reference
   *    ::= identifier '(' expression* ')' // a function call
   *
   * @return an ExpressionAST`
   */
  std::unique_ptr<ExpressionAST> parse_identifier_expression();

  /**
   * Parse a primary expression.
   *
   * @return an ExpressionAST`
   */
  std::unique_ptr<ExpressionAST> parse_primary_expression();

  /**
   * Parse binary operation right hand side with syntax:
   *  binoprhs ::= ('+' unary)* // * means recursion
   *
   * @return an ExpressionAST`
   */
  std::unique_ptr<ExpressionAST>
  parse_binary_operation_rhs(Token expression_precedence,
                             std::unique_ptr<ExpressionAST> lhs);

  /**
   * Parse unary expression with syntax:
   *  unary
   *    ::= primary
   *    ::= '!' unary
   *
   * @return an ExpressionAST`
   */
  std::unique_ptr<ExpressionAST> parse_unary_expression();

  /**
   * Parse an expression with syntax:
   *    expression ::= unary binoprhs
   *
   * @return an ExpressionAST
   */
  std::unique_ptr<ExpressionAST> parse_expression();

  /**
   * Parse a function prototype with syntax:
   *    prototype ::= id '(' id* ')'
   *              ::= binary LETTER number? (id, id)
   *              ::= unary LETTER (id)
   *
   * @return a FunctionPrototypeAST
   */
  std::unique_ptr<FunctionPrototypeAST> parse_function_prototype();

  /**
   * Parse a function definition with syntax:
   *    definition ::= 'def' prototype expression
   *
   * @return a FunctionDefinitionAST
   */
  std::unique_ptr<FunctionDefinitionAST> parse_function_definition();

  /**
   * Parse top level expression with syntax:
   *    toplevelexpr ::= expression
   *
   * @return a FunctionDefinitionAST
   */
  std::unique_ptr<FunctionDefinitionAST> parse_top_level_expression();

  /**
   * Parse if expression with syntax:
   *    ifexpr ::= 'if' expression 'then' expression 'else' expression
   *
   * @return a ExpressionAST
   */
  std::unique_ptr<ExpressionAST> parse_if_expression();

  /**
   * Parse for expression with syntax:
   *    forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in'
   * expression
   *
   * @return a ExpressionAST
   */
  std::unique_ptr<ExpressionAST> parse_for_expression();

  /**
   * Parse external n with syntax:
   *    external ::= 'extern' prototype
   *
   * @return a FunctionPrototypeAST
   */
  std::unique_ptr<FunctionPrototypeAST> parse_external();

  [[nodiscard]] llvm::Function *get_function(const std::string &name);

  void handle_function_definition();
  void handle_extern();
  void handle_top_level_expression();

  /**
   * Handle input expressions with syntax:
   *  top ::= definition | external | expression | ';'
   */
  void run();

  Lexer lexer_;
  std::unique_ptr<llvm::LLVMContext> llvm_context_;
  std::unique_ptr<llvm::IRBuilder<>> llvm_IR_builder_;
  std::unique_ptr<llvm::Module> llvm_module_;
  std::map<std::string, llvm::Value *> variable_names_;
  std::map<std::string, std::unique_ptr<FunctionPrototypeAST>>
      function_prototypes_;

  std::unique_ptr<llvm::FunctionPassManager> function_pass_manager_;
  std::unique_ptr<llvm::LoopAnalysisManager> loop_analysis_manager_;
  std::unique_ptr<llvm::FunctionAnalysisManager> function_analysis_manager_;
  std::unique_ptr<llvm::CGSCCAnalysisManager> cgscc_analysis_manager_;
  std::unique_ptr<llvm::ModuleAnalysisManager> module_analysis_manager_;
  std::unique_ptr<llvm::PassInstrumentationCallbacks>
      pass_instrumentation_callbacks_;
  std::unique_ptr<llvm::StandardInstrumentations> standard_instrumentations_;
  Jit &jit_;
};

} // namespace toy

#endif // TOY_AST_PARSER_HPP
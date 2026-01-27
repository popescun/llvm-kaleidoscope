//
// Created by Nicolae Popescu on 11/11/2025.
//

#ifndef TOY_AST_PARSER_HPP
#define TOY_AST_PARSER_HPP

#include "ast_expressions.hpp"
#include "lexer.hpp"

#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/Analysis/LoopAnalysisManager.h>
#include <llvm/ExecutionEngine/Orc/Core.h>
#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassInstrumentation.h>
#include <llvm/IR/PassManager.h>
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
   * Default constructor;
   */
  explicit ParserAST();

  /**
   * Constructor with Jit.
   */
  explicit ParserAST(Jit *jit);

  void init_common();

  /**
   * Initialize llvm for running as JIT engine.
   */
  void init_jit();

  /**
   * Handle input expressions with syntax:
   *  top ::= definition | external | expression | ';'
   */
  ParserAST &run();

  /**
   * Initialize llvm to compile the code to an object file.
   */
  ParserAST &compile();

  /**
   * Initialize llvm to create a debugging executable.
   */
  ParserAST &debug();

  /**
   * Parse a number expression with syntax:
   *   numberexpr ::= number
   *
   * @return expression id
   */
  IdExpressionAST parse_number_expression();

  /**
   * Parse a parenthesis expression with syntax:
   *  parenexpr ::= '(' expression ')'
   *
   * @return expression id
   */
  IdExpressionAST parse_parenthesis_expression();

  /**
   * Parse an identifier expression with syntax:
   *  identifierexpr
   *    ::= identifier // simple variable reference
   *    ::= identifier '(' expression* ')' // a function call
   *
   * @return expression id
   */
  IdExpressionAST parse_identifier_expression();

  /**
   * Parse a primary expression.
   *
   * @return expression id
   */
  IdExpressionAST parse_primary_expression();

  /**
   * Parse binary operation right hand side with syntax:
   *  binoprhs ::= ('+' unary)* // * means recursion
   *
   * @return expression id
   */
  IdExpressionAST parse_binary_operation_rhs(Token expression_precedence,
                                             IdExpressionAST lhs);

  /**
   * Parse unary expression with syntax:
   *  unary
   *    ::= primary
   *    ::= '!' unary
   *
   * @return expression id
   */
  IdExpressionAST parse_unary_expression();

  /**
   * Parse an expression with syntax:
   *    expression ::= unary binoprhs
   *
   * @return expression id
   */
  IdExpressionAST parse_expression();

  /**
   * Parse a function prototype with syntax:
   *    prototype ::= id '(' id* ')'
   *              ::= binary LETTER number? (id, id)
   *              ::= unary LETTER (id)
   *
   * @return expression id
   */
  IdExpressionAST parse_function_prototype();

  /**
   * Parse a function definition with ysntax:
   *    definition ::= 'def' prototype expression
   *
   * @return expression id
   */
  IdExpressionAST parse_function_definition();

  /**
   * Parse top level expression with syntax:
   *    toplevelexpr ::= expression
   *
   * @return expression id
   */
  IdExpressionAST parse_top_level_expression();

  /**
   * Parse if expression with syntax:
   *    ifexpr ::= 'if' expression 'then' expression 'else' expression
   *
   * @return expression id
   */
  IdExpressionAST parse_if_expression();

  /**
   * Parse for expression with syntax:
   *    forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in'
   * expression
   *
   * @return expression id
   */
  IdExpressionAST parse_for_expression();

  /**
   * Parse var expression with syntax:
   *    varexpr ::= 'var' identifier ('=' expression)?
   *                      (',' identifier ('=' expression)?)* 'in' expression
   *
   * @return expression id
   */
  IdExpressionAST parse_var_expression();

  /**
   * Parse external n with syntax:
   *    external ::= 'extern' prototype
   *
   * @return expression id
   */
  IdExpressionAST parse_external();

  void handle_function_definition();
  void handle_extern();
  void handle_top_level_expression();
  llvm::orc::ResourceTrackerSP add_module();
  bool is_debug_mode() const;

  Lexer lexer_;
  std::unique_ptr<llvm::LLVMContext> llvm_context_;
  std::unique_ptr<llvm::DIBuilder> llvm_DI_builder_;
  std::unique_ptr<llvm::IRBuilder<>> llvm_IR_builder_;
  std::unique_ptr<llvm::Module> llvm_module_;
  std::map<std::string, IdExpressionAST> function_prototypes_;

  std::unique_ptr<llvm::FunctionPassManager> function_pass_manager_;
  std::unique_ptr<llvm::LoopAnalysisManager> loop_analysis_manager_;
  std::unique_ptr<llvm::FunctionAnalysisManager> function_analysis_manager_;
  std::unique_ptr<llvm::CGSCCAnalysisManager> cgscc_analysis_manager_;
  std::unique_ptr<llvm::ModuleAnalysisManager> module_analysis_manager_;
  std::unique_ptr<llvm::PassInstrumentationCallbacks>
      pass_instrumentation_callbacks_;
  std::unique_ptr<llvm::StandardInstrumentations> standard_instrumentations_;
  Jit *jit_;

  struct DebugInfo {
    llvm::DICompileUnit *compile_unit_{nullptr};
    llvm::DIType *double_type_{nullptr};
    ParserAST &parser_ast_;
    std::vector<llvm::DIScope *> lexical_blocks_;

    explicit DebugInfo(ParserAST &parser_ast) : parser_ast_{parser_ast} {}

    llvm::DIType *get_double_type() {
      if (double_type_) {
        return double_type_;
      }
      double_type_ = parser_ast_.llvm_DI_builder_->createBasicType(
          "double", 64, llvm::dwarf::DW_ATE_float);
      return double_type_;
    }

    void emit_location(ExpressionAST *expression) const {
      if (!expression) {
        return parser_ast_.llvm_IR_builder_->SetCurrentDebugLocation(
            llvm::DebugLoc{});
      }
      llvm::DIScope *di_scope =
          lexical_blocks_.empty() ? compile_unit_ : lexical_blocks_.back();
      parser_ast_.llvm_IR_builder_->SetCurrentDebugLocation(
          llvm::DILocation::get(di_scope->getContext(), expression->get_line(),
                                expression->get_col(), di_scope));
    }

    llvm::DISubroutineType *create_function_type(unsigned args_no) {
      llvm::SmallVector<llvm::Metadata *, 8> elt_tys;
      llvm::DIType *double_type = get_double_type();

      // add the result type
      elt_tys.push_back(double_type);

      // add the arguments type
      for (unsigned i = 0; i < args_no; ++i) {
        elt_tys.push_back(double_type);
      }

      return parser_ast_.llvm_DI_builder_->createSubroutineType(
          parser_ast_.llvm_DI_builder_->getOrCreateTypeArray(elt_tys));
    }
  };

  std::unique_ptr<DebugInfo> debug_info_;
};

} // namespace toy

#endif // TOY_AST_PARSER_HPP
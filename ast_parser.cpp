//
// Created by Nicolae Popescu on 11/11/2025.
//

#include <functional>

#include "ast_expressions.hpp"
#include "jit.hpp"

#include <string>
#include <utility>
#include <vector>

#include <llvm/Passes/PassBuilder.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Scalar/Reassociate.h>
#include <llvm/Transforms/Scalar/SimplifyCFG.h>
#include <llvm/Transforms/Utils/Mem2Reg.h>

#include "ir_code_generator.hpp"
#include "utils.hpp"

namespace toy {

using namespace llvm;
using namespace llvm::orc;

namespace {

ExitOnError ExitOnErr;

constexpr auto kAnonymousExpression{"__anon_expr"};

} // namespace

using Token = Token;

// ParserAST definition

ParserAST::ParserAST(Jit &jit) : jit_{jit} { init(); }

void ParserAST::init() {
  llvm_context_ = std::make_unique<LLVMContext>();
  llvm_module_ =
      std::make_unique<Module>(std::string("ToyJIT"), *llvm_context_);
  llvm_module_->setDataLayout(jit_.data_layout_);

  llvm_IR_builder_ = std::make_unique<IRBuilder<>>(*llvm_context_);

  function_pass_manager_ = std::make_unique<FunctionPassManager>();
  loop_analysis_manager_ = std::make_unique<LoopAnalysisManager>();
  function_analysis_manager_ = std::make_unique<FunctionAnalysisManager>();
  cgscc_analysis_manager_ = std::make_unique<CGSCCAnalysisManager>();
  module_analysis_manager_ = std::make_unique<ModuleAnalysisManager>();
  pass_instrumentation_callbacks_ =
      std::make_unique<PassInstrumentationCallbacks>();
  standard_instrumentations_ =
      std::make_unique<StandardInstrumentations>(*llvm_context_, true);

  standard_instrumentations_->registerCallbacks(
      *pass_instrumentation_callbacks_, module_analysis_manager_.get());

  // promote allocas to registers
  function_pass_manager_->addPass(PromotePass());
  // do simple "peephole" and bit-twiddling optimizations
  function_pass_manager_->addPass(InstCombinePass());
  // reassociate expression
  function_pass_manager_->addPass(ReassociatePass());
  // eliminate common sub-expression
  function_pass_manager_->addPass(GVNPass());
  // simplify the control flow graph (deleting unreachable blocks, etc).
  function_pass_manager_->addPass(SimplifyCFGPass());

  PassBuilder pass_builder;
  pass_builder.registerModuleAnalyses(*module_analysis_manager_);
  pass_builder.registerFunctionAnalyses(*function_analysis_manager_);
  pass_builder.crossRegisterProxies(
      *loop_analysis_manager_, *function_analysis_manager_,
      *cgscc_analysis_manager_, *module_analysis_manager_);
}

std::unique_ptr<ExpressionAST> ParserAST::parse_number_expression() {
  auto result =
      std::make_unique<NumberExpressionAST>(*this, lexer_.number_value_);
  lexer_.next_token();
  return std::move(result);
}

std::unique_ptr<ExpressionAST> ParserAST::parse_parenthesis_expression() {
  // eat opening '('
  lexer_.next_token();
  auto expression = parse_expression();
  if (!expression) {
    return {};
  }

  if (lexer_.current_token_ != ')') {
    log_error("expected ')'", lexer_.row_, lexer_.col_);
    return {};
  }
  // eat ending ')'
  lexer_.next_token();
  return expression;
}

std::unique_ptr<ExpressionAST> ParserAST::parse_identifier_expression() {
  auto id_name = lexer_.identifier_;
  // eat identifier
  lexer_.next_token();

  // simple variable reference
  if (lexer_.current_token_ !=
      Lexer::to_token(ReservedToken::token_leading_parenthesis)) {
    return std::make_unique<VariableExpressionAST>(*this, id_name);
  }

  // function call
  // eat opening '('
  lexer_.next_token();
  std::vector<std::unique_ptr<ExpressionAST>> arguments;
  if (lexer_.current_token_ !=
      Lexer::to_token(ReservedToken::token_trailing_parenthesis)) {
    while (true) {
      if (auto argument = parse_expression()) {
        arguments.push_back(std::move(argument));
      } else {
        return {};
      }

      if (lexer_.current_token_ ==
          Lexer::to_token(ReservedToken::token_trailing_parenthesis)) {
        break;
      }

      if (lexer_.current_token_ !=
          Lexer::to_token(ReservedToken::token_comma)) {
        log_error("expected ')' or ',' in argument list", lexer_.row_,
                  lexer_.col_);
        return {};
      }
      lexer_.next_token();
    }
  }

  // eat ending ')'
  lexer_.next_token();

  return std::make_unique<CallExpressionAST>(*this, id_name,
                                             std::move(arguments));
}

/**
 * primary
 *   ::= identifierexpr
 *   ::= numberexpr
 *   ::= parenexpr
 *   ::= ifexpr
 *   ::= forexpr
 *   ::= varexpr
 */
std::unique_ptr<ExpressionAST> ParserAST::parse_primary_expression() {
  switch (Lexer::to_reserved_token(lexer_.current_token_)) {
  case ReservedToken::token_identifier:
    return parse_identifier_expression();
  case ReservedToken::token_number:
    return parse_number_expression();
  case ReservedToken::token_leading_parenthesis:
    return parse_parenthesis_expression();
  case ReservedToken::token_if:
    return parse_if_expression();
  case ReservedToken::token_for:
    return parse_for_expression();
  case ReservedToken::token_var:
    return parse_var_expression();
  default:
    log_error("unknown token when expecting an expression", lexer_.row_,
              lexer_.col_);
    return {};
  }
}

std::unique_ptr<ExpressionAST>
ParserAST::parse_binary_operation_rhs(Token expression_precedence,
                                      std::unique_ptr<ExpressionAST> lhs) {
  // if this is a binary operation, find its precedence
  while (true) {
    const auto token_precedence = lexer_.get_current_token_precedence();

    // if this is a binary operator that binds at least tightly as the current
    // binary operator, consume it, otherwise we are done.
    if (token_precedence < expression_precedence) {
      return lhs;
    }

    // alright, we know this is a binary operator
    auto binary_operator = lexer_.current_token_;
    // eat binary operator
    lexer_.next_token();

    // parse the unary expression after the binary operator
    auto rhs = parse_unary_expression();
    if (!rhs) {
      return {};
    }

    const auto next_precedence = lexer_.get_current_token_precedence();

    // if binary operation binds less tightly with rhs than the operator after
    // rhs, let the pending operator take rhs as its lhs.
    if (token_precedence < next_precedence) {
      rhs = parse_binary_operation_rhs(static_cast<Token>(token_precedence + 1),
                                       std::move(rhs));
      if (!rhs) {
        return {};
      }
    }

    lhs = std::make_unique<BinaryExpressionAST>(*this, binary_operator,
                                                std::move(lhs), std::move(rhs));
  }
}

std::unique_ptr<ExpressionAST> ParserAST::parse_unary_expression() {
  // if the current token is not an operator, it must be ap primary expression
  if (!isascii(lexer_.current_token_) ||
      lexer_.current_token_ ==
          Lexer::to_token(ReservedToken::token_leading_parenthesis) ||
      // todo: checking comma token might be wrong as it is used in function
      // prototype as well?
      lexer_.current_token_ == Lexer::to_token(ReservedToken::token_comma)) {
    return parse_primary_expression();
  }

  // if this is a unary operator, read it
  auto operation_code = lexer_.current_token_;
  // eat the operation code
  lexer_.next_token();
  if (auto operand = parse_unary_expression()) {
    return std::make_unique<UnaryExpressionAST>(*this, operation_code,
                                                std::move(operand));
  }

  return {};
}

std::unique_ptr<ExpressionAST> ParserAST::parse_expression() {
  auto lhs = parse_unary_expression();
  if (!lhs) {
    return {};
  }
  return parse_binary_operation_rhs(0, std::move(lhs));
}

std::unique_ptr<FunctionPrototypeAST> ParserAST::parse_function_prototype() {
  std::string function_name;
  std::uint8_t type{0}; // 0 = identifier, 1 = unary, 2 = binary
  std::uint8_t binary_operator_precedence{30};

  switch (Lexer::to_reserved_token(lexer_.current_token_)) {
  case ReservedToken::token_identifier:
    function_name = lexer_.identifier_;
    type = 0;
    // eat identifier
    lexer_.next_token();
    break;
  case ReservedToken::token_unary:
    // eat "unary"
    lexer_.next_token();
    if (!isascii(lexer_.current_token_)) {
      log_error_prototype("expected unary operator", lexer_.row_, lexer_.col_);
      return {};
    }
    function_name = keyword_token_unary;
    function_name += static_cast<char>(lexer_.current_token_);
    type = 1;
    // eat operator
    lexer_.next_token();
    break;
  case ReservedToken::token_binary:
    // eat "binary"
    lexer_.next_token();
    if (!isascii(lexer_.current_token_)) {
      log_error_prototype("expected binary operator", lexer_.row_, lexer_.col_);
      return {};
    }
    function_name = keyword_token_binary;
    function_name += static_cast<char>(lexer_.current_token_);
    type = 2;
    // eat binary operator
    lexer_.next_token();

    // read the precedence if present
    if (lexer_.current_token_ == Lexer::to_token(ReservedToken::token_number)) {
      if (lexer_.number_value_ <= lexer_.binary_op_precedence_['m'] ||
          lexer_.number_value_ >= lexer_.binary_op_precedence_['M']) {
        log_error_prototype("invalid precedence: must be 1..100", lexer_.row_,
                            lexer_.col_);
        return {};
      }
      binary_operator_precedence =
          static_cast<std::uint8_t>(lexer_.number_value_);
      // eat the precedence number
      lexer_.next_token();
    }
    break;
  default:
    log_error_prototype("expected function name in prototype", lexer_.row_,
                        lexer_.col_);
    return {};
  }

  if (lexer_.current_token_ !=
      Lexer::to_token(ReservedToken::token_leading_parenthesis)) {
    log_error_prototype("expected '(' in prototype", lexer_.row_, lexer_.col_);
    return {};
  }

  // eat leading '('
  lexer_.next_token();

  std::vector<std::string> arguments_names;
  if (lexer_.current_token_ !=
      Lexer::to_token(ReservedToken::token_trailing_parenthesis)) {
    while (true) {
      if (auto expression = parse_identifier_expression()) {
        const auto *variable =
            dynamic_cast<VariableExpressionAST *>(expression.get());
        if (!variable) {
          log_error(" ExpressionAST to VariableExpressionAST cast failed",
                    lexer_.row_, lexer_.col_);
          return {};
        }
        arguments_names.push_back(variable->name_);
      } else {
        return {};
      }

      if (lexer_.current_token_ ==
          Lexer::to_token(ReservedToken::token_trailing_parenthesis)) {
        break;
      }

      if (lexer_.current_token_ !=
          Lexer::to_token(ReservedToken::token_comma)) {
        log_error("expected ')' or ',' in argument list", lexer_.row_,
                  lexer_.col_);
        return {};
      }
      lexer_.next_token();
    }
  }

  // eat ending ')'
  lexer_.next_token();

  // verify right number of operands for operator
  if (type && arguments_names.size() != type) {
    log_error_prototype("invalid number of operands for operator", lexer_.row_,
                        lexer_.col_);
    return {};
  }

  // todo: function_name is first copied into constructor and then moved
  // should we move all along? Then last identifier_ is lost in lexer.
  return std::make_unique<FunctionPrototypeAST>(
      *this, function_name, std::move(arguments_names), type != 0,
      binary_operator_precedence);
}

std::unique_ptr<FunctionDefinitionAST> ParserAST::parse_function_definition() {
  // eat 'def'
  lexer_.next_token();

  // parse function prototype
  auto function_prototype = parse_function_prototype();
  if (!function_prototype) {
    return {};
  }

  // building block
  if (auto expression = parse_expression()) {
    return std::make_unique<FunctionDefinitionAST>(
        *this, std::move(function_prototype), std::move(expression));
  }

  return {};
}

std::unique_ptr<FunctionDefinitionAST> ParserAST::parse_top_level_expression() {
  if (auto expression = parse_expression()) {
    // make a function prototype with anonymous name
    auto function_prototype = std::make_unique<FunctionPrototypeAST>(
        *this, kAnonymousExpression, std::vector<std::string>());
    return std::make_unique<FunctionDefinitionAST>(
        *this, std::move(function_prototype), std::move(expression));
  }
  return {};
}

std::unique_ptr<ExpressionAST> ParserAST::parse_if_expression() {
  // eat `if` token
  lexer_.next_token();

  // condition
  auto if_condition = parse_expression();
  if (!if_condition) {
    return {};
  }

  if (lexer_.current_token_ != Lexer::to_token(ReservedToken::token_then)) {
    log_error("expected `then` expression in `if..then..else` expression",
              lexer_.row_, lexer_.col_);
    return {};
  }

  // eat `then`
  lexer_.next_token();

  auto then_expression = parse_expression();
  if (!then_expression) {
    return {};
  }

  if (lexer_.current_token_ != Lexer::to_token(ReservedToken::token_else)) {
    log_error("expected `else` expression in `if..then..else` expression",
              lexer_.row_, lexer_.col_);
    return {};
  }

  // eat `else`
  lexer_.next_token();

  auto else_expression = parse_expression();
  if (!else_expression) {
    return {};
  }

  return std::make_unique<IfExpressionAST>(*this, std::move(if_condition),
                                           std::move(then_expression),
                                           std::move(else_expression));
}

std::unique_ptr<ExpressionAST> ParserAST::parse_for_expression() {
  // eat for
  lexer_.next_token();

  if (lexer_.current_token_ !=
      Lexer::to_token(ReservedToken::token_identifier)) {
    log_error("expected identifier after for", lexer_.row_, lexer_.col_);
    return {};
  }

  auto variable_name = lexer_.identifier_;

  // eat identifier
  lexer_.next_token();

  if (lexer_.current_token_ !=
      Lexer::to_token(ReservedToken::token_operator_assignment)) {
    log_error("expected '=' after for", lexer_.row_, lexer_.col_);
    return {};
  }

  // eat =
  lexer_.next_token();

  auto start_expression = parse_expression();
  if (!start_expression) {
    return {};
  }

  if (lexer_.current_token_ != Lexer::to_token(ReservedToken::token_comma)) {
    log_error("expected `,` after for start expression", lexer_.row_,
              lexer_.col_);
    return {};
  }

  // eat comma
  lexer_.next_token();

  auto end_expression = parse_expression();
  if (!end_expression) {
    return {};
  }

  // the step value is optional
  std::unique_ptr<ExpressionAST> step_expression;
  if (lexer_.current_token_ == Lexer::to_token(ReservedToken::token_comma)) {
    // eat comma before for step
    lexer_.next_token();
    step_expression = parse_expression();
    if (!step_expression) {
      return {};
    }
  }

  if (lexer_.current_token_ != Lexer::to_token(ReservedToken::token_in)) {
    log_error("expected in after for expression", lexer_.row_, lexer_.col_);
    return {};
  }

  // eat in
  lexer_.next_token();

  auto body_expression = parse_expression();
  if (!body_expression) {
    return {};
  }

  // todo: here variable_name is copied to constructor and then moved
  return std::make_unique<ForExpressionAST>(
      *this, variable_name, std::move(start_expression),
      std::move(end_expression), std::move(step_expression),
      std::move(body_expression));
}

std::unique_ptr<ExpressionAST> ParserAST::parse_var_expression() {
  // eat `var`
  lexer_.next_token();

  std::vector<std::pair<std::string, std::unique_ptr<ExpressionAST>>> variables;

  // at least one variable name is required
  if (lexer_.current_token_ !=
      Lexer::to_token(ReservedToken::token_identifier)) {
    log_error("expected identifier after `var`", lexer_.row_, lexer_.col_);
    return {};
  }

  while (true) {
    const auto name = lexer_.identifier_;
    // east identifier
    lexer_.next_token();

    // read the optional initializer
    std::unique_ptr<ExpressionAST> initializer;
    if (lexer_.current_token_ ==
        Lexer::to_token(ReservedToken::token_operator_assignment)) {
      // eat `=`
      lexer_.next_token();

      initializer = parse_expression();
      if (!initializer) {
        return {};
      }
    }

    variables.emplace_back(name, std::move(initializer));

    // end of var list, exit loop
    if (lexer_.current_token_ != Lexer::to_token(ReservedToken::token_comma)) {
      break;
    }
    // eat `,`
    lexer_.next_token();

    if (lexer_.current_token_ !=
        Lexer::to_token(ReservedToken::token_identifier)) {
      log_error("expected identifier after `,` in `var` identifier list",
                lexer_.row_, lexer_.col_);
      return {};
    }
  }

  // at this point, we have to have `in`
  if (lexer_.current_token_ != Lexer::to_token(ReservedToken::token_in)) {
    log_error("expected `in` keyword after `var` identifier list", lexer_.row_,
              lexer_.col_);
    return {};
  }
  // eat `in`
  lexer_.next_token();

  auto body = parse_expression();
  if (!body) {
    return {};
  }

  return std::make_unique<VarExpressionAST>(*this, std::move(variables),
                                            std::move(body));
}

std::unique_ptr<FunctionPrototypeAST> ParserAST::parse_external() {
  // eat extern
  lexer_.next_token();
  return parse_function_prototype();
}

void ParserAST::handle_function_definition() {
  if (const auto function_definition = parse_function_definition()) {
    // todo: duplicate code, extract it to a function
    // first, transfer ownership of the prototype to function prototypes map,
    // but keep a reference to it for use bellow
    function_prototypes_[function_definition->prototype_name_] =
        std::move(function_definition->prototype_);
    if (const auto *ir_code = function_definition->generate_IR_code()) {
      fprintf(stderr, "parsed a function definition\n");
      ir_code->print(errs());
      fprintf(stderr, "\n");

      // warning, this is an expected error:
      //   In ToyJIT, duplicate definition of symbol '_test'
      // see
      // https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl04.html
      // :
      //   "Duplication of symbols in separate modules is not allowed since
      //   LLVM-9"
      // ExitOnErr(jit_.addModule(
      //     ThreadSafeModule(std::move(llvm_module_),
      //     std::move(llvm_context_)), resource_tracker));

      (void)add_module();
    }
  }
  // else {
  //   // skip token for error recovery
  //   lexer_.next_token();
  // }
}

void ParserAST::handle_extern() {
  if (auto function_prototype = parse_external()) {
    if (const auto *ir_code = function_prototype->generate_IR_code()) {
      fprintf(stderr, "parsed an external function\n");
      ir_code->print(errs());
      fprintf(stderr, "\n");
      function_prototypes_[function_prototype->name_] =
          std::move(function_prototype);
    }
  } else {
    // skip token for error recovery
    lexer_.next_token();
  }
}

void ParserAST::handle_top_level_expression() {
  // evaluate a top-level expression into anonymous function
  if (const auto function_definition = parse_top_level_expression()) {
    // first, transfer ownership of the prototype to function prototypes map,
    // but keep a reference to it for use bellow
    function_prototypes_[function_definition->prototype_name_] =
        std::move(function_definition->prototype_);
    if (const auto ir_code = function_definition->generate_IR_code()) {
      fprintf(stderr, "parsed a top level expression\n");
      ir_code->print(errs());
      fprintf(stderr, "\n");

      auto resource_tracker = add_module();

      // search the Jit for __anon_expr symbol
      auto expected_symbol = jit_.lookup(kAnonymousExpression);
      if (!expected_symbol) {
        log_error("anonymous symbol not found", lexer_.row_, lexer_.col_);
        auto expected_error = expected_symbol.takeError();
        log_error(toString(std::move(expected_error)).c_str(), lexer_.row_,
                  lexer_.col_);
      } else {
        // get the symbol's address and cast it to the right type (takes no
        // arguments, returns a double) so we can call it as a native function.
        double (*function_pointer)() =
            expected_symbol->getAddress().toPtr<double (*)()>();
        fprintf(stderr, "evaluated to %f\n", function_pointer());
      }

      // delete the module containing anonymous expression from Jit
      ExitOnErr(resource_tracker->remove());
      //
      // // todo: fix, this removes the IR code, so is not printed on `dump()`
      // //  remove the anonymous expression
      // ir_code->eraseFromParent();
    }
  } else {
    // skip token for error recovery
    lexer_.next_token();
  }
}

ResourceTrackerSP ParserAST::add_module() {
  // create a `ResourceTracker` to track JIT'd memory allocated to our
  // anonymous expression, that way we can free it after executing.
  const auto resource_tracker = jit_.jit_dylib_.createResourceTracker();
  auto thread_safe_module =
      ThreadSafeModule(std::move(llvm_module_), std::move(llvm_context_));

  // llvm modules are IR code containers that can be added to the JIT or
  // compiler. It does not matter how many modules are created and added but
  // rather the names of the symbols and how they are grouped in the language
  // logical containers (e.g. language modules(e.g a struct that represents code
  // in a file), structs, globals(e.g. a unique struct per process that contains
  // global symbols))
  auto error = jit_.add_module(std::move(thread_safe_module), resource_tracker);
  if (error) {
    log_error("failed to add module", lexer_.row_, lexer_.col_);
    log_error(toString(std::move(error)).c_str(), lexer_.row_, lexer_.col_);
  }

  // llvm module once added it cannot be modified, so it's safe to
  // re-initialize
  init();

  return resource_tracker;
}

void ParserAST::run() {
  // prime the first token
  if (!lexer_.file_.is_open()) {
    fprintf(stderr, "toy> ");
  }
  lexer_.next_token();

  // top ::= definition | external | expression | ';'
  while (true) {
    switch (Lexer::to_reserved_token(lexer_.current_token_)) {
    case ReservedToken::token_eof:
      fprintf(stderr, "toy> ");
      lexer_.next_token();
      break;
    // ignore top-level semicolon
    case ReservedToken::token_semicolon:
    case ReservedToken::token_new_line:
      if (!lexer_.file_.is_open()) {
        fprintf(stderr, "toy> ");
      }
      lexer_.next_token();
      break;
    case ReservedToken::token_function_definition:
      try {
        handle_function_definition();
      } catch (const std::exception &e) {
        log_error(e.what(), lexer_.row_, lexer_.col_);
      }
      break;
    case ReservedToken::token_external_function:
      handle_extern();
      break;
    case ReservedToken::token_exit:
      fprintf(stderr, "dump module\n");
      llvm_module_->dump();
      return;
    default:
      handle_top_level_expression();
      break;
    }
  }
}

} // namespace toy

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

/**
 * Print a character.
 */
extern "C" DLLEXPORT double putch(double x) {
  fputc(static_cast<char>(x), stderr);
  // fprintf(stderr, "ASCII code %d, char:%c\n", static_cast<int>(x),
  //         static_cast<char>(x));
  return 0;
}

/**
 * printf that takes a double prints it as "%f\n", returning 0.
 */
extern "C" DLLEXPORT double printd(double x) {
  fprintf(stderr, "%f\n", x);
  return 0;
}
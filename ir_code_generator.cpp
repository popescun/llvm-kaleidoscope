//
// Created by Nicolae Popescu on 03/12/2025.
//

#include "ir_code_generator.hpp"
#include "utils.hpp"

#include <llvm/IR/InstVisitor.h>
#include <llvm/IR/Verifier.h>

#include <map>

namespace toy {
using namespace llvm;

namespace {
std::map<std::string, AllocaInst *> variable_names;

Function *get_function(ParserAST &parser_ast, const std::string &name) {
  // first, see if the function is present in the module
  if (auto *function = parser_ast.llvm_module_->getFunction(name)) {
    return function;
  }

  // if not, check whether we can codegen the declaration from some existing
  // prototype
  auto it = parser_ast.function_prototypes_.find(name);
  if (it != parser_ast.function_prototypes_.end()) {
    // cast Value* to Function*
    // todo: is there a llvm cast function?
    if (it->second) {
      return reinterpret_cast<Function *>(it->second->generate_IR_code());
    }

    log_error("function prototype gen code failed", parser_ast.lexer_.row_,
              parser_ast.lexer_.col_);
  }

  // if no existing prototype exists, return null
  return {};
}

/**
 * Create an alloca instruction in the entry block of the function.
 * This is used for mutable variables etc.
 */
AllocaInst *create_entry_block_alloca(const ParserAST &parser_ast,
                                      Function *function,
                                      const StringRef variable_name) {
  IRBuilder tmp_block(&function->getEntryBlock(),
                      function->getEntryBlock().begin());
  return tmp_block.CreateAlloca(Type::getDoubleTy(*parser_ast.llvm_context_),
                                nullptr, variable_name);
}
} // namespace

IRCodeGenerator::IRCodeGenerator(ParserAST &parser_ast)
    : parser_ast_{parser_ast} {}

Value *
IRCodeGenerator::operator()(const NumberExpressionAST &expression) const {
  return ConstantFP::get(*parser_ast_.llvm_context_,
                         APFloat(expression.value_));
}

Value *
IRCodeGenerator::operator()(const VariableExpressionAST &expression) const {
  // look this variable up in a function block
  auto *variable = variable_names[expression.name_];
  if (!variable) {
    log_error("unknown variable name", parser_ast_.lexer_.row_,
              parser_ast_.lexer_.col_);
    return {};
  }
  // load the value
  return parser_ast_.llvm_IR_builder_->CreateLoad(
      variable->getAllocatedType(), variable, expression.name_.c_str());
}

Value *IRCodeGenerator::operator()(const VarExpressionAST &expression) const {
  std::vector<AllocaInst *> old_bindings;
  Function *function =
      parser_ast_.llvm_IR_builder_->GetInsertBlock()->getParent();

  // register all variables and gen code for their initializer
  for (const auto &[first, second] : expression.variables_) {
    const auto &variable_name = first;
    auto *initializer_expression = second.get();

    // gen code for the initializer before adding the variable scope, this
    // prevents the initializer from referencing the variable itself, and
    // permits stuff like this:
    //  var a = 1 in
    //    var a = a in ... # refers to outer `a`
    Value *initializer_value =
        ConstantFP::get(*parser_ast_.llvm_context_, APFloat(0.0));
    if (initializer_expression) {
      initializer_value = initializer_expression->generate_IR_code();
      if (!initializer_value) {
        return {};
      }
    }

    auto *alloc =
        create_entry_block_alloca(parser_ast_, function, variable_name);
    parser_ast_.llvm_IR_builder_->CreateStore(initializer_value, alloc);

    // remember the old variable binding so that we can restore the binding when
    // we un-recurse
    old_bindings.push_back(variable_names[variable_name]);

    // remember this binding
    variable_names[variable_name] = alloc;
  }

  // code gen the body, now that all variables are in scope
  Value *body = expression.body_->generate_IR_code();
  if (!body) {
    return {};
  }

  // pop all our variables from scope
  for (std::size_t i = 0, n = expression.variables_.size(); i != n; ++i) {
    variable_names[expression.variables_[i].first] = old_bindings[i];
  }

  // return the body computation
  return body;
}

Value *
IRCodeGenerator::operator()(const BinaryExpressionAST &expression) const {

  // special case `=` because we don't want to gen code for `lhs` as an
  // expression
  if (expression.operator_ == ReservedToken::token_operator_assignment) {
    const auto *lhs =
        dynamic_cast<VariableExpressionAST *>(expression.lhs_.get());
    if (!lhs) {
      log_error("left side of '=' must be a variable", parser_ast_.lexer_.row_,
                parser_ast_.lexer_.col_);
      return {};
    }
    auto *value = expression.rhs_->generate_IR_code();
    if (!value) {
      log_error("rhs_ code gen for assignment operator failed",
                parser_ast_.lexer_.row_, parser_ast_.lexer_.col_);
      return {};
    }

    // look up the name
    auto *variable = variable_names[lhs->name_];
    if (!variable) {
      log_error("unknown variable name for assignment operator",
                parser_ast_.lexer_.row_, parser_ast_.lexer_.col_);
      return {};
    }

    parser_ast_.llvm_IR_builder_->CreateStore(value, variable);

    return value;
  }

  auto *left = expression.lhs_->generate_IR_code();
  auto *right = expression.rhs_->generate_IR_code();
  if (!left || !right) {
    return {};
  }

  // generate code for built-in operators
  switch (expression.operator_) {
  case ReservedToken::token_operator_add:
    // Note: last param in `CreateFAdd` is `Twine` type:
    // https://llvm.org/doxygen/classllvm_1_1Twine.html#details It's a faster
    // representation of strings that uses a binary-tree instead of an array.
    // See also: -
    // https://www.geeksforgeeks.org/dsa/ropes-data-structure-fast-string-concatenation/
    //           - https://stl.boost.org/Rope.html
    return parser_ast_.llvm_IR_builder_->CreateFAdd(left, right, "addtmp");
  case ReservedToken::token_operator_subtract:
    return parser_ast_.llvm_IR_builder_->CreateFSub(left, right, "subtmp");
  case ReservedToken::token_operator_multiply:
    return parser_ast_.llvm_IR_builder_->CreateFMul(left, right, "multmp");
  case ReservedToken::token_operator_less:
    // store cmp result in `left`s
    left = parser_ast_.llvm_IR_builder_->CreateFCmpULT(left, right, "cmptmp");
    // convert result to double
    return parser_ast_.llvm_IR_builder_->CreateUIToFP(
        left, Type::getDoubleTy(*parser_ast_.llvm_context_), "booltmp");
  default:
    // parser_ast_.log_error("invalid binary operator");
    break;
  }

  // if it wasn't a builtin binary operator, it must be a user defined one.
  // Generate a call to it.
  Function *function =
      get_function(parser_ast_, std::string(keyword_token_binary) +
                                    static_cast<char>(expression.operator_));
  assert(function && "binary operator not found");
  return parser_ast_.llvm_IR_builder_->CreateCall(function, {left, right},
                                                  "binop");
}

Value *IRCodeGenerator::operator()(const UnaryExpressionAST &expression) const {
  Value *operand = expression.operand_->generate_IR_code();
  if (!operand) {
    log_error("unary expression operand code generation failed",
              parser_ast_.lexer_.row_, parser_ast_.lexer_.col_);
    return {};
  }

  Function *function = get_function(
      parser_ast_, std::string(keyword_token_unary) +
                       static_cast<char>(expression.operation_code_));
  if (!function) {
    log_error("unknown unary operator", parser_ast_.lexer_.row_,
              parser_ast_.lexer_.col_);
    return {};
  }

  return parser_ast_.llvm_IR_builder_->CreateCall(function, operand, "unop");
}

Value *IRCodeGenerator::operator()(const CallExpressionAST &expression) const {
  // look up the name in the global module table
  auto *function = get_function(parser_ast_, expression.callee_);
  if (!function) {
    log_error("unknown function referenced", parser_ast_.lexer_.row_,
              parser_ast_.lexer_.col_);
    return {};
  }

  // if argument mismatch error
  if (function->arg_size() != expression.arguments_.size()) {
    log_error("incorrect arguments size", parser_ast_.lexer_.row_,
              parser_ast_.lexer_.col_);
    return {};
  }

  std::vector<Value *> arguments_values;
  for (const auto &arg : expression.arguments_) {
    if (const auto ir_code = arg->generate_IR_code()) {
      arguments_values.push_back(ir_code);
    }
  }
  if (arguments_values.empty()) {
    return {};
  }
  return parser_ast_.llvm_IR_builder_->CreateCall(function, arguments_values,
                                                  "calltmp");
}

Value *
IRCodeGenerator::operator()(const FunctionPrototypeAST &expression) const {
  // make the function type: double(double, double) etc.
  const std::vector<Type *> arguments_types{
      expression.arguments_names_.size(),
      Type::getDoubleTy(*parser_ast_.llvm_context_)};
  // This doc needs clarification: "Note that Types in LLVM are uniqued just
  // like Constants are, so you don’t “new” a type, you “get” it."
  // https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl03.html#function-code-generation
  FunctionType *function_type = FunctionType::get(
      Type::getDoubleTy(*parser_ast_.llvm_context_), arguments_types, false);
  Function *function =
      Function::Create(function_type, Function::ExternalLinkage,
                       expression.name_, parser_ast_.llvm_module_.get());
  // set names for all arguments
  std::uint32_t idx{0};
  for (auto &arg : function->args()) {
    arg.setName(expression.arguments_names_[idx++]);
  }

  return function;
}

/**
 * todo: This code does have a bug, though:
 * If the FunctionAST::codegen() method finds an existing IR Function, it does
 * not validate its signature against the definition’s own prototype. This
 * means that an earlier ‘extern’ declaration will take precedence over the
 * function definition’s signature, which can cause codegen to fail, for
 * instance if the function arguments are named differently. There are a
 * number of ways to fix this bug, see what you can come up with! Here is a
 * testcase:
 *     - extern foo(a);     # ok, defines foo.
 *     - def foo(b) b;      # Error: Unknown variable name. (decl using 'a'
 * takes precedence).
 */
Value *
IRCodeGenerator::operator()(const FunctionDefinitionAST &expression) const {
  // check for existing function from previous `extern` declaration
  // todo: does it regard all functions added to this module, not only
  // `extern`s
  Function *function = get_function(parser_ast_, expression.prototype_name_);
  if (!function) {
    log_error("function cannot be redefined", parser_ast_.lexer_.row_,
              parser_ast_.lexer_.col_);
    return {};
  }

  // if this is a user defined operator, register it in
  // Lexer::binary_op_precedence_
  const auto it =
      parser_ast_.function_prototypes_.find(expression.prototype_name_);
  assert(it != parser_ast_.function_prototypes_.end() &&
         "function prototype not found");
  const auto &function_prototype = it->second;
  assert(function_prototype && "function prototype is null");
  if (function_prototype->is_binary_operator()) {
    parser_ast_.lexer_
        .binary_op_precedence_[function_prototype->get_operator_name()] =
        static_cast<Token>(
            function_prototype->get_binary_operator_precedence());
  }

  // create a new basic block(body) to start insertion into
  BasicBlock *basic_block =
      BasicBlock::Create(*parser_ast_.llvm_context_, "entry", function);
  // from doc: This specifies that created instructions should be appended to
  // the end of the specified block.
  parser_ast_.llvm_IR_builder_->SetInsertPoint(basic_block);

  // record the function arguments in the function parameters map.
  // todo: how global `variable_names_` could be moved locally to a
  // todo: function definition?
  variable_names.clear();
  for (auto &arg : function->args()) {
    // create an alloca for this variable
    auto *alloca =
        create_entry_block_alloca(parser_ast_, function, arg.getName());

    // store the initial value into the alloca
    parser_ast_.llvm_IR_builder_->CreateStore(&arg, alloca);

    // add arguments to variable symbol table
    variable_names[std::string{arg.getName()}] = alloca;
  }

  Value *body_value = expression.body_->generate_IR_code();
  if (!body_value) {
    log_error("function body generated IR code failed", parser_ast_.lexer_.row_,
              parser_ast_.lexer_.col_);
    function->eraseFromParent();
    return {};
  }

  // finish off the function
  Value *ret_value = parser_ast_.llvm_IR_builder_->CreateRet(body_value);
  if (!ret_value) {
    log_error("function create ret failed", parser_ast_.lexer_.row_,
              parser_ast_.lexer_.col_);
    function->eraseFromParent();
    return {};
  }

  // validate the generated code, checking for consistency
  if (verifyFunction(*function, &errs())) {
    log_error("function verification failed", parser_ast_.lexer_.row_,
              parser_ast_.lexer_.col_);
    function->eraseFromParent();
    return {};
  }

  if (function_prototype->is_binary_operator()) {
    parser_ast_.lexer_.binary_op_precedence_.erase(static_cast<Token>(
        function_prototype->get_binary_operator_precedence()));
    return {};
  }

  // run the optimizer on the function
  if (parser_ast_.function_pass_manager_) {
    parser_ast_.function_pass_manager_->run(
        *function, *parser_ast_.function_analysis_manager_);
  }

  return function;
}

Value *IRCodeGenerator::operator()(const IfExpressionAST &expression) const {
  Value *if_value = expression.if_->generate_IR_code();
  if (!if_value) {
    return {};
  }

  // convert `if` condition to a truth value(bool) by comparing non-equal to
  // 0.0
  if_value = parser_ast_.llvm_IR_builder_->CreateFCmpONE(
      if_value, ConstantFP::get(*parser_ast_.llvm_context_, APFloat(0.0)),
      "ifcond");

  // get enclosing function
  Function *parent_function =
      parser_ast_.llvm_IR_builder_->GetInsertBlock()->getParent();
  if (!parent_function) {
    return {};
  }

  BasicBlock *then_block =
      BasicBlock::Create(*parser_ast_.llvm_context_, "then", parent_function);
  BasicBlock *else_block =
      BasicBlock::Create(*parser_ast_.llvm_context_, "else");
  parser_ast_.llvm_IR_builder_->CreateCondBr(if_value, then_block, else_block);

  // codegen `then` value
  parser_ast_.llvm_IR_builder_->SetInsertPoint(then_block);
  Value *then_value = expression.then_->generate_IR_code();
  if (!then_value) {
    return {};
  }

  BasicBlock *merge_block =
      BasicBlock::Create(*parser_ast_.llvm_context_, "ifcont");
  parser_ast_.llvm_IR_builder_->CreateBr(merge_block);

  // codegen of `then` by changing the current block, update `then` block for
  // PHI.
  then_block = parser_ast_.llvm_IR_builder_->GetInsertBlock();

  // codegen `else` block
  parent_function->insert(parent_function->end(), else_block);
  parser_ast_.llvm_IR_builder_->SetInsertPoint(else_block);
  Value *else_value = expression.else_->generate_IR_code();
  if (!else_value) {
    return {};
  }
  parser_ast_.llvm_IR_builder_->CreateBr(merge_block);
  // codegen of `else` can change the current block, update else block for
  // PHI.
  else_block = parser_ast_.llvm_IR_builder_->GetInsertBlock();

  // emit `merge` block
  parent_function->insert(parent_function->end(), merge_block);
  parser_ast_.llvm_IR_builder_->SetInsertPoint(merge_block);
  PHINode *phi_node = parser_ast_.llvm_IR_builder_->CreatePHI(
      Type::getDoubleTy(*parser_ast_.llvm_context_), 2, "iftmp");
  if (!phi_node) {
    return {};
  }

  phi_node->addIncoming(then_value, then_block);
  phi_node->addIncoming(else_value, else_block);

  return phi_node;
}

// Output for-loop as:
//   var = alloca double
//   ...
//   start = startexpr
//   store start -> var
//   goto loop
// loop:
//   ...
//   bodyexpr
//   ...
// loopend:
//   step = stepexpr
//   endcond = endexpr
//
//   curvar = load var
//   nextvar = curvar + step
//   store nextvar -> var
//   br endcond, loop, endloop
// outloop:
Value *IRCodeGenerator::operator()(const ForExpressionAST &expression) const {
  Function *parent_function =
      parser_ast_.llvm_IR_builder_->GetInsertBlock()->getParent();

  // create an alloca for the variable in the entry block.
  auto *alloca = create_entry_block_alloca(parser_ast_, parent_function,
                                           expression.variable_name_);

  // first, codegen the start code, without variable in scope
  Value *start_value = expression.start_->generate_IR_code();
  if (!start_value) {
    return {};
  }

  // store the value into the alloca
  parser_ast_.llvm_IR_builder_->CreateStore(start_value, alloca);

  // make new basic block for loop header, inserting after current block
  BasicBlock *loop_block =
      BasicBlock::Create(*parser_ast_.llvm_context_, "loop", parent_function);

  // insert an explicit fall through from the current block to the loop block
  parser_ast_.llvm_IR_builder_->CreateBr(loop_block);

  // start insertion in loop block
  parser_ast_.llvm_IR_builder_->SetInsertPoint(loop_block);

  // within loop, the variable is defined equal to the alloca value.
  // If it shadows an existing variable, we have to restore it, so save it
  // now.
  auto *old_value = variable_names[expression.variable_name_];
  // we store variable name to be consumed when `body_` code is generated.
  // Afterward, the old_value is restored.
  variable_names[expression.variable_name_] = alloca;

  // emit the body of the loop. This, like any other expression, can change
  // the current block. Note that we ignore the value computed by the body,
  // but don't allow an error.
  if (!expression.body_->generate_IR_code()) {
    return {};
  }

  // emit step value
  Value *step_value{nullptr};
  if (expression.step_) {
    step_value = expression.step_->generate_IR_code();
    if (!step_value) {
      return {};
    }
  } else {
    // if not specified, use 1.0
    step_value = ConstantFP::get(*parser_ast_.llvm_context_, APFloat{1.0});
  }

  // reload, increment, and restore the alloca. This handles the case where the
  // body of the loop mutates the variables.
  auto *current_variable = parser_ast_.llvm_IR_builder_->CreateLoad(
      alloca->getAllocatedType(), alloca, expression.variable_name_.c_str());
  Value *next_variable = parser_ast_.llvm_IR_builder_->CreateFAdd(
      current_variable, step_value, "nextvar");
  parser_ast_.llvm_IR_builder_->CreateStore(next_variable, alloca);

  // compute the end condition
  Value *end_condition = expression.end_->generate_IR_code();
  if (!end_condition) {
    log_error("End condition code gen failed", parser_ast_.lexer_.row_,
              parser_ast_.lexer_.col_);
    return {};
  }

  // convert end condition to a bool by comparing non-equal to 0.0
  // end_condition = parser_ast_.llvm_IR_builder_->CreateFCmpONE(
  //     end_condition, ConstantFP::get(*parser_ast_.llvm_context_,
  //     APFloat(0.0)), "loopcond");

  // convert end condition to unsigned integer 1 bit ( this represents boolean
  // in llvm)
  end_condition = parser_ast_.llvm_IR_builder_->CreateFPToUI(
      end_condition, Type::getInt1Ty(*parser_ast_.llvm_context_), "loopcond");

  // create the block for the loop exit("afterloop"), and insert it
  BasicBlock *loop_after_block = BasicBlock::Create(
      *parser_ast_.llvm_context_, "afterloop", parent_function);

  // insert the condition branch into the end of loop end block
  // todo: this behaves like do..while loop as the condition is at the end of
  // todo: the loop block, whereas normally the for loop check the condition
  // todo: first. I guess we need to handle it like in if..then where the
  // todo: condition is checked first.
  parser_ast_.llvm_IR_builder_->CreateCondBr(end_condition, loop_block,
                                             loop_after_block);

  // any new code will be inserted in loop after block
  parser_ast_.llvm_IR_builder_->SetInsertPoint(loop_after_block);

  // restore the unshadowed variable
  if (old_value) {
    variable_names[expression.variable_name_] = old_value;
  } else {
    variable_names.erase(expression.variable_name_);
  }

  // for expression always returns 0.0
  return Constant::getNullValue(Type::getDoubleTy(*parser_ast_.llvm_context_));
}
}; // namespace toy
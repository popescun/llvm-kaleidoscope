//
// Created by Nicolae Popescu on 17/11/2025.
//

#ifndef TOY_JIT_HPP
#define TOY_JIT_HPP

#include "ast_parser.hpp"

#include <llvm/ExecutionEngine/Orc/Core.h>
#include <llvm/ExecutionEngine/Orc/IRCompileLayer.h>
#include <llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h>
#include <llvm/ExecutionEngine/Orc/Mangling.h>
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/Orc/Shared/ExecutorSymbolDef.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/Support/Error.h>

#include <memory>

namespace toy {

struct Jit {

  Jit(std::unique_ptr<llvm::orc::ExecutionSession> execution_session,
      llvm::orc::JITTargetMachineBuilder jit_target_machine_builder,
      const llvm::DataLayout &data_layout);
  virtual ~Jit();

  void run() const;

  const llvm::DataLayout &get_data_layout() const;
  llvm::orc::JITDylib &get_jit_dylib() const;
  llvm::Error
  add_module(llvm::orc::ThreadSafeModule thread_safe_module,
             llvm::orc::ResourceTrackerSP resource_tracker_sp = nullptr);
  llvm::Expected<llvm::orc::ExecutorSymbolDef> lookup(llvm::StringRef name);

  std::unique_ptr<llvm::orc::ExecutionSession> execution_session_;
  llvm::DataLayout data_layout_;
  llvm::orc::MangleAndInterner mangle_and_interner_;
  llvm::orc::RTDyldObjectLinkingLayer rtdyld_object_linking_layer_;
  llvm::orc::IRCompileLayer ir_compile_layer_;
  llvm::orc::JITDylib &jit_dylib_;
  std::unique_ptr<ParserAST> parser_ast_;

  /**
   * Factory to create a `Jit` instance.
   *
   * @return Jit instance.
   */
  static std::unique_ptr<Jit> create();
};

} // namespace toy

#endif // TOY_JIT_HPP
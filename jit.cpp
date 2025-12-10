//
// Created by Nicolae Popescu on 17/11/2025.
//

#include "jit.hpp"

#include <llvm/ExecutionEngine/Orc/CompileUtils.h>
#include <llvm/ExecutionEngine/Orc/ExecutionUtils.h>
#include <llvm/ExecutionEngine/Orc/SelfExecutorProcessControl.h>
#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/Support/TargetSelect.h>

namespace toy {
using namespace llvm;
using namespace llvm::orc;

ExitOnError ExitOnErr;

Jit::Jit(std::unique_ptr<ExecutionSession> execution_session,
         JITTargetMachineBuilder jit_target_machine_builder,
         const DataLayout &data_layout)
    : execution_session_{std::move(execution_session)},
      data_layout_{data_layout},
      mangle_and_interner_{*execution_session_, data_layout_},
      rtdyld_object_linking_layer_{
          *execution_session_,
          [](const llvm::MemoryBuffer &) {
            return std::make_unique<SectionMemoryManager>();
          }},
      ir_compile_layer_{*execution_session_, rtdyld_object_linking_layer_,
                        std::make_unique<ConcurrentIRCompiler>(
                            std::move(jit_target_machine_builder))},
      jit_dylib_{execution_session_->createBareJITDylib("<main>")},
      parser_ast_{std::make_unique<ParserAST>(*this)} {
  jit_dylib_.addGenerator(
      cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(
          data_layout_.getGlobalPrefix())));
  if (jit_target_machine_builder.getTargetTriple().isOSBinFormatCOFF()) {
    rtdyld_object_linking_layer_.setOverrideObjectFlagsWithResponsibilityFlags(
        true);
    rtdyld_object_linking_layer_.setAutoClaimResponsibilityForObjectSymbols(
        true);
  }
}
Jit::~Jit() {
  if (auto err = execution_session_->endSession()) {
    execution_session_->reportError(std::move(err));
  }
}

void Jit::run() const { parser_ast_->run(); }

std::unique_ptr<Jit> Jit::create() {
  InitializeNativeTarget();
  InitializeNativeTargetAsmPrinter();
  InitializeNativeTargetAsmParser();

  auto self_executor_process_control = SelfExecutorProcessControl::Create();
  if (!self_executor_process_control) {
    ExitOnErr(self_executor_process_control.takeError());
    return {};
  }

  auto execution_session = std::make_unique<ExecutionSession>(
      std::move(*self_executor_process_control));

  JITTargetMachineBuilder jit_target_machine_builder(
      execution_session->getExecutorProcessControl().getTargetTriple());
  auto data_layout = jit_target_machine_builder.getDefaultDataLayoutForTarget();
  if (!data_layout) {
    ExitOnErr(data_layout.takeError());
    return {};
  }
  return std::make_unique<Jit>(std::move(execution_session),
                               std::move(jit_target_machine_builder),
                               std::move(*data_layout));
}

const DataLayout &Jit::get_data_layout() const { return data_layout_; }

JITDylib &Jit::get_jit_dylib() const { return jit_dylib_; }

Error Jit::add_module(ThreadSafeModule thread_safe_module,
                      ResourceTrackerSP resource_tracker_sp) {
  if (!resource_tracker_sp) {
    resource_tracker_sp = jit_dylib_.getDefaultResourceTracker();
  }
  return ir_compile_layer_.add(resource_tracker_sp,
                               std::move(thread_safe_module));
}

Expected<ExecutorSymbolDef> Jit::lookup(StringRef name) {
  return execution_session_->lookup({&jit_dylib_},
                                    mangle_and_interner_(name.str()));
}

} // namespace toy
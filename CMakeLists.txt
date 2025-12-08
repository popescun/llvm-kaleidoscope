cmake_minimum_required(VERSION 4.1)

# https://cmake.org/cmake/help/latest/prop_tgt/CXX_STANDARD.html
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

project(toy)

# https://llvm.org/docs/CMake.html#embedding-llvm-in-your-project
set(LLVM_PATH /Users/popescun/github/llvm-project/llvm/install)
link_directories(${LLVM_PATH}/lib)
include_directories(${LLVM_PATH}/include)
# add cmake module for function `llvm_map_components_to_libnames` that generates llvm libs list
include(${LLVM_PATH}/lib/cmake/llvm/LLVM-Config.cmake)
llvm_map_components_to_libnames(llvm_libs
  support
  core
  demangle
  remarks
  bitstreamreader
  binaryformat
  targetparser
  analysis
  irprinter
  passes
  codegen
  transformutils
  mc
  profiledata
  cgdata
  debuginfodwarf
  debuginfodwarflowlevel
  bitreader
  object
  mcparser
  irreader
  asmparser
  bitwriter
  ipo
  scalaropts
  target
  objcarcopts
  codegentypes
  textapi
  instcombine
  cfguard
  instrumentation
  coroutines
  frontendopenmp
  frontendatomic
  frontendoffloading
  vectorize
  hipstdpar
  aggressiveinstcombine
  globalisel
  frontendhlsl
  objectyaml
  linker
  sandboxir
  executionengine
  runtimedyld
  orcjit
  orcshared
  jitlink
  option
  orctargetprocess
  aarch64asmparser
  aarch64codegen
  aarch64info
  aarch64desc
  aarch64utils
  asmprinter
  selectiondag
  debuginfocodeview
)

#include(/opt/homebrew/Cellar/llvm@18/18.1.8/lib/cmake/llvm/LLVMConfig.cmake)
#find_package(LLVM REQUIRED)

find_package(benchmark REQUIRED)
include(/opt/homebrew/Cellar/zstd/1.5.7/lib/cmake/zstd/zstdConfig.cmake)
find_package(ZSTD REQUIRED STATIC)
if (!ZSTD_FOUND)
  message(FATAL_ERROR "zstd not found")
endif ()

find_package(ZLIB REQUIRED)
if (!ZLIB_FOUND)
  message(FATAL_ERROR "zlib not found")
endif ()

file(GLOB headers *.hpp)
file(GLOB cpp_files *.cpp)
add_executable(${PROJECT_NAME}
  ${headers}
  ${cpp_files}
)

target_link_libraries(${PROJECT_NAME}
  ${llvm_libs}
  benchmark::benchmark
  zstd::libzstd_shared
  ${ZLIB_LIBRARY}
)
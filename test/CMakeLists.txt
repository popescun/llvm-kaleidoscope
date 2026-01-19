
project(test_linking)

if(EXISTS "${CMAKE_BINARY_DIR}/output.o")
  add_executable(${PROJECT_NAME}
    ${CMAKE_BINARY_DIR}/output.o
    test_linking_object.cpp
  )
endif ()


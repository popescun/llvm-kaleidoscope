
project(test_linking)

add_executable(${PROJECT_NAME}
  ../build/output.o
  test_linking_object.cpp
)


cmake_minimum_required(VERSION 3.10)
project(toy_compiler)

find_package(LLVM REQUIRED CONFIG)
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

file(GLOB SOURCES src/*.cpp)

add_executable(toy_compiler ${SOURCES})
target_link_libraries(toy_compiler LLVM)

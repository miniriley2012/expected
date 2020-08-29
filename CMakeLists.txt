cmake_minimum_required(VERSION 3.17)
project(expected)

set(CMAKE_CXX_STANDARD 20)

add_library("${PROJECT_NAME}" INTERFACE)

target_include_directories("${PROJECT_NAME}" INTERFACE "${PROJECT_SOURCE_DIR}/include")
target_compile_features("${PROJECT_NAME}" INTERFACE cxx_std_20)

export(TARGETS "${PROJECT_NAME}" FILE "${PROJECT_NAME}_targets.cmake")
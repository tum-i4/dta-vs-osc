cmake_minimum_required(VERSION 3.8)
project(taint VERSION 0.1 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

# enable_testing()

set(TAINT_MAIN_INCLUDE_DIR ${CMAKE_CURRENT_LIST_DIR}/include)
include_directories("${TAINT_MAIN_INCLUDE_DIR}")

# add_subdirectory(extern/googletest EXCLUDE_FROM_ALL)

add_subdirectory(extern/fmt EXCLUDE_FROM_ALL)
link_libraries(fmt::fmt)
add_subdirectory(extern/Triton)
# add_library(clara INTERFACE)
# target_include_directories(clara INTERFACE extern/Clara)
add_library(clipp INTERFACE)
target_include_directories(clipp INTERFACE extern/Clipp)
# find_library(libtriton)
set(BUILD_LIBS_ONLY ON)
add_subdirectory(extern/keystone)

set(LLVM_COV_EXCLUDE_REGEX "extern/.*|.*.hpp")
include(ClangRuntimeSupport)

if (NOT MSVC)
#   # link fs lib (required for gcc-8, not required for gcc-9)
#   link_libraries("stdc++fs")
else()
  link_libraries("-INCREMENTAL:NO")
endif()

add_subdirectory(src)
# add_subdirectory(test)

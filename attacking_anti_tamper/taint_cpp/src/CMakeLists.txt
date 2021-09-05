add_asan()

add_library(serialization serialization.cpp)
add_library(cmdline cmdline.cpp)
target_link_libraries(cmdline PRIVATE clipp)

add_library(utils utils.cpp)

add_library(analysis analysis.cpp)
include(CMakePrintHelpers)
target_link_libraries(analysis PUBLIC Triton::triton PRIVATE keystone utils)
# since keystone doesn't seem to provide any INCLUDE_ vars
# or sets include dirs correctly on the target, hack them here
target_include_directories(analysis PRIVATE "${CMAKE_SOURCE_DIR}/extern/keystone/include")

add_executable(taint_main app.cpp)
target_link_libraries(taint_main PRIVATE serialization cmdline utils analysis)

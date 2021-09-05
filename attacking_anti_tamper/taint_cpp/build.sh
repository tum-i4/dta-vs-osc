#!/bin/bash

mypath="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
build_type=$1
if [ -z "${build_type}" ]; then
    build_type=Debug
fi
build_path=${mypath}/../build_taint_cpp_${build_type}/linux
out_path=${mypath}/../out_taint_cpp_${build_type}/linux

if [ ! -d "${build_path}" ]; then
    mkdir -p ${build_path}
    cd ${build_path}

    # export CC=clang
    # export CXX=clang++

    cmake -G "Ninja" \
        -D "CMAKE_BUILD_TYPE:STRING=${build_type}" \
        -D "CMAKE_EXPORT_COMPILE_COMMANDS:BOOL=1" \
        -D "CMAKE_INSTALL_PREFIX=${out_path}" \
        ../../taint_cpp
fi

cd ${build_path}
export NINJA_STATUS="[%f/%t] %es:  "
# cmake --build . --target install --config ${build_type}
cmake --build . --config ${build_type}

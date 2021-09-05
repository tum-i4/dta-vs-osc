#!/bin/bash

mypath="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
build_type=$1
if [ -z "${build_type}" ]; then
    build_type=Debug
fi
build_path=${mypath}/../build_tracer_${build_type}/linux
out_path=${mypath}/../out_tracer_${build_type}/linux

if [ ! -d "${build_path}" ]; then
    mkdir -p ${build_path}
    cd ${build_path}

    # export CC=clang
    # export CXX=clang++

    cmake -G "Ninja" \
        -D "CMAKE_BUILD_TYPE:STRING=${build_type}" \
        -D "CMAKE_EXPORT_COMPILE_COMMANDS:BOOL=1" \
        -D "CMAKE_INSTALL_PREFIX=${out_path}" \
        -D "DynamoRIO_DIR:PATH=${mypath}/../../DynamoRIO-Linux-8.0.0-1/cmake" \
        ../../tracer
fi

cd ${build_path}
export NINJA_STATUS="[%f/%t] %es:  "
# cmake --build . --target install --config ${build_type}
cmake --build . --config ${build_type}

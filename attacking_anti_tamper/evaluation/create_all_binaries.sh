#!/bin/bash

./build_virtsc_version.sh VirtSC_New
./compile_dataset.sh binaries_new
./build_virtsc_version.sh VirtSC_New_Dummy_Check
./compile_dataset.sh binaries_new_dummy_check
./build_virtsc_version.sh VirtSC_New_Tampered
./compile_dataset.sh binaries_new_tampered
./build_virtsc_version.sh VirtSC_Old
./compile_dataset.sh binaries_old
./build_virtsc_version.sh VirtSC_Old_Dummy_check
./compile_dataset.sh binaries_old_dummy_check
./build_virtsc_version.sh VirtSC_Old_Tampered
./compile_dataset.sh binaries_old_tampered

#!/bin/bash

if [ $# -ne 2 ]
then
  echo "Wrong amount of arguments: first argument contains path to unprotected .bc file, second argument contains path to directory where to save protected executable"
  exit 1
fi

program_base=${1##*/}
program_name=${program_base%%.*}
program_path=${1%/*}
if [ -d "$program_path/link-libraries" ]
then
  fdir=$(echo $program_base | awk -F"." '{print $1}')
  lib_file=$(ls $program_path/link-libraries | grep $fdir)
  if [ -n "$lib_file" ]
  then
    libraries=$(cat $program_path/link-libraries/$lib_file)
  fi
fi

mkdir -p $2
/home/sip/built-sc-virt/bin/opt -reg2mem < $1 > $2/scvirt_prep.bc
/home/sip/built-sc-virt/bin/opt -load LLVMScVirt.so -scvirt < $2/scvirt_prep.bc > $2/scvirt_out.bc
/home/sip/built-sc-virt/bin/clang++ $libraries $2/scvirt_out.bc -o $2/${program_name}.out
rm $2/scvirt_prep.bc
rm $2/scvirt_out.bc
echo "Generated $2/${program_name}.out"

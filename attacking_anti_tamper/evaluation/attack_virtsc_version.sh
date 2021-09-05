#!/bin/bash

if [ $# -ne 1 ]
then
  echo "Argument missing for which directory to attack"
  exit 1
fi

output_dir=./attack_data/${1##*binaries_}
mkdir -p $output_dir
for f in $1/*.out
do
  program_base=${f##*/}
  program_name=${program_base%.*}
  if [ -f "${output_dir}/${program_name}.txt" ]
  then
    echo "${output_dir}/${program_name}.txt exists already"
  else
    if [ -f "dataset/cmdline-args/$program_name" ]
    then
      read_args=$(<dataset/cmdline-args/$program_name)
      ../run.py $f -o ${output_dir}/${program_name}.txt --args "${read_args}"
    else
      ../run.py $f -o ${output_dir}/${program_name}.txt
    fi
  fi
done

#!/bin/bash

if [ $# -ne 1 ]
then
  echo "Argument missing for which directory to compile to"
  exit 1
fi

for f in ./dataset/*.bc
do
  ./protect.sh $f $1
done

for f in ./dataset/input/*
do
  file_base=${f##*/}
  cp $f $1/${file_base}
done

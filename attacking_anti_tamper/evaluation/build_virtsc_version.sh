#!/bin/bash

if [ $# -ne 1 ]
then
  echo "Argument missing for which directory to use"
  exit 1
fi

cd $1
cp ScVirt.cpp /home/sip/sc-virt/src/lib/Transforms/ScVirt/ScVirt.cpp
cp ISATranslator.cpp /home/sip/sc-virt/src/lib/Transforms/ScVirt/ISATranslator.cpp
mkdir -p /home/sip/built-sc-virt/
cd /home/sip/built-sc-virt/
make
echo "Compiled VirtSC using $1"

#!/bin/bash

trap ctrl_c INT

function ctrl_c() 
{
  exit 0
}

for i in {10..30}
do
  echo "============================> SIZE $i"
  python ../src/main_generate_program.py . --basic-blocks $i --unstructured --filename program$i.txt --loops 1 --subprograms 10
  python ../src/main_super_blocks.py program$i.txt --use-ilp --use-tree-based
done

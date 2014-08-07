#!/bin/bash

trap ctrl_c INT

function ctrl_c() 
{
  exit 0
}

for i in {20..50}
do
  echo "============================> SIZE $i"
  python ../src/main_generate_program.py . --basic-blocks $i --unstructured --filename program$i.txt --loops 2 --nesting-depth 1 --subprograms 50
  python ../src/main_super_blocks.py program$i.txt --use-ilp --use-tree-based
done

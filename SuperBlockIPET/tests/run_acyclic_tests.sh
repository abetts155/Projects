#!/bin/bash

trap ctrl_c INT

function ctrl_c() 
{
  exit 0
}

for i in {10..50}
do
  echo "===============================> $i"
  python ../src/main_generate_program.py . --basic-blocks $i --unstructured --filename program$i.txt --subprograms 50
  python ../src/main_super_blocks.py program$i.txt --shuffle-constraints --use-ilp --use-tree-based
done

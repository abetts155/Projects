#!/bin/bash

trap ctrl_c INT

function ctrl_c() 
{
  exit 0
}

lower=${lower:=10}
upper=${upper:=50}
subprograms=${subprograms:=10}

for i in $(seq $lower $upper) 
do
  echo "===============================> $i"
  python ../src/main_generate_program.py . --basic-blocks $i --unstructured --filename program$i.txt --subprograms $subprograms
  python ../src/main_super_blocks.py program$i.txt --shuffle-constraints --use-ilp --use-tree-based
done

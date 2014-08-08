#!/bin/bash

trap ctrl_c INT

function ctrl_c() 
{
  exit 0
}

lower=${lower:=10}
upper=${upper:=50}
subprograms=${subprograms:=10}
loops=${loops:=1}
depth=${depth:=1}

for i in $(seq $lower $upper)
do
  echo "============================> SIZE $i"
  python ../src/main_generate_program.py . --basic-blocks $i --filename program$i.txt --loops $loops --nesting-depth $depth --subprograms $subprograms
  python ../src/main_super_blocks.py program$i.txt --use-ilp --use-tree-based
done

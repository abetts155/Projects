#!/bin/bash

trap ctrl_c INT

function ctrl_c() 
{
  exit 0
}

loopbound=${loopbound:=10}
lower=${lower:=20}
upper=${upper:=100}
subprograms=${subprograms:=10}
loops=${loops:=1}
depth=${depth:=1}

for i in $(seq $lower $upper)
do
  echo "============================> SIZE $i"
  python ../src/main_generate_program.py . --unstructured --basic-blocks $i --filename program$i.txt --loops $loops --nesting-depth $depth --subprograms $subprograms --maximum-loop-bound $loopbound
  python ../src/main_super_blocks.py program$i.txt --region-based
done

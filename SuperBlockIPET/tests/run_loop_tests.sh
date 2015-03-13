#!/bin/bash

trap ctrl_c INT

function ctrl_c() 
{
  exit 0
}

loopbound=${loopbound:=20}
lower=${lower:=40}
upper=${upper:=100}
subprograms=${subprograms:=1}
loops=${loops:=5}
depth=${depth:=5}

for i in $(seq $lower $upper)
do
  echo "============================> SIZE $i"
  python ../src/main_generate_program.py . --unstructured --basic-blocks $i --filename program$i.txt --loops $loops --nesting-depth $depth --subprograms $subprograms --maximum-loop-bound $loopbound --add-WCET-information
  python ../src/main_super_blocks.py program$i.txt --region-based
done

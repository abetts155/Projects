#!/bin/bash

trap ctrl_c INT

function ctrl_c() 
{
  exit 0
}

for i in {10..1000}
do
  python ../src/main_generate_program.py . --basic-blocks $i --unstructured --subprograms 10 --filename program$i.txt
  python ../src/main_super_blocks.py program$i.txt --repeat-calculation 10 --shuffle-constraints --log-to-file program$i.output.txt --use-ilp
done

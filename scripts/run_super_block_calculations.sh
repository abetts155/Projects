#!/bin/bash

function clean_up {
  rm -f [0-9][0-9]*.txt *.ilp
  exit
}

trap clean_up INT

MIN_VERTICES=10
MAX_VERTICES=1000
SUBPROGRAMS=100
LOOPS=0
NESTING_DEPTH=1
OUTPUT_DIR=super_block_results

mkdir -p $OUTPUT_DIR

while getopts ":s:l:n:a:b" opt; do
  case $opt in
    a) MIN_VERTICES=$OPTARG
      ;;
    b) MAX_VERTICES=$OPTARG
      ;;
    s) SUBPROGRAMS=$OPTARG
      ;;
    l) LOOPS=$OPTARG
      ;;
    n) NESTING_DEPTH=$OPTARG
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      exit 1
      ;;
    :)
      echo "Option -$OPTARG requires an argument." >&2
      exit 1
      ;;
  esac
done

for i in `seq $MIN_VERTICES $MAX_VERTICES`;
do
  echo "Doing analysis on control flow graphs with $i vertices"
  python3 ../tools/program_generator.py --vertices $i --program_file $i.txt\
    --subprograms $SUBPROGRAMS --loops $LOOPS --nesting-depth $NESTING_DEPTH
  python3 ../tools/super_block_calculations.py $i.txt\
    --output $OUTPUT_DIR/$i.subprograms_$SUBPROGRAMS.loops_$LOOPS.depth_$NESTING_DEPTH.txt\
		--repeat 100
done



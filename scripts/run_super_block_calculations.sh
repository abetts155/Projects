#!/bin/bash

SUBPROGRAMS=100
LOOPS=0
NESTING_DEPTH=0

while getopts ":s:l:n" opt; do
  case $opt in
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

function clean_up {
	rm -f [0-9][0-9]*.txt *.ilp
	exit
}

trap clean_up INT

for i in `seq 10 1000`;
do
	echo "Doing analysis on control flow graphs with $i vertices"
	python3 ../tools/program_generator.py --vertices $i --program_file $i.txt --subprograms $SUBPROGRAMS --loops $LOOPS --nesting-depth $NESTING_DEPTH
	python3 ../tools/super_block_calculations.py $i.txt --output $i.results --repeat 1000
done


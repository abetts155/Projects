#!/bin/bash

function header() {
	echo
	date +'%H:%M:%S'
	echo "$(tput bold)> $1 $(tput sgr0)"
}

header 'No draws/wins/losses'
./fixtures.sh -E draw win loss -n --minimum 10 "$@"

header 'Draws'
./fixtures.sh -E draw --minimum 3 "$@"

header 'Goals for = 0'
./fixtures.sh -E gf_eq_0 --minimum 3 "$@"

header 'Goals against = 0'
./fixtures.sh -E ga_eq_0 --minimum 3 "$@"

header 'Goals scored <= 1'
./fixtures.sh -E gfa_le_1 --minimum 3 "$@"

header 'Goals scored <= 2'
./fixtures.sh -E gfa_le_2 --minimum 5 "$@"

header 'Wins or losses'
./fixtures.sh -E win loss --minimum 8 "$@"

header 'No BTS'
./fixtures.sh -E bts -n --minimum 6 "$@"

header '0-0 draws'
./fixtures.sh -E gfa_eq_0 "$@"

header 'No goals (consecutive halves)'
./fixtures.sh -E gfa_eq_0 --half separate --minimum 4 "$@"

header 'Goals scored = 0 (first half)'
./fixtures.sh -E gfa_eq_0 --half first --minimum 4 "$@"

header 'Goals scored = 0 (second half)'
./fixtures.sh -E gfa_eq_0 --half second --minimum 3 "$@"

header 'Goals against = 0 (second half)'
./fixtures.sh -E ga_eq_0 --half second --minimum 5 "$@"

header 'Goals for = 0 (second half)'
./fixtures.sh -E gf_eq_0 --half second --minimum 6 "$@"

header 'Goals for = 0 (home only)'
./fixtures.sh -E gf_eq_0 --venue home --minimum 2 "$@"

header 'Goals for = 0 (away only)'
./fixtures.sh -E gf_eq_0 --venue away --minimum 3 "$@"

header 'Draws (first half)'
./fixtures.sh -E draw --half first --minimum 5 "$@"

header 'Draws (second half)'
./fixtures.sh -E draw --half second --minimum 4 "$@"

header 'Draws (consecutive halves)'
./fixtures.sh -E draw --half separate --minimum 5 "$@"

header 'Wins (consecutive halves)'
./fixtures.sh -E win --half separate --minimum 5 "$@"

header 'No draws (consecutive halves)'
./fixtures.sh -E draw -n --half separate --minimum 10 "$@"

header 'Goals scored >= 3'
./fixtures.sh -E gfa_ge_3 --minimum 6 "$@"

header 'Goals scored >= 4'
./fixtures.sh -E gfa_ge_4 --minimum 4 "$@"

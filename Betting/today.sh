#!/bin/bash

function header() {
  echo
	echo "$(tput bold)> $1 $(tput sgr0)"
}

header 'No draws'
./fixtures.sh -E draw -n --minimum 10 "$@"

header 'Draws'
./fixtures.sh -E draw --minimum 3 "$@"

header 'Goals for = 0'
./fixtures.sh -E gf_eq_0 --minimum 3 "$@"

header 'Goals against = 0'
./fixtures.sh -E ga_eq_0 --minimum 3 "$@"

header 'Goals scored <= 1'
./fixtures.sh -E gfa_le_1 --minimum 3 "$@"

header 'Goals scored <= 2'
./fixtures.sh -E gfa_le_2 --minimum 7 "$@"

header 'Wins'
./fixtures.sh -E win --minimum 8 "$@"

header 'No BTS'
./fixtures.sh -E bts -n --minimum 7 "$@"

header '0-0 draws'
./fixtures.sh -E gfa_eq_0 --minimum 1 "$@"

header 'No goals (consecutive halves)'
./fixtures.sh -E gfa_eq_0 --half first second --minimum 4 "$@"

header 'No goals (home only) (consecutive halves)'
./fixtures.sh -E gfa_eq_0 --half first second --minimum 3 --venue home "$@"

header 'No goals (away only) (consecutive halves)'
./fixtures.sh -E gfa_eq_0 --half first second --minimum 3 --venue away "$@"

header 'Goals scored = 0 (first half)'
./fixtures.sh -E gfa_eq_0 --half first --minimum 4 "$@"

header 'Goals scored = 0 (home only) (first half)'
./fixtures.sh -E gfa_eq_0 --half first --minimum 4 --venue home "$@"

header 'Goals scored = 0 (away only) (first half)'
./fixtures.sh -E gfa_eq_0 --half first --minimum 4 --venue away "$@"

header 'Goals scored = 0 (second half)'
./fixtures.sh -E gfa_eq_0 --half second --minimum 3 "$@"

header 'Goals scored = 0 (home only) (second half)'
./fixtures.sh -E gfa_eq_0 --half second --minimum 4 --venue home "$@"

header 'Goals scored = 0 (away only) (second half)'
./fixtures.sh -E gfa_eq_0 --half second --minimum 4 --venue away "$@"

header 'Goals against = 0 (second half)'
./fixtures.sh -E ga_eq_0 --half second --minimum 7 "$@"

header 'Goals for = 0 (second half)'
./fixtures.sh -E gf_eq_0 --half second --minimum 7 "$@"

header 'Goals for = 0 (home only)'
./fixtures.sh -E gf_eq_0 --venue home --minimum 2 "$@"

header 'Goals for = 0 (away only)'
./fixtures.sh -E gf_eq_0 --venue away --minimum 3 "$@"

header 'Draws (first half)'
./fixtures.sh -E draw --half first --minimum 5 "$@"

header 'Draws (second half)'
./fixtures.sh -E draw --half second --minimum 4 "$@"

header 'Draws (consecutive halves)'
./fixtures.sh -E draw --half first second --minimum 5 "$@"

header 'Wins (consecutive halves)'
./fixtures.sh -E win --half first second --minimum 6 "$@"

header 'Losses (consecutive halves)'
./fixtures.sh -E loss --half first second --minimum 5 "$@"

header 'No draws (first half)'
./fixtures.sh -E draw -n --half first --minimum 7 "$@"

header 'No draws (consecutive halves)'
./fixtures.sh -E draw -n --half first second --minimum 12 "$@"

header 'Goals scored != 0 (first halves)'
./fixtures.sh -E gfa_ne_0 --half first --minimum 12 "$@"

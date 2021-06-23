#!/bin/bash

function header() {
	echo
	echo "$(tput bold)> $1 $(tput sgr0)"
}
	
header 'No draws/wins/losses'
./todays_fixtures.sh -E draw win defeat -n --minimum 10 "$@"
 
header 'Draws'
./todays_fixtures.sh -E draw --minimum 3 "$@"
 
header 'Goals for/against <= 0'
./todays_fixtures.sh -E gf_le_0 ga_le_0 --minimum 3 "$@"

header 'Goals scored <= 1'
./todays_fixtures.sh -E gfa_le_1 --minimum 4 "$@"

header 'Goals scored <= 2'
./todays_fixtures.sh -E gfa_le_2 --minimum 6 "$@"

header 'Wins or losses'
./todays_fixtures.sh -E win defeat --minimum 8 "$@"

header 'No BTS'
./todays_fixtures.sh -E bts -n --minimum 5 "$@"

header 'No draws away'
./todays_fixtures.sh -E draw -n --minimum 8 --venue away "$@"

header '0-0 draws'
./todays_fixtures.sh -E gfa_eq_0 "$@"

header 'No goals (consecutive halves)'
./todays_fixtures.sh -E gfa_eq_0 --half separate --minimum 2 "$@"

header 'No goals for (consecutive halves)'
./todays_fixtures.sh -E gf_eq_0 --half separate --minimum 5 "$@"

header 'Goals for/against <= 0 (first half)'
./todays_fixtures.sh -E gfa_eq_0 --half first --minimum 4 "$@"

header 'Goals for/against <= 0 (second half)'
./todays_fixtures.sh -E gfa_eq_0 --half second --minimum 3 "$@"

header 'Goals scored > 2'
./todays_fixtures.sh -E gfa_gt_2 --minimum 6 "$@"

header 'Draws (consecutive halves)'
./todays_fixtures.sh -E draw --half separate --minimum 6 "$@"


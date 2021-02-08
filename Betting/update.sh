#!/bin/bash
declare -i x=0
while read line
do
	if ((x >= 25)); then
		sleep 3700
		date
		x=0
	fi
	league_code=$line
	echo $league_code
	./update_fixtures.sh -L $league_code -v
	((x++))
done < "current_leagues.txt"


#!/bin/bash
while read line
do
	league_code=$line
	./analyse_sequences.sh -L $league_code -E "$@"
done < "current_leagues.txt"


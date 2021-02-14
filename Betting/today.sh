#!/bin/bash
timeout={$1:-5}
./todays_fixtures.sh -E draw win defeat -n --minimum 10
sleep $timeout
echo 
echo
./todays_fixtures.sh -E draw --minimum 3
sleep $timeout
echo 
echo
./todays_fixtures.sh -E gf_le_0 ga_le_0 --minimum 3
sleep $timeout
echo
echo
./todays_fixtures.sh -E gfa_le_1 --minimum 4
sleep $timeout
echo
echo
./todays_fixtures.sh -E win defeat --minimum 8
sleep $timeout
echo
echo
./todays_fixtures.sh -E bts -n --minimum 5
sleep $timeout
echo
echo
./todays_fixtures.sh -E bts --minimum 5
sleep $timeout
echo
echo
./todays_fixtures.sh -E draw win defeat --minimum 5 --half first
sleep $timeout
echo
echo
./todays_fixtures.sh -E draw win defeat --minimum 5 --half second
sleep $timeout
echo 
echo
./todays_fixtures.sh -E gfa_le_0 --minimum 8 --half first
sleep $timeout
echo 
echo
./todays_fixtures.sh -E gfa_le_0 --minimum 8 --half second
echo 
echo

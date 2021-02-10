#!/bin/bash
./todays_fixtures.sh -E draw win defeat -n --minimum 10
./todays_fixtures.sh -E draw --minimum 3
./todays_fixtures.sh -E win defeat --minimum 8
./todays_fixtures.sh -E more_than_0 -n --minimum 2
./todays_fixtures.sh -E goals_for goals_against -n --minimum 3
./todays_fixtures.sh -E bts -n --minimum 5


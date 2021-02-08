#!/bin/bash
./todays_fixtures.sh --database football.db -E draw win defeat -n --minimum 10
./todays_fixtures.sh --database football.db -E more_than_0 -n --minimum 2
./todays_fixtures.sh --database football.db -E goals_for goals_against -n --minimum 3


#!/bin/bash

date=$(date -v+1d +%Y-%m-%d)
year=${date:0:4}
month=${date:5:2}
day=${date:8:2}
./today.sh -Y $year -M $month -D $day -l 00 -u 08


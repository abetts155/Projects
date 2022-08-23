#!/bin/bash

./today.sh "$@" > output.txt 2> /dev/null
python3 prepare.py


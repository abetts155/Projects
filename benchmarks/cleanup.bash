#!/bin/sh


for file in *

do
        if [ -d "$file" ]; then
                cd "$file";
                rm -rf *.trc *.asm
                cd ..;
                continue
        fi
done

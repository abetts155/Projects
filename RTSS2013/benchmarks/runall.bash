#!/bin/bash
DEBUG=0
python ../src/ToolGem5.py binary_search.c -r binary_search
python ../src/ToolSuperBlocks.py binary_search.txt -d $DEBUG -u
python ../src/ToolGem5.py bubblesort.c -r bubblesort
python ../src/ToolSuperBlocks.py bubblesort.txt -d $DEBUG -u
python ../src/ToolGem5.py compress.c -r compress
python ../src/ToolSuperBlocks.py compress.txt -d $DEBUG -u
python ../src/ToolGem5.py cover.c -r cover
python ../src/ToolSuperBlocks.py cover.txt -d $DEBUG -u
python ../src/ToolGem5.py crc.c -r crc
python ../src/ToolSuperBlocks.py crc.txt -d $DEBUG -u
python ../src/ToolGem5.py duffsdevice.c -r duffsdevice
python ../src/ToolSuperBlocks.py duffsdevice.txt -d $DEBUG -u
python ../src/ToolGem5.py edn.c -r edn
python ../src/ToolSuperBlocks.py edn.txt -d $DEBUG -u
python ../src/ToolGem5.py embedded.c -r embedded
python ../src/ToolSuperBlocks.py embedded.txt -d $DEBUG -u
python ../src/ToolGem5.py exponential_integral.c -r exponential_integral
python ../src/ToolSuperBlocks.py exponential_integral.txt -d $DEBUG -u
python ../src/ToolGem5.py factorial.c -r factorial
python ../src/ToolSuperBlocks.py factorial.txt -d $DEBUG -u
python ../src/ToolGem5.py fastDiscreteCosineTransform.c -r fastDiscreteCosineTransform
python ../src/ToolSuperBlocks.py fastDiscreteCosineTransform.txt -d $DEBUG -u
python ../src/ToolGem5.py fft.c -r fft
python ../src/ToolSuperBlocks.py fft.txt -d $DEBUG -u
python ../src/ToolGem5.py fibonacci.c -r fibonacci
python ../src/ToolSuperBlocks.py fibonacci.txt -d $DEBUG -u
python ../src/ToolGem5.py finiteImpulseResponse.c -r finiteImpulseResponse
python ../src/ToolSuperBlocks.py finiteImpulseResponse.txt -d $DEBUG -u
python ../src/ToolGem5.py insertsort.c -r insertsort
python ../src/ToolSuperBlocks.py insertsort.txt  -d $DEBUG -u
python ../src/ToolGem5.py janne_complex.c -r janne_complex
python ../src/ToolSuperBlocks.py janne_complex.txt -d $DEBUG -u
python ../src/ToolGem5.py lcdnum.c -r lcdnum
python ../src/ToolSuperBlocks.py lcdnum.txt -d $DEBUG -u
python ../src/ToolGem5.py LUdecomposition.c -r LUdecomposition
python ../src/ToolSuperBlocks.py LUdecomposition.txt -d $DEBUG -u
python ../src/ToolGem5.py matrix_count.c -r matrix_count
python ../src/ToolSuperBlocks.py matrix_count.txt -d $DEBUG -u
python ../src/ToolGem5.py matrix_inverse.c -r matrix_inverse
python ../src/ToolSuperBlocks.py matrix_inverse.txt -d $DEBUG -u
python ../src/ToolGem5.py matrixmultiply.c -r matrixmultiply
python ../src/ToolSuperBlocks.py matrixmultiply.txt -d $DEBUG -u
python ../src/ToolGem5.py mergesort.c -r mergesort
python ../src/ToolSuperBlocks.py mergesort.txt -d $DEBUG -u
python ../src/ToolGem5.py petri.c -r petri
python ../src/ToolSuperBlocks.py petri.txt -d $DEBUG -u
python ../src/ToolGem5.py prime.c -r prime
python ../src/ToolSuperBlocks.py prime.txt -d $DEBUG -u
python ../src/ToolGem5.py quadraticroots.c -r quadraticroots
python ../src/ToolSuperBlocks.py quadraticroots.txt -d $DEBUG -u
python ../src/ToolGem5.py select.c -r select
python ../src/ToolSuperBlocks.py select.txt -d $DEBUG -u
python ../src/ToolGem5.py squareroot.c -r squareroot
python ../src/ToolSuperBlocks.py squareroot.txt -d $DEBUG -u
python ../src/ToolGem5.py statemate.c -r statemate
python ../src/ToolSuperBlocks.py statemate.txt -d $DEBUG -u
python ../src/ToolGem5.py statistics.c -r statistics
python ../src/ToolSuperBlocks.py statistics.txt -d $DEBUG -u



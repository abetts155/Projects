#!/bin/bash
DEBUG=0
python ../src/ToolGem5.py --ga binary_search.c -r binary_search --exclusive-size 1
python ../src/ToolGem5.py --ga bubblesort.c -r bubblesort  --exclusive-size 1
python ../src/ToolGem5.py --ga compress.c -r compress  --exclusive-size 1
python ../src/ToolGem5.py --ga cover.c -r cover  --exclusive-size 1
python ../src/ToolGem5.py --ga crc.c -r crc  --exclusive-size 1
python ../src/ToolGem5.py --ga edn.c -r edn  --exclusive-size 1
python ../src/ToolGem5.py --ga embedded.c -r embedded  --exclusive-size 1
python ../src/ToolGem5.py --ga exponential_integral.c -r exponential_integral  --exclusive-size 1
python ../src/ToolGem5.py --ga fastDiscreteCosineTransform.c -r fastDiscreteCosineTransform  --exclusive-size 1
python ../src/ToolGem5.py --ga fft.c -r fft  --exclusive-size 1
python ../src/ToolGem5.py --ga fibonacci.c -r fibonacci  --exclusive-size 1
python ../src/ToolGem5.py --ga finiteImpulseResponse.c -r finiteImpulseResponse  --exclusive-size 1
python ../src/ToolGem5.py --ga insertsort.c -r insertsort  --exclusive-size 1
python ../src/ToolGem5.py --ga janne_complex.c -r janne_complex  --exclusive-size 1
python ../src/ToolGem5.py --ga lcdnum.c -r lcdnum  --exclusive-size 1
python ../src/ToolGem5.py --ga LUdecomposition.c -r LUdecomposition  --exclusive-size 1
python ../src/ToolGem5.py --ga matrix_count.c -r matrix_count  --exclusive-size 1
python ../src/ToolGem5.py --ga matrix_inverse.c -r matrix_inverse  --exclusive-size 1
python ../src/ToolGem5.py --ga matrixmultiply.c -r matrixmultiply  --exclusive-size 1
python ../src/ToolGem5.py --ga mergesort.c -r mergesort  --exclusive-size 1
#python ../src/ToolGem5.py --ga petri.c -r petri  --exclusive-size 1
python ../src/ToolGem5.py --ga prime.c -r prime  --exclusive-size 1
python ../src/ToolGem5.py --ga quadraticroots.c -r quadraticroots  --exclusive-size 1
python ../src/ToolGem5.py --ga select.c -r select  --exclusive-size 1
python ../src/ToolGem5.py --ga squareroot.c -r squareroot  --exclusive-size 1
#python ../src/ToolGem5.py --ga statemate.c -r statemate  --exclusive-size 1
python ../src/ToolGem5.py --ga statistics.c -r statistics  --exclusive-size 1



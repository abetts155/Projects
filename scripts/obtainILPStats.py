#!/usr/bin/python2.6

import sys
from optparse import OptionParser
from subprocess import Popen, PIPE
from os import environ, sep, rename
from re import split, match
from math import ceil
from matplotlib.font_manager import fontManager, FontProperties
import matplotlib.pyplot as plt
from matplotlib.patches import Ellipse

# Add the 'src' directory to the module search and PYTHONPATH
sys.path.append(sys.path[0] + sep + "src")
from Debug import Debug
 
# The command-line parser and its options
parser = OptionParser(add_help_option=False)

parser.add_option("-d",
                  "--debug",
                  action="store_true",
                  dest="debug",
                  help="Debug mode.",
                  default=False)

parser.add_option("-h",
                  "--help",
                  action="help",
                  help="Display this help message.")

parser.add_option("-F",
                  "--file",
                  action="store",
                  type="string",
                  dest="fileName",
                  help="The file name into which to store results.",
                  metavar="<FILE>")

parser.add_option("-v",
                 "--verbose",
                 action="store_true",
                 dest="verbose",
                 help="Be verbose.",
                 default=False)

(opts, args) = parser.parse_args(sys.argv[1:])
debug = Debug(opts.verbose, opts.debug)

# Check that the user has passed the correct options
assert opts.fileName is not None, "You must supply a file name into where the results are stored"

def doPercentageReduction (val1, val2):
	print(val1, val2)
	
	diff = val2 - val1
	frac = diff/val1
	per = frac * 100
	print(per)

def collectResults ():
	cfgILPConstraints = 0
	sbcfgILPConstraints = 0

	cfgILPVariables = 0
	sbcfgILPVariables = 0

	cfgILPSolvingTime = 0
	sbcfgILPSolvingTime = 0

	f = open(opts.fileName, 'r')
	for line in f:
		line = line.strip()
		if line.startswith("#Vertices"):
			pass
		elif line.startswith("Constraints"):
			tokens = split("\\s", line)
			first = True
			for lexeme in tokens[1:]:
				if first:
					first = False
					cfgILPConstraints += float(lexeme)
				else:
					sbcfgILPConstraints += float(lexeme)
		elif line.startswith("Variables"):
			tokens = split("\\s", line)
			first = True
			for lexeme in tokens[1:]:
				if first:
					first = False
					cfgILPVariables += float(lexeme)
				else:
					sbcfgILPVariables += float(lexeme)
		elif line.startswith("Solving time"):
			tokens = split("\\s", line)
			first = True
			for lexeme in tokens[2:]:
				if first:
					first = False
					cfgILPSolvingTime += float(lexeme)
				else:
					sbcfgILPSolvingTime += float(lexeme)

	doPercentageReduction (cfgILPConstraints, sbcfgILPConstraints)
	doPercentageReduction (cfgILPVariables, sbcfgILPVariables)
	doPercentageReduction (cfgILPSolvingTime, sbcfgILPSolvingTime)

if __name__ == "__main__":
	collectResults ()

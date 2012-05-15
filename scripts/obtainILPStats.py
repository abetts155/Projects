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

def collectResults ():
	f = open(opts.fileName, 'r')
	for line in f:
		print (line)

if __name__ == "__main__":
	collectResults ()

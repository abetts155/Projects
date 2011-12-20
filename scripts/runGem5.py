#!/usr/bin/python2.6

from optparse import OptionParser
from sys import argv, maxint
from subprocess import Popen, PIPE
from os import environ, sep

# These environment variables are needed to simulate a program using gem5
gem5EnvironmentVariable = "GEM5_HOME"
try:
    environ[gem5EnvironmentVariable]
except KeyError:
    print ("Cannot find environment variable '" + var + "' which is needed to simulate the program using gem5.")
    exit(0)

# The command-line parser and its options
parser = OptionParser(add_help_option=False)

parser.add_option("-h",
                  "--help",
                  action="help",
                  help="Display this help message.")

parser.add_option("-p",
                  "--program",
                  action="store",
                  type="string",
                  dest="program",
                  help="The program to run under simulation",
                  metavar="<FILE>")

parser.add_option("-a",
                  "--arguments",
                  action="store",
                  type="string",
                  dest="arguments",
                  help="The command line arguments to pass to the program",
                  metavar="<arguments>")

parser.add_option("-o",
                  "--out-of-order",
                  action="store_true",
                  dest="outOfOrder",
                  help="Use out of order cpu",
                  default=False)

(opts, args) = parser.parse_args(argv[1:])

# Check that the user has passed the correct options
if opts.program is None:
    print("Missing option " + str(parser.get_option("-p")))
    exit(0)

def runCommand (cmd):
    proc = Popen(cmd,
                 shell=True,
                 executable="/bin/bash",
                 stderr=PIPE)
    proc.wait()

    if proc.returncode != 0:
        print("\nProblem running '" + cmd + "'")
        exit(0)

gem5Home = environ[gem5EnvironmentVariable]
gem5Bin = gem5Home + "/build/ARM_SE/m5.opt"
traceFlags = "--debug-flags=\"ExecEnable,ExecUser,ExecTicks\""
configScript = gem5Home + "/configs/example/se.py"

if opts.outOfOrder:
    runCommand("%s %s --trace-file=trace.out %s -c %s -o \"%s\" -d --caches"  % (gem5Bin, traceFlags, configScript, opts.program, opts.arguments))
else:
    runCommand("%s %s --trace-file=trace.out %s -c %s -o \"%s\""  % (gem5Bin, traceFlags, configScript, opts.program, opts.arguments))



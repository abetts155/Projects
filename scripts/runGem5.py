#!/usr/bin/python2.6

from optparse import OptionParser
from sys import argv, maxint
from subprocess import Popen, PIPE
from os import environ, sep

# These environment variables are needed to simulate a program using gem5
gem5EnvironmentVariable = "GEM5_HOME"
wcetToolsEnvironmentVariable = "WCET_HOME"
envVars = [gem5EnvironmentVariable, wcetToolsEnvironmentVariable]
for var in envVars:
    try:
        environ[var]
    except KeyError:
        print ("Cannot find environment variable '" + gem5EnvironmentVariable + "' which is needed to simulate the program using gem5.")
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

parser.add_option("-c",
                  "--config-file",
                  action="store",
                  type="string",
                  dest="config",
                  help="The file specifying the config flags to be used with the gem5 config script",
                  metavar="<FILE>")

(opts, args) = parser.parse_args(argv[1:])

# Check that the user has passed the correct options
if opts.program is None:
    print("Missing option " + str(parser.get_option("-p")))
    exit(0)
if opts.config is None:
    print("Missing option " + str(parser.get_option("-c")))
    exit(0)

def runCommand (cmd):
    proc = Popen(cmd,
                 shell=True,
                 executable="/bin/bash",
                 stderr=PIPE)
    proc.wait()
    stderrdata = proc.communicate()

    if proc.returncode != 0:
        print("\nProblem running '" + cmd + "'")
        print stderrdata
        exit(0)

gem5Home = environ[gem5EnvironmentVariable]
gem5Bin = gem5Home + "/build/ARM/gem5.opt"
traceFlags = "--debug-flags=\"ExecEnable,ExecUser,ExecTicks,ExecMicro\""

wcetHome = environ[wcetToolsEnvironmentVariable]
configScript = wcetHome + "/scripts/gem5Config/se.py"
traceParser = wcetHome + "/scripts/gem5TraceParser.py"

try:
    configFile = open(opts.config, 'r');
except IOError:
    print("Error - cannot open config file: " + opts.config)
    exit(1)

configFlags = ""
for configLine in configFile:
    configFlags += configLine.strip() + " "

runCommand("%s %s --trace-file=trace.out %s -c %s %s%s" \
            % (gem5Bin, traceFlags, configScript, opts.program, configFlags, opts.arguments))
#runCommand("python %s -p %s.xml -t m5out/trace.out -o blockTimings.out -H" % (traceParser, opts.program))



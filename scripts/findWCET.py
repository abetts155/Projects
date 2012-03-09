#!/usr/bin/python2.6

from optparse import OptionParser
from sys import argv, maxint
from subprocess import Popen, PIPE
from os import environ, sep
from os.path import join as joinpath
from re import split
from random import randint

gem5EnvironmentVariable = "GEM5_HOME"
wcetToolsEnvironmentVariable = "WCET_HOME"
envVars = [gem5EnvironmentVariable, wcetToolsEnvironmentVariable]
for var in envVars:
    try:
        environ[var]
    except KeyError:
        print ("Cannot find environment variable '" + gem5EnvironmentVariable + "' which is needed to simulate the program using gem5.")
        exit(0)
gem5Home = environ[gem5EnvironmentVariable]
gem5Binary = joinpath(gem5Home, "build", "ARM", "gem5.opt")
gem5TraceFlags = "--debug-flags=\"ExecEnable,ExecUser,ExecTicks,ExecMicro\""
gem5ConfigScript = joinpath(environ[wcetToolsEnvironmentVariable], "scripts", "gem5Config", "se.py")

worstCaseTime = -1.0
worstCaseVector = ""
bestCaseTime = -1.0
bestCaseVector = ""

# The command-line parser and its options
parser = OptionParser(add_help_option=False)

parser.add_option("-h",
                  "--help",
                  action="help",
                  help="Display this help message.")

parser.add_option("-d",
                  "--debug",
                  action="store_true",
                  dest="debug",
                  help="Debug mode.",
                  default=False)

parser.add_option("-r",
                  "--random",
                  action="store_true",
                  dest="rand",
                  help="Use random vector generation rather than systamatic",
                  default=False)

parser.add_option("-v",
                  "--rand-vectors",
                  action="store",
                  type="int",
                  dest="randVectors",
                  help="The number of random vectors to test",
                  metavar="<INT>")

parser.add_option("-p",
                  "--program",
                  action="store",
                  type="string",
                  dest="program",
                  help="The program to run under simulation",
                  metavar="<FILE>")

parser.add_option("-n",
                  "--parameters",
                  action="store",
                  type="int",
                  dest="numOfParameters",
                  help="The number of parameters the C program expects.",
                  metavar="<INT>")

parser.add_option("-u",
                  "--upper-bound",
                  action="store",
                  type="int",
                  dest="upperBound",
                  help="The upper bound of the program arguments",
                  default=maxint,
                  metavar="<INT>")

parser.add_option("-l",
                  "--lower-bound",
                  action="store",
                  type="int",
                  dest="lowerBound",
                  help="The lower bound of the program arguments",
                  default= -maxint - 1,
                  metavar="<INT>")

parser.add_option("-c",
                  "--cpu-type",
                  action="store",
                  type="string",
                  dest="cpuType",
                  help="Type of cpu to be used (atomic, timing, detailed, inorder)",
                  metavar="CPU_TYPE")

(opts, args) = parser.parse_args(argv[1:])

def checkOptions():
    if opts.program is None:
        print("Missing option " + str(parser.get_option("-p")))
        exit(0)
    if opts.numOfParameters is None:
        print("Missing option " + str(parser.get_option("-n")))
        exit(0)
    if opts.cpuType is None:
        print("Missing option " + str(parser.get_option("-c")))
        exit(0)
    if opts.rand and opts.randVectors is None:
        print("Missing option " + str(parser.get_option("-v")))
        exit(0)

def checkEnvironment():
    import os.path

    if not os.path.isfile(gem5Binary):
        print "Unable to find executable '" + gem5Binary + "'"
        exit(0)

    # Check the gem5 configuration file exists as well
    if not os.path.isfile(gem5ConfigScript):
        print "Unable to find file " + gem5configScript
        exit(0)

def initialise():
    worstCaseTime = -1
    worstCaseVector = ""
    bestCaseTime = -1
    bestCaseVector = ""

def testVector(vector):
    if opts.cpuType == "detailed" or opts.cpuType == "inorder":
        cmd = "%s %s --trace-file=trace.out %s -c %s --cpu-type=%s --caches -o \"%s" \
        % (gem5Binary, gem5TraceFlags, gem5ConfigScript, opts.program, opts.cpuType, vector) + "\""
    else:
        cmd = "%s %s --trace-file=trace.out %s -c %s --cpu-type=%s -o \"%s" \
        % (gem5Binary, gem5TraceFlags, gem5ConfigScript, opts.program, opts.cpuType, vector) + "\""

    if opts.debug:
        print("\nRunning '" + cmd + "'")

    # Run the binary on gem5
    proc = Popen(cmd,
                 shell=True,
                 executable="/bin/bash",
                 stderr=PIPE,
                 stdout=PIPE)
    # ..and get both standard output and standard error streams from gem5
    stoutdata, stderrdata = proc.communicate()

    # If a non-zero return code is detected then gem5 choked
    if proc.returncode != 0:
        print("\nProblem running " + cmd)
        print "\nProcess output:"
        for line in iter(stoutdata.splitlines()):
            print line
        for line in iter(stderrdata.splitlines()):
            print line
        exit(0)

    # Iterate through standard out until we find the tick count
    # (For the moment we ignore standard error as it provides nothing useful)
    it = iter(stoutdata.splitlines())
    found = False
    while not found:
        try:
            line = it.next()
            if line.startswith("Exiting @ tick"):
                tokens = split("\s+", line, 4)
                
                # The tick count is always the 4th token
                # This is the fitness criterion for a chromosone
                score = float(tokens[3])
                found = True
        except:
            raise StopIteration

    if opts.debug:
        print "Vector: " + str(vector) + "\nTime: " + str(score);

    global worstCaseTime, bestCaseTime
    global worstCaseVector, bestCaseVector
    if score > worstCaseTime:
        worstCaseTime = score
        worstCaseVector = vector
        print "New Worst : " + str(score) + " : " + vector
    if score < bestCaseTime or bestCaseTime == -1:
        bestCaseTime = score
        bestCaseVector = vector
        print "New Best : " + str(score) + " : " + vector

def appendToVector(i, vec):
    if vec == "":
        vector = str(i)
    else:
        vector = vec + " " + str(i)
    
    return vector

def systematicSearch(depth, preVector):
    if depth == opts.numOfParameters:
        for i in range(opts.lowerBound, opts.upperBound + 1):
            testVector(appendToVector(i, preVector))
    else:
        for i in range(opts.lowerBound, opts.upperBound + 1):
            systematicSearch(depth + 1, appendToVector(i, preVector))
            if depth == 1:
                fractionComplete = float((i + 1 - opts.lowerBound)) / float(opts.upperBound + 1 - opts.lowerBound);
                print "Percentage complete: " + str(int(fractionComplete * 100)) + "%"

def randomSearch():
    for i in range(opts.randVectors):
        vector = ""
        for j in range(opts.numOfParameters):
            vector = appendToVector(randint(opts.lowerBound, opts.upperBound), vector)
        testVector(vector)

def main():
    checkOptions()
    checkEnvironment()

    if opts.rand:
        randomSearch()
    else:
        systematicSearch(1, "")

if __name__ == "__main__":
    main()

    # Now write out the worst case vector
    print "\nWorst case vector: " + worstCaseVector
    print "Worst case time: " + str(worstCaseTime)
    print "\nBest case vector: " + bestCaseVector
    print "Best case time: " + str(bestCaseTime) + "\n"


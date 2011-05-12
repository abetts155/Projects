#!/usr/bin/python2.6

from optparse import OptionParser
from os import path, environ, pathsep
from os.path import isfile
from random import randint
from re import split
from subprocess import Popen, PIPE
from sys import argv, maxint
import gzip
import os

# The command-line parser and its options
parser = OptionParser(add_help_option=False)

parser.add_option("-c",
                  "--config",
                  action="store",
                  type="string",
                  dest="config",
                  help="The file used to configure SimpleScalar.",
                  metavar="<FILE>")

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

parser.add_option("-l",
                  "--lower-bound",
                  action="store",
                  type="int",
                  dest="lowerBound",
                  help="The lower bound on the range of the input space. [Default is %default].",
                  default= -maxint - 1,
                  metavar="<INT>")

parser.add_option("-n",
                  "--parameters",
                  action="store",
                  type="int",
                  dest="numOfParameters",
                  help="The number of parameters the C program expects.",
                  metavar="<INT>")

parser.add_option("-p",
                  "--program",
                  action="store",
                  type="string",
                  dest="program",
                  help="The binary of the program under analysis.",
                  metavar="<FILE>")

parser.add_option("-r",
                  "--root",
                  action="store",
                  type="string",
                  dest="root",
                  help="Entry point of the program.",
                  metavar="<NAME>")

parser.add_option("-s",
                  "--simplescalar",
                  action="store",
                  type="string",
                  dest="simplescalar",
                  help="The SimpleScalar binary used to run the simulation.",
                  metavar="<PATH>")

parser.add_option("-t",
                  "--tests",
                  action="store",
                  type="int",
                  dest="tests",
                  help="The number of random test vectors to be generated.",
                  default=1,
                  metavar="<INT>")

parser.add_option("-u",
                  "--upper-bound",
                  action="store",
                  type="int",
                  dest="upperBound",
                  help="The upper bound on the range of the input space. [Default is %default].",
                  default=maxint,
                  metavar="<INT>")

parser.add_option("-v",
                 "--verbose",
                 action="store_true",
                 dest="verbose",
                 help="Be verbose.",
                 default=False)

(opts, args) = parser.parse_args(argv[1:])

if opts.numOfParameters is None:
    print("Missing option " + str(parser.get_option("-n")))
    exit(0)
elif opts.program is None:
    print("Missing option " + str(parser.get_option("-p")))
    exit(0)
elif opts.config is None:
    print("Missing option " + str(parser.get_option("-c")))
    exit(0)
elif opts.simplescalar is None:
    print("Missing option " + str(parser.get_option("-s")))
    exit(0)

if opts.debug:
    print("SimpleScalar = " + opts.simplescalar)
    print("Program = " + opts.program)
    print("Lower bound = " + str(opts.lowerBound))
    print("Upper bound = " + str(opts.upperBound))

# The set of generated TVs
TVs = []

# The XML file describing the program structure
xmlFile = opts.program + ".xml"

# The name of the Java utility designed to strip the SimpleScalar trace into
# more digestible format
simpleScalarJar = "simplescalar.jar"

# The names of the instrumentation profiles
BASIC_BLOCK = "BASIC_BLOCK"
PRE_DOMINATOR = "PRE_DOMINATOR"
BRANCH = "BRANCH"
SUPER_BLOCK = "SUPER_BLOCK"

# In order to produce trace files for different instrumentation profiles
# the program structure file must exist, the root function supplied on
# the command line, and the SimpleScalar Java utility must be on the path.
# Check that all of these conditions are met before opening various trace
# files where the traces produced during execution will be dumped
dumpTraces = True
if opts.root is None:
    if opts.debug:
        print("No root function specified")
    dumpTraces = False
elif not os.path.exists(xmlFile):
    if opts.debug:
        print("Unable to find program file " + xmlFile)
    dumpTraces = False
else:
    it = iter(os.environ["PATH"].split(os.pathsep))
    stop = False
    while not stop:
        try:
            path = it.next()
            file = os.path.join(path, simpleScalarJar)
            if isfile(file) and os.access(file, os.X_OK):
                stop = True
                print("Found executable at " + file);
        except:
            # The iterator has reached its end and we did not find the executable
            stop = True
            dumpTraces = False
            print("Unable to find " + simpleScalarJar)

if dumpTraces:
    # Higher compression rate for the basic block trace as it is expected to be larger
    bbTrace = gzip.GzipFile(filename="trace." + BASIC_BLOCK + ".random.gz",
                            mode="wb",
                            compresslevel=9)
#    branchTrace = gzip.GzipFile(filename="trace." + BRANCH + ".random.gz",
#                                mode="wb",
#                                compresslevel=7)
#    predomTrace = gzip.GzipFile(filename="trace." + PRE_DOMINATOR + ".random.gz",
#                                mode="wb",
#                                compresslevel=7)

def dumpTrace ():
    cmd = "%s -p %s -r %s -i %s -t %s.trc" \
     % (simpleScalarJar, xmlFile, opts.root, BASIC_BLOCK, opts.program)

    if opts.debug:
        print("Running '" + cmd + "'")

    proc = Popen(cmd,
                 shell=True,
                 executable="/bin/bash",
                 stderr=PIPE)
    proc.wait()

    if proc.returncode != 0:
        print("\nProblem running '" + cmd + "'")
        exit(0)

    f = open(opts.program + "." + BASIC_BLOCK + ".txt", "r")
    lines = f.readlines()
    f.close()
    for line in lines:
        bbTrace.write(line)

#    f = open(opts.program + "." + BRANCH + ".txt", "r")
#    lines = f.readlines()
#    f.close()
#    for line in lines:
#        branchTrace.write(line)
#
#    f = open(opts.program + "." + PRE_DOMINATOR + ".txt", "r")
#    lines = f.readlines()
#    f.close()
#    for line in lines:
#        predomTrace.write(line)

def main ():
    for i in range(opts.tests):
        newTV = []
        for j in range(opts.numOfParameters):
            newTV.append(randint(opts.lowerBound,opts.upperBound))

        if opts.debug:
            print("TV = %s" % (newTV))
        cmd = "%s -config %s -ptrace %s.trc : %s " \
                % (opts.simplescalar, opts.config, opts.program, opts.program) + \
                ' '.join(map(str, newTV))

        # Run the binary on Simplescalar
        proc = Popen(cmd,
                     shell=True,
                     executable="/bin/bash",
                     stderr=PIPE,
                     stdout=PIPE)
        proc.wait()

        # If a non-zero return code is detected then SimpleScalar choked
        if proc.returncode != 0:
            print("\nProblem running " + cmd)
            exit(0)

        # Remember the TV so it can be written later
        TVs.append(newTV)
        # Dump the sanitised trace to the trace files
        if dumpTraces and opts.simplescalar:
            #TVstr = "// TV = " + str(newTV) + "\n"
            #bbTrace.write(TVstr)
            #branchTrace.write(TVstr)
            #predomTrace.write(TVstr)
            dumpTrace()

if __name__ == "__main__":
    main()

    # Now write out the test vectors generated to a file
    f = open("TVs_Random.txt", "w")
    for tv in TVs:
        f.write(' '.join(map(str, tv)) + "\n")
    f.close()

# Final action is to close the file handles and remove temporary files
if dumpTraces:
    bbTrace.close()
#    branchTrace.close()
#    predomTrace.close()
    
    cmd = "rm -f %s.txt %s.trc" \
    % (opts.program + "." + BASIC_BLOCK, opts.program)
    
    proc = Popen(cmd,
                 shell=True,
                 executable="/bin/bash",
                 stderr=PIPE)
    proc.wait()

    if proc.returncode != 0:
        print("\nProblem running '" + cmd + "'")
        exit(0)

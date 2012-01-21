#!/usr/bin/python2.6

from optparse import OptionParser
from sys import argv, maxint
from subprocess import Popen, PIPE
from os import environ, sep

# These environment variables are needed to compile and disassemble the program under analysis 
wcetToolsEnvironmentVariable = "WCET_HOME"
for var in [wcetToolsEnvironmentVariable]:
    try:
        environ[var]
    except KeyError:
        print ("Cannot find environment variable '" + var + "' which is needed to compile the program.")
        exit(0)
 
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

parser.add_option("-p",
                  "--program",
                  action="store",
                  type="string",
                  dest="program",
                  help="The C program under analysis.",
                  metavar="<FILE>")

parser.add_option("-r",
                  "--root",
                  action="store",
                  type="string",
                  dest="root",
                  help="Entry point of the program.",
                  metavar="<NAME>")

parser.add_option("-g",
                  "--gem5sim",
                  action="store_true",
                  dest="gem5sim",
                  help="Use gem5 Simulator",
                  default=False)

parser.add_option("-v",
                 "--verbose",
                 action="store_true",
                 dest="verbose",
                 help="Be verbose.",
                 default=False)

(opts, args) = parser.parse_args(argv[1:])

# Check that the user has passed the correct options
if opts.program is None:
    print("Missing option " + str(parser.get_option("-p")))
    exit(0)
if opts.root is None:
    print("Missing option " + str(parser.get_option("-r")))
    exit(0)

def runCommand (cmd):
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

rootPath         = environ[wcetToolsEnvironmentVariable]
simpleScalarPath = rootPath + sep + "simplescalar" + sep + "bin"
gcc              = simpleScalarPath + sep + "sslittle-na-sstrix-gcc"
objdump          = simpleScalarPath + sep + "sslittle-na-sstrix-objdump"
disassembler     = "java -jar " + rootPath + sep + "bin" + sep + "disassemble.jar"

if opts.gem5sim:
    armToolchainVar = "ARM_GCC_TOOLCHAIN"
    try:
        armToolchain = environ[armToolchainVar]
    except KeyError:
        print ("Cannot find environment variable '" + armToolchainVar + "' which is needed to compile the program for m5.")
        exit(0)

    gcc     = armToolchain + "-gcc -static"
    objdump = armToolchain + "-objdump"

runCommand("%s -O2 -o %s %s"  % (gcc, opts.program[:-2], opts.program))
runCommand("%s -d -j .text %s > %s.asm"  % (objdump, opts.program[:-2], opts.program[:-2]))
runCommand("%s -p %s.asm -r %s"  % (disassembler, opts.program[:-2], opts.root))

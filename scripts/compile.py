#!/usr/bin/python2.6

import sys
from optparse import OptionParser
from subprocess import Popen, PIPE
from os import environ, sep

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

parser.add_option("-f",
                  "--compiler-flags",
                  action="store",
                  type="string",
                  dest="compFlags",
                  help="Any compiler flags to be used e.g. -O0/1/2, -lm etc. \
						(surround text with quotes e.g. \"-O2 -lm\")",
		  default="",	
                  metavar="<NAME>")

parser.add_option("-g",
                  "--gem5sim",
                  action="store_true",
                  dest="gem5",
                  help="Use gem5 Simulator",
                  default=False)

parser.add_option("-v",
                 "--verbose",
                 action="store_true",
                 dest="verbose",
                 help="Be verbose.",
                 default=False)

(opts, args) = parser.parse_args(sys.argv[1:])
debug = Debug(opts.verbose, opts.debug)

def checkOptions ():
	# Check that the user has passed the correct options
	if opts.program is None:
	    debug.exitMessage("Missing option " + str(parser.get_option("-p")))
	if opts.root is None:
	    debug.exitMessage("Missing option " + str(parser.get_option("-r")))

def checkEnvironment (wcetHome):
	# Check these environment variables exist
	try:
		environ[wcetHome]
	except KeyError:
		debug.exitMessage ("Cannot find environment variable '" + wcetHome + "' which is needed to compile the program.")

	if opts.gem5:
		armHome = "ARM_GCC_TOOLCHAIN"
		try:
			environ[armHome]
		except KeyError:
			debug.exitMessage ("Cannot find environment variable '" + armHome + "' which is needed to compile the program for gem5.")

def runCommand (cmd):
    debug.debugMessage("Running '" + cmd + "'")
    proc = Popen(cmd,
                 shell=True,
                 executable="/bin/bash",
                 stderr=PIPE)
    stoutdata, stderrdata = proc.communicate()
    proc.wait()

    for line in stderrdata.splitlines():
        print line

    if proc.returncode != 0:
        debug.exitMessage("\nProblem running '" + cmd + "'")

def run (wcetHome):
	gccFlags     = opts.compFlags
	gcc          = None
	objdump      = None
	disassembler = "java -jar " + environ[wcetHome] + sep + "bin" + sep + "disassemble.jar"

	if opts.gem5:
		gccFlags += " -static"
		gcc       = "arm-linux-gnueabi-gcc"
    		objdump   = "arm-linux-gnueabi-objdump"
	else:
		simpleScalarPath = environ[wcetHome] + sep + "simplescalar" + sep + "bin"
		gcc              = simpleScalarPath + sep + "sslittle-na-sstrix-gcc"
		objdump          = simpleScalarPath + sep + "sslittle-na-sstrix-objdump"

	runCommand("%s -o %s %s %s"  % (gcc, opts.program[:-2], opts.program, gccFlags))
	runCommand("%s -d -j .text %s > %s.asm"  % (objdump, opts.program[:-2], opts.program[:-2]))
	runCommand("%s -p %s.asm -r %s"  % (disassembler, opts.program[:-2], opts.root))
		
if __name__ == "__main__":
	wcetHome = "WCET_HOME"
	checkOptions ()
	checkEnvironment (wcetHome)
	run (wcetHome)

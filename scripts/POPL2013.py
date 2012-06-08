#!/usr/bin/python2.6

import sys
from optparse import OptionParser
from subprocess import Popen, PIPE
from os import environ, sep, rename
from re import split, match
from math import ceil

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

parser.add_option("-C",
                  "--cfg-variations",
                  action="store",
                  type="int",
                  dest="variations",
                  help="The number of CFG variants to generate for a fixed size of vertices. [Default is %default].",
                  default=1,
                  metavar="<INT>")

parser.add_option("--max-vertices",
                  action="store",
                  type="int",
                  dest="maxVertices",
                  help="Maximum number of vertices in a CFG. [Default is %default].",
                  default=1000,
                  metavar="<INT>")

parser.add_option("--min-vertices",
                  action="store",
                  type="int",
                  dest="minVertices",
                  help="Minimum number of vertices in a CFG. [Default is %default].",
                  default=10,
                  metavar="<INT>")

parser.add_option("-L",
                  "--loops",
                  action="store_true",
                  dest="loops",
                  help="Allow loops in the CFG.",
                  default=False)

parser.add_option("-v",
                 "--verbose",
                 action="store_true",
                 dest="verbose",
                 help="Be verbose.",
                 default=False)

(opts, args) = parser.parse_args(sys.argv[1:])
debug = Debug(opts.verbose, opts.debug)

# Check that the user has passed the correct options
assert opts.minVertices >= 10, "CFGs with less than 10 vertices not supported"

topLevelDir = "experiments"
			
def run ():
	from time import ctime
	from os import chdir, getcwd
	from random import randint

	# These environment variables are needed to compile and disassemble the program under analysis 
	WCET_HOME = "WCET_HOME"
	for var in [WCET_HOME]:
    		try:
        		environ[var]
    		except KeyError:
       		 	debug.exitMessage ("Cannot find environment variable '" + var + "' which is needed to run the simulation.")
	
	chdir(topLevelDir)

	javaPrefix = "java -ea -jar -Xss4m "
	for numOfVertices in range(opts.minVertices, opts.maxVertices + 1):
		debug.verboseMessage("Now generating CFG with " + str(numOfVertices) + " vertices")
		for variation in range(1, opts.variations + 1):
			debug.verboseMessage("Variation #" + str(variation) + " " + ctime())
			programFileName = "program" + str(numOfVertices) + ".xml"
			cmd1 = javaPrefix + environ[WCET_HOME] + sep + "bin" + sep + "program-generator.jar -s 1 -F 6 -V " + str(numOfVertices) + " -o " + programFileName

			if opts.loops:
				numOfLoops   = randint(1, 10)
				nestingLevel = 1
				if numOfLoops > 1:
					nestingLevel = randint(1, numOfLoops - 1)
				cmd1 += " -l " + str(numOfLoops) + " -L " + str(nestingLevel)

			debug.debugMessage("Running '" + cmd1 + "'")
			proc1 = Popen(cmd1, shell=True, executable="/bin/bash", stderr=PIPE, stdout=PIPE)
			stdoutdata, stderrdata = proc1.communicate()
			if proc1.returncode:
				debug.exitMessage("Problem running " + cmd1)

			cmd2 = javaPrefix + environ[WCET_HOME] + sep + "bin" + sep + "pst.jar -p " + programFileName
			debug.debugMessage("Running '" + cmd2 + "'")
			proc2 = Popen(cmd2, shell=True, executable="/bin/bash", stderr=PIPE, stdout=PIPE)
			stdoutdata, stderrdata = proc2.communicate()
			if proc2.returncode:
				debug.exitMessage("Problem running " + cmd2)

def createDirectoryStructure ():
	from os import path, makedirs 
	
	if not path.exists(topLevelDir):
		if path.isdir(topLevelDir):
			debug.exitMessage("A file called %s exists which clashes with the directory name I want to create" % topLevelDir)
		else:
			makedirs("experiments")
		
if __name__ == "__main__":
	createDirectoryStructure ()
	run ()

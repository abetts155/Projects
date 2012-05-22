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
                  help="The CUDA program under analysis.",
                  metavar="<FILE>")

parser.add_option("-r",
                  "--root",
                  action="store",
                  type="string",
                  dest="root",
                  help="Entry point of the program.",
                  metavar="<NAME>")

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

def checkEnvironment (wcetHome):
	# Check these environment variables exist
	try:
		environ[wcetHome]
	except KeyError:
		debug.exitMessage ("Cannot find environment variable '" + wcetHome + "' which is needed to compile the program.")

def runCommand (cmd):
    debug.verboseMessage("Running '" + cmd + "'")
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
	runCommand("nvcc -ptx %s"  % (opts.program))

	ptxAnalyser    = "java -ea -jar " + environ[wcetHome] + sep + "bin" + sep + "ptx-analyser.jar"
	indexOfFileExt = opts.program.find('.')
	runCommand("%s -p %s.ptx -o %s.xml"  % (ptxAnalyser, opts.program[:indexOfFileExt], opts.program[:indexOfFileExt]))
		
if __name__ == "__main__":
	wcetHome = "WCET_HOME"
	checkOptions ()
	checkEnvironment (wcetHome)
	run (wcetHome)

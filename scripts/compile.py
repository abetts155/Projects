#!/usr/bin/python2.6

from optparse import OptionParser
from sys import argv, maxint
from subprocess import Popen, PIPE

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

parser.add_option("-v",
                 "--verbose",
                 action="store_true",
                 dest="verbose",
                 help="Be verbose.",
                 default=False)

(opts, args) = parser.parse_args(argv[1:])

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

if opts.program is None:
    print("Missing option " + str(parser.get_option("-p")))
    exit(0)
if opts.root is None:
    print("Missing option " + str(parser.get_option("-r")))
    exit(0)

compiler     = "sslittle-na-sstrix-gcc"
objdump      = "sslittle-na-sstrix-objdump"
disassembler = "java -jar /home/abetts/workspace/MDH/jar/disassemble.jar"

runCommand("%s -O2 -o %s %s"  % (compiler, opts.program[:-2], opts.program))
runCommand("%s -d -j .text %s > %s.asm"  % (objdump, opts.program[:-2], opts.program[:-2]))
runCommand("%s -p %s.asm -r %s -f XML"  % (disassembler, opts.program[:-2], opts.root))
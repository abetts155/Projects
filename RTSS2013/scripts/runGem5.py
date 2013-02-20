#!/usr/bin/python2.6

import sys, os

# Add parent directory to the module search
parentdir = os.path.dirname(sys.path[0])
sys.path.append(parentdir)

from optparse import OptionParser
from distutils.spawn import find_executable
import src.Debug as Debug
import src.UDrawGraph as UDrawGraph

# The command-line parser and its options
cmdline = OptionParser(add_help_option=False)

cmdline.add_option("-h",
                  "--help",
                  action="help",
                  help="Display this help message.")

cmdline.add_option("-C",
                  "--compile",
                  action="store_true",
                  dest="compile",
                  help="Compile the given program and then simulate. [The program must have a '.c' extension].",
                  default=False)

cmdline.add_option("-d",
                  "--debug",
                  action="store",
                  dest="debug",
                  type="int",
                  help="Debug mode.",
                  default=0)

cmdline.add_option("-u",
                 "--udraw",
                 action="store_true",
                 dest="udraw",
                 help="Generate uDrawGraph files.",
                 default=False)

(opts, args)       = cmdline.parse_args(sys.argv[1:])
Debug.debug        = opts.debug
UDrawGraph.enabled = opts.udraw
armGCC       = "arm-linux-gnueabi-gcc"
armObjdump   = "arm-linux-gnueabi-objdump"

def extractCFGs (disassembly):
    from src.ARM import readARMDisassembly
    readARMDisassembly (disassembly)
    
def runGem5 (gem5base, armSimulator, binary):
    from subprocess import Popen, PIPE
    gem5ConfigFile = gem5base + os.sep + 'configs' + os.sep + 'example' + os.sep + 'se.py'
    assert os.path.exists(gem5ConfigFile), sys.exit("The gem5 configuration file '%s' does not exist" % gem5ConfigFile)
    traceFile  = os.path.basename(binary) + ".trace"
    cmd        = "%s --debug-flags=Fetch --trace-file=%s %s -c %s" % (armSimulator, traceFile, gem5ConfigFile, binary)
    proc       = Popen(cmd, shell=True, stdout=PIPE, stderr=PIPE)    
    returncode = proc.wait()
    if returncode:
        sys.exit("Running '%s' failed" % cmd)
        
def disassembleProgram (binary):
    from subprocess import Popen
    filename = binary + ".dis"
    with open(filename, 'w') as disassembly:
        cmd        = "%s %s -d" % (armObjdump, binary)
        proc       = Popen(cmd, shell=True, stdout=disassembly, stderr=sys.stderr)    
        returncode = proc.wait()
        if returncode:
            sys.exit("Disassembling '%s' failed" % binary) 
    return filename
        
def compileProgram (program):
    from subprocess import Popen
    binary     = program[:-2]
    cmd        = "%s -static %s -o %s" % (armGCC, program, binary)
    proc       = Popen(cmd, shell=True, stdout=sys.stdout, stderr=sys.stderr)    
    returncode = proc.wait()
    if returncode:
        sys.exit("Compiling '%s' failed" % program)
    return binary  

def checkArguments ():
    if len(args) == 1:
        program = args[0]
        if not os.path.exists(program):
            sys.exit("The argument '%s' does not exist" % program)
        elif not os.path.isfile(program):
            sys.exit("The argument '%s' is not a file" % program)
        else:
            if not opts.compile:
                if not os.access(program, os.X_OK):
                    sys.exit("The argument '%s' does not have execute permissions" % program)
            else:
                ext  = os.path.splitext(program)[1]
                cExt = '.c'
                if ext != cExt:
                    sys.exit("Unable to compile '%s' because its extension is not '%s'" % (program, cExt))
                if not find_executable(armGCC):
                    sys.exit("Unable to find ARM GCC cross compiler '%s' on your path" % armGCC)
                if not find_executable(armObjdump):
                    sys.exit("Unable to find ARM GCC object dump facility '%s' on your path" % armObjdump)
        return os.path.abspath(program)
    else:
        sys.exit("Only provide a single argument to the script: the name of the binary or the program to compile")
        
def checkEnvironment ():
    # Need a gem5 environment variable 
    gem5Home = "GEM5_HOME"
    try:
        path = os.environ[gem5Home]
        if not os.path.exists(os.path.abspath(path)):
            sys.exit("Your gem5 base directory '%s' does not exist" % path)
        armSimulator = path + os.sep + 'build' + os.sep + 'ARM' + os.sep + 'gem5.opt'
        if not os.path.exists(armSimulator):
            sys.exit("""Unable to find '%s' in your gem5 distribution, which is the optimised ARM configuration of gem5. 
Ensure that you have built this version using 'scons ARM/build/gem5.opt' in '%s'""" \
                     % (armSimulator, path))
        return path, armSimulator
    except KeyError:
        sys.exit ("You need to set environment variable '%s' to simulate the program using gem5" % gem5Home)
        
if __name__ == "__main__":
    gem5base, armSimulator = checkEnvironment()
    program                = checkArguments()
    binary                 = program
    if opts.compile:
        binary      = compileProgram(program)
        disassembly = disassembleProgram(binary)
        extractCFGs(disassembly)
    runGem5(gem5base, armSimulator, binary)


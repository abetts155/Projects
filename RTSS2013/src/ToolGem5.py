#!/usr/bin/python2.6

import Debug, UDrawGraph
import sys, os
from optparse import OptionParser
from distutils.spawn import find_executable

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
                  metavar="<INT>",
                  help="Debug mode.",
                  default=0)

cmdline.add_option("-T",
                  "--number-of-tests",
                  action="store",
                  type="int",
                  dest="tests",
                  help="The number of times to run the application. [Default is %default].",
                  default=1,
                  metavar="<INT>")

cmdline.add_option("-r",
                  "--root",
                  action="store",
                  type="string",
                  dest="root",
                  help="The function that is the entry point of the analysis. [This should not be 'main'].",
                  metavar="<STRING>")

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

def runGem5 (gem5base, armSimulator, binary, testSpecification):
    from TestHarness import RandomGeneration
    from subprocess import Popen, PIPE
    gem5ConfigFile  = gem5base + os.sep + 'configs' + os.sep + 'example' + os.sep + 'se.py'
    assert os.path.exists(gem5ConfigFile), sys.exit("The gem5 configuration file '%s' does not exist" % gem5ConfigFile)
    # Now run the program n times
    randomTVs  = RandomGeneration(testSpecification)
    gem5Traces = []
    for i in xrange(1,opts.tests+1):
        nextTV     = randomTVs.nextTestVector()
        traceFile  = "%s.%s.%d" % (os.path.basename(binary), "trace", i)
        cmd        = '%s --debug-flags=Fetch --trace-file=%s %s -c %s -o "%s"' % (armSimulator, traceFile, gem5ConfigFile, binary, nextTV)
        Debug.debugMessage("Running '%s' on gem5" % cmd, 1)
        proc       = Popen(cmd, shell=True, stdout=PIPE, stderr=PIPE)    
        returncode = proc.wait()
        if returncode:
            sys.exit("Running '%s' failed" % cmd)
        gem5Trace = os.path.abspath(os.getcwd()) + os.sep + 'm5out' + os.sep + traceFile
        assert os.path.exists(gem5Trace), "Expected to find gem5 trace in '%s' but it is not there" % gem5Trace
        gem5Traces.append(gem5Trace)
    return gem5Traces
    
def getTestSpecification (binary):
    import TestHarness
    programTestFile = binary + '.test'
    assert os.path.exists(programTestFile), "Expected to find the test specification file '%s' but it is not there" % programTestFile
    basetype = None
    length   = None
    lower    = None
    upper    = None
    with open(programTestFile, 'r') as f:
        for line in f:
            index = line.find('=')
            if index == -1:
                sys.exit("Found an invalid line '%s' in the test specification file" % line)
            else:
                lhs = line[:index].strip()
                rhs = line[index+1:].strip()
                if lhs.lower() == 'type':
                    basetype = rhs
                elif lhs.lower() == 'length':
                    try:
                        length = int(rhs)
                    except:
                        sys.exit("The length of the test vector must be a non-negative integer. It is %s." % rhs) 
                elif lhs.lower() == 'lower':
                    try:
                        lower = int(rhs)
                    except:
                        sys.exit("The lower bound on the range of elements in the test vector must be an integer. It is %s." % rhs) 
                elif lhs.lower() == 'upper':
                    try:
                        upper = int(rhs)
                    except:
                        sys.exit("The upper bound on the range of elements in the test vector must be an integer. It is %s." % rhs) 
                else:
                    sys.exit("Do not understand the line '%s' in the test specification file" % line)   
    return TestHarness.TestVectorProperties(length, basetype, lower, upper)
        
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
    if len(args) > 0:
        program = args[0]
        if not os.path.exists(program):
            sys.exit("The first command-line argument must be a file: '%s' does not exist." % program)
        elif not os.path.isfile(program):
            sys.exit("The first command-line argument must be a file: '%s' is not a file" % program)
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
    from ARM import readARMDisassembly
    from ParseProgramFile import createProgram
    from ParseGem5Trace import parse
    gem5base, armSimulator = checkEnvironment()
    filename               = checkArguments()
    binary                 = filename
    if opts.compile:
        if not opts.root:
            sys.exit("To compile and analyse a C file, you must supply a root function via -r.")
        binary      = compileProgram(filename)
        disassembly = disassembleProgram(binary)
        program     = readARMDisassembly (disassembly, opts.root)
    else:
        programFile = binary + '.txt'
        assert os.path.exists(programFile), "Expected to find file with program information in '%s' but it is not there" % programFile
        program     = createProgram(programFile)
    testSpecification = getTestSpecification(binary)
    gem5Traces        = runGem5(gem5base, armSimulator, binary, testSpecification)
    parse(program, gem5Traces)

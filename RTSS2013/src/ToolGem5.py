#!/usr/bin/python2.6

import Debug
import sys, os
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter, ArgumentTypeError
from distutils.spawn import find_executable

def commaSeparatedList (s):
    try:
        return s.split(',')
    except:
        raise ArgumentTypeError("Invalid compiler flags")

# The command-line parser and its options
cmdline = ArgumentParser(formatter_class=ArgumentDefaultsHelpFormatter,
                         description="Run C programs on gem5 and analyse traces")

cmdline.add_argument("program",
                     help="Either a program to compile (with '.c.' extension) or a pre-compiled binary.",
                     nargs='?')

cmdline.add_argument("--compiler-flags",
                      type=commaSeparatedList,
                      help="flags to be passed to the compiler",
                      dest="flags",
                      metavar="<FLAGS>")

cmdline.add_argument("--clean",
                     action="store_true",
                     help="Clean out temporary files.",
                     default=False)

cmdline.add_argument("-d",
                      "--debug",
                      action="store",
                      type=int,
                      help="debug mode",
                      metavar="<INT>",
                      default=0)

cmdline.add_argument("-r",
                      "--root",
                      action="store",
                      help="the function that is the entry point of the analysis. [This should not be 'main']",
                      metavar="<FUNCTION>")

cmdline.add_argument("-T",
                      "--number-of-tests",
                      action="store",
                      type=int,
                      dest="tests",
                      help="the number of times to run the application",
                      metavar="<INT>",
                      default=1)

cmdline.add_argument("-v",
                     "--verbose",
                     action="store_true",
                     help="Be verbose.",
                     default=False)

args          = cmdline.parse_args()
Debug.debug   = args.debug
Debug.verbose = args.verbose
armGCC        = "arm-linux-gnueabi-gcc"
armObjdump    = "arm-linux-gnueabi-objdump"

def runGem5 (gem5base, armSimulator, binary, testSpecification):
    from TestHarness import RandomGeneration
    from subprocess import Popen, PIPE
    gem5ConfigFile  = gem5base + os.sep + 'configs' + os.sep + 'example' + os.sep + 'se.py'
    assert os.path.exists(gem5ConfigFile), sys.exit("The gem5 configuration file '%s' does not exist" % gem5ConfigFile)
    # Now run the program n times
    randomTVs  = RandomGeneration(testSpecification)
    gem5Traces = []
    for i in xrange(1, args.tests+1):
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
    extraFlags = ""
    if args.flags:
        for flag in args.flags:
            extraFlags += "-%s " % flag
    binary     = program[:-2]
    cmd        = "%s -static %s -o %s %s" % (armGCC, program, binary, extraFlags)
    proc       = Popen(cmd, shell=True, stdout=sys.stdout, stderr=sys.stderr)    
    returncode = proc.wait()
    if returncode:
        sys.exit("Compiling '%s' with '%s' failed" % (program, cmd))
    return binary  

def checkArguments ():
    from ParseProgramFile import createProgram
    program = os.path.abspath(args.program)
    if not os.path.exists(program):
        sys.exit("The first command-line argument must be a file: '%s' does not exist." % program)
    elif not os.path.isfile(program):
        sys.exit("The first command-line argument must be a file: '%s' is not a file" % program)
    else:
        ext  = os.path.splitext(program)[1]
        cExt = '.c'
        if ext:
            if ext == cExt:
                if not find_executable(armGCC):
                    sys.exit("Unable to find ARM GCC cross compiler '%s' on your path" % armGCC)
                if not find_executable(armObjdump):
                    sys.exit("Unable to find ARM GCC object dump facility '%s' on your path" % armObjdump)
                if not args.root:
                    sys.exit("To compile and analyse a C file, you must supply a root function via -r.")
                program     = compileProgram(program)
                disassembly = disassembleProgram(program)
                return program, readARMDisassembly(disassembly, args.root)
            else:
                sys.exit("Unable to compile '%s' because its extension is not '%s'" % (program, cExt))
        else:
            if not os.access(program, os.X_OK):
                sys.exit("The argument '%s' does not have execute permissions" % program)
            programFile = program + '.txt'
            assert os.path.exists(programFile), "Expected to find file with program information in '%s' but it is not there" % programFile
            return program, createProgram(programFile)
        
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
        
def clean (abspath):
    import shutil
    for paths, dirs, files in os.walk(os.path.abspath(os.curdir)):
        files.sort()
        for filename in files:
            if filename.endswith('.udraw') or filename.endswith('.dis'):
                fullPath = os.path.join(paths, filename)
                Debug.verboseMessage("Removing '%s'" % fullPath)
                os.remove(fullPath)
            if os.access(filename, os.X_OK) and os.path.exists(filename + '.c'):
                fullPath = os.path.join(paths, filename)
                Debug.verboseMessage("Removing '%s'" % fullPath)
                os.remove(fullPath)
        for directory in dirs:
            if directory == 'm5out':
                fullPath = os.path.join(paths, directory)
                Debug.verboseMessage("Removing '%s'" % fullPath)
                shutil.rmtree(fullPath)
        
if __name__ == "__main__":    
    from ARM import readARMDisassembly
    from ParseGem5Trace import parse
    if args.clean:
        clean(os.path.abspath(os.curdir))
    else:
        gem5base, armSimulator = checkEnvironment()
        binary, program        = checkArguments()
        testSpecification      = getTestSpecification(binary)
        gem5Traces             = runGem5(gem5base, armSimulator, binary, testSpecification)
        parse(program, gem5Traces)

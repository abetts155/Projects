#!/usr/bin/python2.7

import Debug, UDrawGraph, Calculations

armGCC      = 'arm-linux-gnueabi-gcc'
armObjdump  = 'arm-linux-gnueabi-objdump'

def runGem5 (gem5base, armSimulator, gem5ConfigFile, binary, testSpecification):
    import os, sys, re
    from TestHarness import RandomGeneration
    from subprocess import Popen, PIPE
    
    run = 0    
    # Carry on from previous executions (if they exist)
    gem5TraceDirectory = os.path.abspath(os.getcwd()) + os.sep + Utils.m5Directory
    if os.path.exists(gem5TraceDirectory):
        for filename in os.listdir(gem5TraceDirectory):
            match = re.match(r'%s' % os.path.basename(binary), filename)
            if match:
                index = filename.rfind('.')
                num   = int(filename[index+1:])
                if num > run:
                    run = num
    run += 1
    
    # Now run the program n times
    randomTVs  = RandomGeneration(testSpecification)
    gem5Traces = []
    for i in xrange(run, args.tests + run):
        nextTV     = randomTVs.nextTestVector()
        traceFile  = "%s.%s.%d" % (os.path.basename(binary), "trace", i)
        cmd        = '%s --debug-flags=Fetch --trace-file=%s %s -c %s -o "%s"' % (armSimulator, traceFile, gem5ConfigFile, binary, nextTV)
        Debug.debugMessage("Running '%s' on gem5" % cmd, 1)
        proc       = Popen(cmd, shell=True, stdout=PIPE, stderr=PIPE)    
        returncode = proc.wait()
        if returncode:
            sys.exit("Running '%s' failed" % cmd)
        gem5Trace = os.path.abspath(os.getcwd()) + os.sep + Utils.m5Directory + os.sep + traceFile
        assert os.path.exists(gem5Trace), "Expected to find gem5 trace in '%s' but it is not there" % gem5Trace
        gem5Traces.append(gem5Trace)
    return gem5Traces
    
def getTestSpecification (testSpecFile):
    import sys
    import locale
    import TestHarness
    basetype = None
    length   = None
    lower    = None
    upper    = None
    locale.setlocale(locale.LC_ALL, 'en_US.UTF8')
    with open(testSpecFile, 'r') as f:
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
                        sys.exit("The length of the test vector must be a non-negative integer. It is '%s'." % rhs) 
                elif lhs.lower() == 'lower':
                    try:
                        lower = locale.atoi(rhs)
                    except:
                        sys.exit("The lower bound on the range of elements in the test vector must be an integer. It is '%s'." % rhs) 
                elif lhs.lower() == 'upper':
                    try:
                        upper = locale.atoi(rhs)
                    except:
                        sys.exit("The upper bound on the range of elements in the test vector must be an integer. It is '%s'." % rhs) 
                else:
                    sys.exit("Do not understand the line '%s' in the test specification file" % line)   
    return TestHarness.TestVectorProperties(length, basetype, lower, upper)
        
def disassembleProgram (binary):
    Debug.verboseMessage("Disassembling program")
    import sys
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
    Debug.verboseMessage("Compiling program")
    import sys
    from subprocess import Popen
    extraFlags = ""
    if args.flags:
        for flag in args.flags:
            extraFlags += "-%s " % flag
    binary     = program[:-2]
    cmd        = "%s -fno-stack-protector -static %s -o %s %s" % (armGCC, program, binary, extraFlags)
    Debug.debugMessage("Compiling with command '%s'" % cmd, 1)
    proc       = Popen(cmd, shell=True, stdout=sys.stdout, stderr=sys.stderr)    
    returncode = proc.wait()
    if returncode:
        sys.exit("Compiling '%s' with '%s' failed" % (program, cmd))
    return binary  

def checkProgramFiles ():
    import os, sys
    from distutils.spawn import find_executable
    from ParseProgramFile import createProgram
    from ARM import readARMDisassembly
    
    program = os.path.abspath(args.program)
    if not os.path.exists(program):
        sys.exit("The first command-line argument must be a file: '%s' does not exist." % program)
    elif not os.path.isfile(program):
        sys.exit("The first command-line argument must be a file: '%s' is not a file" % program)
    else:
        fileStem, fileExt = os.path.splitext(program)
        testSpecFile      = fileStem + '.test'
        if not os.path.exists(testSpecFile):
            sys.exit("Expected to find the test specification file '%s' but it is not there" % testSpecFile)
        cExt = '.c'
        if fileExt:
            if fileExt == cExt:
                if not find_executable(armGCC):
                    sys.exit("Unable to find ARM GCC cross compiler '%s' on your path" % armGCC)
                if not find_executable(armObjdump):
                    sys.exit("Unable to find ARM GCC object dump facility '%s' on your path" % armObjdump)
                if not args.root:
                    sys.exit("To compile and analyse a C file, you must supply a root function via -r.")
                program     = compileProgram(program)
                disassembly = disassembleProgram(program)
                return program, readARMDisassembly(disassembly, args.root), testSpecFile
            else:
                sys.exit("Unable to compile '%s' because its extension is not '%s'" % (program, cExt))
        else:
            if not os.access(program, os.X_OK):
                sys.exit("The argument '%s' does not have execute permissions" % program)
            programFile = program + '.txt'
            assert os.path.exists(programFile), "Expected to find file with program information in '%s' but it is not there" % programFile
            return program, createProgram(programFile), testSpecFile
        
def checkGem5Settings ():
    import os, sys
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
        gem5ConfigFile  = path + os.sep + 'configs' + os.sep + 'example' + os.sep + 'se.py'
        if not os.path.exists(gem5ConfigFile):
            sys.exit("The gem5 configuration file '%s' does not exist" % gem5ConfigFile)
        return path, armSimulator, gem5ConfigFile
    except KeyError:
        sys.exit ("You need to set environment variable '%s' to simulate the program using gem5" % gem5Home)

def commaSeparatedList (s):
    from argparse import ArgumentTypeError
    try:
        return s.split(',')
    except:
        raise ArgumentTypeError("Invalid compiler flags")

def commandLine ():
    from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
    
    # The command-line parser and its options
    cmdline = ArgumentParser(formatter_class=ArgumentDefaultsHelpFormatter,
                             description="Run C programs on gem5 and analyse traces")
    
    cmdline.add_argument("program",
                         help="either a program to compile (with '.c.' extension) or a pre-compiled binary")
    
    cmdline.add_argument("--compiler-flags",
                          type=commaSeparatedList,
                          help="flags to be passed to the compiler",
                          dest="flags",
                          metavar="<FLAGS>")
    
    cmdline.add_argument("-C",
                         "--calculation",
                         action="store_true",
                         help="do WCET calculation",
                         default=False)
    
    cmdline.add_argument("--clean",
                         action="store_true",
                         help="clean out temporary files",
                         default=False)
    
    cmdline.add_argument("-d",
                          "--debug",
                          action="store",
                          type=int,
                          help="debug mode",
                          metavar="<INT>",
                          default=0)
    
    cmdline.add_argument("--exclusive-size",
                          action="store",
                          type=int,
                          help="size of subsets of mutually exclusive basic blocks to compute",
                          metavar="<INT>")
    
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
    
    cmdline.add_argument("-u",
                         "--udraw",
                         action="store_true",
                         help="generate uDrawGraph files",
                         default=False)
    
    cmdline.add_argument("-v",
                         "--verbose",
                         action="store_true",
                         help="be verbose",
                         default=False)
    
    return cmdline.parse_args()

if __name__ == "__main__":   
    import Utils, Traces, SuperBlocks
    import os
    
    args                = commandLine()
    basename            = os.path.splitext(args.program)[0]
    basepath            = os.path.abspath(os.path.dirname(args.program))
    Debug.debug         = args.debug
    Debug.verbose       = args.verbose
    UDrawGraph.enabled  = args.udraw
    UDrawGraph.basename = basename
    
    if args.exclusive_size:
        SuperBlocks.exclusiveSetSize = args.exclusive_size
    
    if args.clean:
        Utils.clean()
    
    Debug.verboseMessage("%s Analysing program '%s' %s" % ('*' * 10, args.program, '*' * 10))
    Debug.verboseMessage("Checking gem5 configuration...")
    gem5base, armSimulator, gem5ConfigFile = checkGem5Settings()
    Debug.verboseMessage("...all good")
    Debug.verboseMessage("Checking program configuration...")
    binary, program, testSpecFile          = checkProgramFiles()
    program.inlineCalls()
    Debug.verboseMessage("...all good")
    Debug.verboseMessage("Checking test specification...")
    testSpecification                      = getTestSpecification(testSpecFile)
    Debug.verboseMessage("...all good")
    Debug.verboseMessage("Running program on gem5 with %d tests" % args.tests)
    gem5Traces                             = runGem5(gem5base, armSimulator, gem5ConfigFile, binary, testSpecification)
    Debug.verboseMessage("Parsing gem5 traces")
    Traces.Gem5Parser(program, gem5Traces)
    if args.calculation:
        Calculations.WCETCalculation(program, basepath, basename)
    for superg in program.getSuperBlockCFGs():
        superg.getSuperBlockPathInformationGraph().output()
    program.generateUDrawFiles()
    
    
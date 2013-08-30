#!/usr/bin/python2.7

import Debug, TestHarness, Timing

armGCC      = 'arm-linux-gnueabi-gcc'
armObjdump  = 'arm-linux-gnueabi-objdump'

def doCompression (gem5Trace):
    import gzip
    compressedFile = gem5Trace + '.gz'
    f_in  = open(gem5Trace, 'rb')
    f_out = gzip.open(compressedFile, 'wb')
    f_out.writelines(f_in)
    f_out.close()
    f_in.close()
    os.remove(gem5Trace)
    return compressedFile

def fitnessFunction (chromosome):
    import os, sys, shlex
    from subprocess import Popen, PIPE
    
    try:
        if fitnessFunction.vectorProperties.baseType == TestHarness.TestVectorProperties.Type[2]:
            # Sometimes this conversion fails and I don't see why?
            # Just catch it and move on
            chromosome.genomeList = [chr(val) for val in chromosome.genomeList]
    except TypeError:
        pass
        
    fitnessFunction.run += 1
    traceFile  = "%s.%s.%d" % (os.path.basename(binary), "trace", fitnessFunction.run)
    cmd        = '%s --debug-flags=Fetch --trace-file=%s %s --cpu-type=timing -c %s -o "%s"' % \
     (armSimulator, traceFile, gem5ConfigFile, binary, ' '.join(map(str, chromosome.genomeList)))
    Debug.debugMessage("Running '%s' on gem5" % cmd, 1)
    proc       = Popen(cmd, shell=True, stdout=PIPE, stderr=PIPE)    
    returncode = proc.wait()
    if returncode:
        sys.exit("Running '%s' failed" % cmd)
    gem5Trace = os.path.abspath(os.getcwd()) + os.sep + Utils.m5Directory + os.sep + traceFile
    assert os.path.exists(gem5Trace), "Expected to find gem5 trace in '%s' but it is not there" % gem5Trace
    firstLines = os.popen("head -1 %s" % gem5Trace).readlines()
    lastLines  = os.popen("tail -1 %s" % gem5Trace).readlines()
    assert len(firstLines) == 1
    assert len(lastLines) == 1
    firstLine = firstLines[0]
    lastLine  = lastLines[0]
    time1 = shlex.split(firstLine)[0]
    time2 = shlex.split(lastLine)[0]
    time1 = time1[:-1]
    time2 = time2[:-1]
    score = int(time2) - int(time1)
    Debug.debugMessage("Score = %d" % score, 1)
    fitnessFunction.gem5Traces.append(doCompression(gem5Trace))
    return score

def runGAGem5 (gem5base, armSimulator, gem5ConfigFile, binary, vectorProperties, populationSize=100, generations=100):
    from pyevolve import G1DList, GSimpleGA, Crossovers, Mutators

    # Create the population
    genome = G1DList.G1DList(vectorProperties.length)
    genome.setParams(rangemin=vectorProperties.lowerBound, \
                     rangemax=vectorProperties.upperBound)
    genome.evaluator.set(fitnessFunction)
    genome.mutator.set(Mutators.G1DListMutatorIntegerRange)
    
    # Cannot crossover if there is only a single gene in the chromosone
    if vectorProperties.length == 1:
        genome.crossover.clear()
    else:
        genome.crossover.set(Crossovers.G1DListCrossoverTwoPoint)
    
    # Set up the engine
    ga = GSimpleGA.GSimpleGA(genome)
    ga.setPopulationSize(populationSize)
    ga.setGenerations(generations)
    ga.setCrossoverRate(0.9)
    ga.setMutationRate(0.01)
    ga.setElitism(True)
    # Set up the fitness function static variables
    fitnessFunction.gem5Traces       = []
    fitnessFunction.vectorProperties = vectorProperties
    fitnessFunction.run              = getNextTraceFileNumber(binary)
    # Run the GA
    ga.evolve (freq_stats=1)    
    return fitnessFunction.gem5Traces

def getNextTraceFileNumber (binary):
    import re
    # Carry on from previous executions (if they exist)
    nextrun = 0
    gem5TraceDirectory = os.path.abspath(os.getcwd()) + os.sep + Utils.m5Directory
    if os.path.exists(gem5TraceDirectory):
        for filename in os.listdir(gem5TraceDirectory):
            match = re.match(r'%s' % os.path.basename(binary), filename)
            if match:
                index1  = filename.rfind('.')
                index2  = filename[:index1].rfind('.')
                run     = int(filename[index2+1:index1])
                nextrun = max(nextrun, run)
    return nextrun
    
def runGem5 (gem5base, armSimulator, gem5ConfigFile, binary, vectorProperties):
    import os, sys
    from TestHarness import RandomGeneration
    from subprocess import Popen, PIPE
    
    # Get the next run number
    run = getNextTraceFileNumber(binary) + 1
    # Now run the program n times
    randomTVs  = RandomGeneration(vectorProperties)
    gem5Traces = []
    for i in xrange(run, args.tests + run):
        nextTV     = randomTVs.nextTestVector()
        traceFile  = "%s.%s.%d.gz" % (os.path.basename(binary), "trace", i)
        cmd        = '%s --debug-flags=Fetch --trace-file=%s %s --cpu-type=timing -c %s -o "%s"' % (armSimulator, traceFile, gem5ConfigFile, binary, nextTV)
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

def generateAssembly (program):
    Debug.verboseMessage("Generating assembly")
    import sys, ARM
    from subprocess import Popen
    cmd = "%s -S %s" % (armGCC, program)
    Debug.debugMessage("Compiling with command '%s'" % cmd, 1)
    proc       = Popen(cmd, shell=True, stdout=sys.stdout, stderr=sys.stderr)    
    returncode = proc.wait()
    if returncode:
        sys.exit("Compiling '%s' with '%s' failed" % (program, cmd))
    assembly = program[:-2] + '.s'
    return ARM.readARMAssembly(assembly, args.root)  
        
def compileProgram (program):
    Debug.verboseMessage("Compiling program")
    import sys, re
    from subprocess import Popen
    optimisation = ""
    extraFlags   = ""
    if args.flags:
        for flag in args.flags:
            extraFlags += "-%s " % flag
            if re.match(r'O[1-9]+', flag):
                optimisation = flag
    binary     = program[:-2] + optimisation
    cmd        = "%s -fno-stack-protector -static %s %s -o %s" % (armGCC, extraFlags, program, binary)
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
    
    args.program = os.path.abspath(args.program)
    if not os.path.exists(args.program):
        sys.exit("The first command-line argument must be a file: '%s' does not exist." % args.program)
    elif not os.path.isfile(args.program):
        sys.exit("The first command-line argument must be a file: '%s' is not a file" % args.program)
    else:
        fileStem, fileExt = os.path.splitext(args.program)
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
                binary      = compileProgram(args.program)
                disassembly = disassembleProgram(binary)
                return binary, readARMDisassembly(disassembly, args.root), testSpecFile
            else:
                sys.exit("Unable to compile '%s' because its extension is not '%s'" % (args.program, cExt))
        else:
            if not os.access(args.program, os.X_OK):
                sys.exit("The argument '%s' does not have execute permissions" % args.program)
            programFile = args.program + '.txt'
            assert os.path.exists(programFile), "Expected to find file with program information in '%s' but it is not there" % programFile
            return args.program, createProgram(programFile), testSpecFile
        
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
    
    cmdline.add_argument("gem5Traces", 
                         nargs='*',
                         help="previous gem5 runs")
    
    cmdline.add_argument("-C",
                         "--compile",
                         action="store_true",
                         help="only compile program",
                         default=False)
    
    cmdline.add_argument("-S",
                         dest="assembly",
                         action="store_true",
                         help="generate program assembly",
                         default=False)
    
    cmdline.add_argument("--compiler-flags",
                          type=commaSeparatedList,
                          help="flags to be passed to the compiler",
                          dest="flags",
                          metavar="<FLAGS>")
    
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
    
    cmdline.add_argument("-G",
                         "--ga",
                         action="store_true",
                         help="use a genetic algorithm to generate test vectors",
                         default=False)
    
    cmdline.add_argument("-I",
                         "--inline",
                         action="store_true",
                         help="do analysis with fully inlined program where applicable",
                         default=False)
    
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

def doAnalysis (gem5Traces, program, inline, basepath, basename):
    import Traces, Calculations    
    time1 = Timing.log("TRACE PARSING RUN #1 (NO INLINING)")
    data = Traces.Gem5Parser(program, gem5Traces)
    Debug.verboseMessage("HWMT = %d" % data.getLongestTime())   
    Calculations.WCETCalculation(program, data, basepath, basename)
    program.output(data)
    program.generateAllUDrawFiles()

    if program.getCallGraph().numOfVertices() > 1 and inline:
        program.inlineCalls()
        time2 = Timing.log("TRACE PARSING RUN #2 (INLINED PROGRAM)")
        data = Traces.Gem5Parser(program, gem5Traces)
        Debug.verboseMessage("HWMT = %d" % data.getLongestTime())
        Calculations.WCETCalculation(program, data, basepath, basename)
        program.output(data)
        program.generateAllUDrawFiles("inlined")
    
def checkTraceFiles (gem5Traces, basename):
    import re
    files = []
    for arg in gem5Traces:
        arg = os.path.abspath(arg)
        if not re.match(r'.*%s\.trace\.[0-9]+' % basename, arg):
            Debug.exitMessage("The file '%s' is not a valid gem5 trace for '%s'" % (arg, basename)) 
        if not os.path.isfile(arg):
            Debug.exitMessage("The argument '%s' is not a valid file" % arg) 
        files.append(arg)
    return files  

if __name__ == "__main__":   
    import Utils, UDrawGraph
    import os
    
    args                = commandLine()
    basename            = os.path.splitext(args.program)[0]
    basepath            = os.path.abspath(os.path.dirname(args.program))
    Debug.debug         = args.debug
    Debug.verbose       = args.verbose
    UDrawGraph.enabled  = args.udraw
    UDrawGraph.basename = basename

    if args.clean:
        Utils.clean()       
    
    Debug.verboseMessage("%s Analysing program '%s' %s" % ('*' * 10, args.program, '*' * 10))
    Debug.verboseMessage("Checking program configuration...")
    time1 = Timing.log("COMPILING BEGIN")
    binary, program, testSpecFile = checkProgramFiles()
    if args.assembly:
        program2 = generateAssembly(args.program)
    time2 = Timing.log("COMPILING END")
    Debug.verboseMessage("...all good")
    if args.compile:
        Debug.exitMessage("DONE")
    if args.gem5Traces:
        gem5Traces = checkTraceFiles(args.gem5Traces, basename)
    else:
        Debug.verboseMessage("Checking gem5 configuration...")
        gem5base, armSimulator, gem5ConfigFile = checkGem5Settings()
        Debug.verboseMessage("...all good")
        Debug.verboseMessage("Checking test specification...")
        testSpecification = getTestSpecification(testSpecFile)
        Debug.verboseMessage("...all good")
        if args.ga:
            Debug.verboseMessage("Using GA to generate test vectors")
            gem5Traces = []
            time3 = Timing.log("GA RUN #1")
            gem5Traces.extend(runGAGem5(gem5base, armSimulator, gem5ConfigFile, binary, testSpecification))
        else:
            Debug.verboseMessage("Running program on gem5 with %d tests" % args.tests)
            gem5Traces = runGem5(gem5base, armSimulator, gem5ConfigFile, binary, testSpecification)
    doAnalysis(gem5Traces, program, args.inline, basepath, basename)
    
    
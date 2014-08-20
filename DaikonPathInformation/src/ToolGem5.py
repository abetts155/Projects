#!/usr/bin/env python

import config
import Debug
import Timing
import Utils
import Calculations
import Traces
import TestHarness
import ARM
import ParseProgramFile
import argparse
import subprocess
import sys
import os
import re
import shlex
import locale
import gzip
import distutils.spawn

armGCC      = 'arm-linux-gnueabi-gcc'
armObjdump  = 'arm-linux-gnueabi-objdump'

def doCompression (gem5Trace):
    compressedFile = gem5Trace + '.gz'
    f_in  = open(gem5Trace, 'rb')
    f_out = gzip.open(compressedFile, 'wb')
    f_out.writelines(f_in)
    f_out.close()
    f_in.close()
    os.remove(gem5Trace)
    return compressedFile

def fitnessFunction (chromosome):
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
    Debug.debug_message("Running '%s' on gem5" % cmd, 1)
    proc       = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)    
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
    Debug.debug_message("Score = %d" % score, 1)
    fitnessFunction.gem5Traces.append(doCompression(gem5Trace))
    return score

def runGAGem5 (gem5base, armSimulator, gem5ConfigFile, binary, vectorProperties, populationSize=20, generations=20):
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

def getNextTraceFileNumber(binary):
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
    # Get the next run number
    run = getNextTraceFileNumber(binary) + 1
    # Now run the program n times
    randomTVs  = TestHarness.RandomGeneration(vectorProperties)
    gem5Traces = []
    for i in xrange(run, config.Arguments.tests + run):
        nextTV     = randomTVs.nextTestVector()
        traceFile  = "%s.%s.%d.gz" % (os.path.basename(binary), "trace", i)
        cmd        = '%s --debug-flags=Fetch --trace-file=%s %s --cpu-type=timing -c %s -o "%s"' % (armSimulator, traceFile, gem5ConfigFile, binary, nextTV)
        Debug.debug_message("Running '%s' on gem5" % cmd, 1)
        proc       = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)    
        returncode = proc.wait()
        if returncode:
            sys.exit("Running '%s' failed" % cmd)
        gem5Trace = os.path.abspath(os.getcwd()) + os.sep + Utils.m5Directory + os.sep + traceFile
        assert os.path.exists(gem5Trace), "Expected to find gem5 trace in '%s' but it is not there" % gem5Trace
        gem5Traces.append(gem5Trace)
    return gem5Traces
    
def getTestSpecification (testSpecFile):
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
        
def disassembleProgram(binary):
    Debug.verbose_message("Disassembling program", __name__)
    filename = binary + ".dis"
    with open(filename, 'w') as disassembly:
        cmd        = "%s %s -d" % (armObjdump, binary)
        proc       = subprocess.Popen(cmd, shell=True, stdout=disassembly, stderr=sys.stderr)    
        returncode = proc.wait()
        if returncode:
            sys.exit("Disassembling '%s' failed" % binary) 
    return filename

def generateAssembly(program):
    Debug.verbose_message("Generating assembly", __name__)
    cmd = "%s -S %s" % (armGCC, program)
    Debug.debug_message("Compiling with command '%s'" % cmd, 1)
    proc       = subprocess.Popen(cmd, shell=True, stdout=sys.stdout, stderr=sys.stderr)    
    returncode = proc.wait()
    if returncode:
        sys.exit("Compiling '%s' with '%s' failed" % (program, cmd))
    assembly = program[:-2] + '.s'
    return ARM.readARMAssembly(assembly, config.Arguments.root)  
        
def compileProgram(program):
    Debug.verbose_message("Compiling program", __name__)
    optimisation = ""
    extraFlags   = ""
    if config.Arguments.flags:
        for flag in config.Arguments.flags:
            extraFlags += "-%s " % flag
            if re.match(r'O[1-9]+', flag):
                optimisation = flag
    binary     = program[:-2] + optimisation
    cmd        = "%s -fno-stack-protector -static %s %s -o %s" % (armGCC, extraFlags, program, binary)
    Debug.debug_message("Compiling with command '%s'" % cmd, 1)
    proc       = subprocess.Popen(cmd, shell=True, stdout=sys.stdout, stderr=sys.stderr)    
    returncode = proc.wait()
    if returncode:
        sys.exit("Compiling '%s' with '%s' failed" % (program, cmd))
    return binary  

def checkProgramFiles():
    config.Arguments.program_file = os.path.abspath(config.Arguments.program_file)
    if not os.path.exists(config.Arguments.program_file):
        sys.exit("The first command-line argument must be a file: '%s' does not exist." % config.Arguments.program_file)
    elif not os.path.isfile(config.Arguments.program_file):
        sys.exit("The first command-line argument must be a file: '%s' is not a file" % config.Arguments.program_file)
    else:
        fileStem, fileExt = os.path.splitext(config.Arguments.program_file)
        testSpecFile      = fileStem + '.test'
        if not os.path.exists(testSpecFile):
            sys.exit("Expected to find the test specification file '%s' but it is not there" % testSpecFile)
        cExt = '.c'
        if fileExt:
            if fileExt == cExt:
                if not distutils.spawn.find_executable(armGCC):
                    sys.exit("Unable to find ARM GCC cross compiler '%s' on your path" % armGCC)
                if not distutils.spawn.find_executable(armObjdump):
                    sys.exit("Unable to find ARM GCC object dump facility '%s' on your path" % armObjdump)
                if not config.Arguments.root:
                    sys.exit("To compile and analyse a C file, you must supply a root function via -r.")
                binary      = compileProgram(config.Arguments.program_file)
                disassembly = disassembleProgram(binary)
                return binary, ARM.readARMDisassembly(disassembly, config.Arguments.root), testSpecFile
            else:
                sys.exit("Unable to compile '%s' because its extension is not '%s'" % (config.Arguments.program_file, cExt))
        else:
            if not os.access(config.Arguments.program_file, os.X_OK):
                sys.exit("The argument '%s' does not have execute permissions" % config.Arguments.program_file)
            programFile = config.Arguments.program_file + '.txt'
            assert os.path.exists(programFile), "Expected to find file with program information in '%s' but it is not there" % programFile
            return config.Arguments.program_file, ParseProgramFile.createProgram(programFile), testSpecFile
        
def checkGem5Settings():
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

def doAnalysis (gem5Traces, program):   
    time1 = Timing.log("TRACE PARSING RUN #1 (NO INLINING)")
    data = Traces.Gem5Parser(program, gem5Traces)
    Debug.verbose_message("HWMT = %d" % data.getLongestTime(), __name__)   
    Calculations.WCETCalculation(program, data)
    program.output()
    program.generateAllUDrawFiles()

    if program.getCallGraph().numOfVertices() > 1 and config.Arguments.inline:
        program.inlineCalls()
        time2 = Timing.log("TRACE PARSING RUN #2 (INLINED PROGRAM)")
        data = Traces.Gem5Parser(program, gem5Traces)
        Debug.verbose_message("HWMT = %d" % data.getLongestTime(), __name__)
        Calculations.WCETCalculation(program, data)
        program.output()
        program.generateAllUDrawFiles("inlined")
    
def checkTraceFiles():
    files = []
    for arg in config.Arguments.gem5Traces:
        arg = os.path.abspath(arg)
        if not re.match(r'.*%s\.trace\.[0-9]+' % config.Arguments.basename, arg):
            Debug.exitMessage("The file '%s' is not a valid gem5 trace for '%s'" % (arg, config.Arguments.basename)) 
        if not os.path.isfile(arg):
            Debug.exitMessage("The argument '%s' is not a valid file" % arg) 
        files.append(arg)
    return files  

def the_command_line():
    def comma_separated_list(the_list):
        try:
            return the_list.split(',')
        except:
            raise argparse.ArgumentTypeError("Invalid compiler flags")
    
    parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter,
                                      description="Run C programs on gem5 and analyse traces")
    
    parser.add_argument("program_file",
                        help="either a program to compile (with '.c.' extension) or a pre-compiled binary")
    
    parser.add_argument("gem5Traces", 
                        nargs='*',
                        help="previous gem5 runs")
    
    parser.add_argument("-C",
                        "--compile",
                        action="store_true",
                        help="only compile program",
                        default=False)

    parser.add_argument("--compiler-flags",
                        type=comma_separated_list,
                        help="flags to be passed to the compiler",
                        dest="flags",
                        metavar="<FLAGS>")
    
    parser.add_argument("-d",
                         "--debug",
                         action="store",
                         type=int,
                         help="debug mode",
                         metavar="<INT>",
                         default=0)
    
    parser.add_argument("--exclusive-size",
                         action="store",
                         type=int,
                         help="size of subsets of mutually exclusive basic blocks to compute",
                         metavar="<INT>")
    
    parser.add_argument("-G",
                        "--ga",
                        action="store_true",
                        help="use a genetic algorithm to generate test vectors",
                        default=False)
    
    parser.add_argument("-I",
                        "--inline",
                        action="store_true",
                        help="do analysis with fully inlined program where applicable",
                        default=False)
    
    parser.add_argument("-r",
                         "--root",
                         action="store",
                         help="the function that is the entry point of the analysis. [This should not be 'main']",
                         metavar="<FUNCTION>")
    
    parser.add_argument("-T",
                         "--number-of-tests",
                         action="store",
                         type=int,
                         dest="tests",
                         help="the number of times to run the application",
                         metavar="<INT>",
                         default=1)
    
    parser.add_argument("-u",
                        "--udraw",
                        action="store_true",
                        help="generate uDrawGraph files",
                        default=False)
    
    parser.add_argument("-v",
                        "--verbose",
                        action="store_true",
                        help="be verbose",
                        default=False)
    
    parser.parse_args(namespace=config.Arguments)
    
    setattr(config.Arguments, "basename", os.path.splitext(os.path.basename(config.Arguments.program_file))[0])
    setattr(config.Arguments, "basepath", os.path.abspath(os.path.dirname(config.Arguments.program_file)))

if __name__ == "__main__":   
    the_command_line()
    Debug.verbose_message("%s Analysing program '%s' %s" % ('*' * 10, config.Arguments.program_file, '*' * 10), __name__)
    Debug.verbose_message("Checking program configuration...", __name__)
    time1 = Timing.log("COMPILING BEGIN")
    binary, program, testSpecFile = checkProgramFiles()
    time2 = Timing.log("COMPILING END")
    Debug.verbose_message("...all good", __name__)
    if config.Arguments.compile:
        Debug.exitMessage("DONE")
    if config.Arguments.gem5Traces:
        gem5Traces = checkTraceFiles()
    else:
        Debug.verbose_message("Checking gem5 configuration...", __name__)
        gem5base, armSimulator, gem5ConfigFile = checkGem5Settings()
        Debug.verbose_message("...all good", __name__)
        Debug.verbose_message("Checking test specification...", __name__)
        testSpecification = getTestSpecification(testSpecFile)
        Debug.verbose_message("...all good", __name__)
        if config.Arguments.ga:
            Debug.verbose_message("Using GA to generate test vectors", __name__)
            gem5Traces = []
            time3 = Timing.log("GA RUN #1")
            gem5Traces.extend(runGAGem5(gem5base, armSimulator, gem5ConfigFile, binary, testSpecification))
        else:
            Debug.verbose_message("Running program on gem5 with %d tests" % config.Arguments.tests, __name__)
            gem5Traces = runGem5(gem5base, armSimulator, gem5ConfigFile, binary, testSpecification)
    doAnalysis(gem5Traces, program)
    
    

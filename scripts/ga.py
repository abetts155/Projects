#!/usr/bin/python2.6

from optparse import OptionParser
from os import path, environ
from pyevolve import G1DList, GSimpleGA, Crossovers, Mutators
from re import split
from subprocess import Popen, PIPE
from sys import argv, maxint
from os import environ, sep
import gzip

# These environment variables are needed run the program under analysis on SimpleScalar 
wcetHomeEnvironmentVariable = "WCET_HOME"
gem5EnvironmentVariable = "GEM5_HOME"
environmentVariables = [wcetHomeEnvironmentVariable]

for var in environmentVariables:
    try:
        environ[var]
    except KeyError:
        print ("Cannot find environment variable '" + var + "' which is needed to run the program under analysis.")
        exit(0)

# The SimpleScalar binary on which the simulation will be carried out
simpleScalarBinary = environ[wcetHomeEnvironmentVariable] + sep + "simplescalar" + sep + "bin" + sep + "sim-outorder"
simpleScalarJar    = environ[wcetHomeEnvironmentVariable] + sep + "bin" + sep + "simplescalar.jar"

# The names of the instrumentation profiles
BASIC_BLOCK   = "BASIC_BLOCK"
PRE_DOMINATOR = "PRE_DOMINATOR"
BRANCH        = "BRANCH"
SUPER_BLOCK   = "SUPER_BLOCK"

# Global variable controlling whether to dump traces
dumpTraces = False
    
# The command-line parser and its options
parser = OptionParser(add_help_option=False)

parser.add_option("-g",
                  "--gem5sim",
                  action="store_true",
                  dest="gem5sim",
                  help="Use gem5 Simulator",
                  default=False)

parser.add_option("-t",
                  "--cpu-type",
                  action="store",
                  type="string",
                  dest="cpuType",
                  help="Type of cpu to be used (atomic, timing, detailed, inorder)",
                  metavar="CPU_TYPE")

parser.add_option("-c",
                  "--config",
                  action="store",
                  type="string",
                  dest="config",
                  help="The file used to configure SimpleScalar.",
                  metavar="<FILE>")

parser.add_option("-C",
                  "--crossover",
                  action="store",
                  type="float",
                  dest="crossoverRate",
                  help="The crossover rate to be applied. [Default is %default].",
                  default=0.9,
                  metavar="<FLOAT>")

parser.add_option("-d",
                  "--debug",
                  action="store_true",
                  dest="debug",
                  help="Debug mode.",
                  default=False)

parser.add_option("-F",
                 "--separate-files",
                 action="store_true",
                 dest="separateFiles",
                 help="Output traces to separate files rather than one monolithic file.",
                 default=False)

parser.add_option("-G",
                  "--generations",
                  action="store",
                  type="int",
                  dest="numOfGenerations",
                  help="The number of generations at which the GA halts. [Default is %default].",
                  default=100,
                  metavar="<INT>")

parser.add_option("-h",
                  "--help",
                  action="help",
                  help="Display this help message.")

parser.add_option("-i",
                  "--instrumentation",
                  action="append",
                  dest="instrumentation",
                  help="The type of instrumentation. Supported values: %s %s %s %s" 
                  % (BASIC_BLOCK, BRANCH, PRE_DOMINATOR, SUPER_BLOCK),
                  type="string",
                  metavar="<STRING>")

parser.add_option("-l",
                  "--lower-bound",
                  action="store",
                  type="int",
                  dest="lowerBound",
                  help="The lower bound on the range of each gene in a chromosone. [Default is %default].",
                  default= -maxint - 1,
                  metavar="<INT>")

parser.add_option("-m",
                  "--mutation",
                  action="store",
                  type="float",
                  dest="mutationRate",
                  help="The mutation rate to be applied. [Default is %default].",
                  default=0.01,
                  metavar="<FLOAT>")

parser.add_option("-n",
                  "--parameters",
                  action="store",
                  type="int",
                  dest="numOfParameters",
                  help="The number of parameters the C program expects.",
                  metavar="<INT>")

parser.add_option("-P",
                  "--population",
                  action="store",
                  type="int",
                  dest="populationSize",
                  help="The size of the population used by the GA. [Default is %default].",
                  default=100,
                  metavar="<INT>")

parser.add_option("-p",
                  "--program",
                  action="store",
                  type="string",
                  dest="program",
                  help="The binary of the program under analysis.",
                  metavar="<FILE>")

parser.add_option("-r",
                  "--root",
                  action="store",
                  type="string",
                  dest="root",
                  help="Entry point of the program.",
                  metavar="<NAME>")

parser.add_option("-u",
                  "--upper-bound",
                  action="store",
                  type="int",
                  dest="upperBound",
                  help="The upper bound on the range of each gene in a chromosone. [Default is %default].",
                  default=maxint,
                  metavar="<INT>")

parser.add_option("-T",
                 "--no-time",
                 action="store_true",
                 dest="noTime",
                 help="Do NOT output timestamps to the trace.",
                 default=False)

parser.add_option("-v",
                 "--verbose",
                 action="store_true",
                 dest="verbose",
                 help="Be verbose.",
                 default=False)

(opts, args) = parser.parse_args(argv[1:])

# The gem5 binary on which the simulation will be carried out
if opts.gem5sim:
    try:
        environ[gem5EnvironmentVariable]
    except KeyError:
        print ("Cannot find environment variable '" + gem5EnvironmentVariable + "' which is needed to run the program under analysis on gem5.")
        exit(0)
    gem5Home = environ[gem5EnvironmentVariable]
    gem5Binary = gem5Home + sep + "build" + sep + "ARM" + sep + "gem5.opt"
    gem5TraceFlags = "--debug-flags=\"ExecEnable,ExecUser,ExecTicks,ExecMicro\""
    gem5ConfigScript = environ[wcetToolsEnvironmentVariable] + "/scripts/gem5Config/se.py"
    gem5TraceParser = environ[wcetHomeEnvironmentVariable] + sep + "scripts" + sep + "gem5TraceParser.py"

def checkOptions ():
	# Check that the user has passed the correct options
	if opts.numOfParameters is None:
		print("Missing option " + str(parser.get_option("-n")))
		exit(0)
	elif opts.program is None:
		print("Missing option " + str(parser.get_option("-p")))
		exit(0)
	elif opts.crossoverRate < 0.0 or opts.crossoverRate > 1.0:
		print("Crossover rate must be in range 0.0..1.0")
		exit(0)
	elif opts.mutationRate < 0.0 or opts.mutationRate > 1.0:
		print("Mutation rate must be in range 0.0..1.0")
		exit(0)
	elif opts.populationSize <= 1:
		print("Population must have at least 2 chromosones")
		exit(0)

	if opts.instrumentation is None:
		print("You must supply at least one instrumentation profile")
		exit(0)
	else:
		for s in opts.instrumentation:
		    if s not in [BASIC_BLOCK, BRANCH, PRE_DOMINATOR, SUPER_BLOCK]:
		        print("'" + s + "' not recognised as a valid instrumentation profile.")
		        exit(0)    

	if opts.config is None and not opts.gem5sim:
		print("Missing option " + str(parser.get_option("-c")))
		exit(0)
				
	if opts.debug:
		print("Program = " + opts.program)
		print("#Parameters = " + str(opts.numOfParameters))
		print("#Generations = " + str(opts.numOfGenerations))
		print("Population size = " + str(opts.populationSize))
		print("Lower bound = " + str(opts.lowerBound))
		print("Upper bound = " + str(opts.upperBound))
		print("Crossover rate = " + str(opts.crossoverRate))
		print("Mutation rate = " + str(opts.mutationRate))

def checkEnvironment ():
    import os.path

    if opts.gem5sim:
        if not os.path.isfile(gem5Binary):
            print "Unable to find executable '" + gem5Binary + "'"
            exit(0)

        # Check the gem5 configuration file exists as well
        if not os.path.isfile(gem5ConfigScript):
            print "Unable to find file " + gem5configScript
            exit(0)
    else:
        if not os.path.isfile(simpleScalarBinary):
            print "Unable to find executable '" + simpleScalarBinary + "'"
            exit(0)

        # Check the SimpleScalar configuration file exists as well
        if not os.path.isfile(opts.config):
            print "Unable to find file " + opts.config
            exit(0)

def checkWhetherTODumpTraces (xmlFile):
	import os.path
	
	# In order to produce trace files for different instrumentation profiles
	# the program structure file must exist and the root function supplied on
	# the command line. Check that both conditions are met before opening various 
	# trace files where the traces produced during execution will be dumped
	dump = True
	if opts.root is None:
		if opts.debug:
			print("No root function specified")
		dump = False
	elif not os.path.exists(xmlFile):
		if opts.debug:
			print("Unable to find program file " + xmlFile)
		dump = False
	return dump

# The set of TVs used by the GA across all generations
TVs = []

# Higher compression rate for the basic block trace as it is expected to be larger
if BASIC_BLOCK in opts.instrumentation:
	basicBlockTrace = gzip.GzipFile(filename="trace." + BASIC_BLOCK + ".GA.gz",
                                        mode="wb",
                                        compresslevel=9)
    
if BRANCH in opts.instrumentation:
	branchTrace = gzip.GzipFile(filename="trace." + BRANCH + ".GA.gz",
                                    mode="wb",
                                    compresslevel=7)
    
if PRE_DOMINATOR in opts.instrumentation:
	preDominatorTrace = gzip.GzipFile(filename="trace." + PRE_DOMINATOR + ".GA.gz",
                                          mode="wb",
                                          compresslevel=7)
    
if SUPER_BLOCK in opts.instrumentation:
	superBlockTrace = gzip.GzipFile(filename="trace." + SUPER_BLOCK + ".GA.gz",
                                        mode="wb",
                                        compresslevel=7)
                                        
def closeFileHandles ():
    if BASIC_BLOCK in opts.instrumentation:
        basicBlockTrace.close()
        
    if BRANCH in opts.instrumentation:
        branchTrace.close()
        
    if PRE_DOMINATOR in opts.instrumentation:
        preDominatorTrace.close()
        
    if SUPER_BLOCK in opts.instrumentation:
        superBlockTrace.close()

def sanitiseSimpleScalarTrace ():
    cmd = "java -jar " + simpleScalarJar + " -p " + opts.program + ".xml -r " + opts.root + " -t " + opts.program + ".trc"
    #cmd = "java -jar %s -p %s -r %s -t %s.trc -i" % (simpleScalarJar, xmlFile, opts.root, opts.program)
    
    if BASIC_BLOCK in opts.instrumentation:
		cmd += " -i " + BASIC_BLOCK
    if BRANCH in opts.instrumentation:
    	cmd += " %s" % (BRANCH)
    if PRE_DOMINATOR in opts.instrumentation:
    	cmd += " %s" % (PRE_DOMINATOR)
    if SUPER_BLOCK in opts.instrumentation:
    	cmd += " %s" % (SUPER_BLOCK)

    if opts.noTime:
    	cmd += " -T"

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

    if BASIC_BLOCK in opts.instrumentation:
        f = open(opts.program + "." + BASIC_BLOCK + ".txt", "r")
        lines = f.readlines()
        f.close()
        for line in lines:
		basicBlockTrace.write(line)

    if BRANCH in opts.instrumentation:
        f = open(opts.program + "." + BRANCH + ".txt", "r")
        lines = f.readlines()
        f.close()
        for line in lines:
            branchTrace.write(line)
            
    if PRE_DOMINATOR in opts.instrumentation:
        f = open(opts.program + "." + PRE_DOMINATOR + ".txt", "r")
        lines = f.readlines()
        f.close()
        for line in lines:
            preDominatorTrace.write(line)
            
    if SUPER_BLOCK in opts.instrumentation:
        f = open(opts.program + "." + SUPER_BLOCK + ".txt", "r")
        lines = f.readlines()
        f.close()
        for line in lines:
            superBlockTrace.write(line)

def sanitiseGem5Trace ():
    cmd = "python " + gem5TraceParser + " -p " + opts.program + ".xml -t m5out/trace.out -o " + opts.program + ".BASIC_BLOCK.txt"
    
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

    f = open(opts.program + ".BASIC_BLOCK.txt", "r")
    lines = f.readlines()
    f.close()
    for line in lines:
        # print(line)
        basicBlockTrace.write(line)

def executeOnSimplescalar (chromosome):
    cmd = "%s -config %s -ptrace %s.trc : %s " \
    % (simpleScalarBinary, opts.config, opts.program, opts.program) + \
    ' '.join(map(str, chromosome.genomeList))

    if opts.debug:
        print("Running '" + cmd + "'")

    # Run the binary on Simplescalar
    proc = Popen(cmd,
                 shell=True,
                 executable="/bin/bash",
                 stderr=PIPE,
                 stdout=PIPE)
    # ..and get both standard output and standard error streams from SimpleScalar
    stoutdata, stderrdata = proc.communicate()

    # If a non-zero return code is detected then SimpleScalar choked
    if proc.returncode != 0:
        print("\nProblem running " + cmd)
        exit(0)

    # Iterate through standard error until we find the cycle count
    # (For the moment we ignore standard output as it provides nothing useful)
    it = iter(stderrdata.splitlines())
    found = False
    while not found:
        try:
            line = it.next()
            if line.startswith("sim_cycle"):
                tokens = split("\s+", line, 2)
                
                # The cycle count is always the 2nd token
                # This is the fitness criterion for a chromosone
                score = float(tokens[1])
                found = True
        except:
            raise StopIteration

    return score

def executeOnGem5 (chromosome):
    if opts.cpuType == "detailed" or opts.cpuType == "inorder":
        cmd = "%s %s --trace-file=trace.out %s -c %s --cpu-type=%s --caches -o \"" \
        % (gem5Binary, gem5TraceFlags, gem5ConfigScript, opts.program, opts.cpuType) + \
        ' '.join(map(str, chromosome.genomeList)) + "\""
    else:
        cmd = "%s %s --trace-file=trace.out %s -c %s --cpu-type=%s -o \"" \
        % (gem5Binary, gem5TraceFlags, gem5ConfigScript, opts.program, opts.cpuType) + \
        ' '.join(map(str, chromosome.genomeList)) + "\""

    if opts.debug:
        print("Running '" + cmd + "'")

    # Run the binary on gem5
    proc = Popen(cmd,
                 shell=True,
                 executable="/bin/bash",
                 stderr=PIPE,
                 stdout=PIPE)
    # ..and get both standard output and standard error streams from gem5
    stoutdata, stderrdata = proc.communicate()

    # If a non-zero return code is detected then gem5 choked
    if proc.returncode != 0:
        print("\nProblem running " + cmd)
        exit(0)

    # Iterate through standard out until we find the tick count
    # (For the moment we ignore standard error as it provides nothing useful)
    it = iter(stoutdata.splitlines())
    found = False
    while not found:
        try:
            line = it.next()
            if line.startswith("Exiting @ tick"):
                tokens = split("\s+", line, 4)
                
                # The tick count is always the 4th token
                # This is the fitness criterion for a chromosone
                score = float(tokens[3])
                print score
                found = True
        except:
            raise StopIteration

    return score

def fitnessFunction (chromosome):
    if opts.gem5sim:
        score = executeOnGem5 (chromosome)
    else:
        score = executeOnSimplescalar(chromosome)

    it = iter(TVs)
    stop = False
    while not stop:
        try:
            tv = it.next()
            stop = tv == chromosome.genomeList
        except:
            # The iterator has reached its end
            stop = True
            
            # Remember the TV so it can be written later
            TVs.append(chromosome.genomeList)
            
            # Dump the sanitised trace to the trace files
            #TVstr = "// TV = " + str(chromosome.genomeList) + "\n"
            if opts.gem5sim:
                sanitiseGem5Trace ()
            else:
                sanitiseSimpleScalarTrace ()

    if opts.debug:
        print("Fitness = " + str(score))

    return score

def main ():
	checkOptions ()
	checkEnvironment ()
	
	# The XML file describing the program structure
	xmlFile = opts.program + ".xml"
	dumpTraces = checkWhetherTODumpTraces (xmlFile)
	
    # Create the population
	genome = G1DList.G1DList(opts.numOfParameters)
	genome.setParams(rangemin=opts.lowerBound, rangemax=opts.upperBound)
	genome.evaluator.set(fitnessFunction)
	genome.mutator.set(Mutators.G1DListMutatorIntegerRange)

	# Cannot crossover if there is only a single gene in the chromosone
	if opts.numOfParameters == 1:
		genome.crossover.clear()
	else:
		genome.crossover.set(Crossovers.G1DListCrossoverTwoPoint)

	# Set up the engine
	ga = GSimpleGA.GSimpleGA(genome)
	ga.setPopulationSize(opts.populationSize)
	ga.setGenerations(opts.numOfGenerations)
	ga.setCrossoverRate(opts.crossoverRate)
	ga.setMutationRate(opts.mutationRate)
	ga.setElitism(True)
	#ga.setMultiProcessing(True)

	# Start the evolution
	ga.evolve (freq_stats=1)
	
	if dumpTraces:
		closeFileHandles ()

if __name__ == "__main__":
    main()

    # Now write out the test vectors generated to a file
    f = open("TVs_GA.txt", "w")
    for tv in TVs:
        f.write(' '.join(map(str, tv)) + "\n")
    f.close()

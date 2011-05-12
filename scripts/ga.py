#!/usr/bin/python2.6

from optparse import OptionParser
from os import path, environ, pathsep
from os.path import isfile
from pyevolve import G1DList, GSimpleGA, Crossovers, Mutators
from re import split
from subprocess import Popen, PIPE
from sys import argv, maxint
import gzip
import os

# The command-line parser and its options
parser = OptionParser(add_help_option=False)

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

parser.add_option("-G",
                  "--generations",
                  action="store",
                  type="int",
                  dest="numOfGenerations",
                  help="The number of generations at which the GA halts. [Default is %default].",
                  default=100,
                  metavar="<INT>")

parser.add_option("-H",
                 "--host",
                 action="store_true",
                 dest="host",
                 help="Run the binary on the host machine.",
                 default=False)

parser.add_option("-h",
                  "--help",
                  action="help",
                  help="Display this help message.")

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

parser.add_option("-s",
                  "--simplescalar",
                  action="store",
                  type="string",
                  dest="simplescalar",
                  help="The SimpleScalar binary used to run the simulation.",
                  metavar="<PATH>")

parser.add_option("-u",
                  "--upper-bound",
                  action="store",
                  type="int",
                  dest="upperBound",
                  help="The upper bound on the range of each gene in a chromosone. [Default is %default].",
                  default=maxint,
                  metavar="<INT>")

parser.add_option("-v",
                 "--verbose",
                 action="store_true",
                 dest="verbose",
                 help="Be verbose.",
                 default=False)

(opts, args) = parser.parse_args(argv[1:])

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

if not opts.host:
    if opts.config is None:
        print("Missing option " + str(parser.get_option("-c")))
        exit(0)
    elif opts.simplescalar is None and not opts.host:
        print("Missing option " + str(parser.get_option("-s")))
        exit(0)

if opts.debug:
    if not opts.host:
        print("SimpleScalar = " + opts.simplescalar)
    else:
        print("Running on the host")

    print("Program = " + opts.program)
    print("#Parameters = " + str(opts.numOfParameters))
    print("#Generations = " + str(opts.numOfGenerations))
    print("Population size = " + str(opts.populationSize))
    print("Lower bound = " + str(opts.lowerBound))
    print("Upper bound = " + str(opts.upperBound))
    print("Crossover rate = " + str(opts.crossoverRate))
    print("Mutation rate = " + str(opts.mutationRate))

# The set of TVs used by the GA across all generations
TVs = []

# The XML file describing the program structure
xmlFile = opts.program + ".xml"

# The name of the Java utility designed to strip the SimpleScalar trace into
# more digestible format
simpleScalarJar = "java -jar /home/abetts/workspace/MDH/jar/simplescalar.jar"

# The names of the instrumentation profiles
BASIC_BLOCK = "BASIC_BLOCK"
PRE_DOMINATOR = "PRE_DOMINATOR"
BRANCH = "BRANCH"
SUPER_BLOCK = "SUPER_BLOCK"

# In order to produce trace files for different instrumentation profiles
# the program structure file must exist, the root function supplied on
# the command line, and the SimpleScalar Java utility must be on the path.
# Check that all of these conditions are met before opening various trace
# files where the traces produced during execution will be dumped
dumpTraces = True
if opts.root is None:
    if opts.debug:
        print("No root function specified")
    dumpTraces = False
elif not os.path.exists(xmlFile):
    if opts.debug:
        print("Unable to find program file " + xmlFile)
    dumpTraces = False
#else:
#    it = iter(os.environ["PATH"].split(os.pathsep))
#    stop = False
#    while not stop:
#        try:
#            path = it.next()
#            file = os.path.join(path, simpleScalarJar)
#            if isfile(file) and os.access(file, os.X_OK):
#                stop = True
#                print("Found executable at " + file);
#        except:
#            # The iterator has reached its end and we did not find the executable
#            stop = True
#            dumpTraces = False
#            print("Unable to find " + simpleScalarJar)

if dumpTraces:
    # Higher compression rate for the basic block trace as it is expected to be larger
    bbTrace = gzip.GzipFile(filename="trace." + BASIC_BLOCK + ".GA.gz",
                            mode="wb",
                            compresslevel=9)
    predomTrace = gzip.GzipFile(filename="trace." + PRE_DOMINATOR + ".GA.gz",
                                mode="wb",
                                compresslevel=7)

def dumpTrace ():
    cmd = "%s -p %s -r %s -i %s %s -t %s.trc" \
     % (simpleScalarJar, xmlFile, opts.root, BASIC_BLOCK, PRE_DOMINATOR, opts.program)

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

    f = open(opts.program + "." + BASIC_BLOCK + ".txt", "r")
    lines = f.readlines()
    f.close()
    for line in lines:
        bbTrace.write(line)

    f = open(opts.program + "." + PRE_DOMINATOR + ".txt", "r")
    lines = f.readlines()
    f.close()
    for line in lines:
        predomTrace.write(line)

def fitnessFunction (chromosome):
    if opts.host:
        cmd = "time ./%s " % (opts.program) + ' '.join(map(str, chromosome.genomeList))

        if opts.debug:
            print("Running '" + cmd + "'")

        # Run the binary on the host
        proc = Popen(cmd,
                     shell=True,
                     executable="/bin/bash",
                     stderr=PIPE,
                     stdout=PIPE)
        # ..and get standard output
        stoutdata, stderrdata = proc.communicate()

        # If a non-zero return code is detected then executing the binary choked
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
                if line.startswith("user"):
                    tokens = split("\s+", line, 2)
                    timetokens = split("m", tokens[1])
                    score = float(timetokens[0]) * 60 + float(timetokens[1][:-1])
                    found = True
            except:
                raise StopIteration

    else:
        cmd = "%s -config %s -ptrace %s.trc : %s " \
        % (opts.simplescalar, opts.config, opts.program, opts.program) + \
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
            if dumpTraces and opts.simplescalar:
                #TVstr = "// TV = " + str(chromosome.genomeList) + "\n"
                #bbTrace.write(TVstr)
                #branchTrace.write(TVstr)
                #predomTrace.write(TVstr)
                dumpTrace()

    if opts.debug:
        print("Fitness = " + str(score))

    return score

def main ():#
    if not opts.host:
        # Before proceeding, ensure that SimpleScalar exists
        if not isfile(opts.simplescalar):
            print "Unable to find executable " + opts.simplescalar
            exit(0)

        # Check the SimpleScalar configuration file exists as well
        if not isfile(opts.config):
            print "Unable to find file " + opts.config
            exit(0)

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

    # Start the evolution
    ga.evolve (freq_stats=1)

if __name__ == "__main__":
    main()

    # Now write out the test vectors generated to a file
    f = open("TVs_GA.txt", "w")
    for tv in TVs:
        f.write(' '.join(map(str, tv)) + "\n")
    f.close()

# Final action is to close the file handles and remove temporary files
if dumpTraces:
    bbTrace.close()
    predomTrace.close()

    cmd = "rm -f %s.txt %s.txt %s.txt %s.trc" \
    % (opts.program + "." + BASIC_BLOCK, 
       opts.program + "." + PRE_DOMINATOR, 
       opts.program)

    proc = Popen(cmd,
                 shell=True,
                 executable="/bin/bash",
                 stderr=PIPE)
    proc.wait()

    if proc.returncode != 0:
        print("\nProblem running '" + cmd + "'")
        exit(0)

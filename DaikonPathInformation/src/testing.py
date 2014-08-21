import config
import debug
import shlex
import locale
import gzip
import re
import os
import subprocess
import random
import string
from pyevolve import G1DList, Mutators, Crossovers, GSimpleGA

class TestVectorProperties:
    BaseType = ["int", "double", "char"]
    
    def __init__ (self):
        self.read_file()
            
    def read_file(self):
        locale.setlocale(locale.LC_ALL, 'en_US.UTF8')
        with open(config.Arguments.test_specification_file, 'r') as f:
            for line in f:
                index = line.find('=')
                if index == -1:
                    debug.exit_message("Found an invalid line '%s' in the test specification file" % line)
                else:
                    lhs = line[:index].strip()
                    rhs = line[index+1:].strip()
                    if lhs.lower() == "type":
                        self.base_type = rhs
                    elif lhs.lower() == "length":
                        try:
                            self.length = int(rhs)
                        except:
                            debug.exit_message("The length of the test vector must be a non-negative integer. It is '%s'." % rhs) 
                    elif lhs.lower() == "lower":
                        try:
                            self.lower = locale.atoi(rhs)
                        except:
                            debug.exit_message("The lower bound on the range of elements in the test vector must be an integer. It is '%s'." % rhs) 
                    elif lhs.lower() == "upper":
                        try:
                            self.upper = locale.atoi(rhs)
                        except:
                            debug.exit_message("The upper bound on the range of elements in the test vector must be an integer. It is '%s'." % rhs) 
                    else:
                        debug.exit_message("Do not understand the line '%s' in the test specification file" % line)
            
def get_next_trace_file_number(binary):
    # Carry on from previous executions (if they exist)
    nextrun = 0
    gem5TraceDirectory = os.path.abspath(os.getcwd()) + os.sep + config.Arguments.m5_trace_directory
    if os.path.exists(gem5TraceDirectory):
        for filename in os.listdir(gem5TraceDirectory):
            match = re.match(r'%s' % os.path.basename(binary), filename)
            if match:
                index1  = filename.rfind('.')
                index2  = filename[:index1].rfind('.')
                run     = int(filename[index2+1:index1])
                nextrun = max(nextrun, run)
    return nextrun

def compress_trace(gem5_trace):
    compressedFile = gem5_trace + '.gz'
    f_in  = open(gem5_trace, 'rb')
    f_out = gzip.open(compressedFile, 'wb')
    f_out.writelines(f_in)
    f_out.close()
    f_in.close()
    os.remove(gem5_trace)
    return compressedFile

def fitness_function(chromosome):
    try:
        if fitness_function.vectorProperties.base_type == TestVectorProperties.BaseType[2]:
            # Sometimes this conversion fails and I don't see why?
            # Just catch it and move on
            chromosome.genomeList = [chr(val) for val in chromosome.genomeList]
    except TypeError:
        pass
        
    fitness_function.run += 1
    traceFile  = "%s.%s.%d" % (os.path.basename(fitness_function.binary), "trace", fitness_function.run)
    cmd        = '%s --debug-flags=Fetch --trace-file=%s %s --cpu-type=timing -c %s -o "%s"' % \
     (config.Arguments.gem5_simulator, traceFile, config.Arguments.gem5_config, fitness_function.binary, ' '.join(map(str, chromosome.genomeList)))
    debug.debug_message("Running '%s' on gem5" % cmd, 1)
    proc       = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)    
    returncode = proc.wait()
    if returncode:
        debug.exit_message("Running '%s' failed" % cmd)
    gem5_trace = os.path.abspath(os.getcwd()) + os.sep + config.Arguments.m5_trace_directory + os.sep + traceFile
    assert os.path.exists(gem5_trace), "Expected to find gem5 trace in '%s' but it is not there" % gem5_trace
    firstLines = os.popen("head -1 %s" % gem5_trace).readlines()
    lastLines  = os.popen("tail -1 %s" % gem5_trace).readlines()
    assert len(firstLines) == 1
    assert len(lastLines) == 1
    firstLine = firstLines[0]
    lastLine  = lastLines[0]
    time1 = shlex.split(firstLine)[0]
    time2 = shlex.split(lastLine)[0]
    time1 = time1[:-1]
    time2 = time2[:-1]
    score = int(time2) - int(time1)
    debug.debug_message("Score = %d" % score, 1)
    fitness_function.gem5traces.append(compress_trace(gem5_trace))
    return score

def runGAGem5(binary, populationSize=20, generations=20):
    test_vector_properties = TestVectorProperties()
    # Set up the fitness function's attributes
    fitness_function.binary           = binary
    fitness_function.gem5traces       = []
    fitness_function.vectorProperties = test_vector_properties
    fitness_function.run              = get_next_trace_file_number(binary)
    # Create the population
    genome = G1DList.G1DList(test_vector_properties.length)
    genome.setParams(rangemin=test_vector_properties.lower, \
                     rangemax=test_vector_properties.upper)
    genome.evaluator.set(fitness_function)
    genome.mutator.set(Mutators.G1DListMutatorIntegerRange)
    
    # Cannot crossover if there is only a single gene in the chromosone
    if test_vector_properties.length == 1:
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
    # Run the GA
    ga.evolve(freq_stats=1)    
    return fitness_function.gem5traces

class RandomGeneration:
    def __init__ (self, test_vector_properties):
        self.test_vector_properties = test_vector_properties
    
    def next_test_vector(self):
        vector = []
        for i in xrange(1, self.test_vector_properties.length+1):
            if self.test_vector_properties.base_type == TestVectorProperties.BaseType[0]: 
                vector.append(random.randint(self.test_vector_properties.lower, self.test_vector_properties.upper))
            elif self.test_vector_properties.base_type == TestVectorProperties.BaseType[1]:
                vector.append(random.uniform(self.test_vector_properties.lower, self.test_vector_properties.upper))
            elif self.test_vector_properties.base_type == TestVectorProperties.BaseType[2]:
                vector.append(random.choice(string.letters))
        assert len(vector) == self.test_vector_properties.length, "Created an invalid test vector of length %d" % len(vector)
        return ' '.join(str(val) for val in vector)
    
def run_gem5(binary):
    test_vector_properties = TestVectorProperties()
    run = get_next_trace_file_number(binary) + 1
    # Now run the program n times
    random_test_vectors = RandomGeneration(test_vector_properties)
    gem5traces          = []
    for i in xrange(run, config.Arguments.tests + run):
        traceFile  = "%s.%s.%d.gz" % (os.path.basename(binary), "trace", i)
        cmd        = '%s --debug-flags=Fetch --trace-file=%s %s --cpu-type=timing -c %s -o "%s"' % \
        (config.Arguments.gem5_simulator, traceFile, config.Arguments.gem5_config, binary, random_test_vectors.next_test_vector())
        debug.debug_message("Running '%s' on gem5" % cmd, 1)
        proc       = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)    
        returncode = proc.wait()
        if returncode:
            debug.exit_message("Running '%s' failed" % cmd)
        gem5_trace = os.path.abspath(os.getcwd()) + os.sep + config.Arguments.m5_trace_directory + os.sep + traceFile
        assert os.path.exists(gem5_trace), "Expected to find gem5 trace in '%s' but it is not there" % gem5_trace
        gem5traces.append(gem5_trace)
    return gem5traces

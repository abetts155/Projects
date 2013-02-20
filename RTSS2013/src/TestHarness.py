
class TestVectorProperties:
    Type = ['int', 'float']
    
    def __init__ (self, vectorLength, vectorBaseType, vectorLowerBound, vectorUpperBound):
        assert vectorLength > 0, "The length of the test vector must be greater than zero"
        assert vectorBaseType in TestVectorProperties.Type, "Invalid test vector base type"
        assert vectorUpperBound >= vectorLowerBound, "The range [%d..%d] is not permissible" % (vectorLowerBound, vectorUpperBound)
        self.length     = vectorLength
        self.baseType   = vectorBaseType
        self.lowerBound = vectorLowerBound
        self.upperBound = vectorUpperBound

class GeneticAlgorithm:
    def __init__ (self, vectorProperties, fitnessFunction, generations=100, populationSize=100, crossoverRate=0.9, mutationRate=0.01):
        from pyevolve import G1DList, GSimpleGA, Crossovers, Mutators
        # Create the population
        genome = G1DList.G1DList(vectorProperties.length)
        genome.setParams(rangemin=vectorProperties.lowerBound, rangemax=vectorProperties.upperBound)
        genome.evaluator.set(fitnessFunction)
        genome.mutator.set(Mutators.G1DListMutatorIntegerRange)
        
        # Cannot crossover if there is only a single gene in the chromosone
        if vectorProperties.length == 1:
            genome.crossover.clear()
        else:
            genome.crossover.set(Crossovers.G1DListCrossoverTwoPoint)
        
        # Set up the engine
        self.__ga = GSimpleGA.GSimpleGA(genome)
        self.__ga.setPopulationSize(populationSize)
        self.__ga.setGenerations(generations)
        self.__ga.setCrossoverRate(crossoverRate)
        self.__ga.setMutationRate(mutationRate)
        self.__ga.setElitism(True)
        
    def evolve (self):
        # Start the evolution
        self.__ga.evolve (freq_stats=1)

class RandomGeneration:
    def __init__ (self, vectorProperties):
        self.__vectorProperties = vectorProperties
    
    def nextTestVector (self):
        import random
        vector = []
        for i in xrange(1, self.__vectorProperties.length+1):
            if self.__vectorProperties.baseType == TestVectorProperties.Type[0]: 
                vector.append(random.randint(self.__vectorProperties.lowerBound, self.__vectorProperties.upperBound))
            elif self.__vectorProperties.baseType == TestVectorProperties.Type[1]:
                vector.append(random.uniform(self.__vectorProperties.lowerBound, self.__vectorProperties.upperBound))
        assert len(vector) == self.__vectorProperties.length, "Created an invalid test vector of length %d" % len(vector)
        return ' '.join(str(val) for val in vector)
                
import random
import string

class TestVectorProperties:
    Type = ['int', 'double', 'char']
    
    def __init__ (self, vectorLength, vectorBaseType, vectorLowerBound, vectorUpperBound):
        assert vectorLength >= 0, "The length of the test vector must be greater than or equal to zero"
        self.length = vectorLength
        if self.length > 0:
            assert vectorBaseType in TestVectorProperties.Type, "Invalid test vector base type"
            assert vectorUpperBound >= vectorLowerBound, "The range [%d..%d] is not permissible" % (vectorLowerBound, vectorUpperBound)
            self.baseType   = vectorBaseType
            self.lowerBound = vectorLowerBound
            self.upperBound = vectorUpperBound

class RandomGeneration:
    def __init__ (self, vectorProperties):
        self.__vectorProperties = vectorProperties
    
    def nextTestVector (self):
        vector = []
        for i in xrange(1, self.__vectorProperties.length+1):
            if self.__vectorProperties.baseType == TestVectorProperties.Type[0]: 
                vector.append(random.randint(self.__vectorProperties.lowerBound, self.__vectorProperties.upperBound))
            elif self.__vectorProperties.baseType == TestVectorProperties.Type[1]:
                vector.append(random.uniform(self.__vectorProperties.lowerBound, self.__vectorProperties.upperBound))
            elif self.__vectorProperties.baseType == TestVectorProperties.Type[2]:
                vector.append(random.choice(string.letters))
        assert len(vector) == self.__vectorProperties.length, "Created an invalid test vector of length %d" % len(vector)
        return ' '.join(str(val) for val in vector)
                

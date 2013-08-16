import sys

class WCETInformation:
    pass

class MutualExclusion (WCETInformation):
    def __init__(self, theSet):
        assert isinstance(theSet, set)
        self._theSet = theSet
    
    def __str__(self):
        return "Exclusive set: {%s}" % ', '.join(str(bbID) for bbID in self._theSet)    
        
class MutualInclusion (WCETInformation):
    def __init__(self, theSet):
        assert isinstance(theSet, set)
        self._theSet = theSet
        
    def __str__(self):
        return "Inclusive set: {%s}" % ', '.join(str(bbID) for bbID in self._theSet)
        
class ExecutionBounds (WCETInformation):
    def __init__(self, bbID, lowerBound=0, upperBound=sys.maxint):
        self._bbID = bbID
        self._lowerBound = lowerBound
        self._upperBound = upperBound
        
    def __str__(self):
        return "Execution bound constraint on %d: [%d, %d]" % (self._bbID, self._lowerBound, self._upperBound)    
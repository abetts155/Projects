import Debug, Vertices

class CreateWCETData:    
    def __init__ (self, ipg, lnt):
        import random 
        self.__headerToBound  = {}
        self.__transitionWCET = {}
        for v in ipg:
            for succID in v.getSuccessorIDs():
                succe = v.getSuccessorEdge(succID)
                if not succe.isDummyEdge():
                    transitionWCET = random.randint(1, 100)
                    self.__transitionWCET[(v.getVertexID(), succID)] = transitionWCET
                    Debug.debugMessage("WCET(%d, %d) = %d" % (v.getVertexID(), succID, transitionWCET), 1)
        for level, vertices in lnt.levelIterator(True):
            for v in vertices:
                if isinstance(v, Vertices.HeaderVertex):
                    if level > 0:
                        bound = random.randint(5, 20)
                        self.__headerToBound[v.getHeaderID()] = bound
                        Debug.debugMessage("Bound(%d) = %d" % (v.getHeaderID(), bound), 1)
    
    def getTransitionWCET (self, predID, succID):
        if (predID, succID) in self.__transitionWCET:
            return self.__transitionWCET[(predID, succID)]
        else:
            return 0
        
    def getLoopBound (self, headerID):
        assert headerID in self.__headerToBound, "Unable to find bound for header %d" % headerID
        return self.__headerToBound[headerID]
    
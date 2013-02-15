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
                    transitionWCET = random.randint(1, 10)
                    self.__transitionWCET[(v.getVertexID(), succID)] = transitionWCET
                    Debug.debugMessage("WCET(%d, %d) = %d" % (v.getVertexID(), succID, transitionWCET), 1)
        for level, vertices in lnt.levelIterator(True):
            for v in vertices:
                if isinstance(v, Vertices.HeaderVertex):
                    if level > 0:
                        headerID                       = v.getHeaderID()
                        self.__headerToBound[headerID] = {}
                        for ancestorv in lnt.getAllProperAncestors(v.getVertexID()):
                            ancestorHeaderID = ancestorv.getHeaderID()
                            bound            = random.randint(3, 10)
                            self.__headerToBound[headerID][ancestorHeaderID] = bound
                            Debug.debugMessage("Bound(%d w.r.t %d) = %d" % (headerID, ancestorHeaderID, bound), 1)
    
    def getTransitionWCET (self, predID, succID):
        if (predID, succID) in self.__transitionWCET:
            return self.__transitionWCET[(predID, succID)]
        else:
            Debug.verboseMessage("WARNING: returning WCET of 0 for edge (%d, %d)" % (predID, succID))
            return 0
        
    def getLoopBound (self, headerID, ancestorID):
        assert headerID in self.__headerToBound, "Unable to find bound for header %d" % headerID
        assert ancestorID in self.__headerToBound[headerID], "Unable to find bound for header %d w.r.t. header %d" % (headerID, ancestorID)
        return self.__headerToBound[headerID][ancestorID]
    
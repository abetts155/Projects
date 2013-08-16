import Debug, Vertices
import random

class CreateWCETData:    
    def __init__ (self, icfg, lnt, ipg, useBasicBlockExecutionTimes=True):
        self.__headerToBound  = {}
        self.__bbToWCET       = {}
        self.__transitionWCET = {}
        if useBasicBlockExecutionTimes:
            self.__assignWCETsToBasicBlocks(icfg)
            self.__assignWCETsToIPGTransitions(ipg, icfg)
        else:
            self.__assignWCETsToIPGTransitions(ipg)
        self.__assignLoopBounds(lnt)
                            
    def __assignWCETsToBasicBlocks (self, icfg):
        Debug.debugMessage("Assigning WCETs to basic blocks", 1)
        for v in icfg:
            vertexID = v.getVertexID()
            if not icfg.isIpoint(vertexID):
                self.__bbToWCET[vertexID] = random.randint(1, 10)
                Debug.debugMessage("WCET(%d) = %d" % (vertexID, self.__bbToWCET[vertexID]), 1)
                
    def __assignWCETsToIPGTransitions (self, ipg, icfg=None):
        Debug.debugMessage("Assigning WCETs to IPG transitions", 1)
        if icfg:
            for v in ipg:
                for succID in v.getSuccessorIDs():
                    transitionWCET = 0
                    succe          = v.getSuccessorEdge(succID)
                    for bbID in succe.getEdgeLabel():
                        transitionWCET += self.__bbToWCET[bbID]
                    self.__transitionWCET[(v.getVertexID(), succID)] = transitionWCET
                    Debug.debugMessage("WCET(%d, %d) = %d \t[Basic blocks = %s]" % (v.getVertexID(), succID, transitionWCET, succe.getEdgeLabel()), 1)
        else:
            for v in ipg:
                for succID in v.getSuccessorIDs():
                    succe = v.getSuccessorEdge(succID)
                    if not succe.isDummyEdge():
                        transitionWCET = random.randint(1, 10)
                        self.__transitionWCET[(v.getVertexID(), succID)] = transitionWCET
                        Debug.debugMessage("WCET(%d, %d) = %d" % (v.getVertexID(), succID, transitionWCET), 1)
    
    def __assignLoopBounds (self, lnt):
        Debug.debugMessage("Assigning bounds to loop", 1)
        for level, vertices in lnt.levelIterator(True):
            for v in vertices:
                if isinstance(v, Vertices.HeaderVertex):
                    if level > 0:
                        bound                          = 0
                        headerID                       = v.getHeaderID()
                        self.__headerToBound[headerID] = {}
                        for ancestorv in lnt.getAllProperAncestors(v.getVertexID()):
                            ancestorHeaderID = ancestorv.getHeaderID()
                            bound            += random.randint(3, 10)
                            self.__headerToBound[headerID][ancestorHeaderID] = bound
                            Debug.debugMessage("Bound(%d w.r.t %d) = %d" % (headerID, ancestorHeaderID, bound), 1)
    
    def getBasicBlockWCET (self, vertexID):
        if vertexID in self.__bbToWCET:
            return self.__bbToWCET[vertexID]
        else:
            Debug.warningMessage("Returning WCET of 0 for vertex %d" % vertexID)
            return 0
    
    def getTransitionWCET (self, predID, succID):
        if (predID, succID) in self.__transitionWCET:
            return self.__transitionWCET[(predID, succID)]
        else:
            Debug.warningMessage("Returning WCET of 0 for edge (%d, %d)" % (predID, succID))
            return 0
        
    def getLoopBound (self, headerID, ancestorID):
        assert headerID in self.__headerToBound, "Unable to find bound for header %d" % headerID
        assert ancestorID in self.__headerToBound[headerID], "Unable to find bound for header %d w.r.t. header %d" % (headerID, ancestorID)
        return self.__headerToBound[headerID][ancestorID]
    
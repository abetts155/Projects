import debug
import vertices
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
        debug.debug_message("Assigning WCETs to basic blocks", __name__, 1)
        for v in icfg:
            if not icfg.isIpoint(v.vertexID):
                self.__bbToWCET[v.vertexID] = random.randint(1, 10)
                debug.debug_message("WCET(%d) = %d" % (v.vertexID, self.__bbToWCET[v.vertexID]), 
                                    __name__,
                                    1)
                
    def __assignWCETsToIPGTransitions (self, ipg, icfg=None):
        debug.debug_message("Assigning WCETs to IPG transitions", __name__, 1)
        if icfg:
            for v in ipg:
                for succID in v.getSuccessorIDs():
                    transitionWCET = 0
                    succe          = v.getSuccessorEdge(succID)
                    for bbID in succe.edge_label:
                        transitionWCET += self.__bbToWCET[bbID]
                    self.__transitionWCET[(v.vertexID, succID)] = transitionWCET
                    debug.debug_message("WCET(%d, %d) = %d \t[Basic blocks = %s]" % (v.vertexID, succID, transitionWCET, succe.edge_label), 
                                        __name__,
                                        1)
        else:
            for v in ipg:
                for succID in v.getSuccessorIDs():
                    succe = v.getSuccessorEdge(succID)
                    if not succe.isDummyEdge():
                        transitionWCET = random.randint(1, 10)
                        self.__transitionWCET[(v.getVertexID(), succID)] = transitionWCET
                        debug.debug_message("WCET(%d, %d) = %d" % (v.getVertexID(), succID, transitionWCET), 
                                            __name__,
                                            1)
    
    def __assignLoopBounds (self, lnt):
        debug.debug_message("Assigning bounds to loop", 1)
        for level, the_vertices in lnt.levelIterator(True):
            for v in the_vertices:
                if isinstance(v, vertices.HeaderVertex):
                    if level > 0:
                        bound                          = 0
                        headerID                       = v.headerID
                        self.__headerToBound[headerID] = {}
                        for ancestorv in lnt.getAllProperAncestors(v.vertexID):
                            ancestorHeaderID = ancestorv.headerID
                            bound            += random.randint(3, 10)
                            self.__headerToBound[headerID][ancestorHeaderID] = bound
                            debug.debug_message("Bound(%d w.r.t %d) = %d" % (headerID, ancestorHeaderID, bound), 
                                                __name__, 
                                                1)
    
    def getBasicBlockWCET (self, vertexID):
        if vertexID in self.__bbToWCET:
            return self.__bbToWCET[vertexID]
        else:
            debug.warning_message("Returning WCET of 0 for vertex %d" % vertexID)
            return 0
    
    def getTransitionWCET (self, predID, succID):
        if (predID, succID) in self.__transitionWCET:
            return self.__transitionWCET[(predID, succID)]
        else:
            debug.warning_message("Returning WCET of 0 for edge (%d, %d)" % (predID, succID))
            return 0
        
    def getLoopBound (self, headerID, ancestorID):
        assert headerID in self.__headerToBound, "Unable to find bound for header %d" % headerID
        assert ancestorID in self.__headerToBound[headerID], "Unable to find bound for header %d w.r.t. header %d" % (headerID, ancestorID)
        return self.__headerToBound[headerID][ancestorID]
    
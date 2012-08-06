import IPGs, Vertices, Edges, Debug

class WarpTrace ():
    def __init__(self, multiprocessorID, warpID):
        self.__multiprocessorID = multiprocessorID
        self.__warpID = warpID
        self.__theTrace = []
                
    def getWarpID (self):
        return self.__warpID
    
    def getMultiprocessorID (self):
        return self.__multiprocessorID
    
    def appendToTrace (self, traceTuple):
        self.__theTrace.append(traceTuple)
    
    def getTrace (self):
        return self.__theTrace
    
class _TraceParser ():
    def __init__(self, warpTrace, ipg):
        Debug.debugMessage("SM %s, WARP %s" % (warpTrace.getMultiprocessorID(), warpTrace.getWarpID()), 10)
        self.__warpTrace = warpTrace
        self.numberOfTraces = 0
        self.edgeIDToWCET = {}
        self.edgeIDToExecutionCounts = {}
        self.edgeIDToTotalTime = {}
        self.__doParsing(ipg)
        
    def __doParsing (self, ipg):
        newTrace = True
        currentv = None
        lastTime = 0
        for t in self.__warpTrace.getTrace():
            ipointID = int(t[0], 0)
            time     = long(t[1])
            if newTrace:
                self.numberOfTraces += 1
                newTrace = False
                currentv = ipg.getVertex(ipg.getEntryID())
                assert currentv.getIpointID () == ipointID
                lastTime = time
            else:
                succID = currentv.getIpointSuccessor(ipointID)
                succe  = currentv.getSuccessorEdge(succID)
                self.__analyseEdgeTime(succe, time - lastTime)
                # Advance transition
                lastTime = time
                currentv = ipg.getVertex(succID)
                if succID == ipg.getExitID():
                    newTrace = True
                    
    def __analyseEdgeTime (self, succe, time):
        edgeID = succe.getEdgeID()
        Debug.debugMessage("Time for edge %s = %s" % (edgeID, time), 10)
        if edgeID not in self.edgeIDToWCET.keys():
            self.edgeIDToWCET[edgeID]            = time
            self.edgeIDToTotalTime[edgeID]       = time
            self.edgeIDToExecutionCounts[edgeID] = 1
        else:
            if time > self.edgeIDToWCET[edgeID]:
                self.edgeIDToWCET[edgeID] = time
            self.edgeIDToTotalTime[edgeID]       += time
            self.edgeIDToExecutionCounts[edgeID] += 1
            
class TraceData ():
    def __init__(self, allWarpTraces, ipg):
        self.__edgeIDToWCET = {}
        self.__edgeIDToExecutionCounts = {}
        self.__edgeIDToTotalTime = {}
        
        tps = set([])          
        for w in allWarpTraces:
            tps.add(_TraceParser(w, ipg))
        
        for v in ipg:
            for succe in v.getSuccessorEdges():
                edgeID = succe.getEdgeID()
                self.__edgeIDToWCET[edgeID]            = 0
                self.__edgeIDToTotalTime[edgeID]       = 0
                self.__edgeIDToExecutionCounts[edgeID] = 0
                for tp in tps:
                    if edgeID in tp.edgeIDToWCET.keys():
                        wcet = tp.edgeIDToWCET[edgeID]
                        if wcet > self.__edgeIDToWCET[edgeID]:
                            self.__edgeIDToWCET[edgeID] = wcet
                        self.__edgeIDToTotalTime[edgeID] += tp.edgeIDToTotalTime[edgeID]
                        self.__edgeIDToExecutionCounts[edgeID] += tp.edgeIDToExecutionCounts[edgeID]
                        
    def getWCETOfEdge (self, edgeID):
        if edgeID in self.__edgeIDToWCET.keys():
            return self.__edgeIDToWCET[edgeID]
        else:
            return 0
        
    def getACETOfEdge (self, edgeID):
        if edgeID in self.__edgeIDToTotalTime.keys():
            assert self.__edgeIDToExecutionCounts[edgeID] != 0
            return self.__edgeIDToTotalTime[edgeID]/self.__edgeIDToExecutionCounts[edgeID]
        else:
            return 0
        
    def getExecutionCountOfEdge (self, edgeID):
        if edgeID in self.__edgeIDToExecutionCounts.keys():
            return self.__edgeIDToExecutionCounts[edgeID]
        else:
            return 0
    
    def output (self):
        for edgeID, WCET in self.__edgeIDToWCET.iteritems():
            executionCount = self.__edgeIDToExecutionCounts[edgeID]
            print "%s Edge %d %s" % ("=" * 10, edgeID, "=" * 10)
            if executionCount > 0:
                totalTime = self.__edgeIDToTotalTime[edgeID]
                print "WCET:            %d" % WCET
                print "Total time:      %d" % totalTime
                print "Execution count: %d" % executionCount
                print "ACET:            %d" % (totalTime/executionCount)
            else:
                print "NOT executed"
            
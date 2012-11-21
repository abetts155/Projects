import Debug
import sys

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
    def __init__(self, warpTrace, program):
        Debug.debugMessage("Parsing traces in SM %s, WARP %s" % (warpTrace.getMultiprocessorID(), warpTrace.getWarpID()), 5)
        self.numberOfTraces = {}
        # High water mark time
        self.highWaterMark = {}
        self.totalEnd2End = {}
        # Best-case execution time
        self.edgeIDToBCET = {}
        # Worst-case execution time
        self.edgeIDToWCET = {}
        # Worst-case execution count (overall and in run currently under analysis)
        self.edgeIDToWCEC = {}
        self.edgeIDToWCECInRun = {}
        # Total execution count
        self.edgeIDToExecutionCounts = {}
        # Total execution time consumed
        self.edgeIDToTotalTime = {}
        self.warpTrace = warpTrace
        self.__doParsing(program)
        
    def __getIPG (self, program, ipointID):
        for ipg in program.getIPGs():
            startv = ipg.getVertex(ipg.getEntryID())
            if startv.getIpointID() == ipointID:
                functionName = ipg.getName()
                if functionName not in self.highWaterMark:
                    self.highWaterMark[functionName] = 0
                    self.totalEnd2End[functionName] = 0
                    self.numberOfTraces[functionName] = 0
                return ipg
        assert False, "Cannot find IPG whose entry Ipoint ID is %d" % ipointID
        
    def __doParsing (self, program):
        newTrace  = True
        currentv  = None
        lastTime  = 0
        startTime = 0
        ipg       = None
        
        for t in self.warpTrace.getTrace():
            ipointID = int(t[0], 0)
            time     = long(t[1])
            Debug.debugMessage("Trace tuple (0x%04X, %d)" % (ipointID, time), 10)
            if newTrace:
                ipg          = self.__getIPG(program, ipointID)
                functionName = ipg.getName()
                self.numberOfTraces[functionName] += 1
                newTrace = False
                currentv = ipg.getVertex(ipg.getEntryID())
                assert currentv.getIpointID () == ipointID
                lastTime = time
                startTime = time
            else:
                succID = currentv.getIpointSuccessor(ipointID)
                succe  = currentv.getSuccessorEdge(succID)
                self.__analyseEdgeTime(succe, time - lastTime)
                # Advance transition
                lastTime = time
                currentv = ipg.getVertex(succID)
                if succID == ipg.getExitID():
                    newTrace     = True
                    runTime      = lastTime - startTime
                    functionName = ipg.getName()
                    self.totalEnd2End[functionName] += runTime
                    if runTime > self.highWaterMark[functionName]:
                        self.highWaterMark[functionName] = runTime
                    self.__analyseWorstCaseExecutionCounts()
                    ipg = None
                    
    def __analyseEdgeTime (self, succe, time):
        edgeID = succe.getEdgeID()
        if edgeID not in self.edgeIDToWCET.keys():
            self.edgeIDToBCET[edgeID]            = time
            self.edgeIDToWCET[edgeID]            = time
            self.edgeIDToTotalTime[edgeID]       = time
            self.edgeIDToExecutionCounts[edgeID] = 1
            self.edgeIDToWCECInRun[edgeID]       = 1
        else:
            if time > self.edgeIDToWCET[edgeID]:
                self.edgeIDToWCET[edgeID] = time
            if time < self.edgeIDToBCET[edgeID]:
                self.edgeIDToBCET[edgeID] = time
            self.edgeIDToTotalTime[edgeID]       += time
            self.edgeIDToExecutionCounts[edgeID] += 1
            self.edgeIDToWCECInRun[edgeID]       += 1
             
    def __analyseWorstCaseExecutionCounts (self):
        for edgeID, WCEC in self.edgeIDToWCECInRun.iteritems():
            if edgeID not in self.edgeIDToWCEC.keys():
                self.edgeIDToWCEC[edgeID] = WCEC
            else:
                if WCEC > self.edgeIDToWCEC[edgeID]:
                    self.edgeIDToWCEC[edgeID] = WCEC
            self.edgeIDToWCECInRun[edgeID] = 0

class TraceData ():
    def __init__(self, allWarpTraces, program):
        self.__highWaterMark = {}
        self.__end2endACET = {}
        self.__edgeIDToBCET = {}
        self.__edgeIDToWCET = {}
        self.__edgeIDToWCEC = {}
        self.__edgeIDToExecutionCounts = {}
        self.__edgeIDToTotalTime = {}
        self.__numberOfWarps = len(allWarpTraces)
        self.__SMTOWarps = {}
        self.__edgeIDToWorstCaseWarpTrace = {}
        self.__edgeIDToBestCaseWarpTrace = {}
        self.__program = program
        
        # Parse each warp-specific trace
        self.tps = set([])          
        for w in allWarpTraces.values():
            self.tps.add(_TraceParser(w, program))
            SMID = w.getMultiprocessorID()
            if SMID not in self.__SMTOWarps.keys():
                self.__SMTOWarps[SMID] = [w.getWarpID()]
            else:
                self.__SMTOWarps[SMID].append(w.getWarpID())
                
        for ipg in program.getIPGs():
            functionName = ipg.getName()
            self.__highWaterMark[functionName] = 0
            self.__end2endACET[functionName] = 0
            
            # First work out the HWMT for each kernel and its average end-to-end
            # execution time
            averageDividend = 0
            averageDivisor  = 0
            for t in self.tps:
                if functionName in t.highWaterMark:
                    if t.highWaterMark[functionName] > self.__highWaterMark[functionName]:
                        self.__highWaterMark[functionName] = t.highWaterMark[functionName]
                    averageDividend += t.totalEnd2End[functionName]
                    averageDivisor  += t.numberOfTraces[functionName]
                    assert averageDivisor > 0
                    self.__end2endACET[functionName] = averageDividend/averageDivisor
            
            # Now work out timing information related to each edge
            for v in ipg:
                for succe in v.getSuccessorEdges():
                    edgeID = succe.getEdgeID()
                    self.__edgeIDToBCET[edgeID]            = sys.maxint
                    self.__edgeIDToWCET[edgeID]            = 0
                    self.__edgeIDToWCEC[edgeID]            = 0
                    self.__edgeIDToTotalTime[edgeID]       = 0
                    self.__edgeIDToExecutionCounts[edgeID] = 0
                    for tp in self.tps:
                        if edgeID in tp.edgeIDToWCET.keys():
                            wcet = tp.edgeIDToWCET[edgeID]
                            wcec = tp.edgeIDToWCEC[edgeID]
                            bcet = tp.edgeIDToBCET[edgeID]
                            if wcet > self.__edgeIDToWCET[edgeID]:
                                self.__edgeIDToWCET[edgeID] = wcet
                                self.__edgeIDToWorstCaseWarpTrace[edgeID] = tp.warpTrace
                            if wcec > self.__edgeIDToWCEC[edgeID]:
                                self.__edgeIDToWCEC[edgeID] = wcec
                            if bcet < self.__edgeIDToBCET[edgeID]:
                                self.__edgeIDToBCET[edgeID] = bcet
                                self.__edgeIDToBestCaseWarpTrace[edgeID] = tp.warpTrace
                            self.__edgeIDToTotalTime[edgeID] += tp.edgeIDToTotalTime[edgeID]
                            self.__edgeIDToExecutionCounts[edgeID] += tp.edgeIDToExecutionCounts[edgeID]
                        
    def getWCETOfEdge (self, edgeID):
        if edgeID in self.__edgeIDToWCET.keys():
            return self.__edgeIDToWCET[edgeID]
        else:
            return 0
        
    def getWCECOfEdge (self, edgeID):
        if edgeID in self.__edgeIDToWCEC.keys():
            return self.__edgeIDToWCEC[edgeID]
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
        
    def getEdgeIDs (self):
        return self.__edgeIDToWCET.keys()
    
    def getHWMT(self, functionName):
        assert functionName in self.__highWaterMark, "Unable to find HWMT for '%s'" % functionName
        return self.__highWaterMark[functionName]
    
    def getACET(self, functionName):
        assert functionName in self.__end2endACET, "Unable to find ACET for '%s'" % functionName
        return self.__end2endACET[functionName]
    
    def output (self):
        print "%s IPG edge data %s" % ("=" * 11, "=" * 11)
        for edgeID, WCET in self.__edgeIDToWCET.iteritems():
            executionCount = self.__edgeIDToExecutionCounts[edgeID]
            print "%s Edge %d %s" % ("=" * 22, edgeID, "=" * 22)
            if executionCount > 0:
                totalTime = self.__edgeIDToTotalTime[edgeID]
                print "BCET:            %d. (Arises from warp %d in SM %d)" \
                % (self.__edgeIDToBCET[edgeID], self.__edgeIDToBestCaseWarpTrace[edgeID].getWarpID(), self.__edgeIDToBestCaseWarpTrace[edgeID].getMultiprocessorID())
                print "ACET:            %d" % (totalTime/executionCount)
                print "WCET:            %d. (Arises from warp %d in SM %d)" \
                % (WCET, self.__edgeIDToWorstCaseWarpTrace[edgeID].getWarpID(), self.__edgeIDToWorstCaseWarpTrace[edgeID].getMultiprocessorID())
                print "WCEC:            %d" % self.__edgeIDToWCEC[edgeID]
                print "Total time:      %d" % totalTime
                print "Execution count: %d" % (executionCount/self.__numberOfWarps)
            else:
                print "NOT executed"
        
        print "%s Streaming Multiprocessor data %s" % ("=" * 11, "=" * 11)
        for SMID, warps in self.__SMTOWarps.iteritems():
            print "SM %d had %d warps" % (SMID, len(warps))

            
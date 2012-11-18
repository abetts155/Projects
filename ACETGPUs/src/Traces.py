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
    def __init__(self, warpTrace, ipg):
        Debug.debugMessage("SM %s, WARP %s" % (warpTrace.getMultiprocessorID(), warpTrace.getWarpID()), 10)
        self.numberOfTraces = 0
        # High water mark time
        self.highWaterMark = 0
        self.totalEnd2End = 0
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
        self.__doParsing(ipg)
        
    def __doParsing (self, ipg):
        newTrace  = True
        currentv  = None
        lastTime  = 0
        startTime = 0
        
        for t in self.warpTrace.getTrace():
            ipointID = int(t[0], 0)
            time     = long(t[1])
            if newTrace:
                self.numberOfTraces += 1
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
                    newTrace = True
                    runTime  = lastTime - startTime
                    self.totalEnd2End += runTime
                    if runTime > self.highWaterMark:
                        self.highWaterMark = runTime
                    self.__analyseWorstCaseExecutionCounts()
                    
    def __analyseEdgeTime (self, succe, time):
        edgeID = succe.getEdgeID()
        Debug.debugMessage("Time for edge %s = %s" % (edgeID, time), 10)
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
    def __init__(self, allWarpTraces, ipg):
        self.__highWaterMark = 0
        self.__end2endACET = 0
        self.__edgeIDToBCET = {}
        self.__edgeIDToWCET = {}
        self.__edgeIDToWCEC = {}
        self.__edgeIDToExecutionCounts = {}
        self.__edgeIDToTotalTime = {}
        self.__numberOfWarps = len(allWarpTraces)
        self.__SMTOWarps = {}
        self.__edgeIDToWorstCaseWarpTrace = {}
        self.__edgeIDToBestCaseWarpTrace = {}
        
        Debug.debugMessage("Total number of warps = %d" % self.__numberOfWarps, 1)
        
        tps = set([])          
        for w in allWarpTraces:
            tps.add(_TraceParser(w, ipg))
            SMID = w.getMultiprocessorID()
            if SMID not in self.__SMTOWarps.keys():
                self.__SMTOWarps[SMID] = [w.getWarpID()]
            else:
                self.__SMTOWarps[SMID].append(w.getWarpID())
        
        averageDividend = 0
        averageDivisor  = 0
        for t in tps:
            if t.highWaterMark > self.__highWaterMark:
                self.__highWaterMark = t.highWaterMark
            averageDividend += t.totalEnd2End
            averageDivisor  += t.numberOfTraces
        assert averageDivisor > 0
        self.__end2endACET = averageDividend/averageDivisor
        
        for v in ipg:
            for succe in v.getSuccessorEdges():
                edgeID = succe.getEdgeID()
                self.__edgeIDToBCET[edgeID]            = sys.maxint
                self.__edgeIDToWCET[edgeID]            = 0
                self.__edgeIDToWCEC[edgeID]            = 0
                self.__edgeIDToTotalTime[edgeID]       = 0
                self.__edgeIDToExecutionCounts[edgeID] = 0
                for tp in tps:
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
    
    def output (self):
        print "ACET = %d" % self.__end2endACET
        print "HWMT = %d" % self.__highWaterMark
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
            
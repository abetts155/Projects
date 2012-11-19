import Debug

edgePrefix = "e_"
endStmt    = ";"
plus       = " + "
equals     = " = "
ltOrEqual  = " <= "
comma      = ", "
newLine    = "\n"

class LinearProgram ():
    
    def __init__(self, ipg, traceData, inputFilename):
        self.__wcet    = 0
        index          = inputFilename.find('.')
        outputFilename = inputFilename[:index] + ".ilp"
        with open(outputFilename, 'w') as f:
            self.__writeObjectiveFunction(f, traceData)
            self.__writeStructuralConstraints(f, ipg)
            self.__writeCountConstraints(f, traceData, ipg)
            self.__writeNonNegativeConstraints(f, traceData)
        self.__solve(outputFilename)
        
    def __solve(self, ilpFile):
        from subprocess import Popen, PIPE
        import shlex
        Debug.debugMessage("Solving ILP", 1)
        command = "lp_solve %s" % ilpFile 
        proc = Popen(command, shell=True, executable="/bin/bash", stdout=PIPE, stderr=PIPE)
        returnCode = proc.wait()
        if returnCode != 0:
            Debug.exitMessage("Running '%s' failed" % command)
        for line in proc.stdout.readlines():
            if line.startswith("Value of objective function"):
                lexemes  = shlex.split(line)
                wcet     = long(lexemes[-1])
                self.__wcet = wcet
                print "WCET = %ld" % wcet
            
    def __writeObjectiveFunction(self, f, traceData):
        f.write("max: ")
        count = 0
        numOfEdges = len(traceData.getEdgeIDs())
        for edgeID in traceData.getEdgeIDs():
            count += 1
            wcet = traceData.getWCETOfEdge(edgeID)
            f.write("%d %s%d" % (wcet, edgePrefix, edgeID))
            if count < numOfEdges:
                f.write(plus)
        f.write("%s%s" % (endStmt, newLine))
    
    def __writeStructuralConstraints(self, f, ipg):
        f.write(newLine)
        for vertexID in ipg.vertices:
            v     = ipg.getVertex(vertexID)
            count = 0
            for prede in v.getPredecessorEdges ():
                count += 1
                f.write("%s%d" % (edgePrefix, prede.getEdgeID()))
                if count < v.numberOfPredecessors():
                    f.write(plus)
    
            f.write(equals)
            
            count = 0
            for succe in v.getSuccessorEdges ():
                count += 1
                f.write("%s%d" % (edgePrefix, succe.getEdgeID()))
                if count < v.numberOfSuccessors():
                    f.write(plus)
                
            f.write("%s%s" % (endStmt, newLine))
            
    def __writeCountConstraints(self, f, traceData, ipg):
        f.write(newLine)
        startv = ipg.getVertex(ipg.getEntryID())
        for edgeID in traceData.getEdgeIDs():
            wcec = traceData.getWCECOfEdge(edgeID)
            for prede in startv.getPredecessorEdges():
                if edgeID == prede.getEdgeID():
                    wcec = 1
            f.write("%s%d%s%d" % (edgePrefix, edgeID, ltOrEqual, wcec))
            f.write("%s%s" % (endStmt, newLine))
    
    def __writeNonNegativeConstraints(self, f, traceData):
        f.write(newLine)
        f.write("int ")
        count = 0
        numOfEdges = len(traceData.getEdgeIDs())
        for edgeID in traceData.getEdgeIDs():
            count += 1
            f.write("%s%d" % (edgePrefix, edgeID))
            if count < numOfEdges:
                f.write(comma)
        f.write("%s%s" % (endStmt, newLine))
    
    
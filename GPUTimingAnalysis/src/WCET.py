import Debug
import decimal, os

edgePrefix = "e_"
endStmt    = ";"
plus       = " + "
equals     = " = "
ltOrEqual  = " <= "
comma      = ", "
newLine    = "\n"

class LinearProgram ():
    def __init__(self, ipg, traceData, basename, basepath):
        self.__wcet    = 0
        outputFilename = basepath + os.sep + basename + ".ilp"
        with open(outputFilename, 'w') as f:
            self.__writeObjectiveFunction(f, ipg, traceData)
            self.__writeStructuralConstraints(f, ipg)
            self.__writeCountConstraints(f, ipg, traceData)
            self.__writeNonNegativeConstraints(f, ipg)
        self.__solve(ipg, outputFilename)
    
    def getWCET (self):
        return self.__wcet
        
    def __solve(self, ipg, ilpFile):
        from subprocess import Popen, PIPE
        import shlex
        Debug.debugMessage("Solving ILP", 10)
        command = "lp_solve %s" % ilpFile 
        proc = Popen(command, shell=True, executable="/bin/bash", stdout=PIPE, stderr=PIPE)
        returnCode = proc.wait()
        if returnCode != 0:
            Debug.exitMessage("Running '%s' failed" % command)
        for line in proc.stdout.readlines():
            if line.startswith("Value of objective function"):
                lexemes  = shlex.split(line)
                wcet     = long(decimal.Decimal(lexemes[-1]))
                self.__wcet = wcet
            
    def __writeObjectiveFunction(self, f, ipg, traceData):
        f.write("max: ")
        count = 0
        numOfEdges = ipg.numOfEdges()
        for vertexID in ipg.vertices:
            v  = ipg.getVertex(vertexID)
            for succe in v.getSuccessorEdges ():
                count += 1
                edgeID = succe.getEdgeID()
                wcet   = traceData.getWCETOfEdge(edgeID)
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
            
    def __writeCountConstraints(self, f, ipg, traceData):
        f.write(newLine)
        for vertexID in ipg.vertices:
            v  = ipg.getVertex(vertexID)
            for succe in v.getSuccessorEdges ():
                edgeID = succe.getEdgeID()
                wcec = traceData.getWCECOfEdge(edgeID)
                if vertexID == ipg.getExitID():
                    wcec = 1
                f.write("%s%d%s%d" % (edgePrefix, edgeID, ltOrEqual, wcec))
                f.write("%s%s" % (endStmt, newLine))
    
    def __writeNonNegativeConstraints(self, f, ipg):
        f.write(newLine)
        f.write("int ")
        count = 0
        numOfEdges = ipg.numOfEdges()
        for vertexID in ipg.vertices:
            v  = ipg.getVertex(vertexID)
            for succe in v.getSuccessorEdges ():
                edgeID = succe.getEdgeID()
                count += 1
                f.write("%s%d" % (edgePrefix, edgeID))
                if count < numOfEdges:
                    f.write(comma)
        f.write("%s%s" % (endStmt, newLine))
    
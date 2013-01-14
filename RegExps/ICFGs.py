import CFGs, Debug

class ICFG (CFGs.CFG):
    def __init__ (self):
        CFGs.CFG.__init__(self)
    
    def addBasicBlock (self, bb):
        bbID = bb.getVertexID()
        assert bbID not in self.vertices, \
        "Adding basic block %d which is already in graph" % bbID
        self.vertices[bbID] = bb

    def addIpoint (self, ipoint):
        vertexID = ipoint.getVertexID()
        assert vertexID not in self.vertices, \
        "Adding Ipoint %d which is already in graph" % vertexID
        self.vertices[vertexID] = ipoint
        
    def isIpoint (self, v):
        return isinstance(v, CFGs.Ipoint)
    
    def isBasicBlock (self, v):
        return isinstance(v, CFGs.BasicBlock)
    
    def addIpointEdges (self):
        bbToIpoint = {}
        bbToPosition = {}
        for v in self:
            if isinstance(v, CFGs.BasicBlock):
                if v.hasIpoint():
                    vertexID = self.getNextVertexID()
                    ipoint   = CFGs.Ipoint(vertexID, vertexID)
                    self.vertices[ipoint.getVertexID()] = ipoint
                    bbToIpoint[v] = ipoint
                    bbToPosition[v] = v.ipointPosition()
                    Debug.debugMessage("Adding Ipoint %d for basic block %d" % (vertexID, v.getVertexID()), 10)
        for bb, ipoint in bbToIpoint.items():
            if bbToPosition[bb] == CFGs.BasicBlock.IpointPosition.start:
                for predID in bb.getPredecessorIDs():
                    self.addEdge(predID, ipoint.getVertexID())
                    self.removeEdge(predID, bb.getVertexID())
                self.addEdge(ipoint.getVertexID(), bb.getVertexID())
            else:
                for succID in bb.getSuccessorIDs():
                    self.addEdge(ipoint.getVertexID(), succID)
                    self.removeEdge(bb.getVertexID(), succID)
                self.addEdge(bb.getVertexID(), ipoint.getVertexID())
            
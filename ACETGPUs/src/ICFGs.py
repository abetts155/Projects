import CFGs, Vertices, Debug
import copy

class ICFG (CFGs.CFG):
    def __init__ (self, cfg):
        Debug.debugMessage(cfg, 10)
        CFGs.CFG.__init__(self)
        self.setName(cfg.getName())
        for bb in cfg:
            bbID = bb.getVertexID()
            self.vertices[bbID] = copy.deepcopy(bb)
        self.__addIpoints()
    
    def __addIpoints (self):
        for bb in self:
            address, instr = bb.getFirstInstruction()
            vertexID = self.getNextVertexID ()
            ipoint = Vertices.Ipoint(vertexID, int(address, 16))
            self.vertices[vertexID] = ipoint
            Debug.debugMessage("Adding Ipoint %s with ID %s" % (vertexID, address), 4)
            self.__linkIpoint(bb, ipoint)
            
    def __linkIpoint (self, bb, ipoint):
        for predID in bb.getPredecessorIDs():
            predv = self.getVertex(predID)
            self.addEdge(predID, ipoint.getVertexID())
            predv.removeSuccessor(bb.getVertexID())
            bb.removePredecessor(predID)
        self.addEdge(ipoint.getVertexID(), bb.getVertexID())
        
    def isIpoint (self, vertexID):
        v = self.getVertex(vertexID)
        return isinstance(v, Vertices.Ipoint)
    
    def getIpoint (self, vertexID):
        v = self.getVertex(vertexID)
        assert isinstance(v, Vertices.Ipoint), "Vertex %s is not an Ipoint" % vertexID
        return v
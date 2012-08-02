import CFGs, Vertices, Debug
import copy

class ICFG (CFGs.CFG):
    def __init__ (self, cfg):
        Debug.debugMessage(cfg, 10)
        CFGs.CFG.__init__(self)
        for bb in cfg:
            bbID = bb.getVertexID()
            self.vertices[bbID] = copy.deepcopy(bb)
        self.addIpoints()
    
    def addIpoints (self):
        for bb in self:
            address, instr = bb.getFirstInstruction()
            vertexID = self.getNextVertexID ()
            ipoint = Vertices.Ipoint(vertexID, address)
            self.vertices[vertexID] = ipoint
            Debug.debugMessage("Adding Ipoint %s with ID %s" % (vertexID, address), 4)
            self.linkIpoint(bb, ipoint)
            
    def linkIpoint (self, bb, ipoint):
        for predID in bb.getPredecessorIDs():
            predv = self.getVertex(predID)
            self.addEdge(predID, ipoint.getVertexID())
            predv.removeSuccessor(bb.getVertexID())
            bb.removePredecessor(predID)
        self.addEdge(ipoint.getVertexID(), bb.getVertexID())
        
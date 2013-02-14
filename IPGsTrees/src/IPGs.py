from DirectedGraphs import FlowGraph
from Trees import DepthFirstSearch
from Vertices import Ipoint
from Edges import IPGEdge
import Debug

edgeID = 1

class IPG (FlowGraph):
    def __init__(self, icfg):
        FlowGraph.__init__(self)
        self.__icfg              = icfg
        self.__ipointIDToVertex  = {}
        self.__vertexToReachable = {}
        self.__initialise()
        self.__addEdges()
        self.__setEntryAndExit()
        self.setName(icfg.getName())
        
    def getIpointVertex (self, ipointID):
        assert ipointID in self.__ipointIDToVertex, "Unable to find Ipoint with trace ID 0x%04X" % ipointID
        return self.__ipointIDToVertex[ipointID]
    
    def __initialise (self):
        for v in self.__icfg:
            vertexID = v.getVertexID()
            self.__vertexToReachable[vertexID] = set([])
            if self.__icfg.isIpoint(vertexID):
                ipointv = Ipoint(vertexID, v.getIpointID())
                self.vertices[vertexID] = ipointv
                self.__ipointIDToVertex[v.getIpointID()] = ipointv 
                
    def __addEdges (self):
        changed = True
        dfs     = DepthFirstSearch(self.__icfg, self.__icfg.getEntryID())
        # Do data-flow analysis
        while changed:
            changed = False
            for vertexID in reversed(dfs.getPostorder()):
                v = self.__icfg.getVertex(vertexID)
                for predID in v.getPredecessorIDs():
                    if self.__icfg.isIpoint(predID):
                        if predID not in self.__vertexToReachable[vertexID]:
                            self.__vertexToReachable[vertexID].add(predID)
                            changed = True
                    else:
                        for keyID in self.__vertexToReachable[predID]:
                            if keyID not in self.__vertexToReachable[vertexID]:
                                self.__vertexToReachable[vertexID].add(keyID)
                                changed = True
        # Now add the edges 
        for vertexID, predIDs in self.__vertexToReachable.iteritems():
            if self.__icfg.isIpoint(vertexID):
                for predID in predIDs:
                    self.__addEdge(predID, vertexID)
    
    def __addEdge (self, predID, succID):
        global edgeID
        Debug.debugMessage("Adding IPG Edge %s => %s" % (predID, succID), 10)
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
        predv.addIpointSuccessor (succv.getIpointID(), succID)
        succe = IPGEdge(succID, edgeID)
        prede = IPGEdge(predID, edgeID)
        predv.addSuccessorEdge(succe)
        succv.addPredecessorEdge(prede) 
        edgeID += 1
                    
    def __setEntryAndExit (self):
        entryv = self.vertices[self.__icfg.getEntryID()]
        self._entryID = entryv.getVertexID()
        assert entryv.numberOfPredecessors() == 1, "Entry vertex %s of IPG has multiple predecessors" % self._entryID
        for predID in entryv.getPredecessorIDs():
            self._exitID = predID

    
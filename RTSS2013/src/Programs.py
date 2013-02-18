from DirectedGraphs import DirectedGraph, dummyVertexID
from Vertices import CallGraphVertex
import Debug

class CallGraph (DirectedGraph):   
    def __init__ (self):
        DirectedGraph.__init__(self)
        self.__rootID = dummyVertexID
        self.__functionNameToVertex = {}
        
    def addVertex (self, functionName):
        assert functionName not in self.__functionNameToVertex, "Trying to add duplicate call graph vertex for function '%s'" % functionName
        Debug.debugMessage("Adding call graph vertex for function '%s'" % functionName, 5)
        vertexID = self.getNextVertexID()
        callv    = CallGraphVertex(vertexID, functionName)
        self.vertices[vertexID] = callv 
        self.__functionNameToVertex[functionName] = callv
        
    def getVertexWithName (self, functionName):
        assert functionName in self.__functionNameToVertex, "Unable to find call graph vertex for function '%s'" % functionName
        return self.__functionNameToVertex[functionName]
        
    def setRootID (self, rootID):
        self.__rootID = rootID
    
    def getRootID (self):
        assert self.__rootID != dummyVertexID, "Root vertex of call graph has not been set"
        return self.__rootID
    
    def addEdge (self, predName, succName, callSiteID):
        Debug.debugMessage("Adding call graph edge %s => %s" % (predName, succName), 5)
        predv = self.getVertexWithName(predName)
        succv = self.getVertexWithName(succName)
        predv.addSuccessor(succv.getVertexID(), callSiteID)            
        succv.addPredecessor(predv.getVertexID(), callSiteID)
    
class Program():
    def __init__(self):
        self.__callg = CallGraph()
        self.__icfgs = {}
        self.__ipgs = {}
        self.__lnts = {}
        
    def getCallGraph (self):
        return self.__callg
       
    def addICFG (self, icfg, functionName):
        assert functionName not in self.__icfgs, "Trying to add duplicate ICFG for function '%s'" % functionName
        self.__callg.addVertex(functionName)
        self.__icfgs[functionName] = icfg
        
    def addIPG (self, ipg, functionName):
        assert functionName not in self.__ipgs, "Trying to add duplicate IPG for function '%s'" % functionName
        self.__ipgs[functionName] = ipg
        
    def addLNT (self, lnt, functionName):
        assert functionName not in self.__lnts, "Trying to add duplicate LNT for function '%s'" % functionName
        self.__lnts[functionName] = lnt
        
    def getICFG (self, functionName):
        assert functionName in self.__icfgs, "Unable to find ICFG for function '%s'" % functionName
        return self.__icfgs[functionName]
    
    def getIPG (self, functionName):
        assert functionName in self.__ipgs, "Unable to find IPG for function '%s'" % functionName
        return self.__ipgs[functionName]

    def getLNT (self, functionName):
        assert functionName in self.__lnts, "Unable to find LNT for function '%s'" % functionName
        return self.__lnts[functionName]

    def getICFGs (self):
        return self.__icfgs.values().__iter__() 

    def getIPGs (self):
        return self.__ipgs.values().__iter__() 
    
    def getLNTs (self):
        return self.__lnts.values().__iter__() 

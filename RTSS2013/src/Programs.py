from DirectedGraphs import DirectedGraph
from Vertices import CallGraphVertex, dummyVertexID
import Debug

class CallGraph (DirectedGraph):   
    def __init__ (self):
        DirectedGraph.__init__(self)
        self.__rootID = dummyVertexID
        self.__functionNameToVertex = {}
        self.__callSites = {}
        
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
    
    def getRootVertex (self):
        assert self.__rootID != dummyVertexID, "The root of the call graph has not been set"
        return self.getVertex(self.__rootID)
    
    def findAndSetRoot (self):
        withoutPred = []
        for v in self:
            if v.numberOfPredecessors() == 0:
                withoutPred.append(v.getVertexID())
        if len(withoutPred) == 0:
            Debug.exitMessage("Could not find program entry point as there are no functions without predecessors")
        elif len(withoutPred) > 1:
            debugStr = ""
            for vertexID in withoutPred:
                callv    = self.getVertex(vertexID)
                debugStr += callv.__str__()
            Debug.exitMessage("Call graph has too many entry points: %s" % debugStr)
        else:
            self.__rootID = withoutPred[0]
        assert self.__rootID, "Unable to set root ID of call graph"
        
    def setRoot (self, functionName):
        rootv = self.getVertexWithName(functionName)
        if rootv.numberOfPredecessors() > 0:
            Debug.warningMessage("Root function '%s' has incoming function calls" % functionName)
        self.__rootID = rootv.getVertexID()
        
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
        self.__callSites[callSiteID] = (predName, succName)
        
    def isCallSite (self, vertexID):
        return vertexID in self.__callSites
    
    def getCallEdgeNames (self, callSiteID):
        assert callSiteID in self.__callSites, "Unable to find call information for call site %d" % callSiteID
        return self.__callSites[callSiteID]
    
class Program():
    def __init__(self):
        self.__callg          = CallGraph()
        self.__icfgs          = {}
        self.__ipgs           = {}
        self.__lnts           = {}
        self.__superblockcfgs = {}
        
    def getRootICFG (self):
        rootcallv = self.__callg.getRootVertex()
        return self.__icfgs[rootcallv.getName()]
        
    def getCallGraph (self):
        return self.__callg
       
    def addICFG (self, icfg, functionName):
        assert functionName not in self.__icfgs, "Trying to add duplicate ICFG for function '%s'" % functionName
        self.__callg.addVertex(functionName)
        self.__icfgs[functionName] = icfg
        
    def addSuperBlockCFG (self, superg, functionName):
        assert functionName not in self.__superblockcfgs, "Trying to add duplicate super block CFG for function '%s'" % functionName
        self.__superblockcfgs[functionName] = superg
        
    def addIPG (self, ipg, functionName):
        assert functionName not in self.__ipgs, "Trying to add duplicate IPG for function '%s'" % functionName
        self.__ipgs[functionName] = ipg
        
    def addLNT (self, lnt, functionName):
        assert functionName not in self.__lnts, "Trying to add duplicate LNT for function '%s'" % functionName
        self.__lnts[functionName] = lnt
        
    def getICFG (self, functionName):
        assert functionName in self.__icfgs, "Unable to find ICFG for function '%s'" % functionName
        return self.__icfgs[functionName]
    
    def getSuperBlockCFG (self, functionName):
        assert functionName in self.__superblockcfgs, "Unable to find super block CFG for function '%s'" % functionName
        return self.__superblockcfgs[functionName]
    
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

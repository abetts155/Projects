from DirectedGraphs import DirectedGraph
from Vertices import CallGraphVertex, dummyVertexID
from SuperBlocks import SuperBlockGraph
from Trees import LoopNests
from copy import deepcopy
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
        self.__archivedICFGS  = {}
        self.__ICFGs          = {}
        self.__LNTs           = {}
        self.__superblockcfgs = {}
        self.__bbIDToICFG     = {}
        
    def getRootICFG (self):
        rootcallv = self.__callg.getRootVertex()
        return self.getICFG(rootcallv.getName())
    
    def getRootSuperBlockCFG (self):
        rootcallv = self.__callg.getRootVertex()
        return self.getSuperBlockCFG(rootcallv.getName())
        
    def getCallGraph (self):
        return self.__callg
       
    def addICFG (self, icfg, functionName):
        assert functionName not in self.__ICFGs, "Trying to add duplicate ICFG for function '%s'" % functionName
        self.__callg.addVertex(functionName)
        self.__ICFGs[functionName] = icfg
        
    def addSuperBlockCFG (self, superg, functionName):
        assert functionName not in self.__superblockcfgs, "Trying to add duplicate super block CFG for function '%s'" % functionName
        self.__superblockcfgs[functionName] = superg
        
    def addLNT (self, lnt, functionName):
        assert functionName not in self.__LNTs, "Trying to add duplicate LNT for function '%s'" % functionName
        self.__LNTs[functionName] = lnt
        
    def getICFG (self, functionName):
        assert functionName in self.__ICFGs, "Unable to find ICFG for function '%s'" % functionName
        return self.__ICFGs[functionName]
    
    def getSuperBlockCFG (self, functionName):
        if functionName not in self.__superblockcfgs:
            icfg   = self.getICFG(functionName)
            lnt    = self.getLNT(functionName)
            superg = SuperBlockGraph(icfg, lnt)
            self.__superblockcfgs[functionName] = superg
        return self.__superblockcfgs[functionName]

    def getLNT (self, functionName):
        if functionName not in self.__LNTs:
            icfg = self.getICFG(functionName)
            lnt  = LoopNests(icfg, icfg.getEntryID())
            self.__LNTs[functionName] = lnt
        return self.__LNTs[functionName]

    def getICFGs (self):
        return self.__ICFGs.values().__iter__()
    
    def getLNTs (self):
        return self.__LNTs.values().__iter__() 
    
    def getSuperBlockCFGs (self):
        return self.__superblockcfgs.values().__iter__() 
    
    def inlineCalls (self, inliningCapacity):
        self.__inlineNonUniqueCalls(inliningCapacity)
        self.__inlineUniqueCalls(inliningCapacity)
        print self.__ICFGs.keys()
        
    def __inlineNonUniqueCalls (self, inliningCapacity):
        for callv in self.__callg:
            if callv.numberOfPredecessors() > 1:
                for calle in callv.getPredecessorEdges():
                    for callSiteID in calle.getCallSites():
                        (callerName, calleeName) = self.__callg.getCallEdgeNames(callSiteID)
                        callerICFG = self.getICFG(callerName)
                        calleeICFG = self.getICFG(calleeName)
                        if callerICFG.numOfVertices() + calleeICFG.numOfVertices() < inliningCapacity:
                            Debug.debugMessage("Inlining '%s' into '%s' at call site %d" % (calleeName, callerName, callSiteID), 1)
                            self.__doInline(callerICFG, calleeICFG, callSiteID)
                        else:
                            Debug.debugMessage("NOT inlining '%s' into '%s' at call site %d because capacity exceeded" % (calleeName, callerName, callSiteID), 1)
        
    def __inlineUniqueCalls (self, inliningCapacity):
        for callv in self.__callg:
            if callv.numberOfPredecessors() == 1:
                for calle in callv.getPredecessorEdges():
                    if calle.numberOfCallSites() == 1:
                        for callSiteID in calle.getCallSites():
                            (callerName, calleeName) = self.__callg.getCallEdgeNames(callSiteID)
                            callerICFG = self.getICFG(callerName)
                            calleeICFG = self.getICFG(calleeName)
                            if callerICFG.numOfVertices() + calleeICFG.numOfVertices() < inliningCapacity:
                                Debug.debugMessage("Inlining '%s' into '%s' at call site %d" % (calleeName, callerName, callSiteID), 1)
                                self.__doInline(callerICFG, calleeICFG, callSiteID)
                                # Now archive the callee ICFG as it is no longer needed
                                self.__archivedICFGS[calleeName] = calleeICFG
                                del self.__ICFGs[calleeName]
                            else:
                                Debug.debugMessage("NOT inlining '%s' into '%s' at call site %d because capacity exceeded" % (calleeName, callerName, callSiteID), 1)
                    
    def __doInline (self, callerICFG, calleeICFG, callSiteID):
        callSitev  = callerICFG.getVertex(callSiteID)
        assert callSitev.numberOfSuccessors() == 1, "The call site %d does not have one successor exactly" % callSiteID
        returnv    = callerICFG.getVertex(callSitev.getSuccessorIDs()[0])
        newEntryID, newExitID = self.__duplicateCFG(callerICFG, calleeICFG)
        self.__linkDuplicate(callerICFG, callSitev, returnv, newEntryID, newExitID)
        
    def __duplicateCFG (self, callerICFG, calleeICFG):
        oldIDToNewID = {}
        for v in calleeICFG:
            newID  = callerICFG.getNextVertexID()
            oldIDToNewID[v.getVertexID()] = newID
            clonev = deepcopy(v)
            clonev.setVertexID(newID)
            clonev.removeAllPredecessors()
            clonev.removeAllSuccessors()
            callerICFG.addVertex(clonev)
        for v in calleeICFG:
            predID = v.getVertexID()
            for succID in v.getSuccessorIDs():
                if predID != calleeICFG.getExitID() and succID != calleeICFG.getEntryID():
                    newPredID = oldIDToNewID[predID]
                    newSuccID = oldIDToNewID[succID]
                    callerICFG.addEdge(newPredID, newSuccID)
        return oldIDToNewID[calleeICFG.getEntryID()], oldIDToNewID[calleeICFG.getExitID()]
    
    def __linkDuplicate (self, callerICFG, callSitev, returnv, newEntryID, newExitID):    
        callerICFG.addEdge(callSitev.getVertexID(), newEntryID)
        callerICFG.addEdge(newExitID, returnv.getVertexID())
        callerICFG.removeEdge(callSitev.getVertexID(), returnv.getVertexID())
        

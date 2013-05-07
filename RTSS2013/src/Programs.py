from DirectedGraphs import DirectedGraph
from Vertices import CallGraphVertex, dummyVertexID
from SuperBlocks import SuperBlockGraph
from Trees import LoopNests, DepthFirstSearch
from copy import deepcopy
import Debug
import UDrawGraph

class CallGraph (DirectedGraph):   
    def __init__ (self):
        DirectedGraph.__init__(self)
        self.__rootID = dummyVertexID
        self.__functionNameToVertex = {}
        
    def addVertex (self, functionName):
        assert functionName not in self.__functionNameToVertex, "Trying to add duplicate call graph vertex for function '%s'" % functionName
        Debug.debugMessage("Adding call graph vertex of function '%s'" % functionName, 5)
        vertexID = self.getNextVertexID()
        callv    = CallGraphVertex(vertexID, functionName)
        self.vertices[vertexID] = callv 
        self.__functionNameToVertex[functionName] = callv
    
    def removeVertex (self, functionName):
        Debug.debugMessage("Removing call graph vertex of function '%s'" % functionName, 5)
        callv    = self.getVertexWithName(functionName)
        vertexID = callv.getVertexID()
        for succID in callv.getSuccessorIDs():
            self.removeEdge(vertexID, succID)
        for predID in callv.getPredecessorIDs():
            self.removeEdge(predID, vertexID)
        DirectedGraph.removeVertex(self, vertexID)
        
    def hasVertexWithName (self, functionName):
        return functionName in self.__functionNameToVertex
        
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
    
class Program():
    def __init__(self):
        self.__callg          = CallGraph()
        self.__archivedICFGS  = {}
        self.__ICFGs          = {}
        self.__LNTs           = {}
        self.__superblockcfgs = {}
        self.__bbIDToICFG     = {}
        
    def generateUDrawFiles (self):
        UDrawGraph.makeUdrawFile(self.__callg, "callg")
        for functionName, cfg in self.__ICFGs.iteritems():
            UDrawGraph.makeUdrawFile(cfg, "%s.%s" % (functionName, "cfg"))
        for functionName, lnt in self.__LNTs.iteritems():
            UDrawGraph.makeUdrawFile(lnt, "%s.%s" % (functionName, "lnt"))
        for functionName, superg in self.__superblockcfgs.iteritems():
            UDrawGraph.makeUdrawFile(superg, "%s.%s" % (functionName, "superg"))
            UDrawGraph.makeUdrawFile(superg.getSuperBlockPathInformationGraph(), "%s.%s" % (functionName, "pathg"))
        
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
    
    def removeFunction (self, functionName):
        if functionName in self.__ICFGs:
            # Archive the ICFG as it is no longer needed
            cfg = self.__ICFGs[functionName]
            self.__archivedICFGS[functionName] = cfg
            del self.__ICFGs[functionName]
            for v in cfg:
                vertexID = v.getVertexID()
                if cfg.isCallSite(vertexID):
                    cfg.removeCallSite(vertexID)
        if self.__callg.hasVertexWithName(functionName):
            self.__callg.removeVertex(functionName)
            
    def removeProblematicFunctions (self):
        functions = set([])
        for functionName, cfg in self.__ICFGs.iteritems():
            exitID = cfg.getExitID()
            if cfg.isCallSite(exitID) or cfg.getExitID() == dummyVertexID:
                functions.add(functionName)
        for functionName in functions:
            # This check is essential as the function may have been removed in the meantime
            if functionName in self.__ICFGs:
                cfg = self.__ICFGs[functionName]
                dfs = DepthFirstSearch(self.__callg, self.__callg.getVertexWithName(functionName).getVertexID())
                for vertexID in dfs.getPostorder():
                    callv = self.__callg.getVertex(vertexID)
                    self.removeFunction(callv.getName())
    
    def inlineCalls (self):
        Debug.verboseMessage("Inlining to create single CFG")
        rootv = self.__callg.getRootVertex()
        dfs   = DepthFirstSearch(self.__callg, rootv.getVertexID())
        for vertexID in dfs.getPostorder():
            succv = self.__callg.getVertex(vertexID)
            for calle in succv.getPredecessorEdges():
                predID = calle.getVertexID()
                predv  = self.__callg.getVertex(predID)
                for callSiteID in calle.getCallSites():
                    calleeName = succv.getName()
                    calleeICFG = self.getICFG(calleeName)
                    callerName = predv.getName()
                    callerICFG = self.getICFG(callerName)
                    Debug.debugMessage("Inlining '%s' into '%s' at call site %d" % (calleeName, callerName, callSiteID), 1)
                    self.__doInline(callerICFG, calleeICFG, callSiteID)
                
        for vertexID in dfs.getPostorder():
            callv = self.__callg.getVertex(vertexID)
            if callv != rootv:
                self.removeFunction(callv.getName())
            else:
                cfg = self.__ICFGs[rootv.getName()]
                cfg.addEdge(cfg.getExitID(), cfg.getEntryID())

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
        

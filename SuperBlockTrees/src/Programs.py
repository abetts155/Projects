from DirectedGraphs import DirectedGraph
from Vertices import CallGraphVertex, dummyVertexID
from Trees import LoopNests, DepthFirstSearch
from copy import deepcopy
import Debug, SuperBlocks, Visualisation, PathData

class ContextGraph (DirectedGraph):
    nextVertexID = 1
    
    def __init__ (self, callg):
        DirectedGraph.__init__(self)
        self.__rootID = None
        self.__subprogramToContexts = {}
        self.__subprogramToUnused   = {}
        dfs = DepthFirstSearch(callg, callg.getRootID())
        self.__addVertices(callg, dfs)
        self.__addEdges(callg, dfs)
        self.__setRootID()
        
    def __addVertices (self, callg, dfs):
        for functionID in reversed(dfs.getPostorder()):
            callv = callg.getVertex(functionID)
            self.__subprogramToContexts[functionID] = []
            self.__subprogramToUnused[functionID]   = []
            if functionID == callg.getRootID():
                self.__addVertex(functionID, callv.getName())
            else:
                for prede in callv.getPredecessorEdges():
                    predID = prede.getVertexID()
                    numOfVertices = prede.numberOfCallSites() * len(self.__subprogramToContexts[predID])
                    for i in range(1, numOfVertices+1):
                        self.__addVertex(functionID, callv.getName())
    
    def __addVertex (self, functionID, functionName):
        contextID = ContextGraph.nextVertexID
        contextv  = CallGraphVertex(contextID, functionName)
        self.vertices[contextID] = contextv
        self.__subprogramToContexts[functionID].append(contextv)
        self.__subprogramToUnused[functionID].append(contextv)
        ContextGraph.nextVertexID += 1
    
    def __addEdges (self, callg, dfs):
        for callerID in reversed(dfs.getPostorder()):
            callv = callg.getVertex(callerID)
            for callerv in self.__subprogramToContexts[callerID]:
                for succe in callv.getSuccessorEdges():
                    calleeID = succe.getVertexID()
                    for callSiteID in succe.getCallSites():
                        calleev = self.__subprogramToUnused[calleeID].pop()
                        callerv.addSuccessor(calleev.getVertexID(), callSiteID)            
                        calleev.addPredecessor(callerv.getVertexID(), callSiteID)
    
    def __setRootID (self):
        noPreds = []
        for v in self:
            if v.numberOfPredecessors() == 0:
                noPreds.append(v)
        assert len(noPreds) == 1
        self.__rootID = noPreds[0].getVertexID()
    
    def getRootID (self):
        assert self.__rootID != dummyVertexID, "Root vertex of call graph has not been set"
        return self.__rootID

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
    
    def removeVertexWithName (self, functionName):
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
        self.__callg           = CallGraph()
        self.__contextg        = None
        self.__archivedICFGS   = {}
        self.__CFGs            = {}
        self.__LNTs            = {}
        self.__superblockcfgs  = {}
        self.__bbIDToICFG      = {}
        self.__WCETInformation = {}
        
    def output (self, data):
        totalMutualExclusion = 0
        totalAlways = 0
        
        for functionName, cfg in self.__CFGs.iteritems():
            superg = self.getSuperBlockCFG(functionName)
            pathg  = superg.getSuperBlockPathInformationGraph()
            Debug.verboseMessage("In %s..." % functionName)
            Debug.verboseMessage("...#CFG vertices           = %d" % cfg.numOfVertices())
            Debug.verboseMessage("...#CFG edges              = %d" % cfg.numOfEdges())
            Debug.verboseMessage("...#super blocks           = %d" % superg.numOfVertices())
            Debug.verboseMessage("...#monitored              = %d" % pathg.numOfVertices())
            Debug.verboseMessage("...#mutual exclusion pairs = %d" % pathg.numOfEdges())
            Debug.verboseMessage("...#never execute          = %d" % data.getNumberOfNeverExecute(pathg))
            Debug.verboseMessage("...#always execute         = %d" % data.getNumberOfAlwaysExecute(pathg))
            totalMutualExclusion += pathg.numOfEdges()
            totalAlways += data.getNumberOfAlwaysExecute(pathg)
        Debug.verboseMessage("...#TOTAL mutual exclusion pairs = %d" % totalMutualExclusion)
        Debug.verboseMessage("...#TOTAL always execute         = %d" % totalAlways)
            
    def generateVisualisationFiles (self, suffix=""):
        if suffix:
            suffix = '.' + suffix 
        Visualisation.generateGraphviz(self.__callg, "callg%s" % suffix)
        Visualisation.generateGraphviz(self.getContextGraph(), "contextg%s" % suffix)
        for functionName, cfg in self.__CFGs.iteritems():
            Visualisation.generateUdraw(cfg, "%s.cfg%s" % (functionName, suffix))
            Visualisation.generateGraphviz(cfg, "%s.cfg%s" % (functionName, suffix))
            Visualisation.generateGraphviz(self.getLNT(functionName), "%s.lnt%s" % (functionName, suffix))
            Visualisation.generateGraphviz(self.getSuperBlockCFG(functionName), "%s.superg%s" % (functionName, suffix))
            
    def addPathInformation (self, functionName, info):
        assert isinstance(info, PathData.WCETInformation)
        assert functionName in self.__CFGs, "Unable to find CFG '%s' to which the path information '%s' relates" % (functionName, info)
        if functionName not in self.__WCETInformation:
            self.__WCETInformation[functionName] = set([])
        self.__WCETInformation[functionName].add(info)
        
    def getCallGraph (self):
        return self.__callg

    def getContextGraph (self):
        if not self.__contextg:
            self.__contextg = ContextGraph(self.__callg)
        return self.__contextg
       
    def addCFG (self, cfg, functionName):
        assert functionName not in self.__CFGs, "Trying to add duplicate CFG for function '%s'" % functionName
        self.__callg.addVertex(functionName)
        self.__CFGs[functionName] = cfg
        
    def addSuperBlockCFG (self, superg, functionName):
        assert functionName not in self.__superblockcfgs, "Trying to add duplicate super block CFG for function '%s'" % functionName
        self.__superblockcfgs[functionName] = superg
        
    def addLNT (self, lnt, functionName):
        assert functionName not in self.__LNTs, "Trying to add duplicate LNT for function '%s'" % functionName
        self.__LNTs[functionName] = lnt
        
    def getCFG (self, functionName):
        assert functionName in self.__CFGs, "Unable to find ICFG for function '%s'" % functionName
        return self.__CFGs[functionName]
    
    def getSuperBlockCFG (self, functionName):
        if functionName not in self.__superblockcfgs:
            icfg   = self.getCFG(functionName)
            lnt    = self.getLNT(functionName)
            superg = SuperBlocks.SuperBlockGraph(icfg, lnt)
            self.__superblockcfgs[functionName] = superg
        return self.__superblockcfgs[functionName]

    def getLNT (self, functionName):
        if functionName not in self.__LNTs:
            icfg = self.getCFG(functionName)
            lnt  = LoopNests(icfg, icfg.getEntryID())
            self.__LNTs[functionName] = lnt
        return self.__LNTs[functionName]

    def getCFGs (self):
        return self.__CFGs.values().__iter__()
    
    def getSuperBlockCFGs (self):
        return self.__superblockcfgs.values().__iter__()
    
    def getLNTs (self):
        return self.__LNTs.values().__iter__()
    
    def removeFunction (self, functionName):
        if functionName in self.__CFGs:
            # Archive the ICFG as it is no longer needed
            cfg = self.__CFGs[functionName]
            self.__archivedICFGS[functionName] = cfg
            del self.__CFGs[functionName]
        if self.__callg.hasVertexWithName(functionName):
            self.__callg.removeVertexWithName(functionName)
            
    def removeProblematicFunctions (self):
        functions = set([])
        for functionName, cfg in self.__CFGs.iteritems():
            exitID = cfg.getExitID()
            if cfg.isCallSite(exitID) or cfg.getExitID() == dummyVertexID:
                functions.add(functionName)
        for functionName in functions:
            # This check is essential as the function may have been removed in the meantime
            if functionName in self.__CFGs:
                cfg = self.__CFGs[functionName]
                dfs = DepthFirstSearch(self.__callg, self.__callg.getVertexWithName(functionName).getVertexID())
                for vertexID in dfs.getPostorder():
                    callv = self.__callg.getVertex(vertexID)
                    for calle in callv.getPredecessorEdges():
                        predID = calle.getVertexID()
                        predv  = self.__callg.getVertex(predID)
                        for callSiteID in calle.getCallSites():
                            callerName = predv.getName()
                            callerICFG = self.getCFG(callerName)
                            callerICFG.removeCallSite(callSiteID)
                    Debug.debugMessage("Removing function '%s'" % callv.getName(), 10)
                    self.removeFunction(callv.getName())                    
    
    def addExitEntryBackEdges (self):
        for cfg in self.__CFGs.values():
            cfg.addExitEntryEdge()
    
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
                    calleeICFG = self.getCFG(calleeName)
                    callerName = predv.getName()
                    callerICFG = self.getCFG(callerName)
                    Debug.debugMessage("Inlining '%s' into '%s' at call site %d" % (calleeName, callerName, callSiteID), 1)
                    self.__doInline(callerICFG, calleeICFG, callSiteID)
                    callerICFG.removeCallSite(callSiteID)
                
        for vertexID in dfs.getPostorder():
            callv = self.__callg.getVertex(vertexID)
            if callv != rootv:
                self.removeFunction(callv.getName())
            else:
                cfg = self.__CFGs[rootv.getName()]
                cfg.addEdge(cfg.getExitID(), cfg.getEntryID())
        # Reset the data structures so that they are subsequently rebuilt
        self.__contextg       = None
        self.__LNTs           = {}
        self.__superblockcfgs = {}

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
            clonev.setOriginalVertexID(v.getOriginalVertexID())
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
        
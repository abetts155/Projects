import cfgs
import directed_graphs
import vertices
import trees
import super_blocks
import udraw
import debug
import copy

class ContextGraph (directed_graphs.DirectedGraph):
    nextVertexID = 1
    
    def __init__ (self, callg):
        directed_graphs.DirectedGraph.__init__(self)
        self.__rootID = None
        self.__subprogramToContexts = {}
        self.__subprogramToUnused   = {}
        dfs = trees.DepthFirstSearch(callg, callg.getRootID())
        self.__addvertices(callg, dfs)
        self.__addedges(callg, dfs)
        self.__setRootID()
        
    def __addvertices (self, callg, dfs):
        for functionID in reversed(dfs.getPostorder()):
            callv = callg.getVertex(functionID)
            self.__subprogramToContexts[functionID] = []
            self.__subprogramToUnused[functionID]   = []
            if functionID == callg.getRootID():
                self.__addVertex(functionID, callv.getName())
            else:
                for prede in callv.getPredecessoredges():
                    predID = prede.vertexID
                    numOfvertices = prede.numberOfCallSites() * len(self.__subprogramToContexts[predID])
                    for i in range(1, numOfvertices+1):
                        self.__addVertex(functionID, callv.getName())
    
    def __addVertex (self, functionID, functionName):
        contextID = ContextGraph.nextVertexID
        contextv  = vertices.CallGraphVertex(contextID, functionName)
        self.vertices[contextID] = contextv
        self.__subprogramToContexts[functionID].append(contextv)
        self.__subprogramToUnused[functionID].append(contextv)
        ContextGraph.nextVertexID += 1
    
    def __addedges (self, callg, dfs):
        for callerID in reversed(dfs.getPostorder()):
            callv = callg.getVertex(callerID)
            for callerv in self.__subprogramToContexts[callerID]:
                for succe in callv.getSuccessoredges():
                    calleeID = succe.vertexID
                    for callSiteID in succe.getCallSites():
                        calleev = self.__subprogramToUnused[calleeID].pop()
                        callerv.addSuccessor(calleev.vertexID, callSiteID)            
                        calleev.addPredecessor(callerv.vertexID, callSiteID)
    
    def __setRootID (self):
        noPreds = []
        for v in self:
            if v.numberOfPredecessors() == 0:
                noPreds.append(v)
        assert len(noPreds) == 1
        self.__rootID = noPreds[0].vertexID
    
    def getRootID (self):
        assert self.__rootID != vertices.dummyVertexID, "Root vertex of call graph has not been set"
        return self.__rootID

class CallGraph (directed_graphs.DirectedGraph):   
    def __init__ (self):
        directed_graphs.DirectedGraph.__init__(self)
        self.__rootID = vertices.dummyVertexID
        self.__functionNameToVertex = {}
        
    def addVertex (self, functionName):
        assert functionName not in self.__functionNameToVertex, "Trying to add duplicate call graph vertex for function '%s'" % functionName
        debug.debug_message("Adding call graph vertex of function '%s'" % functionName, 5)
        vertexID = self.getNextVertexID()
        callv    = vertices.CallGraphVertex(vertexID, functionName)
        self.vertices[vertexID] = callv 
        self.__functionNameToVertex[functionName] = callv
    
    def removeVertex (self, functionName):
        debug.debug_message("Removing call graph vertex of function '%s'" % functionName, 5)
        callv    = self.getVertexWithName(functionName)
        vertexID = callv.vertexID
        for succID in callv.getSuccessorIDs():
            self.removeEdge(vertexID, succID)
        for predID in callv.getPredecessorIDs():
            self.removeEdge(predID, vertexID)
        directed_graphs.DirectedGraph.removeVertex(self, vertexID)
        
    def hasVertexWithName (self, functionName):
        return functionName in self.__functionNameToVertex
        
    def getVertexWithName (self, functionName):
        assert functionName in self.__functionNameToVertex, "Unable to find call graph vertex for function '%s'" % functionName
        return self.__functionNameToVertex[functionName]
    
    def getRootVertex (self):
        assert self.__rootID != vertices.dummyVertexID, "The root of the call graph has not been set"
        return self.getVertex(self.__rootID)
    
    def findAndSetRoot (self):
        withoutPred = []
        for v in self:
            if v.numberOfPredecessors() == 0:
                withoutPred.append(v.vertexID)
        if len(withoutPred) == 0:
            debug.exit_message("Could not find program entry point as there are no functions without predecessors")
        elif len(withoutPred) > 1:
            debugStr = ""
            for vertexID in withoutPred:
                callv    = self.getVertex(vertexID)
                debugStr += callv.__str__()
            debug.exit_message("Call graph has too many entry points: %s" % debugStr)
        else:
            self.__rootID = withoutPred[0]
        assert self.__rootID, "Unable to set root ID of call graph"
        
    def setRoot (self, functionName):
        rootv = self.getVertexWithName(functionName)
        if rootv.numberOfPredecessors() > 0:
            debug.warning_message("Root function '%s' has incoming function calls" % functionName)
        self.__rootID = rootv.vertexID
        
    def setRootID (self, rootID):
        self.__rootID = rootID
    
    def getRootID (self):
        assert self.__rootID != vertices.dummyVertexID, "Root vertex of call graph has not been set"
        return self.__rootID
    
    def addEdge (self, predName, succName, callSiteID):
        debug.debug_message("Adding call graph edge %s => %s" % (predName, succName), 5)
        predv = self.getVertexWithName(predName)
        succv = self.getVertexWithName(succName)
        predv.addSuccessor(succv.vertexID, callSiteID)            
        succv.addPredecessor(predv.vertexID, callSiteID)
    
class Program():
    def __init__(self):
        self.__callg        = CallGraph()
        self.__contextg     = None
        self.__archivedCFGS = {}
        self.__cfgs         = {}
        self.__LNTs         = {}
        self.__pathgs       = {}
        self.__bbIDToCFG    = {}
        
    def output (self):
        totalMutualExclusion = 0
        totalMutualInclusion = 0
        totalDependencies    = 0
        totalNever           = 0
        totalAlways          = 0        
        for functionName, cfg in self.__cfgs.iteritems():
            pathg                = self.getPathInfoGraph(functionName)
            mutualExclusionPairs = len(pathg.mutualExclusionPairs())
            mutualInclusionPairs = len(pathg.mutualInclusionPairs())
            dependencies         = len(pathg.executionDependencies())
            neverExecute         = pathg.numOfNeverExecuteedges()
            alwaysExecute        = pathg.numOfAlwaysExecuteedges()
            totalMutualExclusion += mutualExclusionPairs
            totalMutualInclusion += mutualInclusionPairs
            totalDependencies    += dependencies
            totalNever           += neverExecute
            totalAlways          += alwaysExecute
            debug.verbose_message("In %s..." % functionName, __name__)
            debug.verbose_message("...#CFG edges              = %d" % cfg.numOfedges(), __name__)
            debug.verbose_message("...#monitored              = %d" % pathg.numOfvertices(), __name__)
            debug.verbose_message("...#mutual exclusion pairs = %d" % mutualExclusionPairs, __name__)
            debug.verbose_message("...#mutual inclusion pairs = %d" % mutualInclusionPairs, __name__)
            debug.verbose_message("...#execution dependencies = %d" % dependencies, __name__)
            debug.verbose_message("...#never execute          = %d" % neverExecute, __name__)
            debug.verbose_message("...#always execute         = %d" % alwaysExecute, __name__)
        debug.verbose_message("...#TOTAL mutual exclusion pairs = %d" % totalMutualExclusion, __name__)
        debug.verbose_message("...#TOTAL mutual inclusion pairs = %d" % totalMutualInclusion, __name__)
        debug.verbose_message("...#TOTAL execution dependencies = %d" % totalDependencies, __name__)
        debug.verbose_message("...#TOTAL never execute          = %d" % totalAlways, __name__)
        debug.verbose_message("...#TOTAL always execute         = %d" % totalAlways, __name__)
            
    def generateAllUDrawFiles (self, suffix=""):
        if suffix:
            suffix = '.' + suffix 
        udraw.makeUdrawFile(self.__callg, "callg%s" % suffix)
        udraw.makeUdrawFile(self.getContextGraph(), "contextg%s" % suffix)
        for functionName, cfg in self.__cfgs.iteritems():
            udraw.makeUdrawFile(cfg, "%s.cfg%s" % (functionName, suffix))
            udraw.makeUdrawFile(self.getPathInfoGraph(functionName), "%s.pathg%s" % (functionName, suffix))
            udraw.makeUdrawFile(self.getLNT(functionName), "%s.lnt%s" % (functionName, suffix))
            pathg = self.getPathInfoGraph(functionName)
            udraw.makeUdrawFile(pathg, "%s.pathg%s" % (functionName, suffix))
            udraw.makeUdrawFile(pathg.getEnhancedCFG(), "%s.enhancedCFG%s" % (functionName, suffix))
        
    def getCallGraph (self):
        return self.__callg

    def getContextGraph (self):
        if not self.__contextg:
            self.__contextg = ContextGraph(self.__callg)
        return self.__contextg
       
    def addCFG (self, cfg, functionName):
        assert functionName not in self.__cfgs, "Trying to add duplicate CFG for function '%s'" % functionName
        self.__callg.addVertex(functionName)
        self.__cfgs[functionName] = cfg
        
    def addLNT (self, lnt, functionName):
        assert functionName not in self.__LNTs, "Trying to add duplicate LNT for function '%s'" % functionName
        self.__LNTs[functionName] = lnt
        
    def getCFG (self, functionName):
        assert functionName in self.__cfgs, "Unable to find CFG for function '%s'" % functionName
        return self.__cfgs[functionName]
    
    def getPathInfoGraph (self, functionName):
        if functionName not in self.__pathgs:
            cfg = self.getCFG(functionName)
            lnt = self.getLNT(functionName)
            enhancedCFG = cfgs.EnhancedCFG(cfg)
            self.__pathgs[functionName] = super_blocks.PathInformationGraph(cfg, lnt, enhancedCFG)
        return self.__pathgs[functionName]

    def getLNT (self, functionName):
        if functionName not in self.__LNTs:
            cfg  = self.getCFG(functionName)
            lnt  = trees.LoopNests(cfg, cfg.getEntryID())
            self.__LNTs[functionName] = lnt
        return self.__LNTs[functionName]

    def getcfgs (self):
        return self.__cfgs.values().__iter__()
    
    def getPathInformationGraphs (self):
        return self.__pathgs.values().__iter__()
    
    def removeFunction (self, functionName):
        if functionName in self.__cfgs:
            # Archive the CFG as it is no longer needed
            cfg = self.__cfgs[functionName]
            self.__archivedCFGS[functionName] = cfg
            del self.__cfgs[functionName]
        if self.__callg.hasVertexWithName(functionName):
            self.__callg.removeVertex(functionName)
            
    def removeProblematicFunctions (self):
        functions = set([])
        for functionName, cfg in self.__cfgs.iteritems():
            exitID = cfg.getExitID()
            if cfg.isCallSite(exitID) or cfg.getExitID() == vertices.dummyVertexID:
                functions.add(functionName)
        for functionName in functions:
            # This check is essential as the function may have been removed in the meantime
            if functionName in self.__cfgs:
                cfg = self.__cfgs[functionName]
                dfs = trees.DepthFirstSearch(self.__callg, self.__callg.getVertexWithName(functionName).vertexID)
                for vertexID in dfs.getPostorder():
                    callv = self.__callg.getVertex(vertexID)
                    for calle in callv.getPredecessoredges():
                        predID = calle.vertexID
                        predv  = self.__callg.getVertex(predID)
                        for callSiteID in calle.getCallSites():
                            callerName = predv.getName()
                            callerCFG  = self.getCFG(callerName)
                            callerCFG.removeCallSite(callSiteID)
                    self.removeFunction(callv.getName())                    
    
    def addExitEntryBackedges (self):
        for cfg in self.__cfgs.values():
            cfg.addExitEntryEdge()
            udraw.makeUdrawFile(cfg, "%s.cfg" % cfg.getName())
    
    def inlineCalls (self):
        debug.verbose_message("Inlining to create single CFG", __name__)
        rootv = self.__callg.getRootVertex()
        dfs   = trees.DepthFirstSearch(self.__callg, rootv.vertexID)
        for vertexID in dfs.getPostorder():
            succv = self.__callg.getVertex(vertexID)
            for calle in succv.getPredecessoredges():
                predID = calle.vertexID
                predv  = self.__callg.getVertex(predID)
                for callSiteID in calle.getCallSites():
                    calleeName = succv.getName()
                    calleeCFG  = self.getCFG(calleeName)
                    callerName = predv.getName()
                    callerCFG  = self.getCFG(callerName)
                    debug.debug_message("Inlining '%s' into '%s' at call site %d" % (calleeName, callerName, callSiteID), 1)
                    self.__doInline(callerCFG, calleeCFG, callSiteID)
                    callerCFG.removeCallSite(callSiteID)
                
        for vertexID in dfs.getPostorder():
            callv = self.__callg.getVertex(vertexID)
            if callv != rootv:
                self.removeFunction(callv.getName())
            else:
                cfg = self.__cfgs[rootv.getName()]
                cfg.addEdge(cfg.getExitID(), cfg.getEntryID())
        # Reset the data structures so that they are subsequently rebuilt
        self.__contextg       = None
        self.__LNTs           = {}
        self.__superblockcfgs = {}

    def __doInline (self, callerCFG, calleeCFG, callSiteID):
        callSitev  = callerCFG.getVertex(callSiteID)
        assert callSitev.numberOfSuccessors() == 1, "The call site %d does not have one successor exactly" % callSiteID
        returnv    = callerCFG.getVertex(callSitev.getSuccessorIDs()[0])
        newEntryID, newExitID = self.__duplicateCFG(callerCFG, calleeCFG)
        self.__linkDuplicate(callerCFG, callSitev, returnv, newEntryID, newExitID)
        
    def __duplicateCFG (self, callerCFG, calleeCFG):
        oldIDToNewID = {}
        for v in calleeCFG:
            newID  = callerCFG.getNextVertexID()
            oldIDToNewID[v.vertexID] = newID
            clonev = copy.deepcopy(v)
            clonev.setVertexID(newID)
            clonev.setOriginalVertexID(v.getOriginalVertexID())
            clonev.removeAllPredecessors()
            clonev.removeAllSuccessors()
            callerCFG.addVertex(clonev)
        for v in calleeCFG:
            predID = v.vertexID
            for succID in v.getSuccessorIDs():
                if predID != calleeCFG.getExitID() and succID != calleeCFG.getEntryID():
                    newPredID = oldIDToNewID[predID]
                    newSuccID = oldIDToNewID[succID]
                    callerCFG.addEdge(newPredID, newSuccID)
        return oldIDToNewID[calleeCFG.getEntryID()], oldIDToNewID[calleeCFG.getExitID()]
    
    def __linkDuplicate (self, callerCFG, callSitev, returnv, newEntryID, newExitID):    
        callerCFG.addEdge(callSitev.vertexID, newEntryID)
        callerCFG.addEdge(newExitID, returnv.vertexID)
        callerCFG.removeEdge(callSitev.vertexID, returnv.vertexID)
        

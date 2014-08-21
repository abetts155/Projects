import directed_graphs
import vertices
import super_blocks
import udraw
import debug
import copy
    
class Program():
    def __init__(self):
        self.callg          = directed_graphs.CallGraph()
        self.__contextg     = None
        self.__archivedCFGS = {}
        self.cfgs         = {}
        self.__LNTs         = {}
        self.__pathgs       = {}
        self.__bbIDToCFG    = {}
        
    def output (self):
        totalMutualExclusion = 0
        totalMutualInclusion = 0
        totalDependencies    = 0
        totalNever           = 0
        totalAlways          = 0        
        for function_name, cfg in self.cfgs.iteritems():
            pathg                = self.getPathInfoGraph(function_name)
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
            debug.verbose_message("In %s..." % function_name, __name__)
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
        udraw.makeUdrawFile(self.callg, "callg%s" % suffix)
        udraw.makeUdrawFile(self.getContextGraph(), "contextg%s" % suffix)
        for function_name, cfg in self.cfgs.iteritems():
            udraw.makeUdrawFile(cfg, "%s.cfg%s" % (function_name, suffix))
            udraw.makeUdrawFile(self.getPathInfoGraph(function_name), "%s.pathg%s" % (function_name, suffix))
            udraw.makeUdrawFile(self.getLNT(function_name), "%s.lnt%s" % (function_name, suffix))
            pathg = self.getPathInfoGraph(function_name)
            udraw.makeUdrawFile(pathg, "%s.pathg%s" % (function_name, suffix))
            udraw.makeUdrawFile(pathg.getEnhancedCFG(), "%s.enhancedCFG%s" % (function_name, suffix))

    def getContextGraph (self):
        if not self.__contextg:
            self.__contextg = directed_graphs.ContextGraph(self.callg)
        return self.__contextg
       
    def addCFG (self, cfg):
        assert cfg.name not in self.cfgs, "Trying to add duplicate CFG for function '%s'" % cfg.name
        self.callg.addVertex(cfg.name)
        self.cfgs[cfg.name] = cfg
        
    def addLNT (self, lnt, function_name):
        assert function_name not in self.__LNTs, "Trying to add duplicate LNT for function '%s'" % function_name
        self.__LNTs[function_name] = lnt
        
    def getCFG (self, function_name):
        assert function_name in self.cfgs, "Unable to find CFG for function '%s'" % function_name
        return self.cfgs[function_name]
    
    def getPathInfoGraph (self, function_name):
        if function_name not in self.__pathgs:
            cfg = self.getCFG(function_name)
            lnt = self.getLNT(function_name)
            enhancedCFG = directed_graphs.EnhancedCFG(cfg)
            self.__pathgs[function_name] = super_blocks.PathInformationGraph(cfg, lnt, enhancedCFG)
        return self.__pathgs[function_name]

    def getLNT (self, function_name):
        if function_name not in self.__LNTs:
            cfg  = self.getCFG(function_name)
            lnt  = directed_graphs.LoopNests(cfg, cfg.get_entryID())
            self.__LNTs[function_name] = lnt
        return self.__LNTs[function_name]

    def getcfgs (self):
        return self.cfgs.values().__iter__()
    
    def getPathInformationGraphs (self):
        return self.__pathgs.values().__iter__()
    
    def removeFunction(self, function_name):
        if function_name in self.cfgs:
            # Archive the CFG as it is no longer needed
            cfg = self.cfgs[function_name]
            self.__archivedCFGS[function_name] = cfg
            del self.cfgs[function_name]
        if self.callg.hasVertexWithName(function_name):
            self.callg.removeVertex(function_name)
            
    def remove_problematic_functions(self):
        functions = set()
        for function_name, cfg in self.cfgs.iteritems():
            callee_name = cfg.is_call_site(cfg.get_exitID())
            if callee_name is not None or cfg.get_exitID() == vertices.dummyID:
                functions.add(function_name)
        for function_name in functions:
            # This check is essential as the function may have been removed in the meantime
            if function_name in self.cfgs:
                cfg = self.cfgs[function_name]
                dfs = directed_graphs.DepthFirstSearch(self.callg, self.callg.getVertexWithName(function_name).vertexID)
                for vertexID in dfs.getPostorder():
                    callv = self.callg.getVertex(vertexID)
                    for calle in callv.getPredecessoredges():
                        predID = calle.vertexID
                        predv  = self.callg.getVertex(predID)
                        for callSiteID in calle.getCallSites():
                            callerName = predv.getName()
                            callerCFG  = self.getCFG(callerName)
                            callerCFG.removeCallSite(callSiteID)
                    self.removeFunction(callv.getName())                    
    
    def addExitEntryBackedges (self):
        for cfg in self.cfgs.values():
            cfg.addExitEntryEdge()
            udraw.makeUdrawFile(cfg, "%s.cfg" % cfg.getName())
    
    def inlineCalls (self):
        debug.verbose_message("Inlining to create single CFG", __name__)
        rootv = self.callg.getRootVertex()
        dfs   = directed_graphs.DepthFirstSearch(self.callg, rootv.vertexID)
        for vertexID in dfs.getPostorder():
            succv = self.callg.getVertex(vertexID)
            for calle in succv.getPredecessoredges():
                predID = calle.vertexID
                predv  = self.callg.getVertex(predID)
                for callSiteID in calle.getCallSites():
                    calleeName = succv.getName()
                    calleeCFG  = self.getCFG(calleeName)
                    callerName = predv.getName()
                    callerCFG  = self.getCFG(callerName)
                    debug.debug_message("Inlining '%s' into '%s' at call site %d" % (calleeName, callerName, callSiteID), 1)
                    self.__doInline(callerCFG, calleeCFG, callSiteID)
                    callerCFG.removeCallSite(callSiteID)
                
        for vertexID in dfs.getPostorder():
            callv = self.callg.getVertex(vertexID)
            if callv != rootv:
                self.removeFunction(callv.getName())
            else:
                cfg = self.cfgs[rootv.getName()]
                cfg.addEdge(cfg.get_exitID(), cfg.get_entryID())
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
                if predID != calleeCFG.get_exitID() and succID != calleeCFG.get_entryID():
                    newPredID = oldIDToNewID[predID]
                    newSuccID = oldIDToNewID[succID]
                    callerCFG.addEdge(newPredID, newSuccID)
        return oldIDToNewID[calleeCFG.get_entryID()], oldIDToNewID[calleeCFG.get_exitID()]
    
    def __linkDuplicate (self, callerCFG, callSitev, returnv, newEntryID, newExitID):    
        callerCFG.addEdge(callSitev.vertexID, newEntryID)
        callerCFG.addEdge(newExitID, returnv.vertexID)
        callerCFG.removeEdge(callSitev.vertexID, returnv.vertexID)
        

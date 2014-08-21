import directed_graphs
import vertices
import debug
import copy
    
class Program:
    def __init__(self):
        self.callg         = directed_graphs.CallGraph()
        self.cfgs          = {}
        self.archived_cfgs = {}

    def get_context_graph(self):
        return directed_graphs.ContextGraph(self.callg)
       
    def addCFG (self, cfg):
        assert cfg.name not in self.cfgs, "Trying to add duplicate CFG for function '%s'" % cfg.name
        self.callg.addVertex(cfg.name)
        self.cfgs[cfg.name] = cfg
    
    def remove_function(self, function_name):
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
                            callerName = predv.name
                            callerCFG  = self.getCFG(callerName)
                            callerCFG.remove_call_site(callSiteID)
                    self.remove_function(callv.name)
    
    def inline_calls(self):
        debug.verbose_message("Inlining to create single CFG", __name__)
        rootv = self.callg.getVertex(self.callg.rootID)
        dfs   = directed_graphs.DepthFirstSearch(self.callg, rootv.vertexID)
        for vertexID in dfs.post_order:
            succv = self.callg.getVertex(vertexID)
            for calle in succv.predecessors.values():
                predv = self.callg.getVertex(calle.vertexID)
                for call_siteID in calle.getCallSites():
                    debug.debug_message("Inlining '%s' into '%s' at call site %d" % (succv.name, predv.name, call_siteID), 1)
                    self.__doInline(self.cfgs[predv.name], self.cfgs[succv.name], call_siteID)
                    self.cfgs[succv.name].remove_call_site(call_siteID)
        for vertexID in dfs.post_order:
            callv = self.callg.getVertex(vertexID)
            if callv != rootv:
                self.remove_function(callv.name)
            else:
                cfg = self.cfgs[rootv.name]
                cfg.addEdge(cfg.get_exitID(), cfg.get_entryID())

    def do_inline(self, callerCFG, calleeCFG, call_siteID):
        call_sitev              = callerCFG.getVertex(call_siteID)
        returnv                 = callerCFG.getVertex(call_sitev.successors.keys()[0])
        new_entryID, new_exitID = self.duplicate_CFG(callerCFG, calleeCFG)
        self.link_duplicate(callerCFG, call_sitev, returnv, new_entryID, new_exitID)
        
    def duplicate_CFG(self, callerCFG, calleeCFG):
        oldID_to_newID = {}
        for v in calleeCFG:
            oldID_to_newID[v.vertexID] = callerCFG.get_next_vertexID()
            clonev              = copy.deepcopy(v)
            clonev.vertexID     = oldID_to_newID[v.vertexID]
            clonev.originalID   = v.originalID
            clonev.predecessors = {}
            clonev.successors   = {}
            callerCFG.addVertex(clonev)
        for v in calleeCFG:
            predID = v.vertexID
            for succID in v.successors.keys():
                if predID != calleeCFG.get_exitID() and succID != calleeCFG.get_entryID():
                    new_predID = oldID_to_newID[predID]
                    new_succID = oldID_to_newID[succID]
                    callerCFG.addEdge(new_predID, new_succID)
        return oldID_to_newID[calleeCFG.get_entryID()], oldID_to_newID[calleeCFG.get_exitID()]
    
    def link_duplicate(self, callerCFG, callSitev, returnv, new_entryID, new_exitID):    
        callerCFG.addEdge(callSitev.vertexID, new_entryID)
        callerCFG.addEdge(new_exitID, returnv.vertexID)
        callerCFG.removeEdge(callSitev.vertexID, returnv.vertexID)
        
    def output(self):
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
        

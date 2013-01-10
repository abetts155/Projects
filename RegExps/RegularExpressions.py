import UDrawGraph, Debug, AcyclicReducibility, Trees, Vertices

class RegExp:
    lambda_       = '@'
    lParen        = ' ('
    rParen        = ') '
    union         = '|'
    kleeneStar    = '*'
    kleenePlus    = '+'
    concatenation = '.'
    
    def __init__(self):
        self.__elements = []
        
    def __str__ (self):
        return ''.join(self.__elements)
        
    def append (self, *args):
        for arg in args:
            if isinstance(arg, RegExp):
                self.__elements.extend(arg.__elements)
            elif isinstance(arg, int):
                self.__elements.append(str(arg))
            else:
                self.__elements.append(arg)
    
    def pop (self):
        del self.__elements[-1] 

class RegularExpressions:
    def __init__(self, cfg):
        self.__lnt = Trees.LoopNests(cfg, cfg.getEntryID())
        functionName = cfg.getName()
        Debug.debugMessage("Doing regular expression analysis of CFG '%s'" % functionName, 1)        
        UDrawGraph.makeUdrawFile (cfg, "%s.%s" % (functionName, "cfg"))
        
        for level, vertices in self.__lnt.levelIterator(True):
            for v in vertices:
                if isinstance(v, Vertices.HeaderVertex):
                    self.__currentHeaderID = v.getHeaderID()
                    self.__currentCFG = self.__lnt.induceSubgraph(v)
                    self.__predomTree  = Trees.Dominators(self.__currentCFG, self.__currentCFG.getEntryID())
                    self.__lca         = Trees.LeastCommonAncestor(self.__predomTree)
                    self.__reversecfg  = self.__currentCFG.getReverseCFG()
                    self.__postdomTree = Trees.Dominators(self.__reversecfg, self.__reversecfg.getEntryID())
                    UDrawGraph.makeUdrawFile (self.__currentCFG, "%s.Header%d" % (functionName, v.getHeaderID()))
                    UDrawGraph.makeUdrawFile (self.__predomTree, "%s.Header%d.%s" % (functionName, v.getHeaderID(), "pre"))
                    UDrawGraph.makeUdrawFile (self.__postdomTree, "%s.Header%d.%s" % (functionName, v.getHeaderID(), "post"))
                    self.__acyclicReducibility  = AcyclicReducibility.AcyclicReducibility(self.__currentCFG, self.__reversecfg, self.__predomTree, self.__postdomTree)
                    self.__initialise()
                    self.__compute()
                    
    def __initialise (self):
        self.__vToRegExp = {}
        for v in self.__currentCFG:
            vertexID = v.getVertexID()
            self.__vToRegExp[vertexID] = RegExp() 
        
    def __compute (self):
        dfs = Trees.DepthFirstSearch(self.__currentCFG, self.__currentCFG.getEntryID())
        for vertexID in reversed(dfs.getPostorder()):
            v = self.__currentCFG.getVertex(vertexID)
            if vertexID == self.__currentCFG.getEntryID():
                self.__vToRegExp[vertexID].append(vertexID)
            else:
                if v.numberOfPredecessors() == 1:
                    predID = v.getPredecessorIDs()[0]
                    predv  = self.__currentCFG.getVertex(predID)
                    if predv.numberOfSuccessors() > 1:
                        if self.__acyclicReducibility.isReducibleBranch(predID):
                            self.__vToRegExp[vertexID].append(vertexID)
                        else:
                            self.__vToRegExp[vertexID].append(self.__vToRegExp[predID], RegExp.concatenation, vertexID)
                    else:
                        self.__vToRegExp[vertexID].append(self.__vToRegExp[predID], RegExp.concatenation, vertexID)
                else:
                    self.__handleMerge(v)
            print "RegExp(%d) = %s" % (vertexID, self.__vToRegExp[vertexID])
    
    def __handleMerge (self, mergev):
        Debug.debugMessage("Analysing merge %d" % mergev.getVertexID(), 1)
        vToTempRegExp = {}
        comPreTree    = Trees.CompressedDominatorTree(self.__predomTree, self.__lca, mergev.getVertexID(), mergev.getPredecessorIDs())
        print comPreTree
        for level, vertices in comPreTree.levelIterator(True):
            for v in vertices:
                vertexID = v.getVertexID()
                if v.numberOfSuccessors() == 0:
                    vToTempRegExp[vertexID] = self.__vToRegExp[vertexID]
                else:
                    newExp = RegExp()
                    if vertexID != comPreTree.getRootID():
                        newExp.append(vertexID)
                    newExp.append(RegExp.lParen)
                    
                    for succID in v.getSuccessorIDs():
                        newExp.append(vToTempRegExp[succID])
                        newExp.append(RegExp.union)
                    
                    if self.__currentCFG.getVertex(vertexID).hasSuccessor(mergev.getVertexID()):
                        newExp.append(RegExp.lambda_)
                    else:
                        newExp.pop()
                        
                    newExp.append(RegExp.rParen)
                    vToTempRegExp[vertexID] = newExp
        # End of compressed dominator tree traversal
        mergeID = mergev.getVertexID()
        rootID  = comPreTree.getRootID()
        if self.__postdomTree.getImmediateDominator(rootID) == mergeID: 
            self.__vToRegExp[mergeID].append(self.__vToRegExp[rootID])
        self.__vToRegExp[mergeID].append(vToTempRegExp[rootID])
        self.__vToRegExp[mergeID].append(mergeID) 
    
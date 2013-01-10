import UDrawGraph, Debug, AcyclicReducibility, Trees

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
        self.__cfg = cfg
        functionName = cfg.getName()
        Debug.debugMessage("Doing regular expression analysis of CFG '%s'" % functionName, 1)        
        UDrawGraph.makeUdrawFile (cfg, "%s.%s" % (functionName, "cfg"))
        self.__predomTree  = Trees.Dominators(cfg, cfg.getEntryID())
        self.__lca         = Trees.LeastCommonAncestor(self.__predomTree)
        self.__reversecfg  = cfg.getReverseCFG()
        self.__postdomTree = Trees.Dominators(self.__reversecfg, self.__reversecfg.getEntryID())
        UDrawGraph.makeUdrawFile (self.__predomTree, "%s.%s" % (functionName, "pre"))
        UDrawGraph.makeUdrawFile (self.__postdomTree, "%s.%s" % (functionName, "post"))
        self.__acyclicReducibility  = AcyclicReducibility.AcyclicReducibility(self.__cfg, self.__reversecfg, self.__predomTree, self.__postdomTree)
        self.__initialise()
        self.__compute()
        
    def __initialise (self):
        self.__vToRegExp = {}
        for v in self.__cfg:
            vertexID = v.getVertexID()
            self.__vToRegExp[vertexID] = RegExp() 
        
    def __compute (self):
        dfs = Trees.DepthFirstSearch(self.__cfg, self.__cfg.getEntryID())
        for vertexID in reversed(dfs.getPostorder()):
            v = self.__cfg.getVertex(vertexID)
            if vertexID == self.__cfg.getEntryID():
                self.__vToRegExp[vertexID].append(vertexID)
            else:
                if v.numberOfPredecessors() == 1:
                    predID = v.getPredecessorIDs()[0]
                    predv  = self.__cfg.getVertex(predID)
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
                    
                    if self.__cfg.getVertex(vertexID).hasSuccessor(mergev.getVertexID()):
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
    
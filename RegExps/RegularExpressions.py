import UDrawGraph, Debug, AcyclicReducibility, Trees, Vertices
from matplotlib.pyparsing import Regex

class RegExp:
    lambda_       = '@'
    union         = ' | '
    kleeneStar    = '*'
    kleenePlus    = '+'
    concatenation = '.'
    
    def __init__(self, reverse):
        self.__reverse = reverse
        self.__elements = []
        
    def __str__ (self):
        if self.__reverse:
            copy = list(self.__elements)
            copy.reverse()
            return ''.join(copy)
        return ''.join(self.__elements)
        
    @staticmethod
    def lParen (space=True):
        if space:
            return ' ('
        else:
            return '('
    
    @staticmethod
    def rParen (space=True):
        if space:
            return ') '
        else:
            return ')'
        
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
    
    def first (self):
        return self.__elements[0] 

    def last (self):
        return self.__elements[-1] 

class RegularExpressions:
    def __init__(self, icfg):
        self.__lnt = Trees.LoopNests(icfg, icfg.getEntryID())
        functionName = icfg.getName()
        Debug.debugMessage("Doing regular expression analysis of CFG '%s'" % functionName, 1)        
        UDrawGraph.makeUdrawFile (icfg, "%s.%s" % (functionName, "icfg"))
        UDrawGraph.makeUdrawFile (self.__lnt, "%s.%s" % (functionName, "lnt"))
        
        self.__headerToPathExp = {}
        self.__headerToDFList  = {}
        for level, vertices in self.__lnt.levelIterator(True):
            for v in vertices:
                if isinstance(v, Vertices.HeaderVertex):
                    forwardICFG = self.__lnt.induceSubgraph(v)
                    predomTree  = Trees.Dominators(forwardICFG, forwardICFG.getEntryID())
                    reverseICFG = forwardICFG.getReverseCFG()
                    postdomTree = Trees.Dominators(reverseICFG, reverseICFG.getEntryID())
                    UDrawGraph.makeUdrawFile (forwardICFG, "%s.Header%d.%s" % (functionName, v.getHeaderID(), "icfg"))
                    UDrawGraph.makeUdrawFile (reverseICFG, "%s.Header%d.%s" % (functionName, v.getHeaderID(), "icfg.reverse"))
                    UDrawGraph.makeUdrawFile (predomTree, "%s.Header%d.%s" % (functionName, v.getHeaderID(), "pre"))
                    UDrawGraph.makeUdrawFile (postdomTree, "%s.Header%d.%s" % (functionName, v.getHeaderID(), "post"))
                    acyclicReducibility = AcyclicReducibility.AcyclicReducibility(forwardICFG, reverseICFG, predomTree, postdomTree)
                    dfs                 = Trees.DepthFirstSearch(forwardICFG, forwardICFG.getEntryID())
                    # Compute pre-dominance frontier list
                    self.__computeDFList(dfs, acyclicReducibility.getPreDF(), v.getHeaderID())
                    # Compute path expressions inside induced acyclic loop subgraph     
                    Debug.debugMessage("Analysing forward induced subgraph of loop header %d" % v.getHeaderID(), 10)
                    self.__reverse = False
                    self.__initialise(forwardICFG)
                    self.__compute(forwardICFG, dfs, predomTree, postdomTree, Trees.LeastCommonAncestor(predomTree), acyclicReducibility)
                    self.__headerToPathExp[forwardICFG.getEntryID()]= self.__vToRegExp[forwardICFG.getExitID()]
                    # Compute path expressions inside REVERSED induced acyclic loop subgraph 
                    Debug.debugMessage("Analysing reverse induced subgraph of loop header %d" % v.getHeaderID(), 10)
                    dfs            = Trees.DepthFirstSearch(reverseICFG, reverseICFG.getEntryID())
                    self.__reverse = True
                    self.__initialise(reverseICFG)
                    self.__compute(reverseICFG, dfs, postdomTree, predomTree, Trees.LeastCommonAncestor(postdomTree), acyclicReducibility)
                    
    def __initialise (self, icfg):
        self.__vToRegExp = {}
        for v in icfg:
            vertexID = v.getVertexID()
            self.__vToRegExp[vertexID] = {}
            
    def __computeDFList (self, dfs, preDF, headerID):
        self.__headerToDFList[headerID] = [] 
        for vertexID in reversed(dfs.getPostorder()):
            if preDF.isEmpty(vertexID):
                self.__headerToDFList[headerID].append(vertexID)
            
    def __output (self, vertexID):
        for keyID in self.__vToRegExp[vertexID]:
            Debug.debugMessage("RegExp(%d@%d) = %s" % (keyID, vertexID, self.__vToRegExp[vertexID][keyID]), 10)
        
    def __compute (self, icfg, dfs, domTree, reverseDomTree, lcaQuery, acyclicReducibility):
        for vertexID in reversed(dfs.getPostorder()):
            v = icfg.getVertex(vertexID)
            if vertexID == icfg.getEntryID():
                self.__vToRegExp[vertexID][vertexID] = RegExp(self.__reverse) 
                self.__vToRegExp[vertexID][vertexID].append(vertexID)
            else:
                if v.numberOfPredecessors() == 1:
                    self.__handleNonMerge(icfg, acyclicReducibility, v)
                else:
                    self.__handleMerge(icfg, domTree, reverseDomTree, lcaQuery, v)
            self.__output(vertexID)
            
    def __handleNonMerge (self, icfg, acyclicReducibility, v):
        vertexID = v.getVertexID()
        predID   = v.getPredecessorIDs()[0]
        predv    = icfg.getVertex(predID)
        if icfg.isIpoint(predID):
            self.__vToRegExp[vertexID][predID] = RegExp(self.__reverse)
            self.__vToRegExp[vertexID][predID].append(predID)
            self.__vToRegExp[vertexID][predID].append(RegExp.concatenation)
        else:  
            for keyID in self.__vToRegExp[predID]:
                self.__vToRegExp[vertexID][keyID] = RegExp(self.__reverse) 
                if (predv.numberOfSuccessors() > 1 and not acyclicReducibility.isReducibleBranch(predID)) \
                or predv.numberOfSuccessors() == 1:
                    self.__vToRegExp[vertexID][keyID].append(self.__vToRegExp[predID][keyID], \
                                                             RegExp.concatenation)
        # Keys added to vertex from predecessor. Now decide what to append to these
        # path expressions depending on the type of vertex
        if self.__lnt.isLoopHeader(vertexID) and vertexID != icfg.getExitID():
            for keyID in self.__vToRegExp[predID]:
                self.__handleLoopHeader(self.__vToRegExp[vertexID][keyID], vertexID)
        else:
            for keyID in self.__vToRegExp[vertexID]:
                self.__vToRegExp[vertexID][keyID].append(vertexID)
                
    def __handleMerge (self, icfg, domTree, reverseDomTree, lcaQuery, mergev):
        mergeID = mergev.getVertexID()
        Debug.debugMessage("Analysing merge %d" % mergeID, 10)
        comPreTree      = Trees.CompressedDominatorTree(domTree, lcaQuery, mergeID, mergev.getPredecessorIDs())
        rootID          = comPreTree.getRootID()
        vToTempRegExp   = {}
        vToAlternatives = {}
        for v in comPreTree:
            vToTempRegExp[v.getVertexID()]   = {}
            vToAlternatives[v.getVertexID()] = {}
        # Now do bottom-up traversal
        for level, vertices in comPreTree.levelIterator(True):
            for v in vertices:
                vertexID = v.getVertexID()
                if v.numberOfSuccessors() == 0:
                    # Leaf vertex
                    if icfg.isIpoint(vertexID):
                        Debug.debugMessage("%d is an Ipoint leaf in the compressed pre-dominator tree" % vertexID, 20)
                        self.__vToRegExp[mergeID][vertexID] = RegExp(self.__reverse)
                    else:
                        parentID = v.getParentID()
                        for keyID in self.__vToRegExp[vertexID]:
                            if keyID in self.__vToRegExp[parentID]:
                                vToTempRegExp[vertexID][keyID]   = self.__vToRegExp[vertexID][keyID]  
                                vToTempRegExp[parentID][keyID]   = RegExp(self.__reverse)
                                self.__vToRegExp[mergeID][keyID] = RegExp(self.__reverse)
                else:
                    # Internal vertex
                    for keyID in vToTempRegExp[vertexID]:
                        for succID in v.getSuccessorIDs():
                            if keyID in vToTempRegExp[succID]:
                                if keyID not in vToAlternatives[vertexID]:
                                    vToAlternatives[vertexID][keyID] = False
                                else:
                                    vToAlternatives[vertexID][keyID] = True
                   
                        if vertexID != rootID:
                            vToTempRegExp[vertexID][keyID].append(self.__vToRegExp[vertexID][keyID])
                                  
                        if vToAlternatives[vertexID][keyID]:
                            if self.__reverse:
                                vToTempRegExp[vertexID][keyID].append(RegExp.rParen())
                            else:
                                vToTempRegExp[vertexID][keyID].append(RegExp.lParen())
                        else:
                                vToTempRegExp[vertexID][keyID].append(RegExp.concatenation)
                            
                        for succID in v.getSuccessorIDs():
                            if keyID in vToTempRegExp[succID]:
                                vToTempRegExp[vertexID][keyID].append(vToTempRegExp[succID][keyID])
                                vToTempRegExp[vertexID][keyID].append(RegExp.union)
                             
                        if icfg.getVertex(vertexID).hasSuccessor(mergeID):
                            vToTempRegExp[vertexID][keyID].append(RegExp.lambda_)
                        else:
                            vToTempRegExp[vertexID][keyID].pop()
                            
                        if vToAlternatives[vertexID][keyID]:
                            if self.__reverse:
                                vToTempRegExp[vertexID][keyID].append(RegExp.lParen())
                            else:
                                vToTempRegExp[vertexID][keyID].append(RegExp.rParen())
                        else:
                            vToTempRegExp[vertexID][keyID].append(RegExp.concatenation)

        # End of compressed dominator tree traversal
        for keyID in self.__vToRegExp[mergeID]:
            if keyID in vToTempRegExp[rootID]:
                if reverseDomTree.getImmediateDominator(rootID) == mergeID:
                    self.__vToRegExp[mergeID][keyID].append(self.__vToRegExp[rootID][keyID])  
                self.__vToRegExp[mergeID][keyID].append(vToTempRegExp[rootID][keyID])
            if not mergev.isDummy():
                self.__vToRegExp[mergeID][keyID].append(mergeID) 
        
    def __handleLoopHeader (self, pathExp, headerID):
        if self.__lnt.isDoWhileLoop(headerID):
            pathExp.append(RegExp.lParen(False))
            pathExp.append(RegExp.lParen(False))
            pathExp.append(self.__headerToPathExp[headerID])
            pathExp.append(RegExp.rParen(False))
            pathExp.append(RegExp.kleenePlus)
            pathExp.append(RegExp.rParen(False))
        else:
            pathExp.append(RegExp.lParen(False))
            pathExp.append(RegExp.lParen(False))
            pathExp.append(self.__headerToPathExp[headerID])
            pathExp.append(RegExp.rParen(False))
            pathExp.append(RegExp.kleeneStar)
            pathExp.append(RegExp.rParen(False))
            pathExp.append(headerID)
            
class RegularExpressionsWithoutIpoints:
    def __init__(self, cfg):
        self.__lnt = Trees.LoopNests(cfg, cfg.getEntryID())
        functionName = cfg.getName()
        Debug.debugMessage("Doing regular expression analysis of CFG '%s'" % functionName, 1)        
        UDrawGraph.makeUdrawFile (cfg, "%s.%s" % (functionName, "cfg"))
        UDrawGraph.makeUdrawFile (self.__lnt, "%s.%s" % (functionName, "lnt"))
        
        self.__headerToPathExp = {}
        for level, vertices in self.__lnt.levelIterator(True):
            for v in vertices:
                if isinstance(v, Vertices.HeaderVertex):
                    self.__currentHeaderID = v.getHeaderID()
                    self.__currentCFG = self.__lnt.induceSubgraph(v)
                    self.__predomTree  = Trees.Dominators(self.__currentCFG, self.__currentCFG.getEntryID())
                    self.__lca         = Trees.LeastCommonAncestor(self.__predomTree)
                    self.__reversecfg  = self.__currentCFG.getReverseCFG()
                    self.__postdomTree = Trees.Dominators(self.__reversecfg, self.__reversecfg.getEntryID())
                    UDrawGraph.makeUdrawFile (self.__currentCFG, "%s.Header%d.%s" % (functionName, v.getHeaderID(), "cfg"))
                    UDrawGraph.makeUdrawFile (self.__predomTree, "%s.Header%d.%s" % (functionName, v.getHeaderID(), "pre"))
                    UDrawGraph.makeUdrawFile (self.__postdomTree, "%s.Header%d.%s" % (functionName, v.getHeaderID(), "post"))
                    self.__acyclicReducibility  = AcyclicReducibility.AcyclicReducibility(self.__currentCFG, self.__reversecfg, self.__predomTree, self.__postdomTree)
                    self.__initialise()
                    self.__compute()
                    entryID = self.__currentCFG.getEntryID()
                    exitID  = self.__currentCFG.getExitID()
                    self.__headerToPathExp[entryID]= self.__vToRegExp[exitID]
                    
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
                    self.__handleNonMerge(v)
                else:
                    self.__handleMerge(v)
            print "RegExp(%d) = %s" % (vertexID, self.__vToRegExp[vertexID])
            
    def __handleNonMerge (self, v):
        vertexID = v.getVertexID()
        predID   = v.getPredecessorIDs()[0]
        predv    = self.__currentCFG.getVertex(predID)
        if (predv.numberOfSuccessors() > 1 and not self.__acyclicReducibility.isReducibleBranch(predID)) \
        or predv.numberOfSuccessors() == 1:
                self.__vToRegExp[vertexID].append(self.__vToRegExp[predID], RegExp.concatenation)
        if self.__lnt.isLoopHeader(vertexID):
            self.__handleLoopHeader(self.__vToRegExp[vertexID], vertexID)
        else:
            self.__vToRegExp[vertexID].append(vertexID)
                
    def __handleMerge (self, mergev):
        Debug.debugMessage("Analysing merge %d" % mergev.getVertexID(), 1)
        vToTempRegExp = {}
        comPreTree    = Trees.CompressedDominatorTree(self.__predomTree, self.__lca, mergev.getVertexID(), mergev.getPredecessorIDs())
        for level, vertices in comPreTree.levelIterator(True):
            for v in vertices:
                vertexID = v.getVertexID()
                # Leaf vertex
                if v.numberOfSuccessors() == 0:
                    vToTempRegExp[vertexID] = self.__vToRegExp[vertexID]
                else:
                    newExp = RegExp()
                    if vertexID != comPreTree.getRootID():
                        newExp.append(vertexID)
                    newExp.append(RegExp.lParen())
                    
                    for succID in v.getSuccessorIDs():
                        newExp.append(vToTempRegExp[succID])
                        newExp.append(RegExp.union)
                    
                    if self.__currentCFG.getVertex(vertexID).hasSuccessor(mergev.getVertexID()):
                        newExp.append(RegExp.lambda_)
                    else:
                        newExp.pop()
                        
                    newExp.append(RegExp.rParen())
                    vToTempRegExp[vertexID] = newExp
        # End of compressed dominator tree traversal
        mergeID = mergev.getVertexID()
        rootID  = comPreTree.getRootID()
        if self.__postdomTree.getImmediateDominator(rootID) == mergeID: 
            self.__vToRegExp[mergeID].append(self.__vToRegExp[rootID])
        self.__vToRegExp[mergeID].append(vToTempRegExp[rootID])
        if not mergev.isDummy():
            self.__vToRegExp[mergeID].append(mergeID) 
        
    def __handleLoopHeader (self, pathExp, headerID):
        if self.__lnt.isDoWhileLoop(headerID):
            pathExp.append(RegExp.lParen(False))
            pathExp.append(RegExp.lParen(False))
            pathExp.append(self.__headerToPathExp[headerID])
            pathExp.append(RegExp.rParen(False))
            pathExp.append(RegExp.kleenePlus)
            pathExp.append(RegExp.rParen(False))
        else:
            pathExp.append(RegExp.lParen(False))
            pathExp.append(RegExp.lParen(False))
            pathExp.append(self.__headerToPathExp[headerID])
            pathExp.append(RegExp.rParen(False))
            pathExp.append(RegExp.kleeneStar)
            pathExp.append(RegExp.rParen(False))
            pathExp.append(headerID)
            
    
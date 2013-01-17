import UDrawGraph, Debug, AcyclicReducibility, Trees, Vertices
from FiniteAutomatons import RegExp, GeneralisedAutomaton
from Vertices import Ipoint
import copy

class RegularExpressions:
    def __init__(self, icfg):        
        Debug.debugMessage("Doing regular expression analysis of ICFG '%s'" % icfg.getName(), 1) 
        self.__icfg = icfg
        self.__lnt  = Trees.LoopNests(icfg, icfg.getEntryID())       
        UDrawGraph.makeUdrawFile (icfg, "%s.%s" % (icfg.getName(), "icfg"))
        UDrawGraph.makeUdrawFile (self.__lnt, "%s.%s" % (icfg.getName(), "lnt"))
        self.__traverseLNT()
        
    def __traverseLNT (self):
        for level, vertices in self.__lnt.levelIterator(True):
            for v in vertices:
                if isinstance(v, Vertices.HeaderVertex):
                    self.__loopAutomaton = GeneralisedAutomaton()
                    forwardICFG = self.__lnt.induceSubgraph(v)
                    predomTree  = Trees.Dominators(forwardICFG, forwardICFG.getEntryID())
                    reverseICFG = forwardICFG.getReverseCFG()
                    postdomTree = Trees.Dominators(reverseICFG, reverseICFG.getEntryID())
                    UDrawGraph.makeUdrawFile (forwardICFG, "%s.Header%d.%s" % (self.__icfg.getName(), v.getHeaderID(), "icfg"))
                    UDrawGraph.makeUdrawFile (reverseICFG, "%s.Header%d.%s" % (self.__icfg.getName(), v.getHeaderID(), "icfg.reverse"))
                    UDrawGraph.makeUdrawFile (predomTree,  "%s.Header%d.%s" % (self.__icfg.getName(), v.getHeaderID(), "pre"))
                    UDrawGraph.makeUdrawFile (postdomTree, "%s.Header%d.%s" % (self.__icfg.getName(), v.getHeaderID(), "post"))
                    acyclicReducibility      = AcyclicReducibility.AcyclicReducibility(forwardICFG, reverseICFG, predomTree, postdomTree)
                    self.__dominanceFrontier = acyclicReducibility.getPreDF()
                    dfs                      = Trees.DepthFirstSearch(forwardICFG, forwardICFG.getEntryID())
                    # Compute path expressions inside induced acyclic loop subgraph     
                    Debug.debugMessage("Analysing forward induced subgraph of loop header %d" % v.getHeaderID(), 10)
                    self.__initialise(forwardICFG)
                    self.__compute(forwardICFG, dfs, predomTree, postdomTree, Trees.LeastCommonAncestor(predomTree), acyclicReducibility)
                    self.__addIpointEdges()
                    UDrawGraph.makeUdrawFile (self.__loopAutomaton, "%s.Header%d.%s" % (self.__icfg.getName(), v.getHeaderID(), "automaton"))
                    self.__outputAllIpointTransitions(self.__loopAutomaton)
                    
    def __initialise (self, icfg):
        self.__vToRegExp = {}
        for v in icfg:
            vertexID = v.getVertexID()
            self.__vToRegExp[vertexID] = {}
    
    def __outputAllIpointTransitions (self, loopAutomaton):
        for v in loopAutomaton:
            if isinstance(v, Vertices.Ipoint):
                vertexID = v.getVertexID()
                vToParent = {}
                stack = []
                stack.append(v)
                pathExp = RegExp()
                while stack:
                    stackv        = stack.pop()
                    stackVertexID = stackv.getVertexID()
                    if isinstance(stackv, Vertices.Ipoint) and stackVertexID != vertexID:
                        prede    = stackv.getPredecessorEdge(vToParent[stackVertexID])
                        predExpr = prede.getExpr()
                        finalPathExp = copy.deepcopy(pathExp)  
                        if not pathExp.isEmpty() and not predExpr.isEmpty():  
                            if pathExp.last().isdigit() and predExpr.first().isdigit():
                                finalPathExp.append(RegExp.concatenation)
                        finalPathExp.append(predExpr)
                        print "(%d, %d) = %s" % (vertexID, stackVertexID, finalPathExp)
                    else:
                        if not isinstance(stackv, Vertices.Ipoint):
                            prede    = stackv.getPredecessorEdge(vToParent[stackVertexID])
                            predExpr = prede.getExpr()
                            if not pathExp.isEmpty() and not predExpr.isEmpty():  
                                if pathExp.last().isdigit() and predExpr.first().isdigit():
                                    pathExp.append(RegExp.concatenation)
                            pathExp.append(predExpr)
                            if not predExpr.isEmpty() and predExpr.last().isdigit():
                                pathExp.append(RegExp.concatenation)
                            pathExp.append(loopAutomaton.getExpr(stackVertexID))
                        for succID in stackv.getSuccessorIDs():
                            vToParent[succID] = stackVertexID
                            stack.append(loopAutomaton.getVertex(succID))
        
            
    def __output (self, vertexID):
        for keyID in self.__vToRegExp[vertexID]:
            Debug.debugMessage("RegExp(%d@%d) = %s" % (keyID, vertexID, self.__vToRegExp[vertexID][keyID]), 10)
    
    def __addIpointEdges (self):
        for v in self.__loopAutomaton:
            if isinstance(v, Ipoint):
                succID = v.getVertexID()
                for predID in self.__vToRegExp[succID]:
                    self.__loopAutomaton.addEdge(predID, succID, self.__vToRegExp[succID][predID])
    
    def __compute (self, icfg, dfs, domTree, reverseDomTree, lcaQuery, acyclicReducibility):
        for vertexID in reversed(dfs.getPostorder()):
            # Add Ipoints to automaton graph
            if icfg.isIpoint(vertexID): 
                self.__loopAutomaton.addVertex(Vertices.Ipoint(vertexID, vertexID))
            v = icfg.getVertex(vertexID)
            if self.__dominanceFrontier.isEmpty(vertexID):
                # Empty pre-dominance frontier. Vertex is ALWAYS executed in this acyclic subgraph
                if vertexID == icfg.getEntryID(): 
                    self.__currentRegExpVertex = self.__loopAutomaton.createRegExpVertex()
                    if icfg.isIpoint(vertexID):
                        self.__loopAutomaton.addEdge(vertexID, self.__currentRegExpVertex.getVertexID())  
                
                if not icfg.isIpoint(vertexID):   
                    if v.numberOfPredecessors() > 1:
                        self.__handleMerge(icfg, domTree, reverseDomTree, lcaQuery, v)
                        # A region has just been discovered and collapsed. 
                        # Create a new regular expression vertex in preparation for basic blocks in a
                        # new region
                        self.__currentRegExpVertex = self.__loopAutomaton.createRegExpVertex()
                        for keyID in self.__vToRegExp[vertexID]:
                            self.__loopAutomaton.addEdge(keyID, \
                                                         self.__currentRegExpVertex.getVertexID(), \
                                                         self.__vToRegExp[vertexID][keyID])
                    # Add the basic block to the current regular expression vertex as it
                    # always executed
                    self.__loopAutomaton.addBasicBlockToVertex(self.__currentRegExpVertex, vertexID)  
                elif vertexID != icfg.getEntryID(): 
                    self.__loopAutomaton.addEdge(self.__currentRegExpVertex.getVertexID(), vertexID)    
                    self.__currentRegExpVertex = self.__loopAutomaton.getVertex(vertexID)                  
                    if v.numberOfSuccessors() == 1:  
                        self.__currentRegExpVertex = self.__loopAutomaton.createRegExpVertex()            
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
            self.__vToRegExp[vertexID][predID] = RegExp()
            if not icfg.isIpoint(vertexID):
                self.__vToRegExp[vertexID][predID].append(vertexID)
        else:
            if predv.numberOfSuccessors () > 1:   
                keyID = self.__currentRegExpVertex.getVertexID()
                self.__vToRegExp[vertexID][keyID] = RegExp()
                if not icfg.isIpoint(vertexID):
                    self.__vToRegExp[vertexID][keyID].append(vertexID) 
            else:
                for keyID in self.__vToRegExp[predID]:
                    self.__vToRegExp[vertexID][keyID] = RegExp()
                    self.__vToRegExp[vertexID][keyID].append(self.__vToRegExp[predID][keyID])
                    if not icfg.isIpoint(vertexID):
                        self.__vToRegExp[vertexID][keyID].append(RegExp.concatenation)   
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
                        self.__vToRegExp[mergeID][vertexID] = RegExp()
                    else:
                        parentID = v.getParentID()
                        for keyID in self.__vToRegExp[vertexID]:
                            if keyID in self.__vToRegExp[parentID] or self.__dominanceFrontier.isEmpty(parentID):
                                vToTempRegExp[vertexID][keyID]   = self.__vToRegExp[vertexID][keyID]  
                                vToTempRegExp[parentID][keyID]   = RegExp()
                                self.__vToRegExp[mergeID][keyID] = RegExp()
                            else:
                                print "%d not in %d" % (keyID, parentID)
                                self.__vToRegExp[mergeID][keyID] = RegExp()
                                self.__vToRegExp[mergeID][keyID].append(self.__vToRegExp[vertexID][keyID])
                                self.__vToRegExp[mergeID][keyID].append(RegExp.concatenation) 
                else:
                    # Internal vertex
                    for keyID in vToTempRegExp[vertexID]:
                        # Calculate how many alternative paths there are from this vertex to the merge
                        vToAlternatives[vertexID][keyID] = 0
                        if icfg.getVertex(vertexID).hasSuccessor(mergeID):
                            vToAlternatives[vertexID][keyID] += 1
                        for succID in v.getSuccessorIDs():
                            if keyID in vToTempRegExp[succID]:
                                vToAlternatives[vertexID][keyID] += 1
                   
                        Debug.debugMessage("v=%d key=%d alternatives=%d" % (vertexID, keyID, vToAlternatives[vertexID][keyID]), 20)
                                           
                        if vertexID != rootID:
                            vToTempRegExp[vertexID][keyID].append(self.__vToRegExp[vertexID][keyID])
                                  
                        if vToAlternatives[vertexID][keyID] > 1:
                            vToTempRegExp[vertexID][keyID].append(RegExp.lParen())
                        
                        temp = vToAlternatives[vertexID][keyID]   
                        for succID in v.getSuccessorIDs():
                            if keyID in vToTempRegExp[succID]:
                                if vToAlternatives[vertexID][keyID] == 1:
                                    if vertexID != rootID \
                                    or (vertexID == rootID and reverseDomTree.getImmediateDominator(rootID) == mergeID and not self.__dominanceFrontier.isEmpty(vertexID)): 
                                        vToTempRegExp[vertexID][keyID].append(RegExp.concatenation)
                                vToTempRegExp[vertexID][keyID].append(vToTempRegExp[succID][keyID])
                                if temp > 1:
                                    vToTempRegExp[vertexID][keyID].append(RegExp.union)
                                    temp -= 1
                             
                        if icfg.getVertex(vertexID).hasSuccessor(mergeID):
                            vToTempRegExp[vertexID][keyID].append(RegExp.lambda_)
                            
                        if vToAlternatives[vertexID][keyID] > 1:
                            vToTempRegExp[vertexID][keyID].append(RegExp.rParen())

        # End of compressed dominator tree traversal
        for keyID in self.__vToRegExp[mergeID]:
            if keyID in vToTempRegExp[rootID]:
                if reverseDomTree.getImmediateDominator(rootID) == mergeID \
                and not self.__dominanceFrontier.isEmpty(rootID):
                    self.__vToRegExp[mergeID][keyID].append(self.__vToRegExp[rootID][keyID])  
                self.__vToRegExp[mergeID][keyID].append(vToTempRegExp[rootID][keyID])    
            if not mergev.isDummy() \
            and not self.__dominanceFrontier.isEmpty(mergeID):
                if keyID in vToAlternatives[rootID] and vToAlternatives[rootID][keyID] == 1:
                    print self.__vToRegExp[mergeID][keyID]
                    self.__vToRegExp[mergeID][keyID].append(RegExp.concatenation) 
                self.__vToRegExp[mergeID][keyID].append(mergeID) 
            
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
            self.__vToRegExp[vertexID] = RegExp(False) 
        
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
                    newExp = RegExp(False)
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
            
    
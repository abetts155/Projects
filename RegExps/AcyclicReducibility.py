
class DominanceFrontiers:
    def __init__(self, cfg, domTree):
        self.__cfg     = cfg
        self.__domTree = domTree
        self.__initialise()
        self.__compute()
        
    def output(self):
        for v in self.__cfg:
            vertexID = v.getVertexID()
            print "DF(%d) = %s" % (vertexID, self.__vToDF[vertexID])
        
    def size (self, vertexID):
        assert vertexID in self.__vToDF, "Unable to find %d in dominance frontiers" % vertexID
        return len(self.__vToDF[vertexID])
    
    def set (self, vertexID):
        assert vertexID in self.__vToDF, "Unable to find %d in dominance frontiers" % vertexID
        return self.__vToDF[vertexID]
    
    def contains (self, vertexID, elementID):
        assert vertexID in self.__vToDF, "Unable to find %d in dominance frontiers" % vertexID
        return elementID in self.__vToDF[vertexID]
    
    def __initialise(self):
        self.__vToDF = {}
        for v in self.__cfg:
            self.__vToDF[v.getVertexID()] = set([])
    
    def __compute(self):
        for v in self.__cfg:
            vertexID = v.getVertexID()    
            if v.numberOfPredecessors() > 1: 
                idomID = self.__domTree.getImmediateDominator(vertexID)
                for predID in v.getPredecessorIDs():
                    runnerID = predID
                    while runnerID != idomID:
                        self.__vToDF[runnerID].add(vertexID)
                        runnerID = self.__domTree.getImmediateDominator(runnerID)

class AcyclicReducibility:
    def __init__(self, cfg, reversecfg, predomTree, postdomTree):
        self.__cfg         = cfg
        self.__reversecfg  = reversecfg
        self.__predomTree  = predomTree
        self.__postdomTree = postdomTree
        self.__irreducibleBranches = set([])
        self.__irreducibleMerges   = set([])
        self.__preDF  = DominanceFrontiers(self.__cfg, self.__predomTree)
        self.__postDF = DominanceFrontiers(self.__reversecfg, self.__postdomTree)
        self.__compute()
        self.output()
    
    def output (self):
        print "%s%s%s" % ('*' * 5, 'BRANCHES', '*' * 5)
        for vertexID in self.__irreducibleBranches:
            print "%d is IRREDUCIBLE" % vertexID
        for v in self.__cfg:
            vertexID = v.getVertexID()
            if v.numberOfSuccessors() > 1 and vertexID not in self.__irreducibleBranches:
                    print "%d is reducible" % vertexID
                    
        print "\n%s%s%s" % ('*' * 5, 'MERGES', '*' * 5)
        for vertexID in self.__irreducibleMerges:
            print "%d is IRREDUCIBLE" % vertexID
        for v in self.__cfg:
            vertexID = v.getVertexID()
            if v.numberOfPredecessors() > 1 and vertexID not in self.__irreducibleMerges:
                    print "%d is reducible" % vertexID
    
    def isReducible (self):
        return len(self.__irreducibleBranches) == 0 and len(self.__irreducibleMerges) == 0
        
    def isReducibleBranch (self, branchID):
        return branchID not in self.__irreducibleBranches
    
    def isReducibleMerge (self, mergeID):
        return mergeID not in self.__irreducibleMerges
    
    def __compute(self):
        for v in self.__cfg:
            vertexID = v.getVertexID()
            # Check for acyclic irreducible branch
            if v.numberOfSuccessors() > 1:
                ipostID = self.__postdomTree.getImmediateDominator(vertexID)
                ipreID  = self.__predomTree.getImmediateDominator(ipostID)
                 
                if ipreID != vertexID:
                    if self.__preDF.size(vertexID) > 1:
                        self.__irreducibleBranches.add(vertexID)
                        
            # Check for acyclic irreducible merge
            if v.numberOfPredecessors() > 1:
                ipreID  = self.__predomTree.getImmediateDominator(vertexID)
                ipostID = self.__postdomTree.getImmediateDominator(ipreID)
                 
                if ipostID != vertexID:
                    if self.__postDF.size(vertexID) > 1:
                        self.__irreducibleMerges.add(vertexID)
                    
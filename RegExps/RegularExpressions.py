import UDrawGraph, Debug, AcyclicReducibility, Trees

class RegExp:
    lambda_       = '@'
    lParen        = '('
    rParen        = ')'
    union         = ' | '
    kleeneStar    = '*'
    kleenePlus    = '+'
    concatenation = '.'
    
    def __init__(self):
        self.__expr = ''
        
    def __str__ (self):
        return self.__expr
        
    def append (self, *args):
        for arg in args:
            self.__expr += arg.__str__()

class RegularExpressions:
    def __init__(self, cfg):
        self.__cfg = cfg
        functionName = cfg.getName()
        Debug.debugMessage("Doing regular expression analysis of CFG '%s'" % functionName, 1)        
        UDrawGraph.makeUdrawFile (cfg, "%s.%s" % (functionName, "cfg"))
        self.__predomTree  = Trees.Dominators(cfg, cfg.getEntryID())
        self.__reversecfg  = cfg.getReverseCFG()
        self.__postdomTree = Trees.Dominators(self.__reversecfg, self.__reversecfg.getEntryID())
        UDrawGraph.makeUdrawFile (self.__predomTree, "%s.%s" % (functionName, "pre"))
        UDrawGraph.makeUdrawFile (self.__postdomTree, "%s.%s" % (functionName, "post"))
        self.__preDF  = AcyclicReducibility.DominanceFrontiers(self.__cfg, self.__predomTree)
        self.__postDF = AcyclicReducibility.DominanceFrontiers(self.__reversecfg, self.__postdomTree)
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
                    self.__vToRegExp[vertexID].append(self.__vToRegExp[predID], RegExp.concatenation, vertexID)
                else:
                    pass
                    
            print "RegExp(%d) = %s" % (vertexID, self.__vToRegExp[vertexID])
                    
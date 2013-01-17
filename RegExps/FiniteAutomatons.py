import DirectedGraphs, Vertices, Edges, Debug
import copy

class RegExp:
    lambda_       = '@'
    union         = ' | '
    kleeneStar    = '*'
    kleenePlus    = '+'
    concatenation = '.'
    
    def __init__(self):
        self.__elements = []     
        
    def __str__ (self):
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
        
    def isEmpty (self):
        return len(self.__elements) == 0
    
    def last (self):
        return self.__elements[-1]
    
    def first (self):
        return self.__elements[0]
        
    def append (self, *args):
        for arg in args:
            if isinstance(arg, RegExp):
                self.__elements.extend(arg.__elements)
            elif isinstance(arg, int):
                self.__elements.append(str(arg))
            else:
                self.__elements.append(arg)

class GeneralisedAutomaton (DirectedGraphs.DirectedGraph):
    def __init__ (self):
        DirectedGraphs.DirectedGraph.__init__(self)
        self.__vToVertexList = {}
        self.__bbToVertex    = {}
        
    def createRegExpVertex (self):
        vertexID = self.getNextVertexID()
        v        = RegExpVertex(vertexID)
        self.vertices[vertexID] = v
        self.__vToVertexList[vertexID] = RegExp()
        return v
    
    def addBasicBlockToVertex (self, v, bbID):
        vertexID = v.getVertexID()
        if not self.__vToVertexList[vertexID].isEmpty():
            self.__vToVertexList[vertexID].append(RegExp.concatenation)
        self.__vToVertexList[vertexID].append(bbID)
        self.__bbToVertex[bbID] = v
        
    def getExpr (self, vertexID):
        return self.__vToVertexList[vertexID]
        
    def addVertex (self, v):
        vertexID = v.getVertexID()
        assert vertexID not in self.vertices, \
        "Adding vertex %d which is already in graph" % vertexID
        self.vertices[vertexID] = v
        
    def addEdge (self, predID, succID, regExp=None):
        Debug.debugMessage("Adding edge (%d, %d) with path expression %s" % (predID, succID, regExp), 20)
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
        predv.addSuccessorEdge(RegExpEdge(succID, regExp))
        succv.addPredecessorEdge(RegExpEdge(predID, regExp))
        
    def getRegExpVertex (self, bbID):
        assert bbID in self.__bbToVertex, "Unable to find basic block %d in generalised automaton vertex"
        return self.__bbToVertex[bbID]
    
    def __str__ (self, vertexID):
        if isinstance(self.getVertex(vertexID), RegExpVertex):
            return self.__vToVertexList[vertexID].__str__()
        else:
            return str(vertexID)
        
class RegExpVertex (Vertices.Vertex):
    def __init__(self, vertexID):
        Vertices.Vertex.__init__(self, vertexID)
        
class RegExpEdge (Edges.Edge):
    def __init__ (self, vertexID, regExp=None):
        Edges.Edge.__init__(self, vertexID)
        if regExp == None:
            self.__expr = RegExp()
        else:
            self.__expr = regExp
        
    def setExpr (self, expr):
        self.__expr = expr
        
    def getExpr (self):
        assert self.__expr, "The edge with destination %d does not have a path expression set" % (self._vertexID)
        return self.__expr
        

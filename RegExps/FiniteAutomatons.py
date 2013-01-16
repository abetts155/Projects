import DirectedGraphs, Vertices, Edges 

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
        self.__vToVertexList[vertexID] = []
        return v
    
    def addBasicBlockToVertex (self, v, bbID):
        vertexID = v.getVertexID()
        self.__vToVertexList[vertexID].append(bbID)
        self.__bbToVertex[bbID] = v
        
    def addVertex (self, v):
        vertexID = v.getVertexID()
        assert vertexID not in self.vertices, \
        "Adding vertex %d which is already in graph" % vertexID
        self.vertices[vertexID] = v
        
    def getRegExpVertex (self, bbID):
        assert bbID in self.__bbToVertex, "Unable to find basic block %d in generalised automaton vertex"
        return self.__bbToVertex[bbID]
    
    def __str__ (self, vertexID):
        if isinstance(self.getVertex(vertexID), RegExpVertex):
            return ', '.join(str(x) for x in self.__vToVertexList[vertexID])
        else:
            return str(vertexID)
        
class RegExpVertex (Vertices.Vertex):
    def __init__(self, vertexID):
        Vertices.Vertex.__init__(self, vertexID)
        
class RegExpEdge (Edges.Edge):
    def __init__ (self, vertexID):
        Edges.Edge.__init__(self, vertexID)
        self.__expr = None
        
    def setExpr (self, expr):
        self.__expr = expr
        
    def getExpr (self):
        assert self.__expr, "The edge with destination %d does not have a path expression set" % (self._vertexID)
        return self.__expr
        

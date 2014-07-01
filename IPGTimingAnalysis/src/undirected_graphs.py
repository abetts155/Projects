import vertices

class UndirectedGraph ():        
    def __init__ (self):
        self.vertices = {}
        
    def addVertex (self, vertexID):
        assert vertexID not in self.vertices, "Vertex " + str(vertexID) + " is not in the graph"
        v = vertices.UndirectedVertex(vertexID)
        self.vertices[vertexID] = v
        
    def getVertex (self, vertexID):
        assert vertexID in self.vertices, "Vertex " + str(vertexID) + " is not in the graph"
        return self.vertices[vertexID]
    
    def removeVertex (self, vertexID):
        assert vertexID in self.vertices, "Vertex " + str(vertexID) + " is not in the graph"
        del self.vertices[vertexID]
    
    def hasVertex (self, vertexID):
        return vertexID in self.vertices
    
    def addEdge (self, vertexID1, vertexID2):
        v1 = self.getVertex(vertexID1)
        v2 = self.getVertex(vertexID2)
        if vertexID1 == vertexID2:
            v1.addEdge(vertexID1)
        else:
            v1.addEdge(vertexID2)
            v2.addEdge(vertexID1)
        
    def __str__ (self):
        string = "*" * 40 + "\n"
        for v in self.vertices.values():
            string += v.__str__()
        string += "*" * 40 + "\n"
        return string
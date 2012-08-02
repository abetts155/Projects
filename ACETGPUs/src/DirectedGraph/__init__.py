import Vertices, Edges, Debug

dummyVertexID = -1

class DirectedGraph ():        
    def __init__ (self):
        self.vertices = {}
    
    def getVertex (self, vertexID):
        assert vertexID in self.vertices, "Vertex " + str(vertexID) + " is not in the graph"
        return self.vertices[vertexID]
    
    def hasVertex (self, vertexID):
        return vertexID in self.vertices
    
    def addEdge (self, predID, succID):
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
        predv.addSuccessor(succID)
        succv.addPredecessor(predID)
    
    def getNextVertexID (self):
        nextID = 0
        while nextID in self.vertices.keys():
            nextID = nextID + 1 
        return nextID
    
    def __iter__ (self):
        return self.vertices.values().__iter__()    
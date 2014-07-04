import vertices
import copy

class DirectedGraph ():        
    def __init__ (self):
        self.the_vertices = {}
        self.name = None 
    
    def addVertex (self, v):
        assert v.vertexID not in self.the_vertices, "Adding vertex %d which is already in graph" % v.vertexID
        self.the_vertices[v.vertexID] = v
    
    def getVertex (self, vertexID):
        assert vertexID in self.the_vertices, "Vertex " + str(vertexID) + " is not in the graph"
        return self.the_vertices[vertexID]
    
    def removeVertex (self, vertexID):
        assert vertexID in self.the_vertices, "Vertex " + str(vertexID) + " is not in the graph"
        del self.the_vertices[vertexID]
    
    def hasVertex (self, vertexID):
        return vertexID in self.the_vertices
    
    def addEdge (self, predID, succID):
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
        predv.add_successor(succID)
        succv.add_predecessor(predID)
        
    def hasEdge (self, predID, succID):
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
        return succID in predv.successors.keys() and predID in succv.predecessors.keys()
    
    def removeEdge (self, predID, succID):
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
        predv.remove_successor(succID)
        succv.remove_predecessor(predID)
        
    def add_predecessor_edges (self):
        for v in self:
            for succID in v.successors.keys():
                succv = self.getVertex(succID)
                if not succv.has_predecessor(v.vertexID):
                    succv.add_predecessor(v.vertexID)
    
    def getNextVertexID (self):
        nextID = 1
        while nextID in self.the_vertices.keys():
            nextID = nextID + 1 
        return nextID
    
    def number_of_vertices(self):
        return len(self.the_vertices)
    
    def number_of_edges(self):
        total = 0
        for v in self.the_vertices.values():
            total += v.number_of_successors()
        return total
    
    def __iter__ (self):
        return self.the_vertices.values().__iter__()

class FlowGraph (DirectedGraph):
    def __init__ (self):
        DirectedGraph.__init__(self)
        self.entryID = vertices.dummyID
        self.exitID = vertices.dummyID
    
    def get_reverse_graph(self):
        reverseg = FlowGraph() 
        # Add vertices
        for v in self:
            copyv = copy.copy(v)
            copyv.successors   = {}
            copyv.predecessors = {}
            reverseg.the_vertices[copyv.vertexID] = copyv
        # Add edges
        for v in self:
            predID = v.vertexID
            predv  = reverseg.getVertex(predID)
            for succID in v.successors.keys():
                succv = reverseg.getVertex(succID)
                predv.add_predecessor(succID)
                succv.add_successor(predID)
        # Set the entry and exit IDs
        reverseg.entryID = self.get_exitID()
        reverseg.exitID  = self.get_entryID()
        return reverseg
            
    def set_edgeIDs (self):
        edgeID = 1
        for v in self:
            for succID in v.successors.keys():
                succe = v.get_successor_edge(succID)
                succe.set_edgeID(edgeID)
                succv = self.getVertex(succID)
                prede = succv.get_predecessor_edge(v.vertexID)
                prede.set_edgeID(edgeID)
                edgeID += 1
        
    def get_entryID (self):
        assert self.entryID != vertices.dummyID, "Entry to flow graph not found"
        return self.entryID
    
    def get_exitID (self):
        assert self.exitID != vertices.dummyID, "Exit to flow graph not found"
        return self.exitID
    
    def __str__ (self):
        the_string = "*" * 40 + "\n" + \
        "Entry ID = %s\n" % str(self.entryID) + \
        "Exit ID  = %s\n" % str(self.exitID) + "\n"
        for v in self.the_vertices.values():
            the_string += v.__str__()
        the_string += "*" * 40 + "\n"
        return the_string

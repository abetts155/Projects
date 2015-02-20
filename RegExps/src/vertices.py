import edges
import collections

dummyID = 0

class Vertex:
    def __init__(self, vertexID, real_vertexID=None):
        self.vertexID = vertexID
        if real_vertexID:
            self.real_vertexID = real_vertexID
        else:
            self.real_vertexID = vertexID
        self.predecessors = collections.OrderedDict()
        self.successors   = collections.OrderedDict()
        self.loop_header  = False
        
    def add_predecessor(self, predID, edgeID=None):
        assert predID not in self.predecessors, "Vertex %d already has predecessor %d" % (self.vertexID, predID)
        self.predecessors[predID] = edges.Edge(predID, edgeID)
        
    def add_predecessor_edge(self, prede):
        assert prede.vertexID not in self.predecessors, "Vertex %d already has predecessor %d" % (self.vertexID, prede.vertexID)
        self.predecessors[prede.vertexID] = prede
            
    def remove_predecessor(self, predID):
        assert predID in self.predecessors, "Cannot remove %d as it is not in predecessor of %d" % (predID, self.vertexID)
        del self.predecessors[predID]
    
    def number_of_predecessors(self):
        return len(self.predecessors)
    
    def has_predecessor(self, predID):
        return predID in self.predecessors.keys()
    
    def get_predecessor_edge(self, predID):
        assert predID in self.predecessors, "Vertex %d is not a predecessor of %d" % (predID, self.vertexID)
        return self.predecessors[predID]
    
    def add_successor(self, succID,edgeID=None):
        assert succID not in self.successors, "Vertex %d already has successor %d" % (self.vertexID, succID)
        self.successors[succID] = edges.Edge(succID, edgeID)
        
    def add_successor_edge(self, succe):
        assert succe.vertexID not in self.successors, "Vertex %d already has successor %d" % (self.vertexID, succe.vertexID)
        self.successors[succe.vertexID] = succe
        
    def remove_successor(self, succID):
        assert succID in self.successors, "Cannot remove %d as it is not in successors of %d" % (succID, self.vertexID)
        del self.successors[succID]
    
    def number_of_successors(self):
        return len(self.successors)
    
    def has_successor(self, succID):
        return succID in self.successors.keys()
    
    def get_successor_edge(self, succID):
        assert succID in self.successors, "Vertex %d is not a successor of %d" % (succID, self.vertexID)
        return self.successors[succID]
    
    def edge_incidence_string(self, the_edges):
        string = "{"
        count = 1
        for predID in sorted(the_edges.keys()):
            string += str(predID)
            if count < len(the_edges):
                string += ","
                count = count + 1
        string += "}"
        return string
    
    def __str__(self):
        return """ID      = %d
real ID = %d
succ    = %s 
pred    = %s
""" % (self.vertexID, 
       self.real_vertexID,
       self.edge_incidence_string(self.successors), 
       self.edge_incidence_string(self.predecessors))
    
class TreeVertex(Vertex):
    def __init__ (self, vertexID):
        Vertex.__init__(self, vertexID)
        self.parentID = dummyID
        self.level    = -1
        
    def __str__(self):
        return """ID     = %d 
parent = %d
""" % (self.vertexID, 
       self.parentID)
    
class HeaderVertex(TreeVertex):
    def __init__ (self, vertexID, headerID):
        TreeVertex.__init__(self, vertexID)
        self.headerID = headerID

class RegExpVertex(Vertex):
    ALTERNATIVE   = "|"
    SEQUENCE      = "."
    FOR_LOOP      = "*"
    
    def __init__(self, vertexID, operator):
        Vertex.__init__(self, vertexID)
        assert operator == RegExpVertex.ALTERNATIVE \
        or operator == RegExpVertex.SEQUENCE \
        or operator == RegExpVertex.FOR_LOOP
        self.operator = operator
        
class ProgramPoint(Vertex):
    def __init__(self, vertexID, the_program_point, edgeID=None):
        Vertex.__init__(self, vertexID)
        self.the_program_point = the_program_point
        self.edgeID = edgeID
        
    def __str__(self):
        return Vertex.__str__(self) + """program point = %s
edge ID  = %s
""" % (self.the_program_point, self.edgeID)
        
        
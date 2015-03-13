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
        self.instrumented = False
        
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

class CallGraphVertex(Vertex):
    def __init__ (self, vertexID, name):
        Vertex.__init__(self, vertexID)
        self.name = name
    
    def add_predecessor(self, predID, call_siteID):
        if predID not in self.predecessors:
            the_edge = edges.CallGraphEdge(predID)
            self.predecessors[predID] = the_edge
        the_edge = self.predecessors[predID]
        the_edge.call_sites.add(call_siteID)
    
    def add_successor(self, succID, call_siteID):
        if succID not in self.successors:
            the_edge = edges.CallGraphEdge(succID)
            self.successors[succID] = the_edge
        the_edge = self.successors[succID]
        the_edge.call_sites.add(call_siteID)
        
    def get_successor_with_call_site(self, call_siteID):
        for succe in self.successors.values():
            if call_siteID in succe.call_sites:
                return succe.vertexID
        assert False, "Can not find call successor from '%s' with call site %d" % (self.name, call_siteID)
    
    def __str__ (self):
        return "%s\n%s" % (self.name, Vertex.__str__(self))
    
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

class CFGEdge(Vertex):
    def __init__(self, vertexID, predID, succID):
        Vertex.__init__(self, vertexID)
        self.edge = (predID, succID)
    
    def __str__(self):
        return "%s%s\n" % (Vertex.__str__(self), self.edge)
        
        
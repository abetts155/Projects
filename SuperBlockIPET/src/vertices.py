import edges
import collections

dummyID = 0

class Vertex:
    def __init__ (self, vertexID):
        self.vertexID = vertexID
        self.predecessors = {}
        self.successors   = {}
        self.dummy        = False
    
    def add_predecessor(self, predID, edgeID=None):
        assert predID not in self.predecessors, "Vertex %d already has predecessor %d" % (self.vertexID, predID)
        e = edges.Edge(predID, edgeID)
        self.predecessors[predID] = e
        
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
        e = edges.Edge(succID, edgeID)
        self.successors[succID] = e
        
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
    
    def predecessor_string(self):
        string = "pred = {"
        count = 1
        for predID in sorted(self.predecessors.keys()):
            string += str(predID)
            if count < len(self.predecessors):
                string += ","
                count = count + 1
        string += "}"
        return string
    
    def successor_string(self):        
        string = "succ = {"
        count = 1
        for succID in sorted(self.successors.keys()):
            string += str(succID)
            if count < len(self.successors):
                string += ","
                count = count + 1
        string += "}"
        return string
    
    def __str__(self):
        return "%d: %s %s\n" % (self.vertexID, self.successor_string(), self.predecessor_string())
    
class TreeVertex(Vertex):
    def __init__ (self, vertexID):
        Vertex.__init__(self, vertexID)
        self.parentID = dummyID
        self.level    = -1
    
class HeaderVertex(TreeVertex):
    def __init__ (self, vertexID, headerID):
        TreeVertex.__init__(self, vertexID)
        self.headerID = headerID
    
class CFGVertex(Vertex):
    def __init__(self, vertexID):
        Vertex.__init__(self, vertexID)
    
class CFGEdge(Vertex):
    def __init__(self, vertexID, predID, succID):
        Vertex.__init__(self, vertexID)
        self.edge = (predID, succID)
    
    def __str__(self):
        return "%s%s\n" % (Vertex.__str__(self), self.edge)
    
class SuperBlock (Vertex):
    def __init__ (self, vertexID, headerID):
        Vertex.__init__(self, vertexID)
        self.headerID             = headerID
        self.program_points       = []
        self.representative       = None
        self.successor_partitions = collections.OrderedDict()
        self.exit_edge            = False

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
        
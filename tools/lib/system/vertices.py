"""
This module includes all vertex types that appear in graphs. 
"""

import collections



class Vertex:
    
    """
    Models a vertex in a graph as a vertex id, a set of predecessors and a set
    of successors.
    """
    
    def __init__(self, vertex_id):
        self._vertex_id = vertex_id
        self._predecessors = collections.OrderedDict()
        self._successors = collections.OrderedDict()

    
    @property
    def vertex_id(self):
        return self._vertex_id
    
    
    def add_predecessor_edge(self, pred_edge):
        assert pred_edge.vertex_id not in self._predecessors,\
        'Vertex %d already has predecessor %d' % (self._vertex_id, 
                                                  pred_edge.vertex_id)
        self._predecessors[pred_edge.vertex_id] = pred_edge
        
            
    def remove_predecessor_edge(self, pred_id):
        try:
            del self._predecessors[pred_id]
        except KeyError:
            raise KeyError('Vertex %d has no predecessor %d to remove' % \
                           (self._vertex_id, pred_id))
    
            
    def get_predecessor_edge(self, pred_id):
        try:
            return self._predecessors[pred_id]
        except KeyError:
            raise KeyError('Vertex %d has no predecessor %d' % \
                           (self._vertex_id, pred_id))
            
    
    def number_of_predecessors(self):
        return len(self._predecessors)
    
    
    def has_predecessor(self, pred_id):
        return pred_id in self._predecessors.keys()
            
    
    def predecessor_edge_iterator(self):
        for _, pred_edge in self._predecessors.items():
            yield pred_edge
            
    
    def get_ith_predecessor_edge(self, index):
        try:
            return list(self._predecessors.values())[index]
        except KeyError:
            raise KeyError('Vertex %d has %d predecessors:' 
                           ' element %d does not exist' % (self._vertex_id,
                                                           self.number_of_predecessors(),
                                                           index))
            
    
    def add_successor_edge(self, succ_edge):
        assert succ_edge.vertex_id not in self._successors,\
        'Vertex %d already has successor %d' % (self._vertex_id, 
                                                succ_edge.vertex_id)
        self._successors[succ_edge.vertex_id] = succ_edge
        
        
    def remove_successor_edge(self, succ_id):
        try:
            del self._successors[succ_id]
        except KeyError:
            raise KeyError('Vertex %d has no successor %d to remove' % \
                           (self._vertex_id, succ_id))
        
        
    def get_successor_edge(self, succ_id):
        try:
            return self._successors[succ_id]
        except KeyError:
            raise KeyError('Vertex %d has no successor %d' % \
                           (self._vertex_id, succ_id))
            
                
    def number_of_successors(self):
        return len(self._successors)
    
    
    def has_successor(self, succ_id):
        return succ_id in self._successors.keys()
    
    
    def successor_edge_iterator(self):
        for _, succ_edge in self._successors.items():
            yield succ_edge
            
    
    def get_ith_successor_edge(self, index):
        try:
            return list(self._successors.values())[index]
        except KeyError:
            raise KeyError('Vertex %d has %d successors:' 
                           ' element %d does not exist' % (self._vertex_id,
                                                           self.number_of_successors(),
                                                           index))
    
    
    def __repr__(self):
        return '%s(id=%r pred=%r succ=%r)' \
            % (self.__class__.__name__,
               self._vertex_id,
               ','.join(repr(value) for value in self._predecessors.values()),
               ','.join(repr(value) for value in self._successors.values()))
            
            
    def __hash__(self):
        return self._vertex_id
            
            
def is_basic_block(program_point):
    return isinstance(program_point, int)

            
class ProgramPointVertex(Vertex):
    
    """
    Models a program point (i.e., a basic block or a transition between two
    basic blocks) as a vertex.  These vertices are used in control flow graphs
    and path expressions.
    """
    
    def __init__(self, vertex_id, program_point, abstract=False):
        Vertex.__init__(self, vertex_id)
        self._program_point = program_point
        self._abstract = abstract
        
    
    @property
    def program_point(self):
        return self._program_point
    
    
    @property
    def abstract(self):
        return self._abstract
    
    
    def __str__(self):
        return str(self._program_point)
        
        

class SubprogramVertex(Vertex):
    
    """
    Models a method/procedure/function of a program in the call graph.
    """
    
    def __init__(self, vertex_id, name):
        Vertex.__init__(self, vertex_id)
        self._name = name
        
        
    @property
    def name(self):
        return self._name


    def __repr__(self):
        return '%s(id=%r name=%r pred=%r succ=%r)' \
            % (self.__class__.__name__,
               self._vertex_id,
               self._name,
               ','.join(repr(value) for value in self._predecessors.values()),
               ','.join(repr(value) for value in self._successors.values()))



class SuperBlock(Vertex):
    
    """
    Models a set of program points whose execution counts are guaranteed to be 
    equal and must always execute together.
    """
    
    def __init__(self, vertex_id):
        Vertex.__init__(self, vertex_id)
        self._vertices = []
        self._representative = None
        self._is_loop_exit_edge = False
        self._successor_partitions = {}
        
        
    @property
    def vertices(self):
        return self._vertices
    
    
    @property
    def representative(self):
        return self._representative
    
    
    @representative.setter
    def representative(self, value):
        self._representative = value 
        
        
    @property
    def is_loop_exit_edge(self):
        return self._is_loop_exit_edge
    
    
    @is_loop_exit_edge.setter
    def is_loop_exit_edge(self, value):
        self._is_loop_exit_edge = value


    def add_successor_edge_to_partition(self, branch_vertex, succ_edge):
        self._successor_partitions.setdefault(branch_vertex, set()).add(succ_edge)
        
    
    def successor_edge_partition_iterator(self):
        for branch_vertex, successor_edges in self._successor_partitions.items():
            yield branch_vertex, successor_edges

        
        
class RegularExpressionVertex(Vertex):
    
    """
    Models an internal vertex of a regular expression tree.
    """
    
    ALTERNATIVE   = '|'
    SEQUENCE      = '.'
    MIGHT_ITERATE = '*'
    MUST_ITERATE  = '+'
    
    def __init__(self, vertex_id, operator):
        Vertex.__init__(self, vertex_id)
        assert operator == RegularExpressionVertex.ALTERNATIVE\
        or operator == RegularExpressionVertex.SEQUENCE\
        or operator == RegularExpressionVertex.MIGHT_ITERATE\
        or operator == RegularExpressionVertex.MUST_ITERATE,\
        'Invalid regular expression operator %r' % operator
        self.operator = operator

        
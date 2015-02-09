import vertices
import edges
import debug
import udraw
import utils
import copy
import itertools

class DirectedGraph:        
    def __init__ (self):
        self.the_vertices = {}
        self.name = None
        
    def add_vertex(self, v):
        assert v.vertexID not in self.the_vertices, "Adding vertex %d which is already in graph" % v.vertexID
        self.the_vertices[v.vertexID] = v
    
    def get_vertex(self, vertexID):
        assert vertexID in self.the_vertices, "Vertex " + str(vertexID) + " is not in the graph"
        return self.the_vertices[vertexID]
    
    def has_vertex(self, vertexID):
        return vertexID in self.the_vertices
    
    def add_edge(self, predID, succID):
        predv = self.get_vertex(predID)
        succv = self.get_vertex(succID)
        predv.add_successor(succID)
        succv.add_predecessor(predID)
        
    def hasEdge(self, predID, succID):
        predv = self.get_vertex(predID)
        succv = self.get_vertex(succID)
        return predv.has_successor(succID) or succv.has_predecessor(predID)
    
    def removeEdge(self, predID, succID):
        predv = self.get_vertex(predID)
        succv = self.get_vertex(succID)
        predv.remove_successor(succID)
        succv.remove_predecessor(predID)
        
    def get_reverse_graph(self):
        reverseg = DirectedGraph() 
        self.add_vertices_to_reverse_graph(reverseg)
        self.add_edges_to_reverse_graph(reverseg)
        return reverseg
    
    def add_vertices_to_reverse_graph(self, reverseg):
        for v in self:
            copyv = copy.copy(v)
            copyv.successors   = {}
            copyv.predecessors = {}
            reverseg.the_vertices[copyv.vertexID] = copyv
            
    def add_edges_to_reverse_graph(self, reverseg):
        for v in self:
            predID = v.vertexID
            predv  = reverseg.get_vertex(predID)
            for succID in v.successors.keys():
                succv = reverseg.get_vertex(succID)
                predv.add_predecessor(succID)
                succv.add_successor(predID)
        
    def add_predecessor_edges(self):
        for v in self:
            for succID in v.successors.keys():
                succv = self.get_vertex(succID)
                if not succv.has_predecessor(v.vertexID):
                    succv.add_predecessor(v.vertexID)
    
    def get_sink_vertices(self):
        sink_vertices = set()
        for v in self.the_vertices.values():
            if v.number_of_successors() == 0:
                sink_vertices.add(v)
        return sink_vertices
    
    def get_source_vertices(self):
        source_vertices = set()
        for v in self.the_vertices.values():
            if v.number_of_predecessors() == 0:
                source_vertices.add(v)
        return source_vertices
    
    def get_next_vertexID(self):
        nextID = 1
        while nextID in self.the_vertices.keys():
            nextID += 1 
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
    
    def __str__ (self):
        string = "*" * 40 + "\n"
        for v in self.the_vertices.values():
            string += v.__str__() + "\n"
        return string

class FlowGraph(DirectedGraph):
    def __init__ (self):
        DirectedGraph.__init__(self)
        self.entryID = vertices.dummyID
        self.exitID  = vertices.dummyID
        
    def get_entryID (self):
        assert self.entryID != vertices.dummyID, "Entry to flow graph not found"
        return self.entryID
    
    def get_exitID (self):
        assert self.exitID != vertices.dummyID, "Exit to flow graph not found"
        return self.exitID
    
    def set_edgeIDs(self):
        edgeID = 1
        for v in self:
            for e_succ in v.successors.values():
                e_succ.edgeID = edgeID
                v_succ = self.get_vertex(e_succ.vertexID)
                e_pred = v_succ.get_predecessor_edge(v.vertexID)
                e_pred.edgeID = edgeID
                edgeID += 1
                

class CFG(FlowGraph):    
    def __init__ (self):
        FlowGraph.__init__(self)
        self.transition_graph = None
                        
    def set_entry_and_exit(self):
        without_predecessors = []
        without_successors   = []
        for v in self:
            if v.number_of_successors() == 0:
                without_successors.append(v.vertexID)
            if v.number_of_predecessors() == 0:
                without_predecessors.append(v.vertexID)
                
        if len(without_predecessors) == 0:
            debug.exit_message("CFG '%s' does not have an entry point" % self.name)
        elif len(without_predecessors) > 1:
            debug_info = ""
            for bbID in without_predecessors:
                bb = self.get_vertex(bbID)
                debug_info += bb.__str__()
            debug.exit_message("CFG '%s' has too many entry points: %s" % (self.name, debug_info))
        else:
            self.entryID = without_predecessors[0]
        
        if len(without_successors) == 0:
            debug.exit_message("CFG '%s' does not have an exit point" % self.name)
        elif len(without_successors) > 1:
            debug_info = ""
            for bbID in without_successors:
                bb = self.get_vertex(bbID)
                debug_info += bb.__str__()
            debug.exit_message("CFG '%s' has too many exit points: %s" % (self.name, debug_info))
        else:
            self.exitID = without_successors[0]
        assert self.entryID, "Unable to set entry ID"
        assert self.exitID, "Unable to set exit ID"
        self.add_edge(self.exitID, self.entryID)
        
    def get_transition_graph(self):
        if not self.transition_graph:
            self.transition_graph = StateTransitionGraph(self)
            udraw.make_file(self.transition_graph, "%s.transition" % self.name)
        return self.transition_graph
    
class StateTransitionGraph(FlowGraph):
    def __init__(self, cfg=None):
        FlowGraph.__init__(self)
        self.depth_first_tree = None
        self.dominator_tree   = None
        self.lnt              = None
        self.component_dag    = None
        self.program_point_to_predecessor_state = {}
        self.program_point_to_successor_state   = {}
        if cfg:
            self.create(cfg)
        
    def create(self, cfg):
        # Add a state transition per CFG vertex 
        new_stateID = 0
        for v_cfg in cfg:
            new_stateID += 1
            v_pred_state = vertices.Vertex(new_stateID)
            self.the_vertices[new_stateID] = v_pred_state
            self.program_point_to_predecessor_state[v_cfg.vertexID] = v_pred_state
            new_stateID += 1
            v_succ_state = vertices.Vertex(new_stateID)
            self.the_vertices[new_stateID] = v_succ_state
            self.program_point_to_successor_state[v_cfg.vertexID] = v_succ_state
            self.add_edge(v_pred_state, v_succ_state, v_cfg.vertexID)
            if v_cfg.vertexID == cfg.entryID:
                self.entryID = v_pred_state.vertexID
            if v_cfg.vertexID == cfg.exitID:
                self.exitID = v_succ_state.vertexID
        
        # Add a state transition per CFG edge
        for v_cfg in cfg:
            predID_cfg = v_cfg.vertexID
            v_pred_state = self.program_point_to_successor_state[predID_cfg]
            for succID_cfg in v_cfg.successors.keys():
                v_succ_state = self.program_point_to_predecessor_state[succID_cfg]
                self.add_edge(v_pred_state, v_succ_state, (predID_cfg, succID_cfg))
                self.program_point_to_predecessor_state[(predID_cfg, succID_cfg)] = v_pred_state
                self.program_point_to_successor_state[(predID_cfg, succID_cfg)] = v_succ_state
                
    def add_edge(self, v_pred, v_succ, the_program_point):
        e_pred = edges.TransitionEdge(v_pred.vertexID, the_program_point)
        e_succ = edges.TransitionEdge(v_succ.vertexID, the_program_point)
        v_pred.add_successor_edge(e_succ)
        v_succ.add_predecessor_edge(e_pred)
    
    def get_depth_first_search_tree(self):
        if self.depth_first_tree is None:
            self.depth_first_tree = DepthFirstSearch(self, self.entryID)
        return self.depth_first_tree            
    
    def get_dominator_tree(self):
        if self.dominator_tree is None:
            self.dominator_tree = Dominators(self, self.entryID)
        return self.dominator_tree
    
    def get_LNT(self):
        if self.lnt is None:
            self.lnt = LoopNests(self, self.entryID)
        return self.lnt

    def get_component_DAG(self):
        if self.component_dag is None:
            self.component_dag = StateTransitionComponentDAG()
            self.component_dag.build(self, self.get_LNT())
            self.component_dag.compute_reachability_information(self)
        return self.component_dag

class StateTransitionComponentDAG(FlowGraph):
    def __init__(self):
        FlowGraph.__init__(self)
        
    def build(self, transition_graph, transition_graph_lnt):
        loop_entry_states = {}
        loop_exit_states  = {}
        self.add_vertices(transition_graph, 
                          transition_graph_lnt, 
                          loop_entry_states,
                          loop_exit_states)
        self.add_edges(transition_graph, 
                       transition_graph_lnt,
                       loop_entry_states,
                       loop_exit_states)
        self.entryID = transition_graph.entryID
        self.exitID  = transition_graph.exitID
        
    def add_vertices(self, 
                     transition_graph, 
                     transition_graph_lnt, 
                     loop_entry_states,
                     loop_exit_states):
        for v in transition_graph:
            if transition_graph_lnt.get_vertex(v.vertexID).parentID == transition_graph_lnt.rootID:
                self.the_vertices[v.vertexID] = vertices.Vertex(v.vertexID)               
        for headerv in transition_graph_lnt.get_header_vertices():
            if headerv.vertexID != transition_graph_lnt.rootID \
            and headerv.parentID == transition_graph_lnt.rootID:
                loop_entry_stateID = self.get_next_vertexID()
                self.the_vertices[loop_entry_stateID] = vertices.Vertex(loop_entry_stateID)
                loop_entry_states[headerv.headerID] = loop_entry_stateID
                loop_exit_states[headerv.headerID] = list()
                for an_edge in transition_graph_lnt.loop_exit_edges_per_header[headerv.headerID]:
                    exit_stateID = self.get_next_vertexID()
                    self.the_vertices[exit_stateID] = vertices.Vertex(exit_stateID)
                    loop_exit_states[headerv.headerID].append(exit_stateID)
                
    def add_edges(self, 
                  transition_graph, 
                  transition_graph_lnt, 
                  loop_entry_states,
                  loop_exit_states):
        for v in transition_graph:
            if transition_graph_lnt.get_vertex(v.vertexID).parentID == transition_graph_lnt.rootID:
                for succe in v.successors.values():
                    if transition_graph_lnt.get_vertex(succe.vertexID).parentID == transition_graph_lnt.rootID:
                        v_pred        = self.get_vertex(v.vertexID)
                        v_succ        = self.get_vertex(succe.vertexID)
                        self.add_edge(v_pred, v_succ, succe.the_program_point)
                    else:
                        v_header_succ = transition_graph_lnt.get_vertex(transition_graph_lnt.get_vertex(succe.vertexID).parentID)
                        v_pred        = self.get_vertex(v.vertexID)
                        v_succ        = self.get_vertex(loop_entry_states[v_header_succ.headerID])
                        self.add_edge(v_pred, v_succ, succe.the_program_point)
        for headerv in transition_graph_lnt.get_header_vertices():
            if headerv.vertexID != transition_graph_lnt.rootID and headerv.parentID == transition_graph_lnt.rootID:
                loop_body = self.compute_loop_nest_body(transition_graph_lnt, headerv) 
                v_pred    = self.get_vertex(loop_entry_states[headerv.headerID])
                for an_edge in transition_graph_lnt.loop_exit_edges_per_header[headerv.headerID]:
                    v_exit = self.get_vertex(loop_exit_states[headerv.headerID].pop())
                    self.add_edge(v_pred, v_exit, loop_body)
                    if transition_graph_lnt.get_vertex(an_edge[1]).parentID == transition_graph_lnt.rootID:
                        v_succ = self.get_vertex(an_edge[1])
                    else:
                        v_succ_header = transition_graph_lnt.get_vertex(transition_graph_lnt.get_vertex(an_edge[1]).parentID)
                        v_succ        = self.get_vertex(loop_entry_states[v_succ_header.headerID])
                    v_pred_in_transitiong = transition_graph.get_vertex(an_edge[0])
                    e_in_transitiong      = v_pred_in_transitiong.get_successor_edge(an_edge[1])
                    self.add_edge(v_exit, v_succ, e_in_transitiong.the_program_point)
                    
    def compute_loop_nest_body(self, transition_graph_lnt, headerv):
        loop_body = set()
        stack = []
        stack.append(headerv)
        while stack:
            v_lnt = stack.pop()
            for succID in v_lnt.successors.keys():
                v_succ_lnt = transition_graph_lnt.get_vertex(succID)
                if isinstance(v_succ_lnt, vertices.HeaderVertex):
                    stack.append(v_succ_lnt)
                elif isinstance(v_succ_lnt, vertices.ProgramPoint):
                    loop_body.add(v_succ_lnt.the_program_point)
        return loop_body
                        
    def add_edge(self, v_pred, v_succ, the_program_point):
        e_pred = edges.TransitionEdge(v_pred.vertexID, the_program_point)
        e_succ = edges.TransitionEdge(v_succ.vertexID, the_program_point)
        v_pred.add_successor_edge(e_succ)
        v_succ.add_predecessor_edge(e_pred)
        
    def get_reverse_graph(self):
        reverse_graph = StateTransitionComponentDAG()
        for v in self:
            reverse_graph.the_vertices[v.vertexID] = vertices.Vertex(v.vertexID)
        for v in self:
            for succe in v.successors.values():
                reverse_graph.add_edge(reverse_graph.get_vertex(succe.vertexID), 
                                      reverse_graph.get_vertex(v.vertexID), 
                                      succe.the_program_point)
        reverse_graph.entryID = self.exitID
        reverse_graph.exitID  = self.entryID
        return reverse_graph
        
    def compute_reachability_information(self, transition_graph):
        # Initialise
        unified  = {}
        forward  = {}
        reverse  = {}
        for v in self:
            for succe in v.successors.values():
                unified[(v.vertexID, succe.vertexID)] = set()
                forward[(v.vertexID, succe.vertexID)] = set()
                reverse[(v.vertexID, succe.vertexID)] = set()
                if isinstance(succe.the_program_point, set):
                    forward[(v.vertexID, succe.vertexID)].update(succe.the_program_point)
                    reverse[(v.vertexID, succe.vertexID)].update(succe.the_program_point)
                else:
                    forward[(v.vertexID, succe.vertexID)].add(succe.the_program_point)
                    reverse[(v.vertexID, succe.vertexID)].add(succe.the_program_point)
         
        # Solve data flow equations in forward direction
        dfs = DepthFirstSearch(self, self.entryID)
        for vertexID in reversed(dfs.post_order):
            v = self.get_vertex(vertexID)
            for succe in v.successors.values():
                for prede in v.predecessors.values():
                    forward[(v.vertexID, succe.vertexID)].update(forward[(prede.vertexID, v.vertexID)])
        
        # Solve data flow equations in backward direction
        reverseg = self.get_reverse_graph()
        dfs      = DepthFirstSearch(reverseg, reverseg.entryID)
        for vertexID in reversed(dfs.post_order):
            v = reverseg.get_vertex(vertexID)
            for succe in v.successors.values():
                for prede in v.predecessors.values():
                    reverse[(succe.vertexID, v.vertexID)].update(reverse[(v.vertexID, prede.vertexID)])
        
        # Unify forward and backward information
        for v in self:
            for succID in v.successors.keys():
                unified[(v.vertexID, succID)] = forward[(v.vertexID, succID)].union(reverse[(v.vertexID, succID)])
        
        # Store reachability information per program point    
        self.reachability_info = {}
        for an_edge in unified.keys():
            v_pred = self.get_vertex(an_edge[0])
            succe  = v_pred.get_successor_edge(an_edge[1])
            if isinstance(succe.the_program_point, set):
                for a_program_point in succe.the_program_point:
                    self.reachability_info[a_program_point] = unified[an_edge]
            else:
                self.reachability_info[succe.the_program_point] = unified[an_edge]
        
    def is_reachable(self, a_program_point, the_set):
        assert a_program_point in self.reachability_info
        return the_set.issubset(self.unified[a_program_point])
    
class Tree(DirectedGraph):
    def __init__ (self):
        DirectedGraph.__init__(self)
        self.rootID = vertices.dummyID
        self.lca    = None
        
    def add_edge(self, predID, succID):
        DirectedGraph.add_edge(self, predID, succID)
        succv = self.get_vertex(succID)
        succv.parentID = predID
    
    def is_ancestor(self, left, right):
        if left == right:
            return True
        elif right == self.rootID:
            return False
        else:
            vertexID = right
            parentID = self.get_vertex(vertexID).parentID
            while parentID != self.rootID and parentID != left:
                vertexID = parentID
                parentID = self.get_vertex(vertexID).parentID
            if parentID == left:
                return True
            else:
                return False
    
    def is_proper_ancestor(self, left, right):
        if left == right:
            return False
        else:
            return self.is_ancestor(left, right)
        
    def level_by_level_iterator(self, up=True):
        rootv        = self.get_vertex(self.rootID)
        rootv.level  = 0
        queue        = [rootv]
        the_vertices = {}
        while queue:
            v = queue.pop()
            for succID in v.successors.keys():
                queue.insert(0, self.get_vertex(succID))
            if v.vertexID == self.rootID:
                the_vertices[0] = [rootv]
            else:
                v.level = self.get_vertex(v.parentID).level + 1
                if v.level not in the_vertices.keys():
                    the_vertices[v.level] = []
                the_vertices[v.level].append(v)
        if up:
            for level in reversed(sorted(the_vertices.keys())):
                yield the_vertices[level]
        else:
            for level in sorted(the_vertices.keys()):
                yield the_vertices[level]
                
    def check_is_tree(self):
        without_predecessors = set()
        for v in self:
            if v.number_of_predecessors() == 0:
                without_predecessors.add(v)
        assert len(without_predecessors) == 1  
        
    def get_LCA(self):
        if self.lca is None:
            self.lca = LeastCommonAncestor(self)
        return self.lca
        
class DepthFirstSearch(Tree):
    Colors = utils.enum('WHITE', 'BLACK', 'GRAY')

    def __init__(self, directedg, rootID):
        Tree.__init__(self)
        assert rootID in directedg.the_vertices.keys(), "Unable to find vertex %d from which to initiate depth-first search" % rootID
        self.rootID = rootID
        self.pre_order  = []
        self.post_order = []
        self.vertex_post_order_numbering = {}
        self.vertex_pre_order_numbering = {}
        self.back_edges = []
        self.initialise(directedg, rootID)
        self.do_search(directedg, rootID)
        
    def initialise(self, directedg, rootID):
        for v in directedg:
            self.the_vertices[v.vertexID] = vertices.TreeVertex(v.vertexID)
            self.vertex_pre_order_numbering[v.vertexID] = 0
            self.vertex_post_order_numbering[v.vertexID] = 0
        self.pre_orderID  = 1
        self.post_orderID = 1   
      
    def do_search(self, directedg, vertexID):
        self.vertex_pre_order_numbering[vertexID] = self.pre_orderID
        self.pre_order.append(vertexID)
        self.pre_orderID += 1
           
        v = directedg.get_vertex(vertexID)
        for succID in v.successors.keys ():
            if self.vertex_pre_order_numbering[succID] == 0:
                self.add_edge(vertexID, succID)
                self.do_search(directedg, succID)
            elif self.vertex_pre_order_numbering[vertexID] < self.vertex_pre_order_numbering[succID]:
                pass
            elif self.vertex_post_order_numbering[succID] == 0:
                self.back_edges.append((vertexID, succID))
        self.vertex_post_order_numbering[vertexID] = self.post_orderID
        self.post_order.append(vertexID)
        self.post_orderID += 1
    
    def getPreorderVertexID (self, preID):
        assert preID - 1 < len(self.pre_order), "Pre-order number %d too high" % preID
        return self.pre_order[preID-1]
    
    def getPostorderVertexID (self, postID):
        assert postID - 1 < len(self.post_order), "Post-order number %d too high" % postID
        return self.post_order[postID-1]
    
    def getPreID (self, vertexID):
        assert vertexID in self.vertex_pre_order_numbering, "Unable to find pre-order numbering for vertex %d" % vertexID
        return self.vertex_pre_order_numbering[vertexID]
    
    def getPostID (self, vertexID):
        assert vertexID in self.vertex_post_order_numbering, "Unable to find post-order numbering for vertex %d" % vertexID
        return self.vertex_post_order_numbering[vertexID]
    
    def isDFSBackedge(self, sourceID, destinationID):
        return (sourceID, destinationID) in self.back_edges
        
class Dominators(Tree):
    def __init__(self, directedg, rootID):
        Tree.__init__(self)
        assert rootID in directedg.the_vertices.keys(), "Unable to find vertex %d from which to initiate depth-first search" % rootID
        self.rootID = rootID
        self.immediate_dominator = {}
        self.initialise(rootID, directedg)
        self.solve(directedg)
        self.add_edges()
    
    def initialise(self, rootID, directedg):
        for v in directedg:           
            self.the_vertices[v.vertexID] = vertices.TreeVertex(v.vertexID)
            if v.vertexID == rootID:
                self.immediate_dominator[v.vertexID] = v.vertexID
            else:
                self.immediate_dominator[v.vertexID] = vertices.dummyID
    
    def solve(self, directedg):
        dfs = DepthFirstSearch(directedg, self.rootID)
        changed = True
        while changed:
            changed = False
            postID  = directedg.number_of_vertices()
            while postID >= 1:
                vertexID = dfs.getPostorderVertexID(postID)
                if vertexID != self.rootID:
                    v               = directedg.get_vertex(vertexID)
                    processedPredID = vertices.dummyID
                    newIdomID       = vertices.dummyID
                    for predID in v.predecessors.keys():
                        if self.immediate_dominator[predID] != vertices.dummyID:
                            processedPredID = predID
                            newIdomID       = processedPredID
                    for predID in v.predecessors.keys():
                        if predID != processedPredID:
                            if self.immediate_dominator[predID] != vertices.dummyID:
                                newIdomID = self.intersect(dfs, predID, newIdomID)
                    if newIdomID != vertices.dummyID:
                        if self.immediate_dominator[vertexID] != newIdomID:
                            changed = True
                            self.immediate_dominator[vertexID] = newIdomID
                postID -= 1
    
    def intersect(self, dfs, left, right):
        uID = left
        vID = right
        while (dfs.getPostID(uID) != dfs.getPostID(vID)):
            while (dfs.getPostID(uID) < dfs.getPostID(vID)):
                uID = self.immediate_dominator[uID]
            while (dfs.getPostID(vID) < dfs.getPostID(uID)):
                vID = self.immediate_dominator[vID]
        return uID
    
    def add_edges(self):
        for v in self:
            if v.vertexID != self.rootID:
                assert self.immediate_dominator[v.vertexID] != vertices.dummyID, "Immediate dominator of %d not set" % v.vertexID
                self.add_edge(self.immediate_dominator[v.vertexID], v.vertexID)
    
class CompressedDominatorTree(Tree):
    def __init__(self, dominator_tree, lca, vertexID, frontierIDs):
        Tree.__init__(self)
        self.build(lca, frontierIDs)
        self.rootID = dominator_tree.get_vertex(vertexID).parentID
        
    def build(self, lca, query_set):
        while len(query_set) > 1:
            vertex_to_LCA = {}
            for a_pair in itertools.combinations(query_set, 2):
                lcaID = lca.query(a_pair)
                if a_pair[0] in vertex_to_LCA:
                    old_lcaID = vertex_to_LCA[a_pair[0]]
                    if lca.vertex_level[lcaID] > lca.vertex_level[old_lcaID] and old_lcaID != a_pair[0]:
                        vertex_to_LCA[a_pair[0]] = lcaID
                else:
                    vertex_to_LCA[a_pair[0]] = lcaID
                if a_pair[1] in vertex_to_LCA:
                    old_lcaID = vertex_to_LCA[a_pair[1]]
                    if lca.vertex_level[lcaID] > lca.vertex_level[old_lcaID] and old_lcaID != a_pair[1]:
                        vertex_to_LCA[a_pair[1]] = lcaID
                else:
                    vertex_to_LCA[a_pair[1]] = lcaID
            # Add edge links                
            for vertexID, parentID in vertex_to_LCA.items():
                if not self.has_vertex(vertexID):
                    self.add_vertex(vertices.TreeVertex(vertexID))                
                if not self.has_vertex(parentID):
                    self.add_vertex(vertices.TreeVertex(parentID))
                if parentID != vertexID:
                    self.add_edge(parentID, vertexID)
            # Any vertex without a predecessor goes into the query set
            new_query_set = set()
            for v in self:
                if v.number_of_predecessors() == 0:
                    new_query_set.add(v.vertexID)
            query_set = new_query_set
    
class LeastCommonAncestor:
    def __init__(self, tree):
        self.tree           = tree
        self.euler_tour     = {}
        self.euler_index    = 0
        self.level          = {}
        self.vertex_level   = {}
        self.representative = {}
        self.compute()
        
    def compute(self):
        self.dummy_level = 0
        self.vertex_level[self.tree.rootID] = 0
        self.do_search(self.tree.rootID)
        self.dummy_level += 1
        self.compute_representatives()
        
    def do_search(self, vertexID):
        v = self.tree.get_vertex(vertexID)
        self.euler_tour[self.euler_index] = vertexID
        self.euler_index += 1
        for succID in v.successors.keys():
            self.vertex_level[succID] = self.vertex_level[vertexID] + 1
            if self.vertex_level[succID] > self.dummy_level:
                self.dummy_level = self.vertex_level[succID]
            self.do_search(succID)
            self.euler_tour[self.euler_index] = vertexID
            self.euler_index += 1
    
    def compute_representatives(self):
        for index, vertexID in self.euler_tour.iteritems():
            self.representative[vertexID] = index
            self.level[index]             = self.vertex_level[vertexID]
            
    def query(self, the_pair):    
        debug.debug_message("Computing lca(%s)" % (the_pair,), __name__, 1) 
        if isinstance(self.tree, LoopNests):
            the_pair = (self.tree.program_point_to_lnt_vertexID[the_pair[0]], 
                        self.tree.program_point_to_lnt_vertexID[the_pair[1]])
        lowest_level = self.dummy_level
        level_index  = 2 * self.tree.number_of_vertices()
        if self.representative[the_pair[0]] < self.representative[the_pair[1]]:
            startIndex = self.representative[the_pair[0]]
            endIndex   = self.representative[the_pair[1]]
        else:
            startIndex = self.representative[the_pair[1]]
            endIndex   = self.representative[the_pair[0]]
        for i in range(startIndex, endIndex+1):
            if self.level[i] < lowest_level:
                lowest_level = self.level[i]
                level_index  = i
        debug.debug_message("lca(%s) = %d" % (the_pair, self.euler_tour[level_index]), __name__, 15)
        return self.euler_tour[level_index]
    
class LoopNests(Tree):
    def __init__(self, transition_graph, rootID):
        assert isinstance(transition_graph, StateTransitionGraph)
        Tree.__init__(self)
        self.__transition_graph            = transition_graph
        self.__dfs                         = DepthFirstSearch(transition_graph, rootID)
        self.abstract_vertices             = {}
        self.loop_bodies_per_backedge      = {}
        self.loop_exit_edges_per_backedge  = {}
        self.loop_bodies_per_header        = {}
        self.loop_entry_edges_per_header   = {}
        self.loop_exit_edges_per_header    = {}
        self.inner_loop_headers_per_header = {}
        self.loop_tails                    = set()
        self.current_parent                = {}
        self.state_to_header               = {}
        self.program_point_to_lnt_vertexID = {}
        self.find_loops_of_states(rootID)
        self.find_loop_exits()
        self.find_loop_entries()
        self.add_vertices()
        self.add_edges()
        # Set the tree root ID to the header vertex representing the root of the 
        # directed graph
        self.rootID = self.abstract_vertices[rootID]
        self.check_is_tree()

    def find_loops_of_states(self, rootID):
        for v in self.__transition_graph:
            self.current_parent[v.vertexID] = v.vertexID
        predom_tree = Dominators(self.__transition_graph, rootID)
        for vertexID in reversed(self.__dfs.pre_order):
            v = self.__transition_graph.get_vertex(vertexID)
            for predID in v.predecessors.keys():
                if self.__dfs.isDFSBackedge(predID, vertexID):
                    assert predom_tree.is_ancestor(vertexID, predID), "Non-reducible loop found with DFS backedge %d => %d" % (predID, vertexID)
                    debug.debug_message("%s => %s is a loop-back edge of non-trivial loop" % (predID, vertexID), __name__, 15)
                    self.loop_bodies_per_backedge[(predID, vertexID)]     = set()
                    self.loop_exit_edges_per_backedge[(predID, vertexID)] = set()
                    self.create_header_information(vertexID)
                    self.find_loop_body(predID, vertexID)
                    self.loop_tails.add(predID)
                    
    def create_header_information(self, headerID):
        if headerID not in self.loop_bodies_per_header:  
            self.loop_bodies_per_header[headerID]        = set()
            self.loop_entry_edges_per_header[headerID]   = set()
            self.loop_exit_edges_per_header[headerID]    = set() 
            self.inner_loop_headers_per_header[headerID] = set()
            
    def find_loop_body(self, tailID, headerID):
        self.loop_bodies_per_backedge[(tailID, headerID)].add(headerID)
        work_list = []
        work_list.append(tailID)
        while work_list:
            listID = work_list.pop()
            self.loop_bodies_per_backedge[(tailID, headerID)].add(listID)
            v = self.__transition_graph.get_vertex(listID)
            for predID in v.predecessors.keys():
                if not self.__dfs.isDFSBackedge(predID, listID):
                    repID = self.current_parent[predID]
                    if repID not in work_list \
                    and repID not in self.loop_bodies_per_backedge[(tailID, headerID)] \
                    and repID != headerID:
                        work_list.append(repID)
        for vertexID in self.loop_bodies_per_backedge[(tailID, headerID)]:
            self.current_parent[vertexID] = headerID
            if vertexID not in self.state_to_header:
                self.state_to_header[vertexID] = headerID
            if vertexID in self.loop_bodies_per_header and vertexID != headerID:
                self.inner_loop_headers_per_header[headerID].add(vertexID)
        self.loop_bodies_per_backedge[(tailID, headerID)].difference_update(self.inner_loop_headers_per_header[headerID])
        self.loop_bodies_per_header[headerID].update(self.loop_bodies_per_backedge[(tailID, headerID)])
    
    def add_vertices(self):
        for statev in self.__transition_graph:
            self.the_vertices[statev.vertexID] = vertices.TreeVertex(statev.vertexID)
        for statev in self.__transition_graph:
            for succe in self.__transition_graph.get_vertex(statev.vertexID).successors.values():
                newID = self.get_next_vertexID()
                self.the_vertices[newID] = vertices.ProgramPoint(newID, succe.the_program_point)
                self.program_point_to_lnt_vertexID[succe.the_program_point] = newID
        for headerID in self.loop_bodies_per_header.keys():
            newID                            = self.get_next_vertexID()
            self.the_vertices[newID]         = vertices.HeaderVertex(newID, headerID)
            self.abstract_vertices[headerID] = newID  
    
    def add_edges(self):
        # Add edges between abstract vertices first
        for outer_headerID, inner_headerIDs in self.inner_loop_headers_per_header.iteritems():
            for inner_headerID in inner_headerIDs:
                outer_headerv = self.get_vertex(self.abstract_vertices[outer_headerID])
                inner_headerv = self.get_vertex(self.abstract_vertices[inner_headerID])
                self.add_edge(outer_headerv.vertexID, inner_headerv.vertexID)
        # Add edges between abstract vertices and states, and abstract vertices and transitions
        for headerID, loop_body in self.loop_bodies_per_header.iteritems():
            headerv = self.get_vertex(self.abstract_vertices[headerID])
            for stateID in loop_body:
                self.add_edge(headerv.vertexID, stateID)
                for succe in self.__transition_graph.get_vertex(stateID).successors.values():
                    transitionID = self.program_point_to_lnt_vertexID[succe.the_program_point]
                    if succe.vertexID in loop_body:
                        self.add_edge(headerv.vertexID, transitionID)
                    else:
                        other_headerv = self.get_vertex(self.abstract_vertices[self.state_to_header[succe.vertexID]])
                        if self.is_nested(headerv.vertexID, other_headerv.vertexID):
                            self.add_edge(other_headerv.vertexID, transitionID)
                        else:
                            self.add_edge(headerv.vertexID, transitionID)
                        
    def find_loop_exits(self):
        # Loop exits per body induced by a backedge
        for (tailID, headerID), loop_body in self.loop_bodies_per_backedge.iteritems():
            for vertexID in loop_body:
                v = self.__transition_graph.get_vertex(vertexID)
                for succID in v.successors.keys():
                    if succID not in loop_body:
                        if self.is_loop_header(vertexID) and headerID != vertexID:
                            if succID not in self.loop_bodies_per_header[vertexID]:
                                self.loop_exit_edges_per_backedge[(tailID, headerID)].add((vertexID, succID))
                        else:
                            self.loop_exit_edges_per_backedge[(tailID, headerID)].add((vertexID, succID))
        # Loop exits per body induced by all backedges with a common header
        for headerID, loop_body in self.loop_bodies_per_header.iteritems():
            for vertexID in loop_body:
                v = self.__transition_graph.get_vertex(vertexID)
                for succID in v.successors.keys():
                    if succID not in loop_body:
                        if self.is_loop_header(vertexID) and headerID != vertexID:
                            if succID not in self.loop_bodies_per_header[vertexID]:
                                self.loop_exit_edges_per_header[headerID].add((vertexID, succID))
                        else:
                            self.loop_exit_edges_per_header[headerID].add((vertexID, succID))
                            
    def find_loop_entries(self):
        for headerID in self.loop_bodies_per_header.keys():
            v = self.__transition_graph.get_vertex(headerID)
            for predID in v.predecessors.keys():
                if (predID, headerID) not in self.loop_bodies_per_backedge.keys():
                    self.loop_entry_edges_per_header[headerID].add((predID, headerID))
    
    def get_header_vertices(self):
        for abstractID in self.abstract_vertices.values():
            yield self.get_vertex(abstractID)
    
    def is_loop_header(self, vertexID):
        return vertexID in self.abstract_vertices.keys()
    
    def is_loop_tail(self, vertexID):
        return vertexID in self.loop_tails
    
    def get_backedges(self, headerID):
        return filter(lambda x: x[1] == headerID, self.loop_bodies_per_backedge.keys())
    
    def is_backedge(self, sourceID, destinationID):
        for backedge in self.loop_bodies_per_backedge.keys():
            if backedge == (sourceID, destinationID):
                return True
        return False
    
    def is_loop_entry_edge(self, sourceID, destinationID):
        for entry_edges in self.loop_entry_edges_per_header.values():
            if (sourceID, destinationID) in entry_edges:
                return True
        return False
    
    def is_loop_exit_edge_of_inner_loop(self, headerID, sourceID, destinationID):
        headerv = self.get_vertex(self.get_vertex(headerID).parentID)
        for succID in headerv.successors.keys():
            succv = self.get_vertex(succID)
            if isinstance(succv, vertices.HeaderVertex):  
                if (sourceID, destinationID) in self.loop_exit_edges_per_header[succv.headerID]:
                    return True
        return False       

    def is_nested(self, left, right):
        return self.is_proper_ancestor(right, left)
    
    def add_edge_vertices_to_enhanced_CFG(self, enhanced_CFG, headerID, tailIDs):
        headerv  = self.get_vertex(self.abstract_vertices[headerID])
        worklist = []
        analysed = set()
        # Start work list with vertices on the frontier of the analysis
        for vertexID in tailIDs:
            v = self.__transition_graph.get_vertex(vertexID)
            worklist.append(v)
        # Work backwards through flow graph until the header is reached, adding edges found along the way
        while worklist:                        
            v = worklist.pop()
            if v.vertexID not in analysed:
                analysed.add(v.vertexID)
                for predID in v.predecessors.keys():
                    predv = self.__transition_graph.get_vertex(predID)
                    if not self.__dfs.isDFSBackedge(predID, v.vertexID):
                        pred_headerv = self.get_vertex(self.get_vertex(predID).parentID)
                        if pred_headerv.headerID == headerID:
                            edge_vertexID = enhanced_CFG.get_next_edge_vertexID()
                            new_edgev     = vertices.CFGEdge(edge_vertexID, predID, v.vertexID)
                            enhanced_CFG.add_vertex(new_edgev)
                            worklist.append(predv)
                        elif self.is_nested(pred_headerv.vertexID, headerv.vertexID):
                            worklist.append(self.__transition_graph.get_vertex(pred_headerv.headerID))
        return analysed 
                            
    def add_edge_vertices_for_loop_tails(self, enhanced_CFG, headerID, tailIDs):
        for tailID in tailIDs:
            edge_vertexID = enhanced_CFG.get_next_edge_vertexID()
            new_edgev     = vertices.CFGEdge(edge_vertexID, tailID, headerID)
            enhanced_CFG.add_vertex(new_edgev) 
            
    def add_edge_vertices_for_loop_exits(self, headerv, enhanced_CFG):
        # Add edge vertices to model loop-exit edges out of this loop
        for sourceID, destinationID in self.loop_exit_edges[headerv.headerID]:
            edge_vertexID = enhanced_CFG.get_next_edge_vertexID()
            new_edgev     = vertices.CFGEdge(edge_vertexID, sourceID, destinationID)
            enhanced_CFG.add_vertex(new_edgev)
            
    def add_edge_vertices_for_inner_loop_exits(self, enhanced_CFG, headerID, analysed_vertices):
        # Add edge vertices to model loop-exit edges out of inner loops
        inner_loop_exit_edges = {}
        headerv = self.get_vertex(self.abstract_vertices[headerID])
        for succID in headerv.successors.keys():
            succv = self.get_vertex(succID)
            if isinstance(succv, vertices.HeaderVertex) and succv.headerID in analysed_vertices:
                for sourceID, destinationID in self.loop_exit_edges_per_header[succv.headerID]:
                    edge_vertexID = enhanced_CFG.get_next_edge_vertexID()
                    new_edgev     = vertices.CFGEdge(edge_vertexID, sourceID, destinationID)
                    enhanced_CFG.add_vertex(new_edgev)
                    inner_loop_exit_edges[new_edgev] = succv.headerID
        return inner_loop_exit_edges
    
    def add_vertices_to_enhanced_CFG(self, enhanced_CFG, headerID):
        headerv = self.get_vertex(self.abstract_vertices[headerID])
        # Add basic blocks and abstract loop vertices 
        for v in enhanced_CFG:
            assert isinstance(v, vertices.CFGEdge)
            sourceID            = v.edge[0]
            destinationID       = v.edge[1]
            source_headerv      = self.get_vertex(self.get_vertex(sourceID).parentID)
            destination_headerv = self.get_vertex(self.get_vertex(destinationID).parentID)
            if source_headerv == headerv:
                if not enhanced_CFG.has_vertex(sourceID):
                    enhanced_CFG.add_vertex(vertices.CFGVertex(sourceID))
            if destination_headerv == headerv:
                if not enhanced_CFG.has_vertex(destinationID):
                    enhanced_CFG.add_vertex(vertices.CFGVertex(destinationID))
            elif self.is_loop_header(destinationID):
                if not enhanced_CFG.has_vertex(destination_headerv.vertexID):
                    enhanced_CFG.add_vertex(vertices.HeaderVertex(destination_headerv.vertexID, destination_headerv.headerID))

    def add_edges_to_enhanced_CFG(self, enhanced_CFG, headerID, backedges, are_backedges_terminal, inner_loop_exit_edges):
        # Link edges
        for v in enhanced_CFG:
            if isinstance(v, vertices.CFGEdge):
                sourceID      = v.edge[0]
                destinationID = v.edge[1] 
                if self.is_loop_exit_edge_of_inner_loop(headerID, sourceID, destinationID):
                    # Loop-exit edge of inner loop  
                    inner_headerID = inner_loop_exit_edges[v]   
                    inner_headerv  = self.get_vertex(self.get_vertex(inner_headerID).parentID)                       
                    enhanced_CFG.add_edge(inner_headerv.vertexID, v.vertexID)
                    enhanced_CFG.add_edge(v.vertexID, destinationID)
                elif self.is_loop_entry_edge(sourceID, destinationID):
                    inner_headerv = self.get_vertex(self.get_vertex(destinationID).parentID)
                    enhanced_CFG.add_edge(sourceID, v.vertexID)
                    enhanced_CFG.add_edge(v.vertexID, inner_headerv.vertexID)
                else:
                    if (sourceID, destinationID) in backedges:
                        if are_backedges_terminal:
                            enhanced_CFG.add_edge(sourceID, v.vertexID)
                        else:
                            enhanced_CFG.add_edge(v.vertexID, destinationID)
                    else:
                        enhanced_CFG.add_edge(sourceID, v.vertexID)
                        enhanced_CFG.add_edge(v.vertexID, destinationID)

    def set_entry_and_exit_in_enhanced_CFG(self, enhanced_CFG, headerID):
        # Gather entry and exit candidates
        entry_candidates = []
        exit_candidates  = []
        for v in enhanced_CFG:
            if v.number_of_predecessors() == 0:
                entry_candidates.append(v)
            if v.number_of_successors() == 0:
                exit_candidates.append(v)
        # Set entry vertex
        if len(entry_candidates) == 1:
            enhanced_CFG.entryID = entry_candidates[0].vertexID
        else:
            enhanced_CFG.entryID = enhanced_CFG.get_next_vertexID()
            new_entryv           = vertices.CFGVertex(enhanced_CFG.entryID)
            new_entryv.dummy     = True
            enhanced_CFG.add_vertex(new_entryv)
            for entryv in entry_candidates:
                enhanced_CFG.add_edge(enhanced_CFG.entryID, entryv.vertexID)
        # Set exit vertex
        if len(exit_candidates) == 1:
            enhanced_CFG.exitID = exit_candidates[0].vertexID
        else:
            enhanced_CFG.exitID = enhanced_CFG.get_next_vertexID()
            new_exitv           = vertices.CFGVertex(enhanced_CFG.exitID)
            new_exitv.dummy     = True
            enhanced_CFG.add_vertex(new_exitv)
            for exitv in exit_candidates:
                enhanced_CFG.add_edge(exitv.vertexID, enhanced_CFG.exitID)
                
    def induce_subgraph_for_backedges(self, backedges):
        tailIDs   = [backedge[0] for backedge in backedges]
        headerIDs = [backedge[1] for backedge in backedges]
        assert len(set(headerIDs)) == 1
        headerID     = headerIDs[0]
        enhanced_CFG = EnhancedCFG()
        analysed_vertices = self.add_edge_vertices_to_enhanced_CFG(enhanced_CFG, headerID, tailIDs)
        self.add_edge_vertices_for_loop_tails(enhanced_CFG, headerID, tailIDs)
        inner_loop_exit_edges = self.add_edge_vertices_for_inner_loop_exits(enhanced_CFG, headerID, analysed_vertices)
        self.add_vertices_to_enhanced_CFG(enhanced_CFG, headerID)
        self.add_edges_to_enhanced_CFG(enhanced_CFG, headerID, backedges, True, inner_loop_exit_edges)
        self.set_entry_and_exit_in_enhanced_CFG(enhanced_CFG, headerID)    
        return enhanced_CFG
    
    def induce_subgraph_for_tail(self, tailID):
        backedges = [an_edge for an_edge in self.loop_bodies_per_backedge.keys() if an_edge[0] == tailID]
        headerIDs = [backedge[1] for backedge in backedges]
        assert len(set(headerIDs)) == 1
        headerID  = headerIDs[0]
        tailIDs   = []
        tailIDs.append(tailID)
        enhanced_CFG = EnhancedCFG()
        analysed_vertices = self.add_edge_vertices_to_enhanced_CFG(enhanced_CFG, headerID, tailIDs)
        self.add_edge_vertices_for_loop_tails(enhanced_CFG, headerID, tailIDs)
        inner_loop_exit_edges = self.add_edge_vertices_for_inner_loop_exits(enhanced_CFG, headerID, analysed_vertices)
        self.add_vertices_to_enhanced_CFG(enhanced_CFG, headerID)
        self.add_edges_to_enhanced_CFG(enhanced_CFG, headerID, backedges, False, inner_loop_exit_edges)
        self.set_entry_and_exit_in_enhanced_CFG(enhanced_CFG, headerID)    
        return enhanced_CFG

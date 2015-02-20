from __future__ import print_function

import directed_graphs
import vertices
import udraw
import debug

class DominanceFrontiers:
    def __init__(self, cfg, dominator_tree):
        self.vertex_DF = {}
        for v in dominator_tree:
            self.vertex_DF[v.vertexID] = set()
        self.compute(cfg, dominator_tree)
        
    def compute(self, cfg, dominator_tree):
        for v in cfg:
            if v.number_of_predecessors() > 1: 
                immediate_dominatorID = dominator_tree.get_vertex(v.vertexID).parentID
                for predID in v.predecessors.keys():
                    runnerID = predID
                    while runnerID != immediate_dominatorID:
                        self.vertex_DF[runnerID].add(v.vertexID)
                        runnerID = dominator_tree.get_vertex(runnerID).parentID

class AcyclicReducibility:
    def __init__(self, cfg, predominator_tree):
        self.irreducible_branches = set()
        self.compute(cfg, predominator_tree)
    
    def compute(self, cfg, predominator_tree):
        predominance_frontier_info = DominanceFrontiers(cfg, predominator_tree)
        for v in cfg:
            if v.number_of_successors() > 1:
                if len(predominance_frontier_info.vertex_DF[v.vertexID]) > 1:
                    self.irreducible_branches.add(v.vertexID)
            
class PathExpression(directed_graphs.DirectedGraph):
    next_vertexID    = 0
    loop_expressions = {}
    
    def __init__(self, transition_graph, transition_graph_lnt, transition_graph_induced, is_loop_body=False):   
        directed_graphs.DirectedGraph.__init__(self)
        if transition_graph_induced.number_of_vertices() > 0:
            self.transition_graph         = transition_graph
            self.transition_graph_lnt     = transition_graph_lnt
            self.transition_graph_induced = transition_graph_induced
            self.predominator_tree        = directed_graphs.Dominators(transition_graph_induced)
            self.predominator_tree.augment_with_program_point_edges()
            self.acyclic_reducible_info   = AcyclicReducibility(transition_graph_induced, self.predominator_tree)
            self.reverse_transitiong      = transition_graph_induced.get_reverse_graph()
            self.postdominator_tree       = directed_graphs.Dominators(self.reverse_transitiong)
            self.reg_exp_trees = {}
            self.dfs           = directed_graphs.DepthFirstSearch(self.transition_graph_induced, self.transition_graph_induced.get_entryID())
            self.the_order     = self.dfs.post_order
            if is_loop_body:
                self.the_order.insert(0, self.transition_graph_induced.get_entryID())
            self.compute()
            self.set_rootID()
            if is_loop_body:
                self.transform_into_loop_expression()
            self.set_edgeIDs()
            if is_loop_body:
                PathExpression.loop_expressions[transition_graph_induced.get_entryID()] = self
            
    def get_next_vertexID(self):
        PathExpression.next_vertexID += 1
        return PathExpression.next_vertexID
    
    def transform_into_loop_expression(self):
        newID = self.get_next_vertexID()
        newv  = vertices.RegExpVertex(newID, vertices.RegExpVertex.FOR_LOOP)
        self.add_vertex(newv)
        self.add_edge(newID, self.rootID)
        self.rootID = newID
        
    def create_program_point_vertex(self, the_program_point):
        newID = self.get_next_vertexID()
        newv  = vertices.ProgramPoint(newID, the_program_point)
        self.add_vertex(newv)
        return newv
    
    def create_sequence_vertex(self):
        newID = self.get_next_vertexID()
        newv  = vertices.RegExpVertex(newID, vertices.RegExpVertex.SEQUENCE)
        self.add_vertex(newv)
        return newv
    
    def create_alternative_vertex(self):
        newID = self.get_next_vertexID()
        newv  = vertices.RegExpVertex(newID, vertices.RegExpVertex.ALTERNATIVE)
        self.add_vertex(newv)
        return newv
        
    def compute(self):
        for index, vertexID_transitiong in enumerate(reversed(self.the_order)):
            if index == 0:
                # Special handling of the first vertex in the depth-first search post order 
                seqv = self.create_sequence_vertex()
                self.rootID = seqv.vertexID
                self.reg_exp_trees[vertexID_transitiong] = seqv
            else:
                v_transitiong = self.transition_graph_induced.get_vertex(vertexID_transitiong)
                for predID_transitiong, prede_transitiong in v_transitiong.predecessors.iteritems():
                    predv_transitiong  = self.transition_graph_induced.get_vertex(predID_transitiong)
                    if predv_transitiong.number_of_successors() > 1:
                        seqv = self.create_sequence_vertex()
                        self.reg_exp_trees[prede_transitiong.edgeID] = seqv
                        if predID_transitiong in self.acyclic_reducible_info.irreducible_branches:
                            self.add_edge(seqv.vertexID, self.reg_exp_trees[predID_transitiong].vertexID)
                    else:
                        self.reg_exp_trees[prede_transitiong.edgeID] = self.reg_exp_trees[predID_transitiong]
                        
                    leafv   = self.create_program_point_vertex(prede_transitiong.the_program_point)
                    parentv = self.reg_exp_trees[prede_transitiong.edgeID]
                    self.add_edge(parentv.vertexID, leafv.vertexID)
                
                if v_transitiong.number_of_predecessors() > 1:
                    self.reg_exp_trees[vertexID_transitiong] = self.handle_merge(v_transitiong)
                else:
                    self.reg_exp_trees[vertexID_transitiong] = self.reg_exp_trees[prede_transitiong.edgeID]
                
                if v_transitiong.loop_header:
                    v_header = self.transition_graph_lnt.get_vertex(self.transition_graph_lnt.get_vertex(vertexID_transitiong).parentID)
                
                    if v_header.headerID not in PathExpression.loop_expressions:
                        # Create path expression for inner loop iteration space on the fly
                        iteration_subgraph = create_induced_subgraph_for_loop_iteration_space(self.transition_graph, 
                                                                                              self.transition_graph_lnt, 
                                                                                              v_header)
                    
                        iteration_subgraph_path_expression = PathExpression(self.transition_graph,
                                                                            self.transition_graph_lnt,
                                                                            iteration_subgraph,
                                                                            True)
                        
                        PathExpression.loop_expressions[v_header.headerID] = iteration_subgraph_path_expression
                    
                    self.union_subgraph(PathExpression.loop_expressions[v_header.headerID])
                        
                    self.add_edge(self.reg_exp_trees[vertexID_transitiong].vertexID, 
                                  PathExpression.loop_expressions[v_header.headerID].rootID)
                
                
    def handle_merge(self, mergev):
        predominator_tree_compressed = directed_graphs.CompressedDominatorTree(self.predominator_tree, 
                                                                               self.predominator_tree.get_LCA(), 
                                                                               mergev)
        
        local_reg_exp_trees = {}
        for the_vertices in predominator_tree_compressed.level_by_level_iterator(True):
            for treev in the_vertices:
                if treev.number_of_successors() == 0:
                    local_reg_exp_trees[treev.edgeID] = self.reg_exp_trees[treev.edgeID]
                else:
                    altv = self.create_alternative_vertex()
                    for succID in treev.successors.keys():
                        succv = predominator_tree_compressed.get_vertex(succID)
                        if isinstance(succv, vertices.ProgramPoint):
                            self.add_edge(altv.vertexID, local_reg_exp_trees[succv.edgeID].vertexID)
                        else:
                            self.add_edge(altv.vertexID, local_reg_exp_trees[succID].vertexID)
                    if treev.vertexID != predominator_tree_compressed.rootID \
                    or self.postdominator_tree.get_vertex(predominator_tree_compressed.rootID).parentID == mergev.vertexID:
                        seqv = self.reg_exp_trees[treev.vertexID]
                    else:
                        seqv = self.create_sequence_vertex()
                    self.add_edge(seqv.vertexID, altv.vertexID)
                    local_reg_exp_trees[treev.vertexID] = seqv
        return local_reg_exp_trees[predominator_tree_compressed.rootID]
    
    def set_rootID(self):
        without_predecessors = []
        for v in self:
            if v.number_of_predecessors() == 0:
                without_predecessors.append(v.vertexID)
        if len(without_predecessors) > 1:
            debug.exit_message("Too many roots found: %s" % ','.join(str(vertexID) for vertexID in without_predecessors))
        elif len(without_predecessors) == 0:
            debug.exit_message("No root found")
        else:
            self.rootID = without_predecessors[0]
            
    def union_subgraph(self, subgraph):
        for v in subgraph:
            if not self.has_vertex(v.vertexID):
                self.add_vertex(v)
            
    def get_textual_format(self):
        if self.number_of_vertices() == 0:
            return "{}"
        else:
            temporary_expressions = {}
            dfs = directed_graphs.DepthFirstSearch(self, self.rootID)
            for vertexID in dfs.post_order:
                v = self.get_vertex(vertexID)
                if isinstance(v, vertices.RegExpVertex):
                    the_text = ""
                    if v.operator == vertices.RegExpVertex.ALTERNATIVE \
                    or v.operator == vertices.RegExpVertex.FOR_LOOP:
                        the_text += "["
                    
                    for succe in v.successors.values():
                        succv = self.get_vertex(succe.vertexID)
                        if isinstance(succv, vertices.ProgramPoint):
                            if len(succv.the_program_point) == 1:
                                the_text += str(succv.the_program_point[0])
                            else:
                                the_text += str(succv.the_program_point)
                        else:
                            the_text += temporary_expressions[succe.edgeID]
                        if succe.vertexID != v.successors.keys()[v.number_of_successors()-1]:
                            the_text += v.operator
                    
                    if v.operator == vertices.RegExpVertex.ALTERNATIVE:
                        the_text += "]"
                    elif v.operator == vertices.RegExpVertex.FOR_LOOP:
                        the_text += "]*"
                        
                    for predID in v.predecessors.keys():
                        predv = self.get_vertex(predID)
                        succe = predv.get_successor_edge(vertexID)
                        temporary_expressions[succe.edgeID] = the_text
                    if vertexID == self.rootID:
                        temporary_expressions[vertexID] = the_text
            return temporary_expressions[vertexID]
    
def create_induced_subgraph_for_loop_iteration_space(transition_graph,
                                                     transition_graph_lnt,
                                                     v_header):
    edges   = []
    visited = set()
    stack   = []
    stack.append(v_header.headerID)
    while stack:
        v_queueID = stack.pop()
        v_queue   = transition_graph.get_vertex(v_queueID)
        visited.add(v_queueID)
        for e_pred in v_queue.predecessors.values():
            v_header_pred = transition_graph_lnt.get_vertex(transition_graph_lnt.get_vertex(e_pred.vertexID).parentID)
            if v_header_pred.headerID == v_header.headerID:
                edges.append((e_pred.vertexID, v_queueID, e_pred.the_program_point))
                if e_pred.vertexID not in visited:
                    stack.append(e_pred.vertexID)
            elif transition_graph_lnt.is_nested(v_header_pred.vertexID, v_header.vertexID) \
            and not transition_graph_lnt.is_backedge(e_pred.vertexID, v_queueID):
                edges.append((e_pred.vertexID, v_queueID, e_pred.the_program_point))
                if e_pred.vertexID not in visited:
                    stack.append(e_pred.vertexID)
                    
    # At this point, the edges we need to add to the induced subgraph have been worked out
    transition_graph_induced = directed_graphs.StateTransitionGraph()
    for an_edge in edges:
        if not transition_graph_induced.has_vertex(an_edge[0]):
            transition_graph_induced.add_vertex(vertices.Vertex(an_edge[0]))
        if not transition_graph_induced.has_vertex(an_edge[1]):
            transition_graph_induced.add_vertex(vertices.Vertex(an_edge[1]))
        if transition_graph_lnt.is_loop_header(an_edge[0]):
            transition_graph_induced.get_vertex(an_edge[0]).loop_header = True
        if transition_graph_lnt.is_loop_header(an_edge[1]):
            transition_graph_induced.get_vertex(an_edge[1]).loop_header = True
        transition_graph_induced.add_edge(an_edge[0], an_edge[1], an_edge[2], False)
        
    transition_graph_induced.set_edgeIDs()
    transition_graph_induced.entryID = v_header.headerID
    transition_graph_induced.exitID  = v_header.headerID    
    return transition_graph_induced

def induce_subgraph_from_source_to_destination(transition_graph,
                                               transition_graph_lnt,
                                               transition_graph_induced,
                                               source_stateID,
                                               destination_stateID):
    edges    = []
    enqueued = set()
    queue    = []
    
    queue.append(source_stateID)
    while queue:
        v_queueID = queue.pop(0)
        v_queue   = transition_graph.get_vertex(v_queueID)
        for e_succ in v_queue.successors.values():
            if transition_graph.is_reachable(e_succ.vertexID, {destination_stateID})\
            and v_queueID != destination_stateID \
            and not transition_graph_lnt.is_backedge(v_queueID, e_succ.vertexID):
                edges.append((v_queueID, e_succ.vertexID, e_succ.the_program_point))
                if e_succ.vertexID not in enqueued:
                    queue.append(e_succ.vertexID)
                    enqueued.add(e_succ.vertexID)
    
    no_predecessors = set()
    real_vertexID_to_new_vertexID = {}
    for an_edge in edges:
        if an_edge[0] not in real_vertexID_to_new_vertexID:
            new_vertexID = transition_graph_induced.get_next_vertexID()
            newv         = vertices.Vertex(new_vertexID, an_edge[0])
            transition_graph_induced.add_vertex(newv)
            real_vertexID_to_new_vertexID[an_edge[0]] = new_vertexID
            no_predecessors.add(new_vertexID)
        if an_edge[1] not in real_vertexID_to_new_vertexID:
            new_vertexID = transition_graph_induced.get_next_vertexID()
            newv         = vertices.Vertex(new_vertexID, an_edge[1])
            transition_graph_induced.add_vertex(newv)
            real_vertexID_to_new_vertexID[an_edge[1]] = new_vertexID
            no_predecessors.add(new_vertexID)
                        
    for an_edge in edges:
        transition_graph_induced.add_edge(real_vertexID_to_new_vertexID[an_edge[0]], 
                                          real_vertexID_to_new_vertexID[an_edge[1]], 
                                          an_edge[2], 
                                          False)
        try:
            no_predecessors.remove(real_vertexID_to_new_vertexID[an_edge[1]])
        except KeyError:
            pass
    
    assert len(no_predecessors) == 1
    return list(no_predecessors)[0]
        
def induce_subgraph_from_source_to_outermost_header(transition_graph,
                                                    transition_graph_lnt,
                                                    transition_graph_induced,
                                                    query_pair):
    edges            = []
    enqueued         = set()
    queue            = [] 
    start_stateID    = transition_graph.program_point_to_predecessor_stateID[query_pair[0]]
    v_current_header = transition_graph_lnt.get_vertex(transition_graph_lnt.get_vertex(start_stateID).parentID)

    queue.append(start_stateID)
    while queue:
        v_queueID = queue.pop(0)
        v_queue   = transition_graph.get_vertex(v_queueID)
        if transition_graph_lnt.is_loop_header(v_queueID)\
        and v_queueID != transition_graph.get_entryID():
            v_current_header = transition_graph_lnt.get_vertex(transition_graph_lnt.get_vertex(v_current_header.vertexID).parentID)
        if v_current_header.vertexID != transition_graph_lnt.rootID:
            for e_succ in v_queue.successors.values():
                if transition_graph.header_is_reachable(e_succ.vertexID, v_current_header.headerID):
                    edges.append((v_queueID, e_succ.vertexID, e_succ.the_program_point))
                    if e_succ.vertexID not in enqueued:
                        queue.append(e_succ.vertexID)
                        enqueued.add(e_succ.vertexID)
    
    no_successors = set()
    real_vertexID_to_new_vertexID = {}
    for an_edge in edges:
        if an_edge[0] not in real_vertexID_to_new_vertexID:
            new_vertexID = transition_graph_induced.get_next_vertexID()
            newv         = vertices.Vertex(new_vertexID, an_edge[0])
            transition_graph_induced.add_vertex(newv)
            real_vertexID_to_new_vertexID[an_edge[0]] = new_vertexID
            no_successors.add(new_vertexID)
        if an_edge[1] not in real_vertexID_to_new_vertexID:
            new_vertexID = transition_graph_induced.get_next_vertexID()
            newv         = vertices.Vertex(new_vertexID, an_edge[1])
            transition_graph_induced.add_vertex(newv)
            real_vertexID_to_new_vertexID[an_edge[1]] = new_vertexID
            no_successors.add(new_vertexID)
                        
    for an_edge in edges:
        transition_graph_induced.add_edge(real_vertexID_to_new_vertexID[an_edge[0]], 
                                          real_vertexID_to_new_vertexID[an_edge[1]], 
                                          an_edge[2], 
                                          False)
        try:
            no_successors.remove(real_vertexID_to_new_vertexID[an_edge[0]])
        except KeyError:
            pass
        
    assert len(no_successors) == 1
    return list(no_successors)[0]

def create_path_expression(cfg, query_pair):
    transition_graph         = cfg.get_transition_graph()
    transition_graph_lnt     = transition_graph.get_LNT()
    v_lnt_source             = transition_graph_lnt.get_vertex(transition_graph_lnt.program_point_to_lnt_vertexID[query_pair[0]])
    source_stateID           = transition_graph.program_point_to_predecessor_stateID[query_pair[0]]
    destination_stateID      = transition_graph.program_point_to_successor_stateID[query_pair[1]]
    transition_graph_induced = directed_graphs.StateTransitionGraph()
    
    if transition_graph.is_reachable(source_stateID, {destination_stateID}):
        if v_lnt_source.parentID == transition_graph_lnt.rootID:
            induce_subgraph_from_source_to_destination(transition_graph, 
                                                       transition_graph_lnt,
                                                       transition_graph_induced,
                                                       source_stateID,
                                                       destination_stateID)
        else:
            v_pred_stateID = induce_subgraph_from_source_to_outermost_header(transition_graph,
                                                                             transition_graph_lnt,
                                                                             transition_graph_induced,
                                                                             query_pair)
            
            v_succ_stateID = induce_subgraph_from_source_to_destination(transition_graph,
                                                                        transition_graph_lnt,
                                                                        transition_graph_induced,
                                                                        transition_graph_lnt.get_vertex(v_lnt_source.parentID).headerID,
                                                                        destination_stateID)
            
            transition_graph_induced.add_edge(v_pred_stateID, v_succ_stateID, (), False)
            
        transition_graph_induced.set_entry_and_exit() 
        transition_graph_induced.set_edgeIDs()
        
    udraw_suffix = "%s-%s" % (('_'.join(map(str, query_pair[0]))), ('_'.join(map(str, query_pair[1]))))
    
    udraw.make_file(transition_graph_induced, 
                    "%s.transition.%s" % (cfg.name, udraw_suffix)) 
    
    expressiont = PathExpression(transition_graph,
                                 transition_graph_lnt,
                                 transition_graph_induced)
    
    udraw.make_file(expressiont, 
                    "%s.re.%s" % (cfg.name, udraw_suffix))  
    
    print("P(%s) = %s" % (udraw_suffix, expressiont.get_textual_format()))
    
def create_path_expression_for_all_loops(cfg):
    print("%s CFG: %s %s" % ('*' * 10, cfg.name, '*' * 10))
    transition_graph     = cfg.get_transition_graph()
    transition_graph_lnt = transition_graph.get_LNT()
    for the_vertices in transition_graph_lnt.level_by_level_iterator(True):
        for v_header in [v_tree for v_tree in the_vertices if isinstance(v_tree, vertices.HeaderVertex)]:
            v_state       = transition_graph.get_vertex(v_header.headerID)
            e_succ        = v_state.successors.values()[0]
            real_headerID = e_succ.the_program_point[0]
            
            iteration_subgraph = create_induced_subgraph_for_loop_iteration_space(transition_graph, 
                                                                                  transition_graph_lnt,
                                                                                  v_header)
            
            udraw.make_file(iteration_subgraph, "%s.induced.L_%d" % (cfg.name, real_headerID))
            
            iteration_subgraph_path_expression = PathExpression(transition_graph,
                                                                transition_graph_lnt,
                                                                iteration_subgraph,
                                                                v_header.vertexID != transition_graph_lnt.rootID)
            
            print("P(L_%d)::\n%s\n" % (real_headerID, iteration_subgraph_path_expression.get_textual_format()))
        
            udraw.make_file(iteration_subgraph_path_expression, "%s.re.L_%d" % (cfg.name, real_headerID))
            
            
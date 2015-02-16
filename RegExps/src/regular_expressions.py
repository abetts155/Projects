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
    
    def __init__(self, transition_graph, transition_graph_lnt, transition_graph_induced):   
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
            self.compute()
            self.set_edgeIDs()
            self.set_rootID()
            
    def get_next_vertexID(self):
        PathExpression.next_vertexID += 1
        return PathExpression.next_vertexID
    
    def transform_into_for_loop(self):
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
        dfs = directed_graphs.DepthFirstSearch(self.transition_graph_induced, self.transition_graph_induced.get_entryID())
        for vertexID_transitiong in reversed(dfs.post_order):
            if vertexID_transitiong == self.transition_graph_induced.entryID:
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
                        
                    if self.transition_graph_lnt.is_loop_header(prede_transitiong.the_program_point):
                        v_header = self.transition_graph_lnt.get_vertex(self.transition_graph_lnt.get_vertex(prede_transitiong.the_program_point).parentID)
                    
                        if v_header.headerID not in PathExpression.loop_expressions:
                            # Create path expressions for inner loop iteration and exit spaces on the fly
                            iteration_subgraph, exit_subgraph = create_induced_subgraphs_for_non_dummy_loop(self.transition_graph, 
                                                                                                            self.transition_graph_lnt, 
                                                                                                            v_header)
                        
                            iteration_subgraph_path_expression = PathExpression(self.transition_graph,
                                                                                self.transition_graph_lnt,
                                                                                iteration_subgraph)
                            iteration_subgraph_path_expression.transform_into_for_loop()
                            
                            exit_subgraph_path_expression = PathExpression(self.transition_graph,
                                                                           self.transition_graph_lnt,
                                                                           exit_subgraph)
                            
                            PathExpression.loop_expressions[v_header.headerID] = (iteration_subgraph_path_expression, exit_subgraph_path_expression)
                        
                        iteration_subgraph_path_expression, exit_subgraph_path_expression = PathExpression.loop_expressions[v_header.headerID]
                        self.union_subgraph(iteration_subgraph_path_expression)
                        self.union_subgraph(exit_subgraph_path_expression)
                        
                        self.add_edge(self.reg_exp_trees[prede_transitiong.edgeID].vertexID, iteration_subgraph_path_expression.rootID)
                        self.add_edge(self.reg_exp_trees[prede_transitiong.edgeID].vertexID, exit_subgraph_path_expression.rootID)
                    else:
                        leafv   = self.create_program_point_vertex(prede_transitiong.the_program_point)
                        parentv = self.reg_exp_trees[prede_transitiong.edgeID]
                        self.add_edge(parentv.vertexID, leafv.vertexID)
                
                if v_transitiong.number_of_predecessors() > 1:
                    self.reg_exp_trees[vertexID_transitiong] = self.handle_merge(v_transitiong)
                else:
                    self.reg_exp_trees[vertexID_transitiong] = self.reg_exp_trees[prede_transitiong.edgeID]
                    
                    
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
            self.add_vertex(v)
            
    def get_textual_format(self):
        if self.number_of_vertices() == 0:
            return PathExpression.EMPTY_SET
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
        
def create_induced_subgraphs_for_non_dummy_loop(transition_graph,
                                                transition_graph_lnt,
                                                v_header):
    return (create_induced_subgraph_for_loop_iteration_space(transition_graph, 
                                                             transition_graph_lnt, 
                                                             v_header),
            create_induced_subgraph_for_loop_exit_space(transition_graph, 
                                                        transition_graph_lnt, 
                                                        v_header))
    
def create_induced_subgraph_for_loop_iteration_space(transition_graph,
                                                     transition_graph_lnt,
                                                     v_header):
    loop_tailIDs = transition_graph_lnt.get_loop_tails(v_header.headerID)
    transition_graph_induced = create_induced_subgraph_for_loop(transition_graph, 
                                                                transition_graph_lnt, 
                                                                v_header,
                                                                loop_tailIDs)
    
    v_unrolled_headerID = transition_graph_induced.get_next_vertexID()
    transition_graph_induced.add_vertex(vertices.Vertex(v_unrolled_headerID))
    
    for tailID in loop_tailIDs:
        v_tail = transition_graph.get_vertex(tailID)
        for e_succ in v_tail.successors.values():
            if e_succ.vertexID == v_header.headerID:
                transition_graph_induced.add_edge(tailID, v_unrolled_headerID, e_succ.the_program_point, False)
        
    transition_graph_induced.set_edgeIDs()
    transition_graph_induced.set_entry_and_exit()
    
    return transition_graph_induced

def create_induced_subgraph_for_loop_exit_space(transition_graph,
                                                transition_graph_lnt,
                                                v_header):
    transition_graph_induced = create_induced_subgraph_for_loop(transition_graph, 
                                                                transition_graph_lnt, 
                                                                v_header,
                                                                transition_graph_lnt.get_loop_exit_sources(v_header.headerID))
    
    transition_graph_induced.set_edgeIDs()
    transition_graph_induced.set_entry_and_exit()
    
    return transition_graph_induced

def create_induced_subgraph_for_loop(transition_graph,
                                     transition_graph_lnt,
                                     v_header,
                                     frontier_vertexIDs):
    edges   = set()
    visited = set()
    stack   = []
    stack.extend(frontier_vertexIDs)
    while stack:
        v_stackID = stack.pop()
        v_stack   = transition_graph.get_vertex(v_stackID)
        visited.add(v_stackID)
        for e_pred in v_stack.predecessors.values():
            if not transition_graph_lnt.is_backedge(e_pred.vertexID, v_stackID):
                v_header_pred = transition_graph_lnt.get_vertex(transition_graph_lnt.get_vertex(e_pred.vertexID).parentID)
                if v_header_pred.headerID == v_header.headerID:
                    edges.add((e_pred.vertexID, v_stackID, e_pred.the_program_point))
                    if e_pred.vertexID not in visited:
                        stack.append(e_pred.vertexID)
                elif transition_graph_lnt.is_nested(v_header_pred.vertexID, v_header.vertexID):
                    stack.append(transition_graph.get_vertex(v_header_pred.headerID).vertexID)
                    edges.add((transition_graph.get_vertex(v_header_pred.headerID).vertexID, e_pred.vertexID, v_header_pred.headerID))
                    edges.add((e_pred.vertexID, v_stackID, e_pred.the_program_point))
                    
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
    return transition_graph_induced

def create_induced_subgraph(transition_graph, 
                            transition_graph_lnt, 
                            headerv, 
                            query_pair,
                            startID, 
                            is_reverse_graph):
    edges     = set()
    visited   = set()
    v_startID = transition_graph.program_point_to_predecessor_state[query_pair[0]]
    stack     = []
    stack.append(v_startID)
    while stack:
        v_stackID = stack.pop()
        v_stack   = transition_graph.get_vertex(v_stackID)
        visited.add(v_stackID)
        for e_succ in v_stack.successors.values():
            if not transition_graph_lnt.is_backedge(v_stack.vertexID, e_succ.vertexID) \
            and not transition_graph_lnt.is_backedge(e_succ.vertexID, v_stack.vertexID):
                if headerv.vertexID == transition_graph_lnt.rootID:
                    if transition_graph.is_reachable(e_succ.the_program_point, set(query_pair)):
                        edges.add((v_stack.vertexID, e_succ.vertexID, e_succ.the_program_point))
                        if e_succ.the_program_point != query_pair[1] and e_succ.vertexID not in visited:
                            stack.append(e_succ.vertexID)
                else:
                    v_succ_lnt = transition_graph_lnt.get_program_point_vertex(e_succ.vertexID)
                    if transition_graph_lnt.is_ancestor(headerv.vertexID, v_succ_lnt.vertexID):
                        edges.add((v_stack.vertexID, e_succ.vertexID, e_succ.the_program_point))
                        if e_succ.vertexID not in visited:
                            stack.append(e_succ.vertexID)
                            
    # At this point, the edges we need to add to the induced subgraph have been worked out
    transition_graph_induced = directed_graphs.StateTransitionGraph()
    for an_edge in edges:
        if not transition_graph_induced.has_vertex(an_edge[0]):
            transition_graph_induced.add_vertex(vertices.Vertex(an_edge[0]))
        if not transition_graph_induced.has_vertex(an_edge[1]):
            transition_graph_induced.add_vertex(vertices.Vertex(an_edge[1]))
        if is_reverse_graph:
            transition_graph_induced.add_edge(an_edge[1], an_edge[0], an_edge[2], False)
        else:
            transition_graph_induced.add_edge(an_edge[0], an_edge[1], an_edge[2], False)
            
    transition_graph_induced.set_edgeIDs()
    transition_graph_induced.set_entry_and_exit()
            
    return transition_graph_induced

def detect_non_terminal_backedges(transition_graph_lnt, transition_graph_induced_global, sink_vertices):
    to_remove = set()
    for v in sink_vertices:
        pass
    return to_remove

def create_connected_transition_graph(transition_graph_induced_global, transition_graph_induced):    
    # Relabel each vertex
    oldID_to_newID = {}
    for v in transition_graph_induced:
        oldID_to_newID[v.vertexID] = transition_graph_induced_global.get_next_vertexID()
        transition_graph_induced_global.add_vertex(vertices.Vertex(oldID_to_newID[v.vertexID]))
    # Create edges according to new numbering
    new_edges = set()
    for v in transition_graph_induced:
        for succe in v.successors.values():
            new_edges.add((oldID_to_newID[v.vertexID], oldID_to_newID[succe.vertexID], succe.the_program_point))    
    # Add new edges     
    for an_edge in new_edges:
        transition_graph_induced_global.add_edge(an_edge[0], an_edge[1], an_edge[2], False)
        
def create_subgraph_for_vertices_in_same_loop_nest(transition_graph,
                                                   transition_graph_lnt,
                                                   transition_graph_induced_global,
                                                   headerv,
                                                   query_pair):
    
    transition_graph_one = create_induced_subgraph(transition_graph, 
                                                   transition_graph_lnt, 
                                                   headerv,  
                                                   query_pair,
                                                   query_pair[0],
                                                   False) 
    
    create_connected_transition_graph(transition_graph_induced_global, transition_graph_one)
    
    if transition_graph_induced_global.number_of_vertices() > 0:
        # Set the entry vertex of the global induced CFG
        source_vertices1 = transition_graph_induced_global.get_source_vertices()
        assert len(source_vertices1) == 1
        transition_graph_induced_global.entryID = list(source_vertices1)[0].vertexID
        
        # Remove any sink vertices which represent loop-back edges but are not terminal in the induced CFG
        sink_vertices1 = transition_graph_induced_global.get_sink_vertices()
        to_remove     = detect_non_terminal_backedges(transition_graph, transition_graph_lnt, sink_vertices1)
        for v in to_remove:
            sink_vertices1.remove(v)
            transition_graph_induced_global.removeVertex(v.vertexID)
        
        program_point_one_header = transition_graph_lnt.get_vertex(transition_graph_lnt.get_program_point_vertex(query_pair[0]).parentID)
        program_point_two_header = transition_graph_lnt.get_vertex(transition_graph_lnt.get_program_point_vertex(query_pair[1]).parentID)
        
        if program_point_one_header.vertexID == transition_graph_lnt.rootID \
        or program_point_two_header.vertexID  == transition_graph_lnt.rootID:
            assert len(sink_vertices1) == 1
            transition_graph_induced_global.exitID  = list(sink_vertices1)[0].vertexID
        else:
            transition_graph_two = create_induced_subgraph(transition_graph.get_reverse_graph(), 
                                                           transition_graph_lnt, 
                                                           headerv, 
                                                           query_pair,
                                                           query_pair[1],
                                                           True)
            create_connected_transition_graph(transition_graph_induced_global, transition_graph_two)
            source_vertices2   = transition_graph_induced_global.get_source_vertices()
            source_vertices2   = source_vertices2.difference(source_vertices1)
            sink_vertices2     = transition_graph_induced_global.get_sink_vertices()
            sink_vertices2     = sink_vertices2.difference(sink_vertices1)
            
            for sourcev in sink_vertices1:
                for destinationv in source_vertices2:
                    transition_graph_induced_global.addEdge(sourcev.vertexID, destinationv.vertexID)
            
            assert len(sink_vertices2) == 1
            transition_graph_induced_global.exitID  = list(sink_vertices2)[0].vertexID

def create_path_expression(cfg, query_pair):
    transition_graph     = cfg.get_transition_graph()
    transition_graph_lnt = transition_graph.get_LNT()
    if query_pair[0] == query_pair[1]:
        lntv    = transition_graph_lnt.get_vertex(transition_graph_lnt.program_point_to_lnt_vertexID[query_pair[0]])
        headerv = transition_graph_lnt.get_vertex(lntv.parentID)
    else:
        headerv = transition_graph_lnt.get_vertex(transition_graph_lnt.get_LCA().query(query_pair))
    
    transition_graph_induced_global = directed_graphs.StateTransitionGraph()
        
    create_subgraph_for_vertices_in_same_loop_nest(transition_graph, 
                                                   transition_graph_lnt,
                                                   transition_graph_induced_global,
                                                   headerv, 
                                                   query_pair)
        
    udraw_suffix = "%s.%s" % (('_'.join(map(str, query_pair[0]))), ('_'.join(map(str, query_pair[1]))))
    
    udraw.make_file(transition_graph_induced_global, 
                    "%s.transition.%s" % (cfg.name, udraw_suffix)) 
    
    expressiont = PathExpression(transition_graph,
                                 transition_graph_lnt,
                                 transition_graph_induced_global)
    
    udraw.make_file(expressiont, 
                    "%s.regexp.%s" % (cfg.name, udraw_suffix))  
    
    print("P(%s) = %s" % (udraw_suffix, expressiont.get_textual_format()))
    
def create_path_expression_for_all_loops(cfg):
    transition_graph     = cfg.get_transition_graph()
    transition_graph_lnt = transition_graph.get_LNT()
    for the_vertices in transition_graph_lnt.level_by_level_iterator(True):
        for v_header in [v_tree for v_tree in the_vertices if isinstance(v_tree, vertices.HeaderVertex)]:
            if v_header.vertexID == transition_graph_lnt.rootID:
                iteration_subgraph = create_induced_subgraph_for_loop_iteration_space(transition_graph, 
                                                                                      transition_graph_lnt,
                                                                                      v_header)
                
                iteration_subgraph_path_expression = PathExpression(transition_graph,
                                                                    transition_graph_lnt,
                                                                    iteration_subgraph)
                print("P(iterations(L_%d)) = %s\n" % (v_header.headerID,
                                                      iteration_subgraph_path_expression.get_textual_format()))
                
                udraw.make_file(iteration_subgraph_path_expression, "%s.L_%d" % (cfg.name, v_header.headerID))
            else:
                iteration_subgraph, exit_subgraph = create_induced_subgraphs_for_non_dummy_loop(transition_graph, 
                                                                                                transition_graph_lnt, 
                                                                                                v_header)
                
                iteration_subgraph_path_expression = PathExpression(transition_graph,
                                                                    transition_graph_lnt,
                                                                    iteration_subgraph)
                iteration_subgraph_path_expression.transform_into_for_loop()
                
                exit_subgraph_path_expression = PathExpression(transition_graph,
                                                               transition_graph_lnt,
                                                               exit_subgraph)
                
                print("P(iterations(L_%d)) = %s\n" % (v_header.headerID,
                                                      iteration_subgraph_path_expression.get_textual_format()))
                
                print("P(exits(L_%d)) = %s\n" % (v_header.headerID,
                                                 exit_subgraph_path_expression.get_textual_format()))
                

                

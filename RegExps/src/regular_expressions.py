from __future__ import print_function

import directed_graphs
import vertices
import udraw

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
    EMPTY_SET         = "{}"
    LAMBDA_EXPRESSION = "__"
    
    def __init__(self, transition_graph_induced_global):   
        directed_graphs.DirectedGraph.__init__(self)
        if transition_graph_induced_global.number_of_vertices() > 0:
            self.transitiong            = transition_graph_induced_global
            self.predominator_tree      = directed_graphs.Dominators(transition_graph_induced_global)
            self.predominator_tree.augment_with_program_point_edges()
            self.acyclic_reducible_info = AcyclicReducibility(transition_graph_induced_global, self.predominator_tree)
            self.reverse_transitiong    = transition_graph_induced_global.get_reverse_graph()
            self.postdominator_tree     = directed_graphs.Dominators(self.reverse_transitiong)
            self.reg_exp_trees = {}
            self.compute()
            self.set_edgeIDs()
        
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
        dfs = directed_graphs.DepthFirstSearch(self.transitiong, self.transitiong.get_entryID())
        for vertexID_transitiong in reversed(dfs.post_order):
            if vertexID_transitiong == self.transitiong.entryID:
                seqv = self.create_sequence_vertex()
                self.rootID = seqv.vertexID
                self.reg_exp_trees[vertexID_transitiong] = seqv
            else:
                v_transitiong = self.transitiong.get_vertex(vertexID_transitiong)
                for predID_transitiong, prede_transitiong in v_transitiong.predecessors.iteritems():
                    predv_transitiong  = self.transitiong.get_vertex(predID_transitiong)
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
                
    def append_loop_body_expression(self, v):
        return
        if self.lnt.is_loop_tail(v.real_vertexID) and self.transitiong.get_vertex(v.vertexID).number_of_successors() > 0:
            enhanced_cfg = self.lnt.induce_subgraph_for_tail(v.real_vertexID)
            path_expr = PathExpression(self.cfg, self.lnt, enhanced_cfg)
            path_expr.the_expr.wrap_in_kleene_operator(RegExp.kleene_star)
            self.vertex_to_regular_expression[v.vertexID].append(path_expr)
            
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
                    if v.operator == vertices.RegExpVertex.ALTERNATIVE:
                        the_text += "[ "
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
                        the_text += " ]"
                        
                    for predID in v.predecessors.keys():
                        predv = self.get_vertex(predID)
                        succe = predv.get_successor_edge(vertexID)
                        temporary_expressions[succe.edgeID] = the_text
                    if vertexID == self.rootID:
                        temporary_expressions[vertexID] = the_text
            return temporary_expressions[vertexID] 

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
            transition_graph_induced.add_edge(an_edge[1], an_edge[0], an_edge[2])
        else:
            transition_graph_induced.add_edge(an_edge[0], an_edge[1], an_edge[2])
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
        transition_graph_induced_global.add_edge(an_edge[0], an_edge[1], an_edge[2])
        
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
        
    transition_graph_induced_global.set_edgeIDs()
    
    udraw_suffix = "%s.%s" % (('_'.join(map(str, query_pair[0]))), ('_'.join(map(str, query_pair[1]))))
    
    udraw.make_file(transition_graph_induced_global, 
                    "%s.transition.%s" % (cfg.name, udraw_suffix)) 
    
    expressiont = PathExpression(transition_graph_induced_global)
    
    udraw.make_file(expressiont, 
                    "%s.regexp.%s" % (cfg.name, udraw_suffix))  
    
    print("P(%s) = %s" % (udraw_suffix, expressiont.get_textual_format()))
    
def create_path_expression_for_all_loops(cfg):
    lnt = cfg.get_LNT()
    for the_vertices in lnt.level_by_level_iterator(True):
        for treev in [treev for treev in the_vertices if isinstance(treev, vertices.HeaderVertex)]: 
            backedges    = lnt.get_backedges(treev.headerID)
            enhanced_cfg = lnt.induce_subgraph(backedges)
            path_expr = PathExpression(cfg, lnt, enhanced_cfg)
            path_expr.the_expr.wrap_in_kleene_operator(RegExp.kleene_star)
            PathExpression.loop_body_expressions[treev.headerID] = path_expr
            print("path expression for loop body with header %d: %s\n" % (treev.headerID,
                                                                          path_expr.__str__()))
            for backedge in backedges:
                enhanced_cfg = lnt.induce_subgraph({backedge})
                path_expr = PathExpression(cfg, lnt, enhanced_cfg)
                path_expr.the_expr.wrap_in_kleene_operator(RegExp.kleene_star)
                PathExpression.loop_body_expressions[backedge] = path_expr
                print("path expression for loop body with backedge %s = %s\n" % (backedge,
                                                                                 path_expr.__str__()))
            

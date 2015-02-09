from __future__ import print_function

import directed_graphs
import vertices
import copy
import udraw
import itertools

class DominanceFrontiers:
    def __init__(self, cfg, dominator_tree):
        self.vertex_DF = {}
        for v in cfg:
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

class RegExp:
    concatenation = '.'
    union         = '|'
    kleene_star   = '*'
    kleene_plus   = '+'
    empty         = '0'
    
    
    @staticmethod
    def l_parenthesis(left_spaces=0):
        return (' ' * left_spaces) + '('
    
    @staticmethod
    def r_parenthesis(right_spaces=0):
        return ')' + (' ' * right_spaces)
    
    def __init__(self):
        self.elements = []
        
    def append(self, *args):
        for arg in args:
            if isinstance(arg, RegExp):
                self.elements.extend(arg.elements)
            elif isinstance(arg, int):
                self.elements.append(str(arg))
            else:
                self.elements.append(arg)
    
    def wrap_in_kleene_operator(self, kleene_operator):
        assert kleene_operator == RegExp.kleene_plus or kleene_operator == RegExp.kleene_star
        self.elements.insert(0, RegExp.l_parenthesis())
        self.elements.append(RegExp.r_parenthesis())
        self.elements.append(kleene_operator)
        
    def lookahead(self, it):
        it1, it2 = itertools.tee(iter(it))
        next(it2)
        return itertools.izip_longest(it1, it2)
        
    def __str__ (self):
        the_string = ""
        for (elem, lookahead) in self.lookahead(self.elements):
            if isinstance(elem, vertices.CFGEdge):
                the_string += "[%d_%d]" % (elem.edge[0], elem.edge[1])
            elif isinstance(elem, vertices.CFGVertex):
                if elem == self.elements[0] or elem == RegExp.l_parenthesis(0):
                    l_space = ""
                else:
                    l_space = " "
                if lookahead == RegExp.r_parenthesis(0):
                    r_space = ""
                else:
                    r_space = " "
                the_string += "%s%d%s" % (l_space, elem.real_vertexID, r_space)
            elif isinstance(elem, vertices.HeaderVertex):
                the_string += " %d " % elem.headerID
            elif isinstance(elem, PathExpression):
                the_string += elem.the_expr.__str__()
            else:
                the_string += elem
                if elem == RegExp.kleene_plus or elem == RegExp.kleene_star:
                    the_string += " "
        return the_string
            
class PathExpression:
    loop_body_expressions = {}
    
    def __init__(self, cfg, lnt, induced_CFG):   
        self.cfg                    = cfg 
        self.lnt                    = lnt    
        self.induced_CFG            = induced_CFG
        self.predominator_tree      = directed_graphs.Dominators(induced_CFG, induced_CFG.get_entryID())
        self.lca                    = self.predominator_tree.get_LCA()
        self.acyclic_reducible_info = AcyclicReducibility(induced_CFG, self.predominator_tree)
        self.reverse_induced_CFG    = induced_CFG.get_reverse_graph()
        self.postdominator_tree     = directed_graphs.Dominators(self.reverse_induced_CFG, self.reverse_induced_CFG.get_entryID())
        self.initialise()
        self.compute()
        self.the_expr = self.vertex_to_regular_expression[induced_CFG.get_exitID()]
                
    def initialise(self):
        self.vertex_to_regular_expression = {}
        for v in self.induced_CFG :
            self.vertex_to_regular_expression[v.vertexID] = RegExp() 
        
    def compute(self):
        dfs = directed_graphs.DepthFirstSearch(self.induced_CFG, self.induced_CFG .get_entryID())
        for vertexID in reversed(dfs.post_order):
            v = self.induced_CFG.get_vertex(vertexID)
            if v.number_of_predecessors() > 1:
                self.handle_merge(v)
            else:
                self.handle_non_merge(v)
            self.append_loop_body_expression(v)
                
    def append_loop_body_expression(self, v):
        if self.lnt.is_loop_tail(v.real_vertexID) and self.induced_CFG.get_vertex(v.vertexID).number_of_successors() > 0:
            enhanced_cfg = self.lnt.induce_subgraph_for_tail(v.real_vertexID)
            path_expr = PathExpression(self.cfg, self.lnt, enhanced_cfg)
            path_expr.the_expr.wrap_in_kleene_operator(RegExp.kleene_star)
            self.vertex_to_regular_expression[v.vertexID].append(path_expr)
        
    def handle_non_merge(self, v):
        if v.number_of_predecessors() == 1:
            predID = v.predecessors.keys()[0]
            predv  = self.induced_CFG.get_vertex(predID)
            if (predv.number_of_successors() > 1 and predID in self.acyclic_reducible_info.irreducible_branches) \
            or predv.number_of_successors() == 1:
                self.vertex_to_regular_expression[v.vertexID].append(self.vertex_to_regular_expression[predID])
        self.vertex_to_regular_expression[v.vertexID].append(v)
                
    def handle_merge(self, mergev):
        vertex_temp_reg_exprs = {}
        compressed_tree       = directed_graphs.CompressedDominatorTree(self.predominator_tree, 
                                                                        self.lca, 
                                                                        mergev.vertexID, 
                                                                        mergev.predecessors.keys())
        for the_vertices in compressed_tree.level_by_level_iterator(True):
            for treev in the_vertices:
                if treev.number_of_successors() == 0:
                    vertex_temp_reg_exprs[treev.vertexID] = self.vertex_to_regular_expression[treev.vertexID]
                else:
                    new_expr = RegExp()
                    if treev.vertexID != compressed_tree.rootID:
                        new_expr.append(self.vertex_to_regular_expression[treev.vertexID])
                    new_expr.append(RegExp.l_parenthesis())
                    counter = treev.number_of_successors()
                    for succID in treev.successors.keys():
                        new_expr.append(vertex_temp_reg_exprs[succID])
                        if counter > 1:
                            new_expr.append(RegExp.union)
                        counter -= 1
                    new_expr.append(RegExp.r_parenthesis())
                    vertex_temp_reg_exprs[treev.vertexID] = new_expr
        if self.postdominator_tree.get_vertex(compressed_tree.rootID).parentID == mergev.vertexID: 
            self.vertex_to_regular_expression[mergev.vertexID].append(self.vertex_to_regular_expression[compressed_tree.rootID])
        self.vertex_to_regular_expression[mergev.vertexID].append(vertex_temp_reg_exprs[compressed_tree.rootID])
        if not mergev.dummy:
            self.vertex_to_regular_expression[mergev.vertexID].append(mergev) 
    
def copy_vertex_into_induced_CFG(cfg, induced_CFG, vertexID):
    if not induced_CFG.has_vertex(vertexID):
        v = cfg.get_vertex(vertexID)
        newv = copy.deepcopy(v)
        newv.predecessors = {}
        newv.successors   = {}
        induced_CFG.add_vertex(newv)

def create_induced_subgraph(enhanced_cfg, 
                            enhanced_cfg_lnt, 
                            reachability_info, 
                            headerv, 
                            the_pair,
                            startID, 
                            is_reverse_graph):
    edges   = set()
    visited = set()
    stack   = []
    stack.append(startID)
    while stack:
        vertexID = stack.pop()
        visited.add(vertexID)
        v = enhanced_cfg.get_vertex(vertexID)
        for succID in v.successors.keys():
            if not enhanced_cfg_lnt.is_backedge(vertexID, succID) \
            and not enhanced_cfg_lnt.is_backedge(succID, vertexID):
                if headerv.vertexID == enhanced_cfg_lnt.rootID:
                    if reachability_info.is_reachable(succID, set(the_pair)):
                        edges.add((vertexID, succID))
                        if succID != the_pair[1] and succID not in visited:
                            stack.append(succID)
                else:
                    if enhanced_cfg_lnt.is_ancestor(headerv.vertexID, succID):
                        edges.add((vertexID, succID))
                        if succID not in visited:
                            stack.append(succID)
    # At this point, the edges we need to add to the induced subgraph have been worked out
    induced_enhanced_CFG = directed_graphs.EnhancedCFG()
    for an_edge in edges:
        copy_vertex_into_induced_CFG(enhanced_cfg, induced_enhanced_CFG, an_edge[0])
        copy_vertex_into_induced_CFG(enhanced_cfg, induced_enhanced_CFG, an_edge[1])
        if is_reverse_graph:
            induced_enhanced_CFG.addEdge(an_edge[1], an_edge[0])
        else:
            induced_enhanced_CFG.addEdge(an_edge[0], an_edge[1])
    return induced_enhanced_CFG

def detect_non_terminal_backedges(enhanced_cfg_lnt, global_induced_CFG, sink_vertices):
    to_remove = set()
    for v in sink_vertices:
        if isinstance(v, vertices.CFGEdge):
            assert v.number_of_predecessors() == 1
            predv = global_induced_CFG.get_vertex(v.predecessors.keys()[0])
            if predv.number_of_successors() > 1:
                to_remove.add(v)
    return to_remove

def create_connected_induced_CFG(global_induced_CFG, induced_CFG):    
    # Relabel each vertex
    oldID_to_newID = {}
    for v in induced_CFG:
        if isinstance(v, vertices.CFGEdge):
            oldID_to_newID[v.vertexID] = global_induced_CFG.get_next_edge_vertexID()
        else:
            oldID_to_newID[v.vertexID] = global_induced_CFG.get_next_vertexID()
        newv              = copy.deepcopy(induced_CFG.get_vertex(v.vertexID))     
        newv.vertexID     = oldID_to_newID[v.vertexID]
        newv.predecessors = {}
        newv.successors   = {}
        global_induced_CFG.add_vertex(newv)
    # Create edges according to new numbering
    new_edges = set()
    for v in induced_CFG:
        for succID in v.successors.keys():
            new_edges.add((oldID_to_newID[v.vertexID], oldID_to_newID[succID]))    
    # Add new edges     
    for an_edge in new_edges:
        global_induced_CFG.addEdge(an_edge[0], an_edge[1])
        
def create_subgraph_for_vertices_in_same_loop_nest(enhanced_cfg,
                                                   enhanced_cfg_lnt,
                                                   enhanced_cfg_reachability,
                                                   headerv,
                                                   the_pair,
                                                   global_induced_CFG):
    induced_CFG1 = create_induced_subgraph(enhanced_cfg, 
                                           enhanced_cfg_lnt, 
                                           enhanced_cfg_reachability, 
                                           headerv,  
                                           the_pair,
                                           the_pair[0],
                                           False) 
    
    create_connected_induced_CFG(global_induced_CFG, induced_CFG1)
    
    # Set the entry vertex of the global induced CFG
    source_vertices1 = global_induced_CFG.get_source_vertices()
    assert len(source_vertices1) == 1
    global_induced_CFG.entryID = list(source_vertices1)[0].vertexID
    
    # Remove any sink vertices which represent loop-back edges but are not terminal in the induced CFG
    sink_vertices1 = global_induced_CFG.get_sink_vertices()
    to_remove     = detect_non_terminal_backedges(enhanced_cfg_lnt, global_induced_CFG, sink_vertices1)
    for v in to_remove:
        sink_vertices1.remove(v)
        global_induced_CFG.removeVertex(v.vertexID)
    
    if enhanced_cfg_lnt.get_vertex(enhanced_cfg_lnt.get_vertex(the_pair[0]).parentID).vertexID == enhanced_cfg_lnt.rootID \
    or enhanced_cfg_lnt.get_vertex(enhanced_cfg_lnt.get_vertex(the_pair[1]).parentID).vertexID  == enhanced_cfg_lnt.rootID:
        assert len(sink_vertices1) == 1
        global_induced_CFG.exitID  = list(sink_vertices1)[0].vertexID
    else:
        induced_CFG2 = create_induced_subgraph(enhanced_cfg.get_reverse_graph(), 
                                               enhanced_cfg_lnt, 
                                               enhanced_cfg_reachability, 
                                               headerv, 
                                               the_pair,
                                               the_pair[1],
                                               True)
        create_connected_induced_CFG(global_induced_CFG, induced_CFG2)
        source_vertices2   = global_induced_CFG.get_source_vertices()
        source_vertices2   = source_vertices2.difference(source_vertices1)
        sink_vertices2     = global_induced_CFG.get_sink_vertices()
        sink_vertices2     = sink_vertices2.difference(sink_vertices1)
        
        for sourcev in sink_vertices1:
            for destinationv in source_vertices2:
                global_induced_CFG.addEdge(sourcev.vertexID, destinationv.vertexID)
        
        assert len(sink_vertices2) == 1
        global_induced_CFG.exitID  = list(sink_vertices2)[0].vertexID


def create_subgraph_for_vertices_in_different_loop_nest(enhanced_cfg,
                                                        enhanced_cfg_lnt,
                                                        enhanced_cfg_reachability,
                                                        headerv,
                                                        the_pair,
                                                        global_induced_CFG):
    pass

def create_path_expression(cfg, the_pair):
    transition_graph          = cfg.get_transition_graph()
    transition_graph.get_reverse_graph()
    udraw.make_file(transition_graph.get_component_DAG(), "%s.dag" % cfg.name)
    transition_graph_lnt      = transition_graph.get_LNT()
    udraw.make_file(transition_graph_lnt, "%s.lnt" % cfg.name)
    headerv                   = transition_graph_lnt.get_vertex(transition_graph_lnt.get_LCA().query(the_pair))
    print(headerv.headerID)
    enhanced_cfg_reachability = enhanced_cfg.get_reachability_info()
    
    if not enhanced_cfg_reachability.is_reachable(the_pair[0], {the_pair[1]}) \
    and not enhanced_cfg_reachability.is_reachable(the_pair[1], {the_pair[0]}) \
    and headerv.vertexID == enhanced_cfg_lnt.rootID:
        reg_expr = RegExp.empty
    else:
        global_induced_CFG = directed_graphs.EnhancedCFG()
        
        if headerv.vertexID == enhanced_cfg_lnt.rootID:
            print("DIFFERENT LOOP NESTS")
        else:
            create_subgraph_for_vertices_in_same_loop_nest(enhanced_cfg, 
                                                           enhanced_cfg_lnt, 
                                                           enhanced_cfg_reachability, 
                                                           headerv, 
                                                           the_pair, 
                                                           global_induced_CFG)
            
        udraw.make_file(global_induced_CFG, "%s.%d.%d" % (cfg.name, the_pair[0], the_pair[1]))
        reg_expr = PathExpression(cfg, cfg.get_LNT(), global_induced_CFG).the_expr
        
    prologue  = "P(%s)" % (the_pair,)
    print("%s\n%s\n%s\n%s" % ('=' * len(prologue), prologue, '=' * len(prologue), reg_expr.__str__()))
    
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
            

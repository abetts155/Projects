from __future__ import print_function

import directed_graphs
import vertices
import copy

class DominanceFrontiers:
    def __init__(self, cfg, dominator_tree):
        self.vertex_DF = {}
        for v in cfg:
            self.vertex_DF[v.vertexID] = set()
        self.compute(cfg, dominator_tree)
        
    def compute(self, cfg, dominator_tree):
        for v in cfg:
            if v.number_of_predecessors() > 1: 
                immediate_dominatorID = dominator_tree.getVertex(v.vertexID).parentID
                for predID in v.predecessors.keys():
                    runnerID = predID
                    while runnerID != immediate_dominatorID:
                        self.vertex_DF[runnerID].add(v.vertexID)
                        runnerID = dominator_tree.getVertex(runnerID).parentID

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
    union         = ' | '
    kleene_star   = '* '
    kleene_plus   = '+ '
    empty         = '0'
    
    @staticmethod
    def l_parenthesis(left_spaces=0):
        return (' ' * left_spaces) + '('
    
    @staticmethod
    def r_parenthesis(right_spaces=0):
        return ')' + (' ' * right_spaces)
    
    def __init__(self):
        self.elements = []
        
    def isEmpty (self):
        return len(self.elements) == 0
    
    def pop(self):
        self.elements.pop()
    
    def last(self):
        return self.elements[-1]
    
    def first(self):
        return self.elements[0]
        
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
        self.elements.insert(0, RegExp.l_parenthesis(1))
        self.elements.append(RegExp.r_parenthesis())
        self.elements.append(kleene_operator)
        
    def __str__ (self):
        the_string = ""
        for elem in self.elements:
            if isinstance(elem, vertices.CFGEdge):
                the_string += "[%d->%d]" % (elem.edge[0], elem.edge[1])
            elif isinstance(elem, vertices.CFGVertex):
                the_string += elem.vertexID.__repr__()
            else:
                the_string += elem
        return the_string
            
class PathExpression:
    loop_back_edge_path_expression_cache = {}
    
    def __init__(self, cfg, lnt, induced_CFG):   
        self.cfg = cfg 
        self.lnt = lnt    
        if induced_CFG.number_of_vertices() == 0:
            self.the_expr = RegExp.empty
        else:
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
            v = self.induced_CFG.getVertex(vertexID)
            if vertexID == self.induced_CFG.get_entryID():
                self.vertex_to_regular_expression[vertexID].append(v)
            else:
                if v.number_of_predecessors() == 1:
                    self.handle_non_merge(v)
                else:
                    self.handle_merge(v)
            
    def handle_non_merge(self, v):
        for predID in v.predecessors.keys():
            predv = self.induced_CFG.getVertex(predID)
            if (predv.number_of_successors() > 1 and predID in self.acyclic_reducible_info.irreducible_branches) \
            or predv.number_of_successors() == 1:
                    self.vertex_to_regular_expression[v.vertexID].append(self.vertex_to_regular_expression[predID], RegExp.concatenation)
            self.vertex_to_regular_expression[v.vertexID].append(v)
            if isinstance(v, vertices.CFGEdge) \
            and self.lnt.is_loop_back_edge(v.edge[0], v.edge[1]) \
            and v.vertexID != self.induced_CFG.exitID:
                if v.edge not in PathExpression.loop_back_edge_path_expression_cache:
                    enhanced_CFG = self.cfg.enhanced_CFGs[v.edge[1]]
                    edgev        = enhanced_CFG.edge_to_vertex_representative[v.edge]
                    induced_CFG  = create_induced_subgraph(self.cfg, v.edge[1], edgev.vertexID)
                    loop_expr    = PathExpression(self.cfg, self.lnt, induced_CFG)
                    self.vertex_to_regular_expression[v.vertexID].append(RegExp.l_parenthesis(1))
                    self.vertex_to_regular_expression[v.vertexID].append(loop_expr.the_expr)
                    self.vertex_to_regular_expression[v.vertexID].append(RegExp.r_parenthesis(0))
                    self.vertex_to_regular_expression[v.vertexID].append(RegExp.kleene_star)
                
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
        if self.postdominator_tree.getVertex(compressed_tree.rootID).parentID == mergev.vertexID: 
            self.vertex_to_regular_expression[mergev.vertexID].append(self.vertex_to_regular_expression[compressed_tree.rootID])
        self.vertex_to_regular_expression[mergev.vertexID].append(vertex_temp_reg_exprs[compressed_tree.rootID])
        if not mergev.dummy:
            self.vertex_to_regular_expression[mergev.vertexID].append(mergev) 
    
    def __str__(self):
        return self.the_expr.__str__()

def union_subgraphs_through_backedges(cfg, lnt, headerv, prefix_graph, suffix_graph):
    induced_CFG = directed_graphs.EnhancedCFG()
    for v in prefix_graph:
        induced_CFG.addVertex(v)
    for v in suffix_graph:
        induced_CFG.addVertex(v)
    enhanced_CFG = cfg.enhanced_CFGs[headerv.headerID]
    for edge in lnt.get_loop_back_edges(headerv.headerID):
        v = enhanced_CFG.edge_to_vertex_representative[edge]
        newv = copy.deepcopy(v)
        newv.predecessors = {}
        newv.successors   = {}
        induced_CFG.addVertex(newv) 
        induced_CFG.addEdge(edge[0], newv.vertexID)
        induced_CFG.addEdge(newv.vertexID, edge[1])
    induced_CFG.entryID = prefix_graph.entryID
    induced_CFG.exitID  = suffix_graph.exitID
    return induced_CFG
            
def create_induced_subgraph(cfg, entry_vertexID, exit_vertexID):
    lnt               = cfg.get_LNT()
    headerv           = lnt.getVertex(lnt.getVertex(entry_vertexID).parentID)
    enhanced_CFG      = cfg.enhanced_CFGs[headerv.headerID]
    reachability_info = cfg.reachability_info[headerv.headerID]
    pair_subset       = set([entry_vertexID, exit_vertexID])
    edges             = set()
    visited           = set()
    stack             = []
    stack.append(exit_vertexID)
    while stack:
        vertexID = stack.pop()
        visited.add(vertexID)
        if vertexID != entry_vertexID:
            v = enhanced_CFG.getVertex(vertexID)
            for predID in v.predecessors.keys():
                if pair_subset.issubset(reachability_info.unified[predID]):
                    edges.add((predID, vertexID))
                    if not predID in visited:
                        stack.append(predID)
    # At this point, the edges we need to add to the induced subgraph have been worked out
    def copy_vertex_into_induced_CFG(vertexID):
        v = enhanced_CFG.getVertex(vertexID)
        newv = copy.deepcopy(v)
        newv.predecessors = {}
        newv.successors   = {}
        induced_CFG.addVertex(newv)
    induced_CFG = directed_graphs.EnhancedCFG()
    for an_edge in edges:
        if not induced_CFG.hasVertex(an_edge[0]):
            copy_vertex_into_induced_CFG(an_edge[0])
        if not induced_CFG.hasVertex(an_edge[1]):
            copy_vertex_into_induced_CFG(an_edge[1])
        induced_CFG.addEdge(an_edge[0], an_edge[1])
    # If there are no edges in the graph then we need to add the
    # entry and exit vertices manually
    if not induced_CFG.hasVertex(entry_vertexID):
        copy_vertex_into_induced_CFG(entry_vertexID)
    if not induced_CFG.hasVertex(exit_vertexID):
        copy_vertex_into_induced_CFG(exit_vertexID)
    induced_CFG.entryID = entry_vertexID
    induced_CFG.exitID  = exit_vertexID
    return induced_CFG
    
def create_path_expression(cfg, entry_vertexID, exit_vertexID):
    lnt = cfg.get_LNT()
    if entry_vertexID == exit_vertexID:
        headerv = lnt.getVertex(lnt.getVertex(entry_vertexID).parentID)
    else:
        headerv = lnt.getVertex(lnt.get_LCA().query(entry_vertexID, exit_vertexID))
    if headerv.vertexID == lnt.rootID:
        induced_CFG = create_induced_subgraph(cfg, entry_vertexID, exit_vertexID)
        path_expr   = PathExpression(cfg, lnt, induced_CFG)
        print("P(%d, %d) = %s" % (entry_vertexID,
                                  exit_vertexID,
                                  path_expr.__str__()))
    else:
        if entry_vertexID != exit_vertexID \
        and exit_vertexID in cfg.reachability_info[headerv.headerID].unified[entry_vertexID]:
            print("%d can reach %d" % (entry_vertexID,exit_vertexID))
            induced_CFG = create_induced_subgraph(cfg, entry_vertexID, exit_vertexID)
            path_expr   = PathExpression(cfg, lnt, induced_CFG)
            print("P(%d, %d) = %s" % (entry_vertexID,
                                      exit_vertexID,
                                      path_expr.__str__()))
        else:
            print("%d can NOT reach %d" % (entry_vertexID,exit_vertexID))
            induced_CFG1 = create_induced_subgraph(cfg, entry_vertexID, lnt.get_loop_tails(headerv.headerID)[0])
            induced_CFG2 = create_induced_subgraph(cfg, headerv.headerID, exit_vertexID)
            induced_CFG  = union_subgraphs_through_backedges(cfg, lnt, headerv, induced_CFG1, induced_CFG2)
            path_expr    = PathExpression(cfg, lnt, induced_CFG)
            print("P(%d, %d) = %s" % (entry_vertexID,
                                      exit_vertexID,
                                      path_expr.__str__()))

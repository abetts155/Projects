from __future__ import print_function

import directed_graphs
import vertices

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
    l_parenthesis = '('
    r_parenthesis = ')'
    kleene_star   = '*'
    kleene_plus   = '+'
    empty         = '0'
    
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
    def __init__(self, induced_CFG):
        if induced_CFG.number_of_vertices() == 0:
            print("P(%d, %d) = %s" % (induced_CFG.get_entryID(),
                                      induced_CFG.get_exitID(), 
                                      RegExp.empty))
        else:
            self.induced_CFG            = induced_CFG
            self.predominator_tree      = directed_graphs.Dominators(induced_CFG, induced_CFG.get_entryID())
            self.lca                    = directed_graphs.LeastCommonAncestor(self.predominator_tree)
            self.acyclic_reducible_info = AcyclicReducibility(induced_CFG, self.predominator_tree)
            self.reverse_induced_CFG    = induced_CFG.get_reverse_graph()
            self.postdominator_tree     = directed_graphs.Dominators(self.reverse_induced_CFG, self.reverse_induced_CFG.get_entryID())
            self.initialise()
            self.compute()
            print("P(%d, %d) = %s" % (induced_CFG.get_entryID(),
                                      induced_CFG.get_exitID(), 
                                      self.vertex_to_regular_expression[induced_CFG.get_exitID()]))
                
    def initialise(self):
        self.vertex_to_regular_expression = {}
        for v in self.induced_CFG :
            self.vertex_to_regular_expression[v.vertexID] = RegExp() 
        
    def compute(self):
        dfs = directed_graphs.DepthFirstSearch(self.induced_CFG, self.induced_CFG .get_entryID())
        for vertexID in reversed(dfs.post_order):
            v = self.induced_CFG.getVertex(vertexID)
            if vertexID == self.induced_CFG .get_entryID():
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
                    new_expr.append(RegExp.l_parenthesis)
                    counter = treev.number_of_successors()
                    for succID in treev.successors.keys():
                        new_expr.append(vertex_temp_reg_exprs[succID])
                        if counter > 1:
                            new_expr.append(RegExp.union)
                        counter -= 1
                    new_expr.append(RegExp.r_parenthesis)
                    vertex_temp_reg_exprs[treev.vertexID] = new_expr
        if self.postdominator_tree.getVertex(compressed_tree.rootID).parentID == mergev.vertexID: 
            self.vertex_to_regular_expression[mergev.vertexID].append(self.vertex_to_regular_expression[compressed_tree.rootID])
        self.vertex_to_regular_expression[mergev.vertexID].append(vertex_temp_reg_exprs[compressed_tree.rootID])
        if not mergev.dummy:
            self.vertex_to_regular_expression[mergev.vertexID].append(mergev) 
            
def create_path_expression(cfg, entry_vertexID, exit_vertexID):
    lnt     = cfg.get_LNT()
    lca     = directed_graphs.LeastCommonAncestor(lnt)
    headerv = lnt.getVertex(lca.get_LCA(entry_vertexID, exit_vertexID))
    if headerv.vertexID == lnt.rootID:
        induced_CFG = cfg.create_induced_subgraph(entry_vertexID, exit_vertexID)
        PathExpression(induced_CFG)
    else:
        induced_CFG = cfg.create_induced_subgraph(entry_vertexID, exit_vertexID)
        PathExpression(induced_CFG)


    
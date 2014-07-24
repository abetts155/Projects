import trees
import vertices

class DominanceFrontiers:
    def __init__(self, cfg, dominator_tree):
        self.vertex_DF = {}
        for v in cfg:
            self.vertex_DF[v.vertexID] = set()
        self.__compute(cfg, dominator_tree)
        
    def compute(self, cfg, dominator_tree):
        for v in self.cfg:
            if v.number_of_predecessors() > 1: 
                immediate_dominatorID = dominator_tree.getVertex(v.vertexID).parentID
                for predID in v.predecessors.keys():
                    runnerID = predID
                    while runnerID != immediate_dominatorID:
                        self.vertex_DF[runnerID].add(v.vertexID)
                        runnerID = dominator_tree.getVertex(runnerID).parentID

class AcyclicReducibility:
    def __init__(self, cfg, reverse_cfg, predominator_tree, postdominator_tree):
        self.irreducible_branches = set()
        self.irreducible_merges   = set()
        self.compute()
    
    def __compute(self, cfg, reverse_cfg, predominator_tree, postdominator_tree):
        predominance_frontier_info  = DominanceFrontiers(self.cfg, predominator_tree)
        postdominance_frontier_info = DominanceFrontiers(self.reverse_cfg, postdominator_tree)
        for v in cfg:
            if v.number_of_successors() > 1:
                if len(predominance_frontier_info.vertex_DF[v.vertexID]) > 1:
                    self.irreducible_branches.add(v.vertexID)
            if v.number_of_predecessors() > 1:
                if len(postdominance_frontier_info.vertex_DF[v.vertexID]) > 1:
                    self.irreducible_branches.add(v.vertexID)

class RegExp:
    lambda_       = '@'
    union         = ' | '
    kleene_star   = '*'
    kleene_plus   = '+'
    concatenation = '.'
    
    @staticmethod
    def lParen(space=True):
        if space:
            return ' ('
        else:
            return '('
    
    @staticmethod
    def rParen(space=True):
        if space:
            return ') '
        else:
            return ')'
    
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
        return ''.join(self.elements)
            
class CFGPathExpression:
    def __init__(self, cfg, lnt):
        self.lnt                       = lnt
        self.header_to_path_expression = {}
        for the_vertices in self.lnt.level_by_level_iterator(True):
            for treev in the_vertices:
                if isinstance(treev, vertices.HeaderVertex):
                    self.enhanced_CFG           = self.lnt.induced_loop_subgraph(treev)
                    self.predom_tree            = trees.Dominators(self.enhanced_CFG, self.enhanced_CFG.get_entryID())
                    self.enhanced_CFG_reverse   = self.enhanced_CFG.get_reverse_graph()
                    self.postdom_tree           = trees.Dominators(self.enhanced_CFG_reverse, self.enhanced_CFG_reverse.get_entryID())
                    self.lca                    = trees.LeastCommonAncestor(self.predom_tree)
                    self.acyclic_reducible_info = AcyclicReducibility(self.enhanced_CFG, 
                                                                      self.enhanced_CFG_reverse, 
                                                                      self.predom_tree, 
                                                                      self.postdom_tree)
                    self.initialise()
                    self.compute()
                    self.header_to_path_expression[self.enhanced_CFG.get_entryID()] = self.vertex_to_regular_expression[self.enhanced_CFG.get_exitID()]
                    
    def initialise(self):
        self.vertex_to_regular_expression = {}
        for v in self.enhanced_CFG:
            self.vertex_to_regular_expression[v.vertexID] = RegExp() 
        
    def compute(self):
        dfs = trees.DepthFirstSearch(self.enhanced_CFG, self.enhanced_CFG.get_entryID())
        for vertexID in reversed(dfs.post_order):
            v = self.enhanced_CFG.getVertex(vertexID)
            if vertexID == self.enhanced_CFG.get_entryID():
                self.vertex_to_regular_expression[vertexID].append(vertexID)
            else:
                if v.number_of_predecessors() == 1:
                    self.handle_non_merge(v)
                else:
                    self.handle_merge(v)
            print "RegExp(%d) = %s" % (vertexID, self.vertex_to_regular_expression[vertexID])
            
    def handle_non_merge(self, v):
        for predID in v.predecessors.keys():
            predv = self.enhanced_CFG.getVertex(predID)
            if (predv.number_of_successors() > 1 and predID in self.acyclic_reducible_info.irreducible_branches) \
            or predv.number_of_successors() == 1:
                    self.vertex_to_regular_expression[v.vertexID].append(self.vertex_to_regular_expression[predID], RegExp.concatenation)
            if self.lnt.is_loop_header(v.vertexID):
                self.handle_loop_header(self.vertex_to_regular_expression[v.vertexID], v.vertexID)
            else:
                self.vertex_to_regular_expression[v.vertexID].append(v.vertexID)
                
    def handle_merge(self, mergev):
        vertex_temp_reg_exprs = {}
        compressed_tree       = trees.CompressedDominatorTree(self.predom_tree, 
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
                    new_expr.append(RegExp.lParen())
                    for succID in treev.successors.keys():
                        new_expr.append(vertex_temp_reg_exprs[succID])
                        new_expr.append(RegExp.union)
                    
                    if self.enhanced_CFG.getVertex(treev.vertexID).hasSuccessor(mergev.vertexID):
                        new_expr.append(RegExp.lambda_)
                    else:
                        new_expr.pop()
                    new_expr.append(RegExp.rParen())
                    vertex_temp_reg_exprs[treev.vertexID] = new_expr
        if self.postdom_tree.getVertex(compressed_tree.rootID).parentID == mergev.vertexID: 
            self.vertex_to_regular_expression[mergev.vertexID].append(self.vertex_to_regular_expression[compressed_tree.rootID])
        self.vertex_to_regular_expression[mergev.vertexID].append(vertex_temp_reg_exprs[compressed_tree.rootID])
        if not mergev.dummy:
            self.vertex_to_regular_expression[mergev.vertexID].append(mergev.vertexID) 
        
    def handle_loop_header(self, current_path_expr, headerID):
        current_path_expr.append(RegExp.lParen(False))
        current_path_expr.append(RegExp.lParen(False))
        current_path_expr.append(self.header_to_path_expression[headerID])
        current_path_expr.append(RegExp.rParen(False))
        current_path_expr.append(RegExp.kleenePlus)
        current_path_expr.append(RegExp.rParen(False))
                
    
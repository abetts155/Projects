import vertices
import debug
import udraw
import utils
import copy
import itertools

class DirectedGraph:        
    def __init__ (self):
        self.the_vertices = {}
        self.name = None
        
    def addVertex(self, v):
        assert v.vertexID not in self.the_vertices, "Adding vertex %d which is already in graph" % v.vertexID
        self.the_vertices[v.vertexID] = v
    
    def getVertex(self, vertexID):
        assert vertexID in self.the_vertices, "Vertex " + str(vertexID) + " is not in the graph"
        return self.the_vertices[vertexID]
    
    def removeVertex(self, vertexID):
        assert vertexID in self.the_vertices, "Vertex " + str(vertexID) + " is not in the graph"
        for v in self:
            if v.has_successor(vertexID):
                v.remove_successor(vertexID)
            if v.has_predecessor(vertexID):
                v.remove_predecessor(vertexID)
        del self.the_vertices[vertexID]
    
    def hasVertex(self, vertexID):
        return vertexID in self.the_vertices
    
    def addEdge(self, predID, succID):
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
        predv.add_successor(succID)
        succv.add_predecessor(predID)
        
    def hasEdge(self, predID, succID):
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
        return predv.has_successor(succID) or succv.hasPredecessor(predID)
    
    def removeEdge(self, predID, succID):
        predv = self.getVertex(predID)
        succv = self.getVertex(succID)
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
            predv  = reverseg.getVertex(predID)
            for succID in v.successors.keys():
                succv = reverseg.getVertex(succID)
                predv.add_predecessor(succID)
                succv.add_successor(predID)
        
    def add_predecessor_edges(self):
        for v in self:
            for succID in v.successors.keys():
                succv = self.getVertex(succID)
                if not succv.has_predecessor(v.vertexID):
                    succv.add_predecessor(v.vertexID)
    
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
            string += v.__str__()
        return string
    
class ReachabilityInformation:
    def __init__(self, forward_flow_graph, reverse_flow_graph):
        self.unified = {}
        self.forward = {}
        self.reverse = {}
        self.initialise(forward_flow_graph)
        self.compute(forward_flow_graph, reverse_flow_graph)
    
    def initialise(self, cfg):
        for v in cfg:
            self.unified[v.vertexID] = set()
            self.forward[v.vertexID] = set()
            self.reverse[v.vertexID] = set()
            self.forward[v.vertexID].add(v.vertexID)
            self.reverse[v.vertexID].add(v.vertexID)
            
    def compute(self, forward_flow_graph, reverse_flow_graph):
        dfs = forward_flow_graph.get_depth_first_search_tree()
        for vertexID in reversed(dfs.post_order):
            v = forward_flow_graph.getVertex(vertexID)
            for predID in v.predecessors.keys():
                self.forward[v.vertexID].update(self.forward[predID])
        dfs = reverse_flow_graph.get_depth_first_search_tree()
        for vertexID in reversed(dfs.post_order):
            v = reverse_flow_graph.getVertex(vertexID)
            for predID in v.predecessors.keys():
                self.reverse[v.vertexID].update(self.reverse[predID])
            self.unified[v.vertexID] = self.forward[v.vertexID].union(self.reverse[v.vertexID])

class FlowGraph(DirectedGraph):
    def __init__ (self):
        DirectedGraph.__init__(self)
        self.entryID          = vertices.dummyID
        self.exitID           = vertices.dummyID
        self.reverse_graph    = None 
        self.depth_first_tree = None
        self.dominator_tree   = None
        
    def get_entryID (self):
        assert self.entryID != vertices.dummyID, "Entry to flow graph not found"
        return self.entryID
    
    def get_exitID (self):
        assert self.exitID != vertices.dummyID, "Exit to flow graph not found"
        return self.exitID
            
    def set_edgeIDs (self):
        edgeID = 1
        for v in self:
            for succID in v.successors.keys():
                succe = v.get_successor_edge(succID)
                succe.set_edgeID(edgeID)
                succv = self.getVertex(succID)
                prede = succv.get_predecessor_edge(v.vertexID)
                prede.set_edgeID(edgeID)
                edgeID += 1
    
    def get_reverse_graph(self):
        if self.reverse_graph is None:
            self.reverse_graph = FlowGraph() 
            self.add_vertices_to_reverse_graph(self.reverse_graph)
            self.add_edges_to_reverse_graph(self.reverse_graph)
            self.reverse_graph.entryID = self.get_exitID()
            self.reverse_graph.exitID  = self.get_entryID()
        return self.reverse_graph
    
    def get_depth_first_search_tree(self):
        if self.depth_first_tree is None:
            self.depth_first_tree = DepthFirstSearch(self, self.entryID)
        return self.depth_first_tree            
    
    def get_dominator_tree(self):
        if self.dominator_tree is None:
            self.dominator_tree = Dominators(self, self.entryID)
        return self.dominator_tree
        
class EnhancedCFG(FlowGraph):    
    def __init__ (self):
        FlowGraph.__init__(self)
        
    def create_from_CFG(self, cfg):
        for v in cfg:
            newv = copy.deepcopy(v) 
            newv.successors   = {}
            newv.predecessors = {}
            self.the_vertices[v.vertexID] = newv
            if v.vertexID == cfg.get_entryID():
                self.entryID = v.vertexID
            if v.vertexID == cfg.get_exitID():
                self.exitID = v.vertexID
        assert self.entryID != vertices.dummyID
        assert self.exitID != vertices.dummyID
        for v in cfg:
            for succID in v.successors.keys():
                newID = self.get_next_edge_vertexID()
                newv  = vertices.CFGEdge(newID, v.vertexID, succID)
                if self.getVertex(v.vertexID).dummy or self.getVertex(succID).dummy:
                    newv.dummy = True
                self.the_vertices[newID] = newv
                self.addEdge(v.vertexID, newID)
                self.addEdge(newID, succID)
                
    def get_next_edge_vertexID(self):
        nextID = -1
        while nextID in self.the_vertices.keys():
            nextID -= 1 
        return nextID                  
        
class CFG(FlowGraph):    
    def __init__ (self):
        FlowGraph.__init__(self)
        self.lnt                   = None
        self.enhanced_CFGs         = {}
        self.reverse_enhanced_CFGs = {}
        self.reachability_info     = {}
        
    def get_LNT(self):
        if self.lnt is None:
            self.lnt = LoopNests(self, self.entryID)
        return self.lnt
    
    def create_per_loop_reachability_info(self):
        lnt = self.get_LNT()
        for the_vertices in lnt.level_by_level_iterator(True):
            for treev in the_vertices:
                if isinstance(treev, vertices.HeaderVertex):
                    enhanced_CFG         = lnt.induce_subgraph_with_tails_and_exits(treev)
                    enhanced_CFG_reverse = enhanced_CFG.get_reverse_graph()
                    udraw.make_file(enhanced_CFG, "%s.header_%d.enhanced_CFG" % (self.name, treev.headerID))
                    self.enhanced_CFGs[treev.headerID]         = enhanced_CFG
                    self.reverse_enhanced_CFGs[treev.headerID] = enhanced_CFG_reverse
                    self.reachability_info[treev.headerID]     = ReachabilityInformation(enhanced_CFG, 
                                                                                         enhanced_CFG_reverse)
                    
    def create_induced_subgraph(self, entry_vertexID, exit_vertexID):
        lnt = self.get_LNT()
        assert lnt.getVertex(entry_vertexID).parentID == lnt.getVertex(exit_vertexID).parentID
        headerv           = lnt.getVertex(lnt.getVertex(entry_vertexID).parentID)
        enhanced_CFG      = self.enhanced_CFGs[headerv.headerID]
        reachability_info = self.reachability_info[headerv.headerID]
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
        induced_CFG = EnhancedCFG()
        for an_edge in edges:
            if not induced_CFG.hasVertex(an_edge[0]):
                v = enhanced_CFG.getVertex(an_edge[0])
                newv = copy.deepcopy(v)
                newv.predecessors = {}
                newv.successors   = {}
                induced_CFG.addVertex(newv)
            if not induced_CFG.hasVertex(an_edge[1]):
                v = enhanced_CFG.getVertex(an_edge[1])
                newv = copy.deepcopy(v)
                newv.predecessors = {}
                newv.successors   = {}
                induced_CFG.addVertex(newv)
            induced_CFG.addEdge(an_edge[0], an_edge[1])
        induced_CFG.entryID = entry_vertexID
        induced_CFG.exitID = exit_vertexID
        return induced_CFG
                        
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
                bb = self.getVertex(bbID)
                debug_info += bb.__str__()
            debug.exit_message("CFG '%s' has too many entry points: %s" % (self.name, debug_info))
        else:
            self.set_entryID(without_predecessors[0])
        
        if len(without_successors) == 0:
            debug.exit_message("CFG '%s' does not have an exit point" % self.name)
        elif len(without_successors) > 1:
            debug_info = ""
            for bbID in without_successors:
                bb = self.getVertex(bbID)
                debug_info += bb.__str__()
            debug.exit_message("CFG '%s' has too many exit points: %s" % (self.name, debug_info))
        else:
            self.set_exitID(without_successors[0])
        assert self.entryID, "Unable to set entry ID"
        assert self.exitID, "Unable to set exit ID"
        self.addEdge(self.exitID, self.entryID)
        
    def set_entryID(self, entryID):
        assert entryID in self.the_vertices, "Cannot find vertex " + str(entryID) + " in vertices"
        assert entryID != vertices.dummyID, "Entry ID " + str(entryID) + " is not positive"
        self.entryID = entryID
        
    def set_exitID(self, exitID):
        assert exitID in self.the_vertices, "Cannot find vertex " + str(exitID) + " in vertices"
        assert exitID != vertices.dummyID, "Exit ID " + str(exitID) + " is not positive"
        self.exitID = exitID
        
class Tree(DirectedGraph):
    def __init__ (self):
        DirectedGraph.__init__(self)
        self.rootID = vertices.dummyID
        
    def addEdge(self, predID, succID):
        DirectedGraph.addEdge(self, predID, succID)
        succv = self.getVertex(succID)
        succv.parentID = predID
    
    def getAllProperAncestors(self, vertexID):
        ancestors = []
        while vertexID != self.rootID:
            parentID = self.getVertex(vertexID).parentID
            ancestors.append(self.getVertex(parentID))
            vertexID = parentID
        return ancestors
    
    def isAncestor(self, left, right):
        if left == right:
            return True
        elif right == self.rootID:
            return False
        else:
            vertexID = right
            parentID = self.getVertex(vertexID).parentID
            while parentID != self.rootID and parentID != left:
                vertexID = parentID
                parentID = self.getVertex(vertexID).parentID
            if parentID == left:
                return True
            else:
                return False
    
    def isProperAncestor(self, left, right):
        if left == right:
            return False
        else:
            return self.isAncestor(left, right)
        
    def level_by_level_iterator(self, up=True):
        rootv        = self.getVertex(self.rootID)
        rootv.level  = 0
        queue        = [rootv]
        the_vertices = {}
        while queue:
            v = queue.pop()
            for succID in v.successors.keys():
                queue.insert(0, self.getVertex(succID))
            if v.vertexID == self.rootID:
                the_vertices[0] = [rootv]
            else:
                v.level = self.getVertex(v.parentID).level + 1
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
        
class DepthFirstSearch (Tree):
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
           
        v = directedg.getVertex(vertexID)
        for succID in v.successors.keys ():
            if self.vertex_pre_order_numbering[succID] == 0:
                self.addEdge(vertexID, succID)
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
                    v               = directedg.getVertex(vertexID)
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
                self.addEdge(self.immediate_dominator[v.vertexID], v.vertexID)
    
class CompressedDominatorTree(Tree):
    def __init__(self, dominator_tree, lca, vertexID, frontierIDs):
        Tree.__init__(self)
        self.build(lca, frontierIDs)
        self.rootID = dominator_tree.getVertex(vertexID).parentID
        
    def build(self, lca, query_set):
        while len(query_set) > 1:
            vertex_to_LCA = {}
            for a_pair in itertools.combinations(query_set, 2):
                lcaID = lca.get_LCA(a_pair[0], a_pair[1])
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
                if not self.hasVertex(vertexID):
                    self.addVertex(vertices.TreeVertex(vertexID))                
                if not self.hasVertex(parentID):
                    self.addVertex(vertices.TreeVertex(parentID))
                if parentID != vertexID:
                    self.addEdge(parentID, vertexID)
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
        v = self.tree.getVertex(vertexID)
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
            
    def get_LCA(self, left, right):    
        debug.debug_message("Computing lca(%d, %d)" % (left, right), 20) 
        lowest_level = self.dummy_level
        level_index  = 2 * self.tree.number_of_vertices()
        if self.representative[left] < self.representative[right]:
            startIndex = self.representative[left]
            endIndex   = self.representative[right]
        else:
            startIndex = self.representative[right]
            endIndex   = self.representative[left]
        for i in range(startIndex, endIndex+1):
            if self.level[i] < lowest_level:
                lowest_level = self.level[i]
                level_index  = i
        debug.debug_message("lca(%d, %d) = %d" % (left, right, self.euler_tour[level_index]), 15)
        return self.euler_tour[level_index]
    
class LoopNests (Tree):
    def __init__(self, directedg, rootID):
        Tree.__init__(self)
        assert rootID in directedg.the_vertices.keys(), "Unable to find vertex %d from which to initiate depth-first search" % rootID
        self.name              = directedg.name
        self.__directedg       = directedg
        self.__dfs             = DepthFirstSearch (self.__directedg, rootID)
        self.__parent          = {}
        self.abstract_vertices = {}
        self.loop_bodies       = {}
        self.loop_back_edges   = {}
        self.loop_exit_edges   = {}
        self.loop_entry_edges  = {}
        self.initialise()
        self.find_loops(rootID)
        self.add_edges()
        self.find_loop_exits()
        self.find_loop_entries()
        # Set the tree root ID to the header vertex representing the root of the 
        # directed graph
        self.rootID = self.abstract_vertices[rootID]
        self.check_is_tree()
        
    def initialise(self):
        for v in self.__directedg:
            self.__parent[v.vertexID] = v.vertexID
            self.the_vertices[v.vertexID] = vertices.TreeVertex(v.vertexID)
            
    def find_loops(self, rootID):
        predom_tree = Dominators(self.__directedg, rootID)
        for vertexID in reversed(self.__dfs.pre_order):
            v = self.__directedg.getVertex(vertexID)
            for predID in v.predecessors.keys():
                if self.__dfs.isDFSBackedge(predID, vertexID):
                    assert predom_tree.isAncestor(vertexID, predID), "Non-reducible loop found with DFS backedge %d => %d" % (predID, vertexID)
                    debug.debug_message("%s => %s is a loop-back edge of non-trivial loop" % (predID, vertexID), __name__, 15)
                    self.add_new_loop(vertexID)
                    self.loop_back_edges[vertexID].add((predID, vertexID))
                    self.find_loop_body(predID, vertexID)
                    
    def add_new_loop(self, headerID, ):
        if headerID not in self.abstract_vertices.keys():
            newID   = self.get_next_vertexID()
            headerv = vertices.HeaderVertex(newID, headerID)
            self.the_vertices[newID]         = headerv
            self.abstract_vertices[headerID] = newID
            self.loop_bodies[headerID]       = set()
            self.loop_back_edges[headerID]   = set()
            self.loop_exit_edges[headerID]   = set()
            self.loop_entry_edges[headerID]  = set()
            
    def add_edges(self):
        for headerID, loop_body in self.loop_bodies.iteritems():
            abstractID = self.abstract_vertices[headerID]
            self.addEdge(abstractID, headerID)
            for vertexID in loop_body:
                if vertexID in self.abstract_vertices.keys():
                    if vertexID != headerID:
                        inner_abstractID = self.abstract_vertices[vertexID]
                        self.addEdge(abstractID, inner_abstractID)
                else:
                    self.addEdge(abstractID, vertexID)        
            
    def find_loop_body(self, tailID, headerID):
        work_list = []
        work_list.append(tailID)
        loop_body = set()
        while work_list:
            listID = work_list.pop()
            loop_body.add(listID)
            v = self.__directedg.getVertex(listID)
            for predID in v.predecessors.keys():
                if not self.__dfs.isDFSBackedge(predID, listID):
                    repID = self.__parent[predID]
                    if repID not in work_list \
                    and repID not in loop_body \
                    and repID != headerID:
                        work_list.append(repID)
        for vertexID in loop_body:
            self.__parent[vertexID] = headerID
        self.loop_bodies[headerID].add(headerID)
        self.loop_bodies[headerID].update(loop_body)             
            
    def find_loop_exits(self):
        for headerID in self.abstract_vertices.keys():
            for vertexID in self.loop_bodies[headerID]:
                v = self.__directedg.getVertex(vertexID)
                for succID in v.successors.keys():
                    if succID not in self.loop_bodies[headerID]:
                        if headerID != vertexID and self.is_loop_header(vertexID):
                            if succID not in self.loop_bodies[vertexID]:
                                self.loop_exit_edges[headerID].add((vertexID, succID))
                        else:
                            self.loop_exit_edges[headerID].add((vertexID, succID))
                            
    def find_loop_entries(self):
        for headerID in self.abstract_vertices.keys():
            v = self.__directedg.getVertex(headerID)
            for predID in v.predecessors.keys():
                if predID not in self.loop_bodies[headerID]:
                    self.loop_entry_edges[headerID].add((predID, headerID))
    
    def get_header_vertices(self):
        for abstractID in self.abstract_vertices.values():
            yield self.getVertex(abstractID)
    
    def is_loop_header(self, vertexID):
        return vertexID in self.abstract_vertices.keys()
    
    def is_loop_tail(self, vertexID):
        for headerID in self.abstract_vertices.keys():
            if vertexID in self.get_loop_tails(headerID):
                return True
        return False
    
    def get_loop_tails(self, headerID):
        assert headerID in self.abstract_vertices.keys(), "Vertex %s is not a loop header" % headerID
        return [edge[0] for edge in self.loop_back_edges[headerID]]
    
    def get_loop_back_edges(self, headerID):
        assert headerID in self.abstract_vertices.keys(), "Vertex %s is not a loop header" % headerID
        return self.loop_back_edges[headerID]
    
    def get_loop_exit_edges(self, headerID):
        assert headerID in self.abstract_vertices.keys(), "Vertex %s is not a loop header" % headerID
        return self.loop_exit_edges[headerID]
    
    def get_loop_exit_sources(self, headerID):
        assert headerID in self.abstract_vertices.keys(), "Vertex %s is not a loop header" % headerID
        return [edge[0] for edge in self.loop_exit_edges[headerID]]
    
    def get_loop_exit_destinations(self, headerID):
        assert headerID in self.abstract_vertices.keys(), "Vertex %s is not a loop header" % headerID
        return [edge[1] for edge in self.loop_exit_edges[headerID]]
    
    def is_loop_exit_source_for_header(self, headerID, vertexID):
        assert headerID in self.abstract_vertices.keys(), "Vertex %s is not a loop header" % headerID
        return vertexID in [edge[0] for edge in self.loop_exit_edges[headerID]]
    
    def is_loop_exit_source(self, vertexID):
        for headerID in self.abstract_vertices.keys():
            if self.is_loop_exit_source_for_header(headerID, vertexID):
                return headerID
        return None
    
    def is_loop_exit_destination_for_header(self, headerID, vertexID):
        assert headerID in self.abstract_vertices.keys(), "Vertex %s is not a loop header" % headerID
        return vertexID in [edge[1] for edge in self.loop_exit_edges[headerID]]
    
    def is_loop_exit_destination(self, vertexID):
        for headerID in self.abstract_vertices.keys():
            if self.is_loop_exit_destination_for_header(headerID, vertexID):
                return headerID
        return None
    
    def get_loop_entry_edges(self, headerID):
        assert headerID in self.abstract_vertices.keys(), "Vertex %s is not a loop header" % headerID
        return self.loop_entry_edges[headerID]
    
    def is_loop_entry_edge(self, predID, succID):
        for headerID in self.abstract_vertices.keys():
            if (predID, succID) in self.loop_entry_edges[headerID]:
                return headerID
        return None
    
    def is_loop_exit_edge_for_header(self, headerID, predID, succID):
        assert headerID in self.abstract_vertices.keys(), "Vertex %s is not a loop header" % headerID
        return (predID, succID) in self.loop_exit_edges[headerID]
    
    def is_loop_exit_edge(self, predID, succID):
        for headerID in self.abstract_vertices.keys():
            if (predID, succID) in self.loop_exit_edges[headerID]:
                return headerID
        return None
    
    def is_loop_back_edge(self, predID, succID):
        for headerID in self.abstract_vertices.keys():
            if (predID, succID) in self.loop_back_edges[headerID]:
                return True
        return False
    
    def is_loop_back_edge_for_header(self, headerID, predID, succID):
        assert headerID in self.abstract_vertices.keys(), "Vertex %s is not a loop header" % headerID
        return (predID, succID) in self.loop_back_edges[headerID]
    
    def is_do_while_loop(self, headerID):
        assert headerID in self.abstract_vertices.keys(), "Vertex %s is not a loop header" % headerID
        exit_sources = set(self.get_loop_exit_sources(headerID))
        tails        = set(self.get_loop_tails(headerID))
        return exit_sources.issubset(tails)
    
    def is_nested(self, left, right):
        return self.isProperAncestor(right, left)
    
    def add_edge_vertices_to_enhanced_CFG(self, headerv, enhanced_CFG, frontier_vertices):
        worklist = []
        analysed = set()
        # Start work list with vertices on the frontier of the analysis
        for vertexID in frontier_vertices:
            v = self.__directedg.getVertex(vertexID)
            if v not in worklist:
                worklist.append(v)
        # Work backwards through flow graph until the header is reached, adding edges found along the way
        while worklist:                        
            v = worklist.pop()
            if v.vertexID not in analysed:
                analysed.add(v.vertexID)
                for predID in v.predecessors.keys():
                    predv = self.__directedg.getVertex(predID)
                    if not self.__dfs.isDFSBackedge(predID, v.vertexID):
                        pred_headerv = self.getVertex(self.getVertex(predID).parentID)
                        if pred_headerv.headerID == headerv.headerID:
                            edge_vertexID = enhanced_CFG.get_next_edge_vertexID()
                            new_edgev     = vertices.CFGEdge(edge_vertexID, predID, v.vertexID)
                            enhanced_CFG.addVertex(new_edgev)
                            worklist.append(predv)
                        elif self.is_nested(pred_headerv.vertexID, headerv.vertexID):
                            worklist.append(self.__directedg.getVertex(pred_headerv.headerID))
        return analysed 
                            
    def add_edge_vertices_for_loop_tails(self, headerv, enhanced_CFG):
        # Add edge vertices to model loop-back edges
        for sourceID, destinationID in self.loop_back_edges[headerv.headerID]:
            edge_vertexID = enhanced_CFG.get_next_edge_vertexID()
            new_edgev     = vertices.CFGEdge(edge_vertexID, sourceID, destinationID)
            enhanced_CFG.addVertex(new_edgev) 
            
    def add_edge_vertices_for_loop_exits(self, headerv, enhanced_CFG):
        # Add edge vertices to model loop-exit edges out of this loop
        for sourceID, destinationID in self.loop_exit_edges[headerv.headerID]:
            edge_vertexID = enhanced_CFG.get_next_edge_vertexID()
            new_edgev     = vertices.CFGEdge(edge_vertexID, sourceID, destinationID)
            enhanced_CFG.addVertex(new_edgev)
            
    def add_edge_vertices_for_inner_loop_exits(self, headerv, enhanced_CFG, analysed_vertices):
        # Add edge vertices to model loop-exit edges out of inner loops
        inner_loop_exit_edges = {}
        for succID in headerv.successors.keys():
            succv = self.getVertex(succID)
            if isinstance(succv, vertices.HeaderVertex):
                if succv.headerID in analysed_vertices:
                    for sourceID, destinationID in self.loop_exit_edges[succv.headerID]:
                        edge_vertexID = enhanced_CFG.get_next_edge_vertexID()
                        new_edgev     = vertices.CFGEdge(edge_vertexID, sourceID, destinationID)
                        enhanced_CFG.addVertex(new_edgev)
                        inner_loop_exit_edges[new_edgev] = succv.headerID
        return inner_loop_exit_edges
    
    def add_vertices_to_enhanced_CFG(self, headerv, enhanced_CFG):
        # Add basic blocks and abstract loop vertices 
        for v in enhanced_CFG:
            assert isinstance(v, vertices.CFGEdge)
            sourceID            = v.edge[0]
            destinationID       = v.edge[1]
            source_headerv      = self.getVertex(self.getVertex(sourceID).parentID)
            destination_headerv = self.getVertex(self.getVertex(destinationID).parentID)
            if source_headerv == headerv:
                if not enhanced_CFG.hasVertex(sourceID):
                    enhanced_CFG.addVertex(vertices.CFGVertex(sourceID))
            if destination_headerv == headerv:
                if not enhanced_CFG.hasVertex(destinationID):
                    enhanced_CFG.addVertex(vertices.CFGVertex(destinationID))
            elif self.is_loop_header(destinationID):
                if not enhanced_CFG.hasVertex(destination_headerv.vertexID):
                    enhanced_CFG.addVertex(vertices.HeaderVertex(destination_headerv.vertexID, destination_headerv.headerID))

    def add_edges_to_enhanced_CFG(self, headerv, enhanced_CFG, inner_loop_exit_edges):
        # Link edges
        for v in enhanced_CFG:
            if isinstance(v, vertices.CFGEdge):
                sourceID      = v.edge[0]
                destinationID = v.edge[1] 
                if self.is_loop_exit_edge(sourceID, destinationID):
                    if self.is_loop_exit_edge_for_header(headerv.headerID, sourceID, destinationID):
                        # Loop-exit edge of this loop
                        enhanced_CFG.addEdge(sourceID, v.vertexID)
                    else:
                        # Loop-exit edge of inner loop  
                        inner_headerID = inner_loop_exit_edges[v]   
                        inner_headerv  = self.getVertex(self.getVertex(inner_headerID).parentID)                       
                        enhanced_CFG.addEdge(inner_headerv.vertexID, v.vertexID)
                        enhanced_CFG.addEdge(v.vertexID, destinationID)
                elif self.is_loop_entry_edge(sourceID, destinationID):
                    inner_headerv = self.getVertex(self.getVertex(destinationID).parentID)
                    enhanced_CFG.addEdge(sourceID, v.vertexID)
                    enhanced_CFG.addEdge(v.vertexID, inner_headerv.vertexID)
                else:
                    enhanced_CFG.addEdge(sourceID, v.vertexID)
                    if not self.is_loop_back_edge_for_header(headerv.headerID, sourceID, destinationID):
                        enhanced_CFG.addEdge(v.vertexID, destinationID)

    def set_entry_and_exit_in_enhanced_CFG(self, headerv, enhanced_CFG):
        # Set entry vertex
        enhanced_CFG.entryID = headerv.headerID
        # Set exit vertex
        exit_candidates = []
        for v in enhanced_CFG:
            if v.number_of_successors() == 0:
                exit_candidates.append(v)      
        if headerv.vertexID == self.rootID:
            assert len(exit_candidates) == 1
            enhanced_CFG.exitID = exit_candidates[0].vertexID
        else:
            assert len(exit_candidates) > 0
            if len(exit_candidates) == 1:
                enhanced_CFG.exitID = exit_candidates[0].vertexID
            else:
                exitv       = vertices.CFGVertex(enhanced_CFG.get_next_vertexID())
                exitv.dummy = True
                enhanced_CFG.addVertex(exitv)
                enhanced_CFG.exitID = exitv.vertexID
                for v in exit_candidates:
                    enhanced_CFG.addEdge(v.vertexID, exitv.vertexID)

    def induce_subgraph_with_exits_only(self, headerv):
        assert isinstance(headerv, vertices.HeaderVertex), "To induce a region of a loop, you must pass an internal vertex of the LNT"
        enhanced_CFG = EnhancedCFG()
        analysed_vertices = self.add_edge_vertices_to_enhanced_CFG(headerv, enhanced_CFG, self.get_loop_exit_sources(headerv.headerID))
        self.add_edge_vertices_for_loop_exits(headerv, enhanced_CFG)
        inner_loop_exit_edges = self.add_edge_vertices_for_inner_loop_exits(headerv, enhanced_CFG, analysed_vertices)
        self.add_vertices_to_enhanced_CFG(headerv, enhanced_CFG)
        self.add_edges_to_enhanced_CFG(headerv, enhanced_CFG, inner_loop_exit_edges)
        self.set_entry_and_exit_in_enhanced_CFG(headerv, enhanced_CFG)    
        return enhanced_CFG
    
    def induce_subgraph_with_tails_only(self, headerv):
        assert isinstance(headerv, vertices.HeaderVertex), "To induce a region of a loop, you must pass an internal vertex of the LNT"
        enhanced_CFG = EnhancedCFG()
        analysed_vertices = self.add_edge_vertices_to_enhanced_CFG(headerv, enhanced_CFG, self.get_loop_tails(headerv.headerID))
        self.add_edge_vertices_for_loop_tails(headerv, enhanced_CFG)
        inner_loop_exit_edges = self.add_edge_vertices_for_inner_loop_exits(headerv, enhanced_CFG, analysed_vertices)
        self.add_vertices_to_enhanced_CFG(headerv, enhanced_CFG)
        self.add_edges_to_enhanced_CFG(headerv, enhanced_CFG, inner_loop_exit_edges)
        self.set_entry_and_exit_in_enhanced_CFG(headerv, enhanced_CFG)    
        return enhanced_CFG
              
    def induce_subgraph_with_tails_and_exits(self, headerv):
        assert isinstance(headerv, vertices.HeaderVertex), "To induce a region of a loop, you must pass an internal vertex of the LNT"
        enhanced_CFG = EnhancedCFG()
        analysed_vertices = self.add_edge_vertices_to_enhanced_CFG(headerv, enhanced_CFG, self.get_loop_tails(headerv.headerID))
        self.add_edge_vertices_for_loop_tails(headerv, enhanced_CFG)
        self.add_edge_vertices_for_loop_exits(headerv, enhanced_CFG)
        inner_loop_exit_edges = self.add_edge_vertices_for_inner_loop_exits(headerv, enhanced_CFG, analysed_vertices)
        self.add_vertices_to_enhanced_CFG(headerv, enhanced_CFG)
        self.add_edges_to_enhanced_CFG(headerv, enhanced_CFG, inner_loop_exit_edges)
        self.set_entry_and_exit_in_enhanced_CFG(headerv, enhanced_CFG)    
        return enhanced_CFG
    
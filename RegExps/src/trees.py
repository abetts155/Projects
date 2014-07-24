import directed_graphs
import vertices
import debug
import utils

class Tree(directed_graphs.DirectedGraph):
    def __init__ (self):
        directed_graphs.DirectedGraph.__init__(self)
        self.rootID = vertices.dummyID
        
    def getRootID (self):
        assert self.rootID != vertices.dummyID, "Root ID has not yet been set"
        return self.rootID
        
    def addEdge(self, predID, succID):
        directed_graphs.DirectedGraph.addEdge(self, predID, succID)
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
        rootv        = self.getVertex(self.getRootID())
        rootv.level  = 0
        queue        = [rootv]
        the_vertices = {}
        while queue:
            v = queue.pop()
            for succID in v.successors.keys():
                queue.insert(0, self.getVertex(succID))
            if v.vertexID == self.getRootID():
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
        
class Dominators (Tree):
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
    def __init__(self, dominator_tree, lca, vertexID, neighbourIDs):
        Tree.__init__(self)
        self.rootID = dominator_tree.immediate_dominator[vertexID]
        self.build(lca, neighbourIDs)
        
    def build(self, lca, neighbourIDs):
        query_set = neighbourIDs     
        while len(query_set) > 1:
            vertex_to_lca = {}
            for v1 in query_set:
                for v2 in query_set:
                    if v1 != v2:
                        lcaID = lca.getLCA(v1, v2)
                        if v1 in vertex_to_lca:
                            old_lcaID = vertex_to_lca[v1]
                            if lca.vertex_to_level[lcaID] > lca.vertex_to_level[old_lcaID] \
                            and old_lcaID != v1:
                                vertex_to_lca[v1] = lcaID
                        else:
                            vertex_to_lca[v1] = lcaID
                        if v2 in vertex_to_lca:
                            old_lcaID = vertex_to_lca[v2]
                            if lca.vertex_to_level[lcaID] > lca.vertex_to_level[old_lcaID] \
                            and old_lcaID != v2:
                                vertex_to_lca[v2] = lcaID
                        else:
                            vertex_to_lca[v2] = lcaID
            # Add edge links                
            for vertexID, parentID in vertex_to_lca.iteritems():
                if not self.hasVertex(vertexID):
                    self.addVertex(vertexID)                
                if not self.hasVertex(parentID):
                    self.addVertex(parentID)
                if parentID != vertexID:
                    self.addEdge(parentID, vertexID)
            # Any vertex without a predecessor goes into the query set
            new_query_set = set()
            for v in self:
                if v.number_of_predecessors() == 0:
                    new_query_set.append(v.vertexID)
            query_set = new_query_set
    
class LeastCommonAncestor:
    def __init__(self, tree):
        self.tree            = tree
        self.euler           = {}
        self.level           = {}
        self.vertex_to_level = {}
        self.representative  = {}
        self.euler_index     = 0
        self.dummy_level     = 0
        self.vertex_to_level[self.tree.rootID] = 0
        self.do_DFS(self.tree.rootID)
        self.dummy_level += 1
        self.compute_representative_indices()
        
    def do_DFS(self, vertexID):
        v = self.tree.getVertex(vertexID)
        self.euler[self.euler_index] = vertexID
        self.euler_index += 1
        for succID in v.getSuccessorIDs():
            self.vertex_to_level[succID] = self.vertex_to_level[vertexID] + 1
            if self.vertex_to_level[succID] > self.dummy_level:
                self.dummy_level = self.vertex_to_level[succID]
            self.__doDFS(succID)
            self.euler[self.euler_index] = vertexID
            self.euler_index += 1
    
    def compute_representative_indices(self):
        for index, vertexID in self.euler.iteritems():
            self.representative[vertexID] = index
            self.level[index]             = self.vertex_to_level[vertexID]
        
    def get_LCA(self, left, right):    
        if self.representative[left] < self.representative[right]:
            start_index = self.representative[left]
            end_index   = self.representative[right]
        else:
            start_index = self.representative[right]
            end_index   = self.representative[left]
        level_index  = 2 * self.tree.number_of_vertices()
        lowest_level = self.dummy_level
        for i in range(start_index, end_index+1):
            if self.level[i] < lowest_level:
                lowest_level = self.level[i]
                level_index  = i
        return self.euler[level_index]
    
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
        self.find_loop_exits()
        self.find_loop_entries()
        # Set the tree root ID to the header vertex representing the root of the 
        # directed graph
        self.rootID = self.abstract_vertices[rootID]
        
    def initialise(self):
        for v in self.__directedg:
            self.__parent[v.vertexID] = v.vertexID
            self.the_vertices[v.vertexID] = vertices.TreeVertex(v.vertexID)
            
    def find_loops(self, rootID):
        predom_tree = Dominators(self.__directedg, rootID)
        for vertexID in reversed(self.__dfs.pre_order):
            v           = self.__directedg.getVertex(vertexID)
            loop_tailID = None
            for predID in v.predecessors.keys():
                if self.__dfs.isDFSBackedge(predID, vertexID):
                    assert predom_tree.isAncestor(vertexID, predID), "Non-reducible loop found with DFS backedge %d => %d" % (predID, vertexID)
                    debug.debug_message("%s => %s is a loop-back edge of non-trivial loop" % (predID, vertexID), __name__, 15)
                    loop_tailID = self.__parent[predID]
            if loop_tailID is not None:
                self.find_loop_body(loop_tailID, vertexID)
            
    def find_loop_body(self, loop_tailID, headerID):
        if headerID not in self.abstract_vertices.keys():
            newID   = self.get_next_vertexID()
            headerv = vertices.HeaderVertex(newID, headerID)
            self.the_vertices[newID]         = headerv
            self.abstract_vertices[headerID] = newID
            self.loop_bodies[headerID]       = set()
            self.loop_back_edges[headerID]   = set()
            self.loop_exit_edges[headerID]   = set()
            self.loop_entry_edges[headerID]  = set()
        self.loop_back_edges[headerID].add((loop_tailID, headerID))
        work_list = []
        work_list.append(loop_tailID)
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
        if loop_body:
            parentID = self.abstract_vertices[headerID]
            self.addEdge(parentID, headerID)
            for vertexID in loop_body:
                self.__parent[vertexID] = headerID
                if vertexID in self.abstract_vertices.keys():
                    childID = self.abstract_vertices[vertexID]
                    self.addEdge(parentID, childID)
                else:
                    self.addEdge(parentID, vertexID)
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
                return True
        return False
    
    def is_loop_exit_destination_for_header(self, headerID, vertexID):
        assert headerID in self.abstract_vertices.keys(), "Vertex %s is not a loop header" % headerID
        return vertexID in [edge[1] for edge in self.loop_exit_edges[headerID]]
    
    def is_loop_exit_destination(self, vertexID):
        for headerID in self.abstract_vertices.keys():
            if self.is_loop_exit_destination_for_header(headerID, vertexID):
                return True
        return False
    
    def get_loop_entry_edges(self, headerID):
        assert headerID in self.abstract_vertices.keys(), "Vertex %s is not a loop header" % headerID
        return self.loop_entry_edges[headerID]
    
    def is_loop_entry_edge(self, predID, succID):
        for headerID in self.abstract_vertices.keys():
            if (predID, succID) in self.loop_entry_edges[headerID]:
                return True
        return False
    
    def is_loop_exit_edge(self, predID, succID):
        for headerID in self.abstract_vertices.keys():
            if (predID, succID) in self.loop_exit_edges[headerID]:
                return True
        return False
    
    def is_loop_back_edge(self, predID, succID):
        for headerID in self.abstract_vertices.keys():
            if (predID, succID) in self.loop_back_edges[headerID]:
                return True
        return False
    
    def is_loop_back_edge_for_header(self, headerID, predID, succID):
        assert headerID in self.abstract_vertices.keys(), "Vertex %s is not a loop header" % headerID
        return (predID, succID) in self.loop_back_edges[headerID]
    
    def is_nested(self, left, right):
        return self.isProperAncestor(right, left)             
    
    def induced_loop_subgraph(self, headerv):
        assert isinstance(headerv, vertices.HeaderVertex), "To induce a region of a loop, you must pass an internal vertex of the LNT"
        enhanced_CFG                     = directed_graphs.EnhancedCFG()
        worklist                         = []
        analysed                         = set()
        inner_loop_exit_edge_header      = {}
        inner_loop_exit_edge_edge_vertex = {} 
        # Start work list with loop tails
        for predID in self.get_loop_tails(headerv.headerID):
            predv = self.__directedg.getVertex(predID)
            if predv not in worklist:
                worklist.append(predv)
        # Work backwards through flow graph until the header is reached, adding edges found along the way
        while worklist:                        
            v = worklist.pop()
            if v not in analysed:
                analysed.add(v)
                for predID in v.predecessors.keys():
                    predv = self.__directedg.getVertex(predID)
                    if not self.__dfs.isDFSBackedge(predID, v.vertexID):
                        headerv_of_pred = self.getVertex(self.getVertex(predID).parentID)
                        if headerv_of_pred.headerID == headerv.headerID:
                            edge_vertexID = enhanced_CFG.get_next_edge_vertexID()
                            new_edgev     = vertices.CFGEdge(edge_vertexID, predID, v.vertexID)
                            enhanced_CFG.addVertex(new_edgev)
                            worklist.append(predv)
                        elif self.is_nested(headerv_of_pred.vertexID, headerv.vertexID):
                            worklist.append(self.__directedg.getVertex(headerv_of_pred.headerID))
                            for sourceID, destinationID in self.loop_exit_edges[headerv_of_pred.headerID]:
                                edge_vertexID = enhanced_CFG.get_next_edge_vertexID()
                                new_edgev     = vertices.CFGEdge(edge_vertexID, sourceID, destinationID)
                                enhanced_CFG.addVertex(new_edgev)                                
                                inner_loop_exit_edge_header[(sourceID, destinationID)] = headerv_of_pred.headerID
                                inner_loop_exit_edge_edge_vertex[(sourceID, destinationID)] = new_edgev      
        # Add edge vertices to model loop-back edges
        for sourceID, destinationID in self.loop_back_edges[headerv.headerID]:
            edge_vertexID = enhanced_CFG.get_next_edge_vertexID()
            new_edgev     = vertices.CFGEdge(edge_vertexID, sourceID, destinationID)
            enhanced_CFG.addVertex(new_edgev) 
        # Add edge vertices to model loop-exit edges
        for sourceID, destinationID in self.loop_exit_edges[headerv.headerID]:
            edge_vertexID = enhanced_CFG.get_next_edge_vertexID()
            new_edgev     = vertices.CFGEdge(edge_vertexID, sourceID, destinationID)
            enhanced_CFG.addVertex(new_edgev) 
        # Link inner headers to vertices which model their loop-exit edges
        for edge, inner_headerID in inner_loop_exit_edge_header.iteritems():
            if not enhanced_CFG.hasVertex(inner_headerID):
                enhanced_CFG.addVertex(vertices.CFGVertex(inner_headerID))
            new_edgev = inner_loop_exit_edge_edge_vertex[edge]
            enhanced_CFG.addEdge(inner_headerID, new_edgev.vertexID)
        # Link remaining edges
        for v in enhanced_CFG:
            if isinstance(v, vertices.CFGEdge):
                sourceID      = v.edge[0]
                destinationID = v.edge[1]
                if not enhanced_CFG.hasVertex(sourceID) and self.getVertex(self.getVertex(sourceID).parentID).headerID == headerv.headerID:
                    enhanced_CFG.addVertex(vertices.CFGVertex(sourceID))
                if not enhanced_CFG.hasVertex(destinationID) and self.getVertex(self.getVertex(destinationID).parentID).headerID == headerv.headerID:
                    enhanced_CFG.addVertex(vertices.CFGVertex(destinationID))
                if self.getVertex(self.getVertex(sourceID).parentID).headerID == headerv.headerID:
                    enhanced_CFG.addEdge(sourceID, v.vertexID)
                if self.getVertex(self.getVertex(destinationID).parentID).headerID == headerv.headerID or self.is_loop_header(destinationID):
                    if not self.is_loop_back_edge(sourceID, destinationID):
                        enhanced_CFG.addEdge(v.vertexID, destinationID)
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
        return enhanced_CFG
    
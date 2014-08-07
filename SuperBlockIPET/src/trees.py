import directed_graphs
import vertices
import debug
import numpy
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
    
    def get_random_loop_bounds(self):
        upper_bounds = {}
        reverseg     = self.__directedg.get_reverse_graph() 
        for the_vertices in self.level_by_level_iterator(False):
            for treev in the_vertices:
                if isinstance(treev, vertices.HeaderVertex):
                    if treev.level == 0:
                        upper_bounds[treev.headerID] = (1,)
                    elif treev.level == 1:
                        upper_bounds[treev.headerID] = (numpy.random.randint(1, 20),)
                    else:                            
                        parentv = self.getVertex(treev.parentID)
                        exit_path_vertices = self.get_vertices_on_exits_paths(parentv, reverseg)
                        upper_bound = ()
                        for number_of_iterations in upper_bounds[parentv.headerID]:
                            for i in range(1, number_of_iterations+1):
                                if i == number_of_iterations:
                                    if treev.headerID in exit_path_vertices:
                                        upper_bound += (numpy.random.randint(1, 20),)
                                    else:
                                        upper_bound += (0,)
                                else:
                                    upper_bound += (numpy.random.randint(1, 20),)
                        upper_bounds[treev.headerID] = upper_bound
        return upper_bounds
    
    def get_vertices_on_exits_paths(self, headerv, reverseg):
        assert isinstance(headerv, vertices.HeaderVertex), "To induce a region of a loop, you must pass an internal vertex of the LNT"
        analysed = set()
        stack    = []
        for vertexID in self.get_loop_exit_sources(headerv.headerID):
            v = reverseg.getVertex(vertexID)
            if v not in stack:
                stack.append(v)
        while stack:
            v = stack.pop()
            analysed.add(v.vertexID)
            if v.vertexID != headerv.headerID:
                for succID in v.successors.keys():
                    if succID not in analysed:
                        stack.append(reverseg.getVertex(succID))
        return analysed
    
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
            if v not in analysed:
                analysed.add(v)
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
            
    def add_edge_vertices_for_inner_loop_exits(self, headerv, enhanced_CFG):
        # Add edge vertices to model loop-exit edges out of inner loops
        inner_loop_exit_edges = {}
        for succID in headerv.successors.keys():
            succv = self.getVertex(succID)
            if isinstance(succv, vertices.HeaderVertex):
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

    def induce_loop_exit_subgraph(self, headerv):
        assert isinstance(headerv, vertices.HeaderVertex), "To induce a region of a loop, you must pass an internal vertex of the LNT"
        enhanced_CFG = directed_graphs.EnhancedCFG()
        self.add_edge_vertices_to_enhanced_CFG(headerv, enhanced_CFG, self.get_loop_exit_sources(headerv.headerID))
        self.add_edge_vertices_for_loop_exits(headerv, enhanced_CFG)
        inner_loop_exit_edges = self.add_edge_vertices_for_inner_loop_exits(headerv, enhanced_CFG)
        self.add_vertices_to_enhanced_CFG(headerv, enhanced_CFG)
        self.add_edges_to_enhanced_CFG(headerv, enhanced_CFG, inner_loop_exit_edges)
        self.set_entry_and_exit_in_enhanced_CFG(headerv, enhanced_CFG)    
        return enhanced_CFG
    
    def induce_loop_subgraph_without_loop_exits(self, headerv):
        assert isinstance(headerv, vertices.HeaderVertex), "To induce a region of a loop, you must pass an internal vertex of the LNT"
        enhanced_CFG = directed_graphs.EnhancedCFG()
        self.add_edge_vertices_to_enhanced_CFG(headerv, enhanced_CFG, self.get_loop_tails(headerv.headerID))
        self.add_edge_vertices_for_loop_tails(headerv, enhanced_CFG)
        inner_loop_exit_edges = self.add_edge_vertices_for_inner_loop_exits(headerv, enhanced_CFG)
        self.add_vertices_to_enhanced_CFG(headerv, enhanced_CFG)
        self.add_edges_to_enhanced_CFG(headerv, enhanced_CFG, inner_loop_exit_edges)
        self.set_entry_and_exit_in_enhanced_CFG(headerv, enhanced_CFG)    
        return enhanced_CFG
              
    def induce_loop_subgraph(self, headerv):
        assert isinstance(headerv, vertices.HeaderVertex), "To induce a region of a loop, you must pass an internal vertex of the LNT"
        enhanced_CFG = directed_graphs.EnhancedCFG()
        self.add_edge_vertices_to_enhanced_CFG(headerv, enhanced_CFG, self.get_loop_tails(headerv.headerID))
        self.add_edge_vertices_for_loop_tails(headerv, enhanced_CFG)
        self.add_edge_vertices_for_loop_exits(headerv, enhanced_CFG)
        inner_loop_exit_edges = self.add_edge_vertices_for_inner_loop_exits(headerv, enhanced_CFG)
        self.add_vertices_to_enhanced_CFG(headerv, enhanced_CFG)
        self.add_edges_to_enhanced_CFG(headerv, enhanced_CFG, inner_loop_exit_edges)
        self.set_entry_and_exit_in_enhanced_CFG(headerv, enhanced_CFG)    
        return enhanced_CFG
    
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
        
    def addVertex (self, vertexID):
        assert vertexID not in self.the_vertices, "Adding vertex %d which is already in tree" % vertexID
        treev = vertices.TreeVertex(vertexID)
        self.the_vertices[vertexID] = treev
        
    def addEdge (self, predID, succID):
        directed_graphs.DirectedGraph.addEdge(self, predID, succID)
        succv = self.getVertex(succID)
        succv.parentID = predID
    
    def getAllProperAncestors (self, vertexID):
        ancestors = []
        while vertexID != self.rootID:
            parentID = self.getVertex(vertexID).parentID
            ancestors.append(self.getVertex(parentID))
            vertexID = parentID
        return ancestors
    
    def isAncestor (self, left, right):
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
    
    def isProperAncestor (self, left, right):
        if left == right:
            return False
        else:
            return self.isAncestor(left, right)
        
    def levelIterator(self, up=True):
        rootv = self.getVertex(self.getRootID())
        rootv.level = 0
        queue = [rootv]
        levelToVertices = {}
        while queue:
            v = queue.pop()
            for succID in v.successors.keys():
                queue.insert(0, self.getVertex(succID))
            if v.vertexID == self.getRootID():
                levelToVertices[0] = [rootv]
            else:
                v.level = self.getVertex(v.parentID).level + 1
                if v.level not in levelToVertices.keys():
                    levelToVertices[v.level] = []
                levelToVertices[v.level].append(v)
        if up:
            for level in reversed(sorted(levelToVertices.keys())):
                yield level, levelToVertices[level]
        else:
            for level in sorted(levelToVertices.keys()):
                yield level, levelToVertices[level]
        
class DepthFirstSearch (Tree):
    Colors = utils.enum('WHITE', 'BLACK', 'GRAY')

    def __init__(self, directedg, rootID):
        Tree.__init__(self)
        assert rootID in directedg.the_vertices.keys(), "Unable to find vertex %d from which to initiate depth-first search" % rootID
        self.rootID = rootID
        self.pre_order  = []
        self.post_order = []
        self.__vertexID2PostID = {}
        self.__vertexID2PreID = {}
        self.back_edges = []
        self.initialise(directedg, rootID)
        self.do_search(directedg, rootID)
        
    def initialise(self, directedg, rootID):
        for v in directedg:
            self.the_vertices[v.vertexID] = vertices.TreeVertex(v.vertexID)
            self.__vertexID2PreID[v.vertexID] = 0
            self.__vertexID2PostID[v.vertexID] = 0
        self.pre_orderID  = 1
        self.post_orderID = 1   
      
    def do_search(self, directedg, vertexID):
        self.__vertexID2PreID[vertexID] = self.pre_orderID
        self.pre_order.append(vertexID)
        self.pre_orderID += 1
           
        v = directedg.getVertex(vertexID)
        for succID in v.successors.keys ():
            if self.__vertexID2PreID[succID] == 0:
                self.addEdge(vertexID, succID)
                self.do_search(directedg, succID)
            elif self.__vertexID2PreID[vertexID] < self.__vertexID2PreID[succID]:
                pass
            elif self.__vertexID2PostID[succID] == 0:
                self.back_edges.append((vertexID, succID))
        self.__vertexID2PostID[vertexID] = self.post_orderID
        self.post_order.append(vertexID)
        self.post_orderID += 1
    
    def getPreorderVertexID (self, preID):
        assert preID - 1 < len(self.pre_order), "Pre-order number %d too high" % preID
        return self.pre_order[preID-1]
    
    def getPostorderVertexID (self, postID):
        assert postID - 1 < len(self.post_order), "Post-order number %d too high" % postID
        return self.post_order[postID-1]
    
    def getPreID (self, vertexID):
        assert vertexID in self.__vertexID2PreID, "Unable to find pre-order numbering for vertex %d" % vertexID
        return self.__vertexID2PreID[vertexID]
    
    def getPostID (self, vertexID):
        assert vertexID in self.__vertexID2PostID, "Unable to find post-order numbering for vertex %d" % vertexID
        return self.__vertexID2PostID[vertexID]
    
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
        self.initialise()
        self.find_loops(rootID)
        self.find_loop_exits()
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
            newVertexID = self.getNextVertexID()
            headerv     = vertices.HeaderVertex(newVertexID, headerID)
            self.the_vertices[newVertexID] = headerv
            self.abstract_vertices[headerID] = newVertexID
            self.loop_bodies[headerID]       = set()
            self.loop_back_edges[headerID]   = set()
            self.loop_exit_edges[headerID]   = set()
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
            debug.debug_message("Exits of %s = %s" % (headerID, self.loop_exit_edges[headerID]), __name__, 10)
            
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
    
    def get_loop_exits_edges(self, headerID):
        assert headerID in self.abstract_vertices.keys(), "Vertex %s is not a loop header" % headerID
        return self.loop_exit_edges[headerID]
    
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
    
    def is_nested(self, left, right):
        return self.isProperAncestor(right, left)
    
    def induce_subgraph(self, headerv):
        assert isinstance(headerv, vertices.HeaderVertex), "To induce the acyclic portion of a loop body, you must pass an internal vertex of the LNT"
        edges    = set()
        analysed = set()
        worklist = []
        worklist.extend(self.get_loop_tails(headerv.headerID))
        while worklist:                        
            vertexID = worklist.pop()
            analysed.add(vertexID)
            v = self.__directedg.getVertex(vertexID)
            for predID in v.predecessors.keys():
                if not self.__dfs.isDFSBackedge(predID, vertexID):
                    treePredv    = self.getVertex(predID)
                    headerPredv  = self.getVertex(treePredv.parentID)
                    predHeaderID = headerPredv.headerID
                    if predHeaderID == headerv.headerID:
                        if predID not in analysed:
                            worklist.append(predID)
                        edges.add((predID, vertexID))
                    elif self.is_nested(headerPredv.vertexID, headerv.vertexID):
                        if predHeaderID not in analysed:
                            worklist.append(predHeaderID)
                        for sourceID, destinationID in self.get_loop_exits_edges(predHeaderID):
                            edges.add((sourceID, destinationID))
                            if predHeaderID != sourceID:
                                edges.add((predHeaderID, sourceID))
            # Add loop-exit edges to outer loops originating at this vertex
            for succID in v.successors.keys():
                treeSuccv    = self.getVertex(succID)
                headerSuccv  = self.getVertex(treeSuccv.parentID)
                succHeaderID = headerSuccv.headerID
                if succHeaderID != headerv.headerID and self.is_nested(headerv.vertexID, headerSuccv.vertexID):
                    edges.add((vertexID, succID))
                
        flowg = directed_graphs.CFG()
        for predID, succID in edges:
            if not flowg.hasVertex(predID):
                v = vertices.CFGVertex(predID)
                flowg.addVertex(v)
            if not flowg.hasVertex(succID):
                v = vertices.CFGVertex(succID)
                flowg.addVertex(v)
            flowg.addEdge(predID, succID)
        # Set the entry vertex in the induced subgraph
        flowg.set_entryID(headerv.headerID)
        # Set the exit vertex in the induced subgraph
        loop_tails = self.get_loop_tails(headerv.headerID)
        if headerv.vertexID == self.rootID:
            assert len(loop_tails) == 1
            flowg.set_exitID(loop_tails[0])
        else:
            exitv       = vertices.CFGVertex(flowg.getNextVertexID())
            exitv.dummy = True
            flowg.addVertex(exitv)
            flowg.set_exitID(exitv.vertexID)
            for v in flowg:
                if v.number_of_successors() == 0 and v.vertexID != flowg.exitID:
                    flowg.addEdge(v.vertexID, exitv.vertexID)      
            duplicatev       = vertices.CFGVertex(flowg.getNextVertexID())  
            duplicatev.dummy = True
            flowg.addVertex(duplicatev)
            flowg.addEdge(duplicatev.vertexID, exitv.vertexID)      
            for loop_tailID in loop_tails:
                flowg.addEdge(loop_tailID, duplicatev.vertexID)  
        return flowg

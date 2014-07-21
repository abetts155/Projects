import directed_graphs
import trees
import vertices
import debug
import utils
import udraw

class SuperBlockSubgraph(directed_graphs.DirectedGraph):
    def __init__(self):
        directed_graphs.DirectedGraph.__init__(self)
        self.program_point_to_superv = {}

class SuperBlockCFG(directed_graphs.DirectedGraph):    
    def __init__(self, cfg, lnt):
        directed_graphs.DirectedGraph.__init__(self)    
        self.name = cfg.name
        self.per_loop_subgraphs     = {}
        self.per_loop_root_vertices = {}
        for the_vertices in lnt.level_by_level_iterator(True):
            for treev in the_vertices:
                if isinstance(treev, vertices.HeaderVertex):
                    debug.debug_message("Analysing header %d" % treev.headerID, __name__, 1)
                    enhanced_CFG          = lnt.induced_loop_subgraph(treev)
                    enhanced_CFG.patch_back_edges_with_correct_header(lnt.get_loop_tails(treev.headerID), treev.headerID)
                    udraw.make_file(enhanced_CFG, "%s.header_%d.enhanced_CFG" % (cfg.name, treev.headerID))
                    predom_tree          = trees.Dominators(enhanced_CFG, enhanced_CFG.get_entryID())
                    enhanced_CFG_reverse = enhanced_CFG.get_reverse_graph()
                    postdom_tree         = trees.Dominators(enhanced_CFG_reverse, enhanced_CFG_reverse.get_entryID())
                    dominator_graph      = DominatorGraph(predom_tree, postdom_tree)
                    sccs                 = StrongComponents(dominator_graph)  
                    self.add_super_blocks(lnt, enhanced_CFG, sccs, treev)     
                    self.add_edges(lnt, enhanced_CFG, treev)
                        
    def add_super_blocks(self, lnt, enhanced_CFG, sccs, headerv):
        subgraph                                  = SuperBlockSubgraph()
        self.per_loop_subgraphs[headerv.headerID] = subgraph
        for sccID in sccs.scc_vertices.keys():
            superv                                 = vertices.SuperBlock(sccID)
            self.the_vertices[superv.vertexID]     = superv
            subgraph.the_vertices[superv.vertexID] = superv
        dfs = trees.DepthFirstSearch(enhanced_CFG, enhanced_CFG.get_entryID())
        for vertexID in reversed(dfs.post_order):
            program_point = enhanced_CFG.getVertex(vertexID)
            if not program_point.dummy: 
                sccID  = sccs.vertex_SCC[vertexID]
                superv = subgraph.getVertex(sccID)
                if isinstance(program_point, vertices.CFGEdge):
                    predv_headerv = lnt.getVertex(lnt.getVertex(program_point.edge[0]).parentID)
                    succv_headerv = lnt.getVertex(lnt.getVertex(program_point.edge[1]).parentID)
                    if predv_headerv == headerv or succv_headerv == headerv:
                        subgraph.program_point_to_superv[program_point.edge] = superv
                        superv.program_points.append(program_point)
                else:
                    basic_block_headerv = lnt.getVertex(lnt.getVertex(program_point.vertexID).parentID)
                    if basic_block_headerv == headerv:
                        subgraph.program_point_to_superv[program_point.vertexID] = superv
                        superv.program_points.append(program_point)
        for superv in self:
            superv.compute_representative()
                
    def add_edges(self, lnt, enhanced_CFG, headerv):
        subgraph = self.per_loop_subgraphs[headerv.headerID]
        for superv in subgraph:
            first_program_point = superv.program_points[0]
            last_program_point  = superv.program_points[-1]
            if isinstance(first_program_point, vertices.CFGEdge):
                # The program point represents a CFG edge.
                # Find the super block which contains the source of the CFG edge 
                # and link the super blocks
                basic_block_predID = first_program_point.edge[0]
                if lnt.getVertex(lnt.getVertex(basic_block_predID).parentID) == headerv:
                    pred_superv        = subgraph.program_point_to_superv[basic_block_predID] 
                    self.addEdge(pred_superv.vertexID, superv.vertexID)
                    assert enhanced_CFG.getVertex(basic_block_predID).number_of_successors() > 1
                    if basic_block_predID not in pred_superv.successor_partitions:
                        pred_superv.successor_partitions[basic_block_predID] = set()
                    pred_superv.successor_partitions[basic_block_predID].add(superv.vertexID)
            else:
                # The program point represents a CFG vertex.
                # Find the CFG edges incident to the CFG vertex.
                # Then find the super blocks containing those CFG edges
                basic_block = enhanced_CFG.getVertex(first_program_point.vertexID)
                for predID in basic_block.predecessors.keys():
                    predv = enhanced_CFG.getVertex(predID)
                    assert isinstance(predv, vertices.CFGEdge)
                    pred_superv = subgraph.program_point_to_superv[predv.edge]
                    self.addEdge(pred_superv.vertexID, superv.vertexID)
            if isinstance(last_program_point, vertices.CFGEdge): 
                if lnt.is_loop_back_edge(last_program_point.edge[0], last_program_point.edge[1]) and headerv.vertexID != lnt.rootID:
                    # The program point represents a CFG loop-back edge. (We ignore the dummy loop-back edge)
                    # Find the super block which contains the destination of the CFG edge 
                    # and link the super blocks
                    basic_block_succID = last_program_point.edge[1]
                    succ_superv        = subgraph.program_point_to_superv[basic_block_succID] 
                    self.addEdge(superv.vertexID, succ_superv.vertexID)
        for succID in headerv.successors.keys():
            succv = lnt.getVertex(succID)
            if isinstance(succv, vertices.HeaderVertex):
                inner_subgraph = self.per_loop_subgraphs[succv.headerID]
                # Re-link super blocks created in inner loops which contain loop-exit edges to the current loop
                for exit_edge in lnt.get_loop_exit_edges(succv.headerID):
                    # Find the super block containing the loop-exit edge in the 
                    # subgraph created for the inner loop
                    exit_edge_superv = inner_subgraph.program_point_to_superv[exit_edge]
                    assert exit_edge_superv.number_of_predecessors() == 1
                    # It should have a unique predecessor.
                    # We want to re-link that predecessor to the super block containing the loop-exit 
                    # edge in the subgraph created for its outer loop
                    exit_edge_pred_superv = self.getVertex(exit_edge_superv.predecessors.keys()[0])
                    succ_superv           = subgraph.program_point_to_superv[exit_edge]
                    self.addEdge(exit_edge_pred_superv.vertexID, succ_superv.vertexID) 
                    # Fix the successor partition information
                    basic_block_predID = exit_edge[0]
                    exit_edge_pred_superv.successor_partitions[basic_block_predID].add(succ_superv.vertexID)
                    exit_edge_pred_superv.successor_partitions[basic_block_predID].remove(exit_edge_superv.vertexID)
                    # And now delete the super block containing the loop-exit edge in the 
                    # subgraph created for the inner loop
                    self.removeVertex(exit_edge_superv.vertexID)
                # Add loop-entry edges into the inner loop
                for entry_edge in lnt.get_loop_entry_edges(succv.headerID):
                    # The program point represents a CFG loop-entry edge.
                    # Find the super block which contains the destination of the CFG edge 
                    # and link the super blocks
                    entry_edge_superv = subgraph.program_point_to_superv[entry_edge]
                    succ_superv       = inner_subgraph.program_point_to_superv[entry_edge[1]]
                    self.addEdge(entry_edge_superv.vertexID, succ_superv.vertexID)
    
    def find_super_block_for_header(self, headerID):
        return self.per_loop_subgraphs[headerID].program_point_to_superv[headerID]
    
class DominatorGraph (directed_graphs.DirectedGraph):
    def __init__ (self, predom_tree, postdom_tree):
        directed_graphs.DirectedGraph.__init__(self)
        self.add_vertices(predom_tree, postdom_tree)
        self.add_edges(predom_tree, postdom_tree)
        
    def add_vertices(self, predom_tree, postdom_tree):
        for v in predom_tree:
            assert postdom_tree.hasVertex(v.vertexID), "Vertex %d in pre-dominator tree but not in post-dominator tree" % v.vertexID
            self.the_vertices[v.vertexID] = vertices.Vertex(v.vertexID)        

    def add_edges(self, predom_tree, postdom_tree):
        for v in predom_tree:
            if v.vertexID != predom_tree.getRootID():
                self.addEdge(v.parentID, v.vertexID)
        for v in postdom_tree:
            if v.vertexID != postdom_tree.getRootID(): 
                if not self.getVertex(v.vertexID).has_predecessor(v.parentID):
                    self.addEdge(v.parentID, v.vertexID)

class StrongComponents:
    Colors = utils.enum('WHITE', 'BLACK', 'GRAY', 'BLUE', 'RED')
    SCCID  = 0
    
    def __init__(self, directedg):
        self.directedg     = directedg
        self.reverseg      = directedg.get_reverse_graph()
        self.vertex_colour = {}
        self.vertex_SCC    = {}
        self.scc_vertices  = {}
        self.initialise()
        self.do_forward_visit()
        self.do_reverse_visit()
        
    def initialise(self):
        for v in self.directedg:
            self.vertex_colour[v.vertexID] = StrongComponents.Colors.WHITE
            
    def do_forward_visit(self):
        self.vertex_list = []
        for v in self.directedg:
            if self.vertex_colour[v.vertexID] == StrongComponents.Colors.WHITE:
                self.visit1(v)

    def do_reverse_visit(self):
        for vertexID in reversed(self.vertex_list):
            if self.vertex_colour[vertexID] == StrongComponents.Colors.BLACK:
                StrongComponents.SCCID += 1
                self.scc_vertices[StrongComponents.SCCID] = set()
                # The vertex v is from the forward directed graph.
                # Need to get the vertex from the reverse graph instead
                self.visit2(self.reverseg.getVertex(vertexID))
    
    def visit1(self, v):
        stack = []
        stack.append(v)
        while stack:
            poppedv = stack.pop()
            if self.vertex_colour[poppedv.vertexID] == StrongComponents.Colors.WHITE:
                self.vertex_colour[poppedv.vertexID] = StrongComponents.Colors.GRAY
                stack.append(poppedv)
                for succID in poppedv.successors.keys():
                    if self.vertex_colour[succID] == StrongComponents.Colors.WHITE:
                        stack.append(self.directedg.getVertex(succID))
            elif self.vertex_colour[poppedv.vertexID] == StrongComponents.Colors.GRAY:  
                self.vertex_colour[poppedv.vertexID] = StrongComponents.Colors.BLACK
                self.vertex_list.append(poppedv.vertexID)
                
    def visit2(self, v):
        stack = []
        stack.append(v)
        while stack:
            poppedv = stack.pop()
            self.vertex_SCC[poppedv.vertexID] = StrongComponents.SCCID
            self.scc_vertices[StrongComponents.SCCID].add(poppedv.vertexID)
            if self.vertex_colour[poppedv.vertexID] == StrongComponents.Colors.BLACK:
                self.vertex_colour[poppedv.vertexID] = StrongComponents.Colors.BLUE
                stack.append(poppedv)
                for succID in poppedv.successors.keys():
                    if self.vertex_colour[succID] == StrongComponents.Colors.BLACK:
                        stack.append(self.reverseg.getVertex(succID))
            elif self.vertex_colour[poppedv.vertexID] == StrongComponents.Colors.BLUE:
                self.vertex_colour[poppedv.vertexID] = StrongComponents.Colors.RED  
    
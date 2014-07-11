import directed_graphs
import trees
import vertices
import debug
import utils
import udraw

class SuperBlockSubgraph(directed_graphs.DirectedGraph):
    def __init__(self):
        directed_graphs.DirectedGraph.__init__(self)
        self.sccID_to_superv         = {}
        self.program_point_to_superv = {}

class SuperBlockCFG(directed_graphs.DirectedGraph):
    next_vertexID = 0
    next_edgeID   = 0
    
    def __init__(self, cfg=None, lnt=None):
        directed_graphs.DirectedGraph.__init__(self)
        if cfg and lnt:
            self.name = cfg.name
            self.per_loop_subgraphs     = {}
            self.per_loop_root_vertices = {}
            for level, the_vertices in lnt.levelIterator(True):
                for treev in the_vertices:
                    if isinstance(treev, vertices.HeaderVertex):
                        debug.debug_message("Analysing header %d" % treev.headerID, __name__, 1)
                        forward_CFG          = lnt.induce_subgraph(treev)
                        enhanced_CFG         = directed_graphs.EnhancedCFG(forward_CFG)
                        enhanced_CFG.patch_back_edges_with_correct_header(lnt.get_loop_tails(treev.headerID), treev.headerID)
                        udraw.make_file(enhanced_CFG, "%s.header_%d.enhanced_icfg" % (cfg.name, treev.headerID))
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
        for sccID in xrange(1, sccs.numberOfSCCs()+1):
            SuperBlockCFG.next_vertexID += 1
            superv                                 = vertices.SuperBlock(SuperBlockCFG.next_vertexID)
            self.the_vertices[superv.vertexID]     = superv
            subgraph.the_vertices[superv.vertexID] = superv
            subgraph.sccID_to_superv[sccID]        = superv
        dfs = trees.DepthFirstSearch(enhanced_CFG, enhanced_CFG.get_entryID())
        for vertexID in reversed(dfs.post_order):
            program_point = enhanced_CFG.getVertex(vertexID)
            if not program_point.dummy: 
                sccID  = sccs.getSCCID(vertexID)
                superv = subgraph.sccID_to_superv[sccID]
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
            if isinstance(first_program_point, vertices.CFGEdge):
                # The program point represents a CFG edge.
                # Find the super block which contains the source of the CFG edge 
                # and link the super blocks
                basic_block_predID = first_program_point.edge[0]
                pred_superv        = subgraph.program_point_to_superv[basic_block_predID] 
                self.addEdge(pred_superv.vertexID, superv.vertexID)
                assert enhanced_CFG.getVertex(basic_block_predID).number_of_successors() > 1
                if basic_block_predID not in pred_superv.successor_partitions:
                    pred_superv.successor_partitions[basic_block_predID] = set()
                pred_superv.successor_partitions[basic_block_predID].add(superv.vertexID)
            elif first_program_point.vertexID != headerv.headerID:
                # The program point represents a CFG vertex.
                # Find the CFG edges incident to the CFG vertex.
                # Then find the super blocks containing those CFG edges
                basic_block = enhanced_CFG.getVertex(first_program_point.vertexID)
                assert enhanced_CFG.getVertex(first_program_point.vertexID).number_of_predecessors() > 1
                for predID in basic_block.predecessors.keys():
                    predv = enhanced_CFG.getVertex(predID)
                    assert isinstance(predv, vertices.CFGEdge)
                    pred_superv = subgraph.program_point_to_superv[predv.edge]
                    self.addEdge(pred_superv.vertexID, superv.vertexID)
    
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
        # Pre-dominator tree edges
        for v in predom_tree:
            if v.vertexID != predom_tree.getRootID():
                self.addEdge(v.parentID, v.vertexID)
        # Post-dominator tree edges
        for v in postdom_tree:
            if v.vertexID != postdom_tree.getRootID(): 
                if not self.getVertex(v.vertexID).has_predecessor(v.parentID):
                    self.addEdge(v.parentID, v.vertexID)

class StrongComponents:
    Colors = utils.enum('WHITE', 'BLACK', 'GRAY', 'BLUE', 'RED')
    
    def __init__ (self, directedg):
        self.directedg     = directedg
        self.reverseg      = directedg.get_reverse_graph()
        self.vertex_colour = {}
        self.vertex_scc    = {}
        self.scc_vertices  = {}
        self.scc_number    = 0
        self.initialise()
        self.do_forward_visit()
        self.do_reverse_visit()
        
    def initialise(self):
        for v in self.directedg:
            self.vertex_colour[v.vertexID] = StrongComponents.Colors.WHITE
            self.vertex_scc[v.vertexID]    = 0
            
    def do_forward_visit(self):
        self.vertex_list = []
        for v in self.directedg:
            if self.vertex_colour[v.vertexID] == StrongComponents.Colors.WHITE:
                self.visit1(v)

    def do_reverse_visit(self):
        for vertexID in reversed(self.vertex_list):
            if self.vertex_colour[vertexID] == StrongComponents.Colors.BLACK:
                self.scc_number += 1
                self.scc_vertices[self.scc_number] = set([])
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
            self.vertex_scc[poppedv.vertexID] = self.scc_number
            self.scc_vertices[self.scc_number].add(poppedv.vertexID)
            if self.vertex_colour[poppedv.vertexID] == StrongComponents.Colors.BLACK:
                self.vertex_colour[poppedv.vertexID] = StrongComponents.Colors.BLUE
                stack.append(poppedv)
                for succID in poppedv.successors.keys():
                    if self.vertex_colour[succID] == StrongComponents.Colors.BLACK:
                        stack.append(self.reverseg.getVertex(succID))
            elif self.vertex_colour[poppedv.vertexID] == StrongComponents.Colors.BLUE:
                self.vertex_colour[poppedv.vertexID] = StrongComponents.Colors.RED
                
    def numberOfSCCs (self):
        return self.scc_number
    
    def getSCCID (self, vertexID):
        assert vertexID in self.vertex_scc, "Unable to find SCC of vertex %d" % vertexID
        return self.vertex_scc[vertexID]
    
    def getVertexIDs (self, sccID):
        assert sccID in self.scc_vertices, "Unable to find set of vertices associated with SCC ID %d" % sccID
        return self.scc_vertices[sccID]  
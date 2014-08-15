import directed_graphs
import vertices
import debug
import random

class DepthFirstSearch:
    def __init__(self, subgraph):
        self.subgraph   = subgraph
        self.post_order = []
        self.visited    = set()
        for superv in subgraph:
            if superv.number_of_predecessors() == 0:
                self.do_depth_first_search(superv)
                
    def do_depth_first_search(self, superv):
        self.visited.add(superv)
        for succID in superv.successors.keys():
            succ_superv = self.subgraph.getVertex(succID)
            assert succ_superv not in self.visited
            self.do_depth_first_search(succ_superv)
        self.post_order.append(superv)
        
class Instrumenter:
    def __init__(self, cfg, lnt, super_block_cfg):
        self.is_instrumented           = {}
        self.instrumented_super_blocks = set()
        if not cfg.program_points_to_profile:
            self.pick_program_points_randomly(cfg)
        self.compute_where_to_place_instrumentation(lnt, super_block_cfg)
        self.print_instrumentation_locations()
        
    def pick_program_points_randomly(self, cfg):
        enhanced_CFG = directed_graphs.EnhancedCFG()
        enhanced_CFG.create_from_CFG(cfg)
        all_program_points = enhanced_CFG.the_vertices.values()
        cfg.program_points_to_profile = random.sample(all_program_points, 
                                                       random.randint(1,enhanced_CFG.number_of_vertices()))
        print cfg.program_points_to_profile
        
    def compute_where_to_place_instrumentation(self, lnt, super_block_cfg):
        for headerv in lnt.get_header_vertices():
            debug.debug_message("Analysing header %d" % headerv.headerID, __name__, 1) 
            forward_subgraph = super_block_cfg.forward_subgraphs[headerv.headerID]
            forward_search   = DepthFirstSearch(forward_subgraph)
            reverse_subgraph = super_block_cfg.reverse_subgraphs[headerv.headerID]
            reverse_search   = DepthFirstSearch(reverse_subgraph)
            
            for superv in forward_search.post_order:
                if superv.number_of_successors() == 0 and superv.number_of_predecessors() > 0:
                    self.instrumented_super_blocks.add(superv)
                    self.is_instrumented[superv] = True
                else:
                    self.is_instrumented[superv] = False
                    for the_partition in superv.successor_partitions.values():
                        instrumented_successors = set()
                        for succID in the_partition:
                            succ_superv = forward_subgraph.getVertex(succID)
                            if self.is_instrumented[succ_superv]:
                                instrumented_successors.add(succID)
                        if len(instrumented_successors) == len(the_partition):
                            self.is_instrumented[superv] = True
                debug.debug_message("superv %d %s covered by instrumentation" % (superv.vertexID, "is" if self.is_instrumented[superv] else "NOT"), 
                                    __name__,
                                    1)
                    
            for superv in reverse_search.post_order:
                paired_superv = super_block_cfg.super_block_pairs[superv]
                if superv.number_of_successors() == 0 and superv.number_of_predecessors() > 0:
                    self.instrumented_super_blocks.add(paired_superv)
                    self.is_instrumented[superv] = True
                else:
                    self.is_instrumented[superv] = False
                    for the_partition in superv.successor_partitions.values():
                        instrumented_successors = set()
                        for succID in the_partition:
                            succ_superv = reverse_subgraph.getVertex(succID)
                            if self.is_instrumented[succ_superv]:
                                instrumented_successors.add(succID)
                        if len(instrumented_successors) == len(the_partition):
                            self.is_instrumented[superv] = True
                debug.debug_message("superv %d %s covered by instrumentation" % (superv.vertexID, "is" if self.is_instrumented[superv] else "NOT"), 
                                    __name__,
                                    1)
                
    def print_instrumentation_locations(self):          
        for superv in self.instrumented_super_blocks:
            if isinstance(superv.representative, vertices.CFGVertex):
                print("Adding instrumentation to CFG vertex %d" % (superv.representative.vertexID))
            else:
                print("Adding instrumentation to CFG edge   %s" % (superv.representative.edge.__repr__()))
    
                
    
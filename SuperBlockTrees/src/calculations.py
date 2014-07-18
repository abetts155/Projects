import trees
import vertices
import debug
import timeit

class TreeBasedCalculation:
    def __init__(self, data, super_block_cfg):
        self.superv_wcets = {}
        start             = timeit.default_timer()
        self.wcet         = self.do_computation(data, super_block_cfg)
        end               = timeit.default_timer()
        self.solve_time   = end - start
        
    def do_computation(self, data, super_block_cfg):
        for headerID, subgraph in super_block_cfg.per_loop_subgraphs.iteritems():
            debug.debug_message("Doing calculation for loop with header %d" % headerID, __name__, 1)
            dfs = trees.DepthFirstSearch(subgraph, subgraph.rootID)
            last_superv = None
            for vertexID in dfs.post_order:
                last_superv = super_block_cfg.getVertex(vertexID)
                self.compute_wcet_of_super_block(data, super_block_cfg, last_superv)
            return self.superv_wcets[last_superv]
            
    def compute_wcet_of_super_block(self, data, super_block_cfg, superv):
        if superv not in self.superv_wcets:
            self.superv_wcets[superv] = 0
        # Sum up the contribution of each basic block in the super block
        for program_point in superv.program_points:
            if isinstance(program_point, vertices.CFGVertex):
                self.superv_wcets[superv] += data.get_basic_block_wcet(program_point.vertexID)
        if superv.number_of_successors() > 1:
            # Add in the contribution caused by branching control flow
            self.superv_wcets[superv] += self.compute_max_of_branches(super_block_cfg, superv)
        if superv.number_of_predecessors() > 1:
            # Propagate the WCET to predecessors which merge at this super block 
            self.propagate_wcets_to_predecessors(super_block_cfg, superv)
    
    def compute_max_of_branches(self, super_block_cfg, superv):
        wcet = 0
        for the_partition in superv.successor_partitions.values():
            max_wcet = 0
            for succID in the_partition:
                succ_superv = super_block_cfg.getVertex(succID)
                max_wcet    = max(self.superv_wcets[succ_superv], max_wcet)
            wcet += max_wcet
        return wcet
    
    def propagate_wcets_to_predecessors(self, super_block_cfg, superv):
        for predID in superv.predecessors.keys():
            pred_superv = super_block_cfg.getVertex(predID)
            self.superv_wcets[pred_superv] = self.superv_wcets[superv]
        
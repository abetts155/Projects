import trees
import vertices
import debug
import timeit
import numpy

class TreeBasedCalculation:
    def __init__(self, data, lnt, super_block_cfg):
        self.superv_wcets   = {}
        self.per_loop_wcets = {}
        start               = timeit.default_timer()
        self.wcet           = self.do_computation(data, lnt, super_block_cfg)
        end                 = timeit.default_timer()
        self.solve_time     = end - start
        
    def do_computation(self, data, lnt, super_block_cfg):
        for the_vertices in lnt.level_by_level_iterator(True):
            for treev in the_vertices:
                if isinstance(treev, vertices.HeaderVertex):
                    debug.debug_message("Doing calculation for loop with header %d" % treev.headerID, __name__, 1)
                    subgraph    = super_block_cfg.per_loop_subgraphs[treev.headerID]
                    rootv       = super_block_cfg.find_super_block_for_header(treev.headerID)
                    dfs         = trees.DepthFirstSearch(subgraph, rootv.vertexID)
                    last_superv = None
                    for vertexID in dfs.post_order:
                        last_superv = super_block_cfg.getVertex(vertexID)
                        self.compute_wcet_of_super_block(data, lnt, super_block_cfg, last_superv, treev)
                    self.per_loop_wcets[treev.headerID] = self.superv_wcets[last_superv]
        rootv = lnt.getVertex(lnt.rootID)
        return self.per_loop_wcets[rootv.headerID]
                    
    def compute_wcet_of_super_block(self, data, lnt, super_block_cfg, superv, headerv):
        # Sum up the contribution of each basic block in the super block
        self.superv_wcets[superv] = self.compute_execution_time_within_super_block(data, superv, headerv)
        self.superv_wcets[superv] = numpy.multiply(self.superv_wcets[superv], data.get_upper_bound_on_header(headerv.headerID))
        if superv.number_of_successors() > 1:
            # Add in the contribution caused by branching control flow
            self.superv_wcets[superv] = numpy.add(self.superv_wcets[superv], self.compute_max_of_branches(data, super_block_cfg, superv, headerv))
        self.superv_wcets[superv] = self.add_inner_loop_wcets(data, lnt, superv, headerv)
        if superv.number_of_predecessors() > 1:
            # Propagate the WCET to predecessors which merge at this super block
            self.propagate_wcets_to_predecessors(super_block_cfg, superv)
        
    def compute_execution_time_within_super_block(self, data, superv, headerv):
        if superv not in self.superv_wcets:
            wcet = numpy.array([0 * bound for bound in data.get_upper_bound_on_header(headerv.headerID)])
        else:
            wcet = self.superv_wcets[superv]
        for program_point in superv.program_points:
            if isinstance(program_point, vertices.CFGVertex):
                wcet = numpy.add(data.get_basic_block_wcet(program_point.vertexID), wcet)
        return wcet
    
    def add_inner_loop_wcets(self, data, lnt, superv, headerv):
        wcet = self.superv_wcets[superv]
        for program_point in superv.program_points:
            if isinstance(program_point, vertices.CFGEdge):
                destinationID = program_point.edge[1]
                if lnt.is_loop_header(destinationID) and destinationID != headerv.headerID:
                    inner_loop_wcet = numpy.sum(self.per_loop_wcets[destinationID])
                    wcet            = numpy.add(inner_loop_wcet, wcet)
        return wcet    
    
    def compute_max_of_branches(self, data, super_block_cfg, superv, headerv):
        wcet = [0 * bound for bound in data.get_upper_bound_on_header(headerv.headerID)]
        for the_partition in superv.successor_partitions.values():
            max_wcet = [0 * bound for bound in data.get_upper_bound_on_header(headerv.headerID)]
            for succID in the_partition:
                succ_superv = super_block_cfg.getVertex(succID)
                max_wcet    = numpy.maximum(self.superv_wcets[succ_superv], max_wcet)
            wcet = numpy.add(max_wcet, wcet)
        return wcet
    
    def propagate_wcets_to_predecessors(self, super_block_cfg, superv):
        for predID in superv.predecessors.keys():
            pred_superv = super_block_cfg.getVertex(predID)
            self.superv_wcets[pred_superv] = self.superv_wcets[superv]
        
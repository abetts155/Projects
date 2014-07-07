import debug
import vertices
import random

class CreateWCETData:    
    def __init__(self, icfg, enhanced_lnt, ipg, loop_by_loop_info, use_basic_block_wcets=True):        
        self.upper_bounds      = {}
        self.basic_block_WCETs = {}
        self.ipg_edge_WCETs    = {}
        if use_basic_block_wcets:
            self.assign_wcets_to_basic_blocks(icfg)
            self.assign_wcets_to_ipg_egdes_using_basic_block_wcets(ipg, icfg)
        else:
            self.assign_wcets_to_ipg_egdes_using_random_values(ipg)
        self.assign_loop_bounds(enhanced_lnt, loop_by_loop_info)
                            
    def assign_wcets_to_basic_blocks (self, icfg):
        for v in icfg:
            if isinstance(v, vertices.CFGVertex) and not v.is_ipoint:
                self.basic_block_WCETs[v.vertexID] = random.randint(1, 20)
                debug.debug_message("WCET(%d) = %d" % (v.vertexID, self.basic_block_WCETs[v.vertexID]), __name__, 1)
                
    def assign_wcets_to_ipg_egdes_using_basic_block_wcets(self, ipg, icfg):
        for v in ipg:
            for succID in v.successors.keys():
                key  = (v.vertexID, succID)
                wcet = 0
                for program_point in v.get_successor_edge(succID).edge_label:
                    if isinstance(program_point, vertices.CFGVertex) and not program_point.is_ipoint:
                        wcet += self.basic_block_WCETs[program_point.vertexID]
                self.ipg_edge_WCETs[key] = wcet
                debug.debug_message("WCET(%s) = %d" % (key, self.ipg_edge_WCETs[key]), __name__, 1)
                    
    def assign_wcets_to_ipg_egdes_using_random_values(self, ipg):
        for v in ipg:
            for succID in v.successors.keys():
                succ_edge = v.getSuccessorEdge(succID)
                if not succ_edge.isDummyEdge():
                    key                      = (v.vertexID, succID)
                    self.ipg_edge_WCETs[key] = random.randint(1, 10)
                    debug.debug_message("WCET(%s) = %d" % (key, self.ipg_edge_WCETs[key]), __name__, 1)
    
    def assign_loop_bounds(self, enhanced_lnt, loop_by_loop_info):
        for level, the_vertices in enhanced_lnt.levelIterator(True):
            for treev in the_vertices:
                if isinstance(treev, vertices.HeaderVertex):
                    if level > 0:
                        bound = random.randint(3, 10)
                    else:
                        bound = 1
                    for succID in treev.successors.keys():
                        succv = enhanced_lnt.getVertex(succID)
                        if not isinstance(succv, vertices.HeaderVertex):
                            if loop_by_loop_info.is_in_loop_exit_region(treev.headerID, succID):
                                self.upper_bounds[succID] = bound
                            else:
                                self.upper_bounds[succID] = bound - 1        
    
    def get_basic_block_wcet(self, vertexID):
        if vertexID in self.basic_block_WCETs:
            return self.basic_block_WCETs[vertexID]
        else:
            return 0
    
    def get_ipg_edge_wcet(self, predID, succID):
        key = (predID, succID)
        if key in self.ipg_edge_WCETs:
            return self.ipg_edge_WCETs[key]
        else:
            return 0
        
    def get_loop_bound(self, vertexID):
        assert vertexID in self.upper_bounds, "Unable to find bound for vertex %d" % vertexID
        return self.upper_bounds[vertexID]
    
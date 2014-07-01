import debug
import vertices
import random

class CreateWCETData:    
    def __init__(self, icfg, lnt, ipg, use_basic_block_wcets=True):        
        self.header_bounds     = {}
        self.basic_block_WCETs = {}
        self.ipg_edge_WCETs    = {}
        if use_basic_block_wcets:
            self.assign_wcets_to_basic_blocks(icfg)
            self.assign_wcets_to_ipg_egdes_using_basic_block_wcets(ipg, icfg)
        else:
            self.assign_wcets_to_ipg_egdes_using_random_values(ipg)
        self.assign_loop_bounds(lnt)
                            
    def assign_wcets_to_basic_blocks (self, icfg):
        for v in icfg:
            if not icfg.isIpoint(v.vertexID):
                self.basic_block_WCETs[v.vertexID] = random.randint(1, 20)
                debug.debug_message("WCET(%d) = %d" % (v.vertexID, self.basic_block_WCETs[v.vertexID]), __name__, 1)
                
    def assign_wcets_to_ipg_egdes_using_basic_block_wcets(self, ipg, icfg):
        for v in ipg:
            for succID in v.successors.keys():
                key  = (v.vertexID, succID)
                wcet = 0
                for bbID in v.get_successor_edge(succID).edge_label:
                    wcet += self.basic_block_WCETs[bbID]
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
    
    def assign_loop_bounds(self, lnt):
        for level, the_vertices in lnt.levelIterator(True):
            for v in the_vertices:
                if isinstance(v, vertices.HeaderVertex):
                    if level > 0:
                        bound = 0
                        self.header_bounds[v.headerID] = {}
                        for ancestorv in lnt.getAllProperAncestors(v.vertexID):
                            ancestorHeaderID = ancestorv.headerID
                            bound            += random.randint(3, 10)
                            self.header_bounds[v.headerID][ancestorHeaderID] = bound
                            debug.debug_message("Bound(%d w.r.t %d) = %d" % (v.headerID, ancestorHeaderID, bound), __name__, 1)
    
    def get_basic_block_wcet(self, vertexID):
        if vertexID in self.basic_block_WCETs:
            return self.basic_block_WCETs[vertexID]
        else:
            debug.warning_message("Returning WCET of 0 for basic block %d" % vertexID)
            return 0
    
    def get_ipg_edge_wcet(self, predID, succID):
        key = (predID, succID)
        if key in self.ipg_edge_WCETs:
            return self.ipg_edge_WCETs[key]
        else:
            debug.warning_message("Returning WCET of 0 for IPG edge %s" % key)
            return 0
        
    def get_loop_bound(self, headerID, ancestorID):
        assert headerID in self.header_bounds, "Unable to find bound for header %d" % headerID
        assert ancestorID in self.header_bounds[headerID], "Unable to find bound for header %d w.r.t. header %d" % (headerID, ancestorID)
        return self.header_bounds[headerID][ancestorID]
    
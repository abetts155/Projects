import debug
import vertices
import random

class CreateWCETData:    
    def __init__(self, cfg, lnt):        
        self.upper_bounds      = {}
        self.basic_block_WCETs = {}
        self.assign_wcets_to_basic_blocks(cfg)
        self.assign_loop_bounds(lnt)
                            
    def assign_wcets_to_basic_blocks (self, cfg):
        for v in cfg:
            if isinstance(v, vertices.CFGVertex):
                self.basic_block_WCETs[v.vertexID] = random.randint(1, 20)
                debug.debug_message("WCET(%d) = %d" % (v.vertexID, self.basic_block_WCETs[v.vertexID]), __name__, 1)
    
    def assign_loop_bounds(self, lnt):
        for level, the_vertices in lnt.levelIterator(True):
            for treev in the_vertices:
                if isinstance(treev, vertices.HeaderVertex):
                    if level > 0:
                        self.upper_bounds[treev.headerID] = random.randint(3, 10)
                    else:
                        self.upper_bounds[treev.headerID] = 1

    def get_basic_block_wcet(self, vertexID):
        if vertexID in self.basic_block_WCETs:
            return self.basic_block_WCETs[vertexID]
        else:
            return 0
        
    def get_loop_bound(self, vertexID):
        assert vertexID in self.upper_bounds, "Unable to find bound for vertex %d" % vertexID
        return self.upper_bounds[vertexID]
    
import debug
import vertices
import random

class FunctionData:
    def __init__(self):        
        self.upper_bounds_on_headers = {}
        self.basic_block_WCETs       = {}

    def get_basic_block_wcet(self, vertexID):
        if vertexID in self.basic_block_WCETs:
            return self.basic_block_WCETs[vertexID]
        else:
            return 0
        
    def get_upper_bound_on_header(self, headerID):
        assert headerID in self.upper_bounds_on_headers, "Unable to find bound for header %d" % headerID
        return self.upper_bounds_on_headers[headerID]
                            
    def assign_wcets_to_basic_blocks(self, cfg):
        for v in cfg:
            if isinstance(v, vertices.CFGVertex):
                self.basic_block_WCETs[v.vertexID] = random.randint(1, 20)
                debug.debug_message("WCET(%d) = %d" % (v.vertexID, self.basic_block_WCETs[v.vertexID]), __name__, 1)
    
    def assign_loop_bounds(self, lnt):
        for the_vertices in lnt.level_by_level_iterator(True):
            for treev in the_vertices:
                if isinstance(treev, vertices.HeaderVertex):
                    if treev.vertexID == lnt.rootID:
                        self.upper_bounds_on_headers[treev.headerID] = 1
                    else:
                        self.upper_bounds_on_headers[treev.headerID] = random.randint(3, 10)
                        
class Database:
    def __init__(self):
        self.function_data = {}

        
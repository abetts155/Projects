import udraw
import flow_graphs
import copy
           
def create_induced_subgraph(program, cfg_name, entry_vertexID, exit_vertexID):
    cfg = program.cfgs[cfg_name]
    lnt = cfg.get_LNT()
    assert lnt.getVertex(entry_vertexID).parentID == lnt.getVertex(exit_vertexID).parentID
    headerv           = lnt.getVertex(lnt.getVertex(entry_vertexID).parentID)
    enhanced_CFG      = cfg.enhanced_CFGs[headerv.headerID]
    reachability_info = cfg.reachability_info[headerv.headerID]
    pair_subset       = set([entry_vertexID, exit_vertexID])
    edges             = set()
    visited           = set()
    stack             = []
    stack.append(exit_vertexID)
    while stack:
        vertexID = stack.pop()
        visited.add(vertexID)
        if vertexID != entry_vertexID:
            v = enhanced_CFG.getVertex(vertexID)
            for predID in v.predecessors.keys():
                if pair_subset.issubset(reachability_info.unified[predID]):
                    edges.add((predID, vertexID))
                    if not predID in visited:
                        stack.append(predID)
    induced_CFG = flow_graphs.EnhancedCFG()
    for an_edge in edges:
        if not induced_CFG.hasVertex(an_edge[0]):
            v = enhanced_CFG.getVertex(an_edge[0])
            newv = copy.deepcopy(v)
            newv.predecessors = {}
            newv.successors   = {}
            induced_CFG.addVertex(newv)
        if not induced_CFG.hasVertex(an_edge[1]):
            v = enhanced_CFG.getVertex(an_edge[1])
            newv = copy.deepcopy(v)
            newv.predecessors = {}
            newv.successors   = {}
            induced_CFG.addVertex(newv)
        induced_CFG.addEdge(an_edge[0], an_edge[1])
    induced_CFG.entryID = entry_vertexID
    induced_CFG.exitID = exit_vertexID
    return induced_CFG        
    
class Program():
    def __init__(self):
        self.cfgs = {}
        
    def add_CFG(self, cfg):
        assert cfg.name
        self.cfgs[cfg.name] = cfg
        udraw.make_file(cfg, "%s.cfg" % (cfg.name))      
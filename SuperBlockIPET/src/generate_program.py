import programs
import directed_graphs
import vertices
import config
import random
import math

class SESEComponent:
    def __init__ (self):
        self.entryID = None
        self.exitID  = None

class CreateLoopBody:
    def __init__(self, number_of_vertices, whole_program_CFG, nested_loop_graphs):
        self.whole_program_CFG     = whole_program_CFG
        self.directedg             = directed_graphs.DirectedGraph()
        self.disconnected_vertices = []
        self.singleton_vertices    = []
        self.add_vertices(number_of_vertices)
        self.create(number_of_vertices, nested_loop_graphs)
        self.connect_SESEs()
        self.connect_singleton_vertices()
        if nested_loop_graphs:
            self.add_nested_loop_entry_and_exit_edges(nested_loop_graphs)
        assert self.directedg.entryID, "Directed graph entry ID not set"
        assert self.directedg.exitID, "Directed graph exit ID not set"
        
    def add_vertices(self, number_of_vertices):
        for i in xrange(1,number_of_vertices+1):
            vertexID = self.whole_program_CFG.get_next_vertexID()
            v        = vertices.CFGVertex(vertexID)
            self.whole_program_CFG.addVertex(v)
            self.directedg.addVertex(v)
            self.disconnected_vertices.append(vertexID)
        
    def create(self, number_of_vertices, nested_loop_graphs):
        remaining_vertices           = number_of_vertices - len(nested_loop_graphs)
        number_of_singleton_vertices = len(nested_loop_graphs)
        number_of_if_then_else       = 0
        number_of_if_then            = 0
        number_of_short_circuit      = 0
        number_of_switch             = 0
        
        if random.random() > 0.3:
            remaining_vertices, number_of_if_then_else = self.determine_number_of_acyclic_region(4, remaining_vertices)
        if random.random() > 0.3:
            remaining_vertices, number_of_short_circuit = self.determine_number_of_acyclic_region(5, remaining_vertices)
        if random.random() > 0.3:
            remaining_vertices, number_of_if_then = self.determine_number_of_acyclic_region(3, remaining_vertices)
        if random.random() > 0.3 and config.Arguments.fan_out > 2:
            remaining_vertices, number_of_switch = self.determine_number_of_acyclic_region(2 + config.Arguments.fan_out, remaining_vertices)
        
        number_of_singleton_vertices += remaining_vertices
        for i in xrange(1, number_of_singleton_vertices):
            vertexID = self.disconnected_vertices.pop()
            self.singleton_vertices.append(vertexID)
        self.add_acyclic_regions(number_of_if_then_else, number_of_if_then, number_of_short_circuit, number_of_switch)
    
    def determine_number_of_acyclic_region(self, sizeOfComponent, number_of_vertices):
        numberOfComponents = 0
        if number_of_vertices >= sizeOfComponent:
            numberOfComponents = random.randint(1,math.floor(number_of_vertices/sizeOfComponent))
            number_of_vertices -= sizeOfComponent * numberOfComponents
        return number_of_vertices, numberOfComponents
    
    def add_acyclic_regions(self, number_of_if_then_else, number_of_if_then, number_of_short_circuit, number_of_switch):
        self.freeSESEs = []
        while number_of_if_then_else > 0 or number_of_if_then > 0 or number_of_short_circuit > 0 or number_of_switch > 0:
            if number_of_if_then_else > 0 and bool(random.getrandbits(1)):
                sese = self.create_if_then_else()
                self.freeSESEs.append(sese)
                number_of_if_then_else -= 1
            if number_of_if_then > 0 and bool(random.getrandbits(1)):
                sese = self.create_if_then()
                self.freeSESEs.append(sese)
                number_of_if_then -= 1
            if number_of_short_circuit > 0 and bool(random.getrandbits(1)):
                sese = self.create_short_circuit()
                self.freeSESEs.append(sese)
                number_of_short_circuit -= 1
            if number_of_switch > 0 and bool(random.getrandbits(1)):
                sese = self.create_switch()
                self.freeSESEs.append(sese)
                number_of_switch -= 1
    
    def create_switch(self):
        branchID = self.disconnected_vertices.pop()
        mergeID  = self.disconnected_vertices.pop()
        for i in xrange(1, config.Arguments.fan_out+1):
            switchArm = self.create_SESE()
            self.directedg.addEdge(branchID, switchArm.entryID)
            self.directedg.addEdge(switchArm.exitID, mergeID)
        sese         = SESEComponent()
        sese.entryID = branchID
        sese.exitID  = mergeID
        return sese
    
    def create_short_circuit(self):
        outerBranchID = self.disconnected_vertices.pop()
        innerBranchID = self.disconnected_vertices.pop()
        mergeID       = self.disconnected_vertices.pop()
        thenBranch = self.create_SESE()
        elseBranch = self.create_SESE()
        self.directedg.addEdge(outerBranchID, innerBranchID)
        self.directedg.addEdge(outerBranchID, elseBranch.entryID)
        self.directedg.addEdge(innerBranchID, thenBranch.entryID)
        self.directedg.addEdge(innerBranchID, elseBranch.entryID)
        self.directedg.addEdge(thenBranch.exitID, mergeID)
        self.directedg.addEdge(elseBranch.exitID, mergeID)
        sese         = SESEComponent()
        sese.entryID = outerBranchID
        sese.exitID  = mergeID
        return sese
    
    def create_if_then(self):
        branchID   = self.disconnected_vertices.pop()
        mergeID    = self.disconnected_vertices.pop()
        thenBranch = self.create_SESE()
        self.directedg.addEdge(branchID, thenBranch.entryID)
        self.directedg.addEdge(thenBranch.exitID, mergeID)
        self.directedg.addEdge(branchID, mergeID)
        sese         = SESEComponent()
        sese.entryID = branchID
        sese.exitID  = mergeID
        return sese
    
    def create_if_then_else(self):
        branchID   = self.disconnected_vertices.pop()
        mergeID    = self.disconnected_vertices.pop()
        thenBranch = self.create_SESE()
        elseBranch = self.create_SESE()
        self.directedg.addEdge(branchID, thenBranch.entryID)
        self.directedg.addEdge(branchID, elseBranch.entryID)
        self.directedg.addEdge(thenBranch.exitID, mergeID)
        self.directedg.addEdge(elseBranch.exitID, mergeID)
        sese         = SESEComponent()
        sese.entryID = branchID
        sese.exitID  = mergeID
        return sese
    
    def create_SESE(self):
        SESE         = SESEComponent()
        startID      = self.disconnected_vertices.pop()
        SESE.entryID = startID
        SESE.exitID  = startID
        if self.singleton_vertices and bool(random.getrandbits(1)):
            vertexID = self.singleton_vertices.pop()
            self.directedg.addEdge(SESE.exitID, vertexID)
            SESE.exitID = vertexID
        elif self.freeSESEs and bool(random.getrandbits(1)):
            nestedSESE = self.freeSESEs.pop()
            self.directedg.addEdge(SESE.exitID, nestedSESE.entryID)
            SESE.exitID = nestedSESE.exitID
        return SESE
    
    def connect_SESEs(self):
        while len(self.freeSESEs) > 1:
            sese1 = self.freeSESEs.pop()
            sese2 = self.freeSESEs.pop()
            sourceID = sese1.exitID
            while self.singleton_vertices and bool(random.getrandbits(1)):
                vertexID = self.singleton_vertices.pop()
                self.directedg.addEdge(sourceID, vertexID)
                sourceID = vertexID
            self.directedg.addEdge(sourceID, sese2.entryID)
            sese1.exitID = sese2.exitID
            self.freeSESEs.append(sese1)
    
    def connect_singleton_vertices(self):
        if self.freeSESEs:
            lastSese = self.freeSESEs.pop()
            while self.singleton_vertices:
                vertexID = self.singleton_vertices.pop()
                if bool(random.getrandbits(1)):
                    self.directedg.addEdge(vertexID, lastSese.entryID)
                    lastSese.entryID = vertexID
                else:
                    self.directedg.addEdge(lastSese.exitID, vertexID)
                    lastSese.exitID = vertexID            
            self.directedg.entryID = lastSese.entryID
            self.directedg.exitID = lastSese.exitID
        else:
            predID   = vertices.dummyID
            vertexID = vertices.dummyID
            while self.singleton_vertices:
                vertexID = self.singleton_vertices.pop()
                if predID != vertices.dummyID:
                    self.directedg.addEdge(predID, vertexID)
                else:
                    self.directedg.entryID = vertexID
                predID = vertexID
            self.directedg.exitID = vertexID
        while self.disconnected_vertices:
            vertexID = self.disconnected_vertices.pop()
            if bool(random.getrandbits(1)):
                self.directedg.addEdge(vertexID, self.directedg.entryID)
                self.directedg.entryID = vertexID
            else:
                self.directedg.addEdge(self.directedg.exitID, vertexID)
                self.directedg.exitID = vertexID
                
    def add_nested_loop_entry_and_exit_edges(self, nested_loop_graphs):
        candidate_sources = []
        for v in self.directedg:
            if v.number_of_successors() == 1 and v.vertexID != self.directedg.exitID:
                candidate_sources.append(v.vertexID)
        random.shuffle(candidate_sources)
        assert len(candidate_sources) >= len(nested_loop_graphs), \
        "Candidates = {%s}, nested loops = %d" % (','.join(str(vertexID) for vertexID in candidate_sources), len(nested_loop_graphs))
        dfs = directed_graphs.DepthFirstSearch(self.directedg, self.directedg.entryID)
        for nested_loop_graph in nested_loop_graphs:
            # Connect to inner loop header
            predID = candidate_sources.pop()
            self.whole_program_CFG.addEdge(predID, nested_loop_graph.headerID)
            # Connect inner loop exits to vertices whose post-order numbering is 
            # less than the predecessor. 
            # This ensures exit control flow is in a forward direction 
            pred_post_order_idx    = dfs.post_order.index(predID)
            candidate_destinations = dfs.post_order[:pred_post_order_idx]
            for exitID in nested_loop_graph.exitIDs:
                succ_post_order_idx = random.randint(0,len(candidate_destinations)-1)
                succID              = candidate_destinations[succ_post_order_idx]
                self.whole_program_CFG.addEdge(exitID, succID)
    
class LoopComponent:
    def __init__ (self, directedg):
        self.directedg = directedg
        self.headerID  = directedg.entryID
        self.tailIDs   = set()
        self.exitIDs   = set()
        self.tailIDs.add(directedg.exitID)
        if config.Arguments.unstructured:
            for v in self.directedg:
                if v.number_of_successors() == 1:
                    if random.random() < 0.2: 
                        self.exitIDs.add(v.vertexID)
                    elif random.random() < 0.1 and v.vertexID != self.headerID:
                        self.tailIDs.add(v.vertexID)
        if not self.tailIDs:
            self.tailIDs.add(directedg.exitID)
        if not self.exitIDs:
            if bool(random.getrandbits(1)):
                self.exitIDs.add(directedg.entryID)
            else:
                self.exitIDs.add(directedg.exitID)
        for tailID in self.tailIDs:
            directedg.addEdge(tailID, self.headerID)  
    
class CreateCFG:
    def __init__(self, name):
        self.loop_components = {}
        lnt_basic_structure  = self.generate_LNT()
        self.cfg      = self.create_CFG(lnt_basic_structure)
        self.cfg.name = name
        self.lnt      = directed_graphs.LoopNests(self.cfg, self.cfg.get_entryID())
        if config.Arguments.unstructured:
            self.find_and_remove_merge_vertices(self.cfg, self.lnt)
        self.check_connected(self.cfg)
    
    def generate_LNT(self):
        lnt = directed_graphs.Tree()
        # Add vertices to the tree, including one extra for the dummy outer loop
        vertex_level = {}
        for i in xrange(1,config.Arguments.loops+2):
            treev = vertices.TreeVertex(i)
            lnt.addVertex(treev)
            lnt.rootID = i
            vertex_level[i] = 0
        # Add edges to the tree
        parentID = lnt.rootID
        for v in lnt:
            if v.vertexID != lnt.rootID:
                newLevel = vertex_level[parentID] + 1
                if newLevel <= config.Arguments.nesting_depth:
                    lnt.addEdge(parentID, v.vertexID)
                    vertex_level[v.vertexID] = newLevel
                    parentID = v.vertexID
                else:
                    # The height of the LNT now exceeds the maximum depth
                    # Backtrack to an arbitrary proper ancestor
                    ancestorID = parentID
                    while True:
                        ancestorID = lnt.getVertex(ancestorID).parentID
                        if bool(random.getrandbits(1)) or ancestorID == lnt.rootID:
                            break
                    parentID = ancestorID
                    lnt.addEdge(parentID, v.vertexID)
                    vertex_level[v.vertexID] = vertex_level[parentID] + 1
                    parentID = v.vertexID
        return lnt
    
    def create_CFG(self, lnt):
        cfg = directed_graphs.CFG()
        number_of_vertices_per_loop = {}
        # Compute number of vertices in each loop
        number_of_vertices_remaining = config.Arguments.basic_blocks
        for treev in lnt:
            # Guarantee each loop has at least 2 vertices + vertices needed to 
            # connect inner nested loops
            min_vertices = 2 + treev.number_of_successors()
            number_of_vertices_per_loop[treev] = min_vertices
            number_of_vertices_remaining -= min_vertices
        while number_of_vertices_remaining > 0:
            for treev in lnt:
                additional_vertices = random.randint(0, number_of_vertices_remaining)
                number_of_vertices_per_loop[treev] += additional_vertices
                number_of_vertices_remaining -= additional_vertices
        # Generate the acyclic region in each loop
        for the_vertices in lnt.level_by_level_iterator(True):
            for treev in the_vertices:
                nested_loop_graphs = []
                for succID in treev.successors.keys():
                    succv = lnt.getVertex(succID)
                    nested_loop_graphs.append(self.loop_components[succv])
                loop_body = CreateLoopBody(number_of_vertices_per_loop[treev], cfg, nested_loop_graphs)
                if treev.vertexID == lnt.rootID:
                    cfg.set_entryID(loop_body.directedg.entryID)
                    cfg.set_exitID(loop_body.directedg.exitID)
                    cfg.addEdge(cfg.exitID, cfg.entryID)
                else:
                    self.loop_components[treev] = LoopComponent(loop_body.directedg)
        return cfg
    
    def find_and_remove_merge_vertices(self, cfg, lnt):
        disconnected_vertices = []
        # Remove merge vertices
        for v in cfg:
            if v.number_of_predecessors() > 1 \
            and bool(random.getrandbits(1)) \
            and not (lnt.is_loop_header(v.vertexID) or 
                     lnt.is_loop_tail(v.vertexID) or 
                     lnt.is_loop_exit_source(v.vertexID) or 
                     lnt.is_loop_exit_destination(v.vertexID) or 
                     cfg.get_exitID() == v.vertexID):
                for predID in v.predecessors.keys():
                    for succID in v.successors.keys():
                        if not cfg.hasEdge(predID, succID):
                            cfg.addEdge(predID, succID)
                for predID in v.predecessors.keys():
                    cfg.removeEdge(predID, v.vertexID)
                for succID in v.successors.keys():
                    cfg.removeEdge(v.vertexID, succID)
                disconnected_vertices.append(v)
        # At this point there is a bunch of vertices disconnected from the CFG.
        # Reconnect them to vertices which currently only have one successor or
        # make the disconnected vertex the entry of the CFG
        candidate_sources = []
        for v in cfg:
            if v.number_of_successors() == 1 and v.vertexID != cfg.exitID:
                assert v not in disconnected_vertices
                candidate_sources.append(v)
        random.shuffle(candidate_sources)
        for v in disconnected_vertices:
            if candidate_sources and bool(random.getrandbits(1)):
                source_idx = random.randint(0, len(candidate_sources)-1)
                sourcev    = candidate_sources[source_idx]
                for succID in sourcev.successors.keys():
                    cfg.addEdge(v.vertexID, succID)
                cfg.addEdge(sourcev.vertexID, v.vertexID)
            else:
                cfg.addEdge(v.vertexID, cfg.entryID)
                cfg.removeEdge(cfg.exitID, cfg.entryID)
                cfg.addEdge(cfg.exitID, v.vertexID)
                cfg.set_entryID(v.vertexID)
        assert cfg.getVertex(cfg.entryID).number_of_predecessors() == 1
        assert cfg.getVertex(cfg.exitID).number_of_successors() == 1
    
    def check_connected(self, cfg):
        visited = set()
        stack   = []
        stack.append(cfg.get_entryID())
        while stack:
            vertexID = stack.pop()
            visited.add(vertexID)
            for succID in cfg.getVertex(vertexID).successors.keys():
                if succID not in visited:
                    stack.append(succID)
        assert not set(cfg.the_vertices.keys()).difference(visited), "The CFG is not connected"
    
def do_it():
    program = programs.Program()
    for functionID in xrange(1, config.Arguments.subprograms+1):
        name = "f%d" % functionID
        cfg  = CreateCFG(name).cfg
        program.add_CFG(cfg)
        cfg.get_LNT()
    return program

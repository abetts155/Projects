import programs
import trees
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
    def __init__(self, number_of_vertices, the_CFG):
        self.directedg = directed_graphs.DirectedGraph()
        self.create(number_of_vertices, the_CFG)
        self.connected_SESEs()
        self.connect_singleton_vertices()
        assert self.directedg.entryID, "Directed graph entry ID not set"
        assert self.directedg.exitID, "Directed graph exit ID not set"
        
    def create(self, number_of_vertices, the_CFG):
        self.disconnected_vertices = []
        self.singleton_vertices    = []
        for i in xrange(1,number_of_vertices+1):
            vertexID = the_CFG.get_next_vertexID()
            v        = vertices.CFGVertex(vertexID)
            the_CFG.addVertex(v)
            self.directedg.addVertex(v)
            self.disconnected_vertices.append(vertexID)
        number_of_singleton_vertices, numberOfIfThenElses, numberOfIfThens, numberOfShortCircuits, numberOfSwitches = self.determine_which_acyclic_regions(number_of_vertices)
        for i in xrange(1, number_of_singleton_vertices):
            vertexID = self.disconnected_vertices.pop()
            self.singleton_vertices.append(vertexID)
        self.add_acyclic_regions(numberOfIfThenElses, numberOfIfThens, numberOfShortCircuits, numberOfSwitches)
        
    def determine_which_acyclic_regions(self, number_of_vertices):
        numberOfIfThenElses   = 0
        numberOfIfThens       = 0
        numberOfShortCircuits = 0
        numberOfSwitches      = 0
        if bool(random.getrandbits(1)):
            number_of_vertices, numberOfIfThenElses = self.determine_number_of_acyclic_region(4, number_of_vertices)
        if bool(random.getrandbits(1)):
            number_of_vertices, numberOfIfThens = self.determine_number_of_acyclic_region(3, number_of_vertices)
        if bool(random.getrandbits(1)):
            number_of_vertices, numberOfShortCircuits = self.determine_number_of_acyclic_region(5, number_of_vertices)
        if config.Arguments.fan_out > 2 and bool(random.getrandbits(1)):
            number_of_vertices, numberOfSwitches = self.determine_number_of_acyclic_region(2 + config.Arguments.fan_out, number_of_vertices)
        return number_of_vertices, numberOfIfThenElses, numberOfIfThens, numberOfShortCircuits, numberOfSwitches 
    
    def determine_number_of_acyclic_region(self, sizeOfComponent, number_of_vertices):
        numberOfComponents = 0
        if number_of_vertices >= sizeOfComponent:
            numberOfComponents = random.randint(1,math.floor(number_of_vertices/sizeOfComponent))
            number_of_vertices -= sizeOfComponent * numberOfComponents
        return number_of_vertices, numberOfComponents
    
    def add_acyclic_regions(self, numberOfIfThenElses, numberOfIfThens, numberOfShortCircuits, numberOfSwitches):
        self.freeSESEs = []
        while numberOfIfThenElses > 0 or numberOfIfThens > 0 or numberOfShortCircuits > 0 or numberOfSwitches > 0:
            if numberOfIfThenElses > 0 and bool(random.getrandbits(1)):
                sese = self.create_if_then_else()
                self.freeSESEs.append(sese)
                numberOfIfThenElses -= 1
            if numberOfIfThens > 0 and bool(random.getrandbits(1)):
                sese = self.create_if_then()
                self.freeSESEs.append(sese)
                numberOfIfThens -= 1
            if numberOfShortCircuits > 0 and bool(random.getrandbits(1)):
                sese = self.create_short_circuit()
                self.freeSESEs.append(sese)
                numberOfShortCircuits -= 1
            if numberOfSwitches > 0 and bool(random.getrandbits(1)):
                sese = self.create_switch()
                self.freeSESEs.append(sese)
                numberOfSwitches -= 1
    
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
    
    def connected_SESEs(self):
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
    
class LoopComponent:
    def __init__ (self, directedg):
        self.directedg = directedg
        self.headerID  = directedg.entryID
        self.tailIDs   = set()
        self.exitIDs   = set()
        self.tailIDs.add(directedg.exitID)
        for v in self.directedg:
            if v.number_of_successors() == 1:
                if random.random() < 0.2: 
                    self.exitIDs.add(v.vertexID)
                elif random.random() < 0.1:
                    self.tailIDs.add(v.vertexID)
        if not self.exitIDs:
            if bool(random.getrandbits(1)):
                self.exitIDs.add(directedg.entryID)
            else:
                self.exitIDs.add(directedg.exitID)
        for tailID in self.tailIDs:
            directedg.addEdge(tailID, self.headerID)  
    
class CreateCFG:
    def __init__(self):
        self.loop_components = {}
        self.generate_LNT()
        self.create_CFG()
        self.check_connected()
    
    def generate_LNT(self):
        self.lnt = trees.Tree()
        # Add vertices to the tree, including one extra for the dummy outer loop
        vertexToLevel = {}
        for i in xrange(1,config.Arguments.loops+2):
            treev = vertices.TreeVertex(i)
            self.lnt.addVertex(treev)
            self.lnt.rootID = i
            vertexToLevel[i] = 0
        # Add edges to the tree
        parentID = self.lnt.rootID
        for v in self.lnt:
            if v.vertexID != self.lnt.rootID:
                newLevel = vertexToLevel[parentID] + 1
                if newLevel <= config.Arguments.nesting_depth:
                    self.lnt.addEdge(parentID, v.vertexID)
                    vertexToLevel[v.vertexID] = newLevel
                    parentID = v.vertexID
                else:
                    # The height of the LNT now exceeds the maximum depth
                    # Backtrack to an arbitrary proper ancestor
                    ancestorID = parentID
                    while True:
                        ancestorID = self.lnt.getVertex(ancestorID).parentID
                        if bool(random.getrandbits(1)) or ancestorID == self.lnt.rootID:
                            break
                    parentID = ancestorID
                    self.lnt.addEdge(parentID, v.vertexID)
                    vertexToLevel[v.vertexID] = vertexToLevel[parentID] + 1
                    parentID = v.vertexID
        return self.lnt
    
    def create_CFG(self):
        self.cfg = directed_graphs.CFG()
        number_of_vertices_per_loop = {}
        # Compute number of vertices in each loop
        number_of_vertices_remaining = config.Arguments.basic_blocks
        for treev in self.lnt:
            # Guarantee each loop has at least 2 vertices
            number_of_vertices_per_loop[treev] = 2
            number_of_vertices_remaining -= 2
        while number_of_vertices_remaining > 0:
            for treev in self.lnt:
                additional_vertices = random.randint(0, number_of_vertices_remaining)
                number_of_vertices_per_loop[treev] += additional_vertices
                number_of_vertices_remaining -= additional_vertices
        # Generate the acyclic region in each loop
        for treev in self.lnt:
            loop_body = CreateLoopBody(number_of_vertices_per_loop[treev], self.cfg)
            self.loop_components[treev] = LoopComponent(loop_body.directedg)
            if treev.vertexID == self.lnt.rootID:
                self.cfg.set_entryID(loop_body.directedg.entryID)
                self.cfg.set_exitID(loop_body.directedg.exitID)
        for treev in self.lnt:
            if treev.number_of_successors() > 0:
                self.connect_nested_loops(treev)
        if config.Arguments.unstructured:
            self.find_and_remove_merge_vertices()
    
    def connect_nested_loops(self, treev):
        firstLoop  = None
        secondLoop = None
        if treev.number_of_successors() > 1:
            succIDs  = treev.successors.keys()
            while len(succIDs) > 1:
                v1       = self.lnt.getVertex(succIDs.pop())
                v2       = self.lnt.getVertex(succIDs[-1])
                predLoop = self.loop_components[v1]
                succLoop = self.loop_components[v2]
                if not firstLoop:
                    firstLoop = predLoop
                secondLoop = succLoop
                for exitID in predLoop.exitIDs:
                    self.cfg.addEdge(exitID, succLoop.headerID)
        else:
            succID     = treev.successors.keys()[0] 
            succv      = self.lnt.getVertex(succID)
            firstLoop  = self.loop_components[succv]
            secondLoop = self.loop_components[succv]
        assert firstLoop, "The first loop component has not been set"
        assert secondLoop, "The second loop component has not been set"
        parentLoop   = self.loop_components[treev]
        sourcev      = None
        destinationv = None 
        for v in parentLoop.directedg:
            v = self.cfg.getVertex(v.vertexID) 
            if v.number_of_successors() == 1 and v.successors.keys()[0] != parentLoop.headerID:
                succID       = v.successors.keys()[0]
                sourcev      = v
                destinationv = self.cfg.getVertex(succID)
                break
        if not sourcev:
            sourcev = self.cfg.getVertex(parentLoop.headerID)
            succIDs = sourcev.successors.keys()
            for succID in succIDs:
                self.cfg.removeEdge(parentLoop.headerID, succID)
            destinationID = succIDs[0]
            for succID in succIDs:
                if destinationID != succID:
                    self.cfg.addEdge(destinationID, succID)
            destinationv = self.cfg.getVertex(destinationID)
        self.cfg.addEdge(sourcev.vertexID, firstLoop.headerID)
        for exitID in secondLoop.exitIDs:
            self.cfg.addEdge(exitID, destinationv.vertexID)
    
    def find_and_remove_merge_vertices(self):
        disconnectedVertices = []
        lnt                  = trees.LoopNests(self.cfg, self.cfg.get_entryID())
        for v in self.cfg:
            if v.number_of_predecessors() > 1 \
            and bool(random.getrandbits(1)) \
            and not (lnt.is_loop_header(v.vertexID) or 
                 lnt.is_loop_tail(v.vertexID) or 
                 lnt.is_loop_exit_source(v.vertexID) or 
                 lnt.is_loop_exit_destination(v.vertexID) or 
                 self.cfg.get_exitID() == v.vertexID):
                for predID in v.predecessors.keys():
                    for succID in v.successors.keys():
                        self.cfg.addEdge(predID, succID)
                for predID in v.predecessors.keys():
                    self.cfg.removeEdge(predID, v.vertexID)
                for succID in v.successors.keys():
                    self.cfg.removeEdge(v.vertexID, succID)
                disconnectedVertices.append(v)
        candidateSources = []
        for v in self.cfg:
            if v.number_of_successors() == 1 and v.vertexID != self.cfg.get_exitID():
                assert v not in disconnectedVertices
                candidateSources.append(v)
        for v in disconnectedVertices:
            if candidateSources and bool(random.getrandbits(1)):
                index   = random.randint(0, len(candidateSources) - 1)
                otherv  = candidateSources[index]
                for succID in otherv.successors.keys():
                    self.cfg.addEdge(v.vertexID, succID)
                self.cfg.addEdge(otherv.vertexID, v.vertexID)
            else:
                self.cfg.addEdge(v.vertexID, self.cfg.get_entryID())
                self.cfg.removeEdge(self.cfg.get_exitID(), self.cfg.get_entryID())
                self.cfg.addEdge(self.cfg.get_exitID(), v.vertexID)
                self.cfg.set_entryID(v.vertexID)
        assert self.cfg.getVertex(self.cfg.get_entryID()).number_of_predecessors() == 1, "entry = %s" % self.cfg.getVertex(self.cfg.get_entryID())
        assert self.cfg.getVertex(self.cfg.get_exitID()).number_of_successors() == 1, "exit = %s" % self.cfg.getVertex(self.cfg.get_exitID())
    
    def check_connected(self):
        visited = set()
        stack   = []
        stack.append(self.cfg.get_entryID())
        while stack:
            vertexID = stack.pop()
            visited.add(vertexID)
            for succID in self.cfg.getVertex(vertexID).successors.keys():
                if succID not in visited:
                    stack.append(succID)
        assert not set(self.cfg.the_vertices.keys()).difference(visited), "The CFG is not connected"

def set_call_graph_root(program, candidateCallSites):
    maxCallSites   = 0
    totalCallSites = 0
    rootName       = None
    for subprogramName, the_vertices in candidateCallSites.iteritems():
        if len(the_vertices) > maxCallSites:
            maxCallSites = len(the_vertices)
            rootName     = subprogramName
        totalCallSites += len(the_vertices)
    assert rootName, "Unable to find root for call graph"
    program.getCallGraph().setRootID(program.getCallGraph().getVertexWithName(rootName).vertexID)
    assert totalCallSites >= program.getCallGraph().numOfVertices()-1, "Unable to link the call graph because there are %d subprograms but only %d call sites" % (program.getCallGraph().numOfVertices()-1, totalCallSites)  

def add_tree_edges_to_call_graph(program, candidateCallSites):
    disconnected = []
    for callv in program.getCallGraph():
        if callv.number_of_predecessors() == 0 and callv.vertexID != program.getCallGraph().getRootID():
            disconnected.append(callv)
    callerv = program.getCallGraph().getVertex(program.getCallGraph().getRootID())
    while disconnected:
        callerName = callerv.getName()
        calleev    = disconnected.pop()
        index      = random.randint(0,len(candidateCallSites[callerName])-1)
        callSiteID = candidateCallSites[callerName][index].getVertexID()
        del candidateCallSites[callerName][index]
        program.getCallGraph().addEdge(callerv.getName(), calleev.getName(),callSiteID)
        if not candidateCallSites[callv.getName()] or bool(random.getrandbits(1)):
            for callv in program.getCallGraph():
                if callv not in disconnected and candidateCallSites[callv.getName()]:
                    callerv = callv
                    
def add_other_edges_to_call_graph(program, candidateCallSites):
    callg = program.getCallGraph()
    dfs   = trees.DepthFirstSearch(callg, callg.getRootID())
    for vertexID in dfs.getPostorder():
        calleev          = callg.getVertex(vertexID)
        candidateCallers = []
        for callv in callg:
            if dfs.getPostID(callv.getVertexID()) > dfs.getPostID(vertexID) \
            and candidateCallSites[callv.getName()] \
            and not calleev.hasPredecessor(callv.getVertexID()):
                candidateCallers.append(callv)
        if candidateCallers:
            numOfCallers = random.randint(1,len(candidateCallers))
            for i in xrange(1,numOfCallers+1):
                index   = random.randint(0,len(candidateCallers)-1)
                callerv = candidateCallers[index]
                del candidateCallers[index]
                callerName     = callerv.getName()
                numOfCallSites = random.randint(1,len(candidateCallSites[callerName]))
                for j in xrange(1,numOfCallSites+1):
                    index2     = random.randint(0,len(candidateCallSites[callerName])-1)
                    callSiteID = candidateCallSites[callerName][index2].getVertexID()
                    del candidateCallSites[callerName][index2]
                    program.getCallGraph().addEdge(callerv.getName(), calleev.getName(),callSiteID)

def cut_edges_from_call_graph(program):
    callg      = program.getCallGraph()
    candidates = []
    for callv in callg:
        if callv.number_of_predecessors() > 1:
            candidates.append(callv) 
    for callv in candidates:  
        if bool(random.getrandbits(1)):
            predIDs = callv.predecessors.keys()
            index   = random.randint(0,len(predIDs)-1)
            predID  = predIDs[index]
            callg.removeEdge(predID, callv.getVertexID()) 
    
def do_it():
    candidateCallSites = {}
    levelInCallgraph   = {}
    program            = programs.Program()
    for subprogramID in xrange(1, config.Arguments.subprograms+1):
        subprogramName                     = "F%d" % subprogramID
        candidateCallSites[subprogramName] = []
        levelInCallgraph[subprogramName]   = 0
        cfg                                = CreateCFG().cfg
        cfg.name                           = subprogramName
        program.add_CFG(cfg)
        for v in cfg:
            if v.number_of_successors() == 1 and v.vertexID != cfg.get_exitID():
                candidateCallSites[subprogramName].append(v)
    #set_call_graph_root(program, candidateCallSites)
    #add_tree_edges_to_call_graph(program, candidateCallSites)
    #add_other_edges_to_call_graph(program, candidateCallSites)
    #cut_edges_from_call_graph(program)
    return program

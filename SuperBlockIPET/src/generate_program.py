import programs
import trees
import directed_graphs
import vertices
import config
import random
import math

singleVertices = None
currentCFG     = None
freeVertices   = []
freeSESEs      = []

class SESEComponent:
    def __init__ (self):
        self.entryID = None
        self.exitID  = None

class LoopComponent:
    def __init__ (self, sese):
        global currentCFG
        self.headerID     = sese.entryID
        self.tailIDs      = set([])
        self.exitIDs      = set([])
        self.the_vertices = set([])
        stack = []
        stack.append(sese.entryID)
        while stack:
            vertexID = stack.pop()
            self.the_vertices.add(vertexID)
            v = currentCFG.getVertex(vertexID)
            for succID in v.successors.keys():
                if succID not in self.the_vertices:
                    stack.append(succID)

def create_loop_component(sese):
    global currentCFG
    loopComponent = LoopComponent(sese)
    loopComponent.headerID = sese.entryID
    loopComponent.tailIDs.add(sese.exitID)
    if bool(random.getrandbits(1)) and currentCFG.getVertex(sese.entryID).number_of_successors() == 1:
        loopComponent.exitIDs.add(sese.entryID)
    else:
        loopComponent.exitIDs.add(sese.exitID) 
    for tailID in loopComponent.tailIDs:
        currentCFG.addEdge(tailID, loopComponent.headerID)   
    return loopComponent

def connectRemainingVertices():
    global currentCFG
    global freeVertices
    global freeSESEs
    global singleVertices
    masterSese = SESEComponent()
    if freeSESEs:
        lastSese = freeSESEs.pop()
        while singleVertices:
            singleVertices -= 1
            vertexID = freeVertices.pop()
            if bool(random.getrandbits(1)):
                currentCFG.addEdge(vertexID, lastSese.entryID)
                lastSese.entryID = vertexID
            else:
                currentCFG.addEdge(lastSese.exitID, vertexID)
                lastSese.exitID = vertexID            
        masterSese.entryID = lastSese.entryID
        masterSese.exitID = lastSese.exitID
    else:
        predID   = vertices.dummyID
        vertexID = vertices.dummyID
        while singleVertices:
            singleVertices -= 1
            vertexID = freeVertices.pop()
            if predID != vertices.dummyID:
                currentCFG.addEdge(predID, vertexID)
            else:
                masterSese.entryID = vertexID
            predID = vertexID
        masterSese.exitID = vertexID
    while freeVertices:
        vertexID = freeVertices.pop()
        if bool(random.getrandbits(1)):
            currentCFG.addEdge(vertexID, masterSese.entryID)
            masterSese.entryID = vertexID
        else:
            currentCFG.addEdge(masterSese.exitID, vertexID)
            masterSese.exitID = vertexID      
    assert masterSese.entryID, "SESE entry ID not set"
    assert masterSese.exitID, "SESE exit ID not set"
    return masterSese

def findMergeVerticesToRemove():
    global currentCFG
    disconnectedVertices = []
    lnt = trees.LoopNests(currentCFG, currentCFG.get_entryID())
    for v in currentCFG:
        if v.number_of_predecessors() > 1 \
        and bool(random.getrandbits(1)) \
        and not (lnt.is_loop_header(v.vertexID) or 
             lnt.is_loop_tail(v.vertexID) or 
             lnt.is_loop_exit_source(v.vertexID) or 
             lnt.is_loop_exit_destination(v.vertexID) or 
             currentCFG.get_exitID() == v.vertexID):
            for predID in v.predecessors.keys():
                for succID in v.successors.keys():
                    currentCFG.addEdge(predID, succID)
            for predID in v.predecessors.keys():
                currentCFG.removeEdge(predID, v.vertexID)
            for succID in v.successors.keys():
                currentCFG.removeEdge(v.vertexID, succID)
            disconnectedVertices.append(v)
    candidateSources = []
    for v in currentCFG:
        if v.number_of_successors() == 1 and v.vertexID != currentCFG.get_exitID():
            assert v not in disconnectedVertices
            candidateSources.append(v)
    for v in disconnectedVertices:
        if candidateSources and bool(random.getrandbits(1)):
            index   = random.randint(0, len(candidateSources) - 1)
            otherv  = candidateSources[index]
            for succID in otherv.successors.keys():
                currentCFG.addEdge(v.vertexID, succID)
            currentCFG.addEdge(otherv.vertexID, v.vertexID)
        else:
            currentCFG.addEdge(v.vertexID, currentCFG.get_entryID())
            currentCFG.removeEdge(currentCFG.get_exitID(), currentCFG.get_entryID())
            currentCFG.addEdge(currentCFG.get_exitID(), v.vertexID)
            currentCFG.set_entryID(v.vertexID)
    assert currentCFG.getVertex(currentCFG.get_entryID()).number_of_predecessors() == 1, "entry = %s" % currentCFG.getVertex(currentCFG.get_entryID())
    assert currentCFG.getVertex(currentCFG.get_exitID()).number_of_successors() == 1, "exit = %s" % currentCFG.getVertex(currentCFG.get_exitID())

def connectDisconnectedComponents():
    global currentCFG
    global freeVertices
    global freeSESEs
    global singleVertices
    while len(freeSESEs) > 1:
        sese1 = freeSESEs.pop()
        sese2 = freeSESEs.pop()
        sourceID = sese1.exitID
        while singleVertices and bool(random.getrandbits(1)):
            singleVertices -= 1
            vertexID = freeVertices.pop()
            currentCFG.addEdge(sourceID, vertexID)
            sourceID = vertexID
        currentCFG.addEdge(sourceID, sese2.entryID)
        sese1.exitID = sese2.exitID
        freeSESEs.append(sese1)

def createSESEComponent():
    global currentCFG
    global freeVertices    
    global singleVertices
    global freeSESEs
    SESE    = SESEComponent()
    startID = freeVertices.pop()
    SESE.entryID = startID
    SESE.exitID  = startID
    if singleVertices and bool(random.getrandbits(1)):
        singleVertices -= 1
        vertexID = freeVertices.pop()
        currentCFG.addEdge(SESE.exitID, vertexID)
        SESE.exitID = vertexID
    elif freeSESEs and bool(random.getrandbits(1)):
        nestedSESE = freeSESEs.pop()
        currentCFG.addEdge(SESE.exitID, nestedSESE.entryID)
        SESE.exitID = nestedSESE.exitID
    return SESE

def createSwitch():
    global currentCFG
    global freeVertices
    branchID = freeVertices.pop()
    mergeID  = freeVertices.pop()
    for i in xrange(1, config.Arguments.fan_out+1):
        switchArm = createSESEComponent()
        currentCFG.addEdge(branchID, switchArm.entryID)
        currentCFG.addEdge(switchArm.exitID, mergeID)
    sese         = SESEComponent()
    sese.entryID = branchID
    sese.exitID  = mergeID
    return sese

def createShortCircuit():
    global currentCFG
    global freeVertices
    outerBranchID = freeVertices.pop()
    innerBranchID = freeVertices.pop()
    mergeID       = freeVertices.pop()
    thenBranch = createSESEComponent()
    elseBranch = createSESEComponent()
    currentCFG.addEdge(outerBranchID, innerBranchID)
    currentCFG.addEdge(outerBranchID, elseBranch.entryID)
    currentCFG.addEdge(innerBranchID, thenBranch.entryID)
    currentCFG.addEdge(innerBranchID, elseBranch.entryID)
    currentCFG.addEdge(thenBranch.exitID, mergeID)
    currentCFG.addEdge(elseBranch.exitID, mergeID)
    sese         = SESEComponent()
    sese.entryID = outerBranchID
    sese.exitID  = mergeID
    return sese

def createIfThen():
    global currentCFG
    global freeVertices
    branchID   = freeVertices.pop()
    mergeID    = freeVertices.pop()
    thenBranch = createSESEComponent()
    currentCFG.addEdge(branchID, thenBranch.entryID)
    currentCFG.addEdge(thenBranch.exitID, mergeID)
    currentCFG.addEdge(branchID, mergeID)
    sese         = SESEComponent()
    sese.entryID = branchID
    sese.exitID  = mergeID
    return sese

def createIfThenElse():
    global currentCFG
    global freeVertices
    branchID   = freeVertices.pop()
    mergeID    = freeVertices.pop()
    thenBranch = createSESEComponent()
    elseBranch = createSESEComponent()
    currentCFG.addEdge(branchID, thenBranch.entryID)
    currentCFG.addEdge(branchID, elseBranch.entryID)
    currentCFG.addEdge(thenBranch.exitID, mergeID)
    currentCFG.addEdge(elseBranch.exitID, mergeID)
    sese         = SESEComponent()
    sese.entryID = branchID
    sese.exitID  = mergeID
    return sese
    
def addAcyclicComponents(numberOfIfThenElses, numberOfIfThens, numberOfShortCircuits, numberOfSwitches):
    while numberOfIfThenElses > 0 or numberOfIfThens > 0 or numberOfShortCircuits > 0 or numberOfSwitches > 0:
        if numberOfIfThenElses > 0 and bool(random.getrandbits(1)):
            sese = createIfThenElse()
            freeSESEs.append(sese)
            numberOfIfThenElses -= 1
        if numberOfIfThens > 0 and bool(random.getrandbits(1)):
            sese = createIfThen()
            freeSESEs.append(sese)
            numberOfIfThens -= 1
        if numberOfShortCircuits > 0 and bool(random.getrandbits(1)):
            sese = createShortCircuit()
            freeSESEs.append(sese)
            numberOfShortCircuits -= 1
        if numberOfSwitches > 0 and bool(random.getrandbits(1)):
            sese = createSwitch()
            freeSESEs.append(sese)
            numberOfSwitches -= 1
        
def setNumberOfComponents(sizeOfComponent, verticesInLoop):
    numberOfComponents = 0
    if verticesInLoop >= sizeOfComponent:
        numberOfComponents = random.randint(1,math.floor(verticesInLoop/sizeOfComponent))
        verticesInLoop -= sizeOfComponent * numberOfComponents
    return verticesInLoop, numberOfComponents

def decideWhichAcyclicComponents(verticesInLoop):
    numberOfIfThenElses   = 0
    numberOfIfThens       = 0
    numberOfShortCircuits = 0
    numberOfSwitches      = 0
    if bool(random.getrandbits(1)):
        verticesInLoop, numberOfIfThenElses = setNumberOfComponents(4, verticesInLoop)
    if bool(random.getrandbits(1)):
        verticesInLoop, numberOfIfThens = setNumberOfComponents(3, verticesInLoop)
    if bool(random.getrandbits(1)):
        verticesInLoop, numberOfShortCircuits = setNumberOfComponents(5, verticesInLoop)
    if config.Arguments.fan_out > 2 and bool(random.getrandbits(1)):
        verticesInLoop, numberOfSwitches = setNumberOfComponents(2 + config.Arguments.fan_out, verticesInLoop)
    return verticesInLoop, numberOfIfThenElses, numberOfIfThens, numberOfShortCircuits, numberOfSwitches 

def add_continues(loopComponent):
    global currentCFG
    extraTails = 1
    for vertexID in loopComponent.the_vertices:
        if vertexID not in loopComponent.exitIDs \
        and vertexID not in loopComponent.tailIDs \
        and vertexID != loopComponent.headerID:
            v = currentCFG.getVertex(vertexID)
            if v.number_of_successors() == 1 and bool(random.getrandbits(1)) and extraTails:
                currentCFG.addEdge(vertexID, loopComponent.headerID)
                loopComponent.tailIDs.add(vertexID)
                extraTails -= 1

def add_breaks(loopComponent):
    global currentCFG
    extraBreaks = 1
    for vertexID in loopComponent.the_vertices:
        if vertexID not in loopComponent.exitIDs \
        and vertexID not in loopComponent.tailIDs \
        and vertexID != loopComponent.headerID:
            v = currentCFG.getVertex(vertexID)
            if v.number_of_successors() == 1 and bool(random.getrandbits(1)) and extraBreaks:
                loopComponent.exitIDs.add(vertexID)
                extraBreaks -= 1

def generateLNT():
    lnt = trees.Tree()
    # Add vertices to the tree, including one extra for the dummy outer loop
    vertexToLevel = {}
    for i in xrange(1,config.Arguments.loops+2):
        lnt.addVertex(i)
        lnt.rootID = i
        vertexToLevel[i] = 0
    # Add edges to the tree
    parentID = lnt.getRootID()
    for v in lnt:
        if v.vertexID != lnt.getRootID():
            newLevel = vertexToLevel[parentID] + 1
            if newLevel <= config.Arguments.nesting_depth:
                lnt.addEdge(parentID, v.vertexID)
                vertexToLevel[v.vertexID] = newLevel
                parentID = v.vertexID
            else:
                # The height of the LNT now exceeds the maximum depth
                # Backtrack to an arbitrary proper ancestor
                ancestorID = parentID
                while True:
                    ancestorID = lnt.getVertex(ancestorID).getParentID()
                    if bool(random.getrandbits(1)) or ancestorID == lnt.rootID:
                        break
                parentID = ancestorID
                lnt.addEdge(parentID, v.vertexID)
                vertexToLevel[v.vertexID] = vertexToLevel[parentID] + 1
                parentID = v.vertexID
    return lnt

def connect_nested_loops(lnt, treev, loopRegions):
    global currentCFG
    firstLoop  = None
    secondLoop = None
    if treev.number_of_successors() > 1:
        succIDs  = treev.successors.keys()
        while len(succIDs) > 1:
            v1       = lnt.getVertex(succIDs.pop())
            v2       = lnt.getVertex(succIDs[-1])
            predLoop = loopRegions[v1]
            succLoop = loopRegions[v2]
            if not firstLoop:
                firstLoop = predLoop
            secondLoop = succLoop
            for exitID in predLoop.exitIDs:
                currentCFG.addEdge(exitID, succLoop.headerID)
    else:
        succID     = treev.successors.keys()[0] 
        succv      = lnt.getVertex(succID)
        firstLoop  = loopRegions[succv]
        secondLoop = loopRegions[succv]
    assert firstLoop, "The first loop component has not been set"
    assert secondLoop, "The second loop component has not been set"
    parentLoop   = loopRegions[treev]
    sourcev      = None
    destinationv = None 
    for vertexID in parentLoop.the_vertices:
        v = currentCFG.getVertex(vertexID) 
        if v.number_of_successors() == 1 and v.successors.keys()[0] != parentLoop.headerID:
            succID       = v.successors.keys()[0]
            sourcev      = v
            destinationv = currentCFG.getVertex(succID)
            break
    if not sourcev:
        sourcev = currentCFG.getVertex(parentLoop.headerID)
        succIDs = sourcev.successors.keys()
        for succID in succIDs:
            currentCFG.removeEdge(parentLoop.headerID, succID)
        destinationID = succIDs[0]
        for succID in succIDs:
            if destinationID != succID:
                currentCFG.addEdge(destinationID, succID)
        destinationv = currentCFG.getVertex(destinationID)
    currentCFG.addEdge(sourcev.vertexID, firstLoop.headerID)
    for exitID in secondLoop.exitIDs:
        currentCFG.addEdge(exitID, destinationv.vertexID)

def create_CFG():
    global currentCFG
    global freeVertices
    global singleVertices
    lnt        = generateLNT()
    currentCFG = directed_graphs.CFG()
    for i in xrange(1,config.Arguments.basic_blocks+1):
        vertexID = currentCFG.getNextVertexID()
        currentCFG.addVertex(vertices.CFGVertex(vertexID))
        freeVertices.append(vertexID)
    loopRegions = {}
    for the_vertices in lnt.level_by_level_iterator(True):
        for treev in the_vertices:
            verticesInLoop = 0
            if treev.vertexID == lnt.rootID:
                verticesInLoop = len(freeVertices)
            else:
                maxLoopSize    = (config.Arguments.basic_blocks - 2)/config.Arguments.loops
                verticesInLoop = 2 + int(random.uniform(0,1) * (maxLoopSize - 1))
            singleVertices, numberOfIfThenElses, numberOfIfThens, numberOfShortCircuits, numberOfSwitches = decideWhichAcyclicComponents(verticesInLoop)
            addAcyclicComponents(numberOfIfThenElses, numberOfIfThens, numberOfShortCircuits, numberOfSwitches)
            connectDisconnectedComponents()
            sese = connectRemainingVertices()
            loopRegions[treev] = create_loop_component(sese)
            if treev.vertexID == lnt.rootID:
                currentCFG.set_entryID(sese.entryID)
                currentCFG.set_exitID(sese.exitID)
            else:
                if config.Arguments.continues:
                    add_continues(loopRegions[treev])
                if config.Arguments.breaks:
                    add_breaks(loopRegions[treev])
    for the_vertices in lnt.level_by_level_iterator(True):
        for treev in the_vertices:
            if treev.number_of_successors() > 0:
                connect_nested_loops(lnt, treev, loopRegions)
    if config.Arguments.unstructured:
        findMergeVerticesToRemove()

def isConnected():
    global currentCFG
    visited = set([])
    stack   = []
    stack.append(currentCFG.get_entryID())
    while stack:
        vertexID = stack.pop()
        visited.add(vertexID)
        for succID in currentCFG.getVertex(vertexID).successors.keys():
            if succID not in visited:
                stack.append(succID)
    assert not set(currentCFG.the_vertices.keys()).difference(visited), "The CFG is not connected"
    return visited

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
        create_CFG()
        currentCFG.name = subprogramName
        program.add_CFG(currentCFG)
        isConnected()
        for v in currentCFG:
            if v.number_of_successors() == 1 and v.vertexID != currentCFG.get_exitID():
                candidateCallSites[subprogramName].append(v)
    #set_call_graph_root(program, candidateCallSites)
    #add_tree_edges_to_call_graph(program, candidateCallSites)
    #add_other_edges_to_call_graph(program, candidateCallSites)
    #cut_edges_from_call_graph(program)
    return program

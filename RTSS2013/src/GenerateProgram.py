import Debug, Programs, Trees, CFGs, Vertices, UDrawGraph
import random, math

singleVertices = None
currentCFG     = None
maxFanOut      = 2
freeVertices   = []
freeSESEs      = []

class SESEComponent:
    def __init__ (self):
        self.entryID = None
        self.exitID  = None

class LoopComponent:
    def __init__ (self, sese):
        global currentCFG
        self.headerID = sese.entryID
        self.tailIDs  = set([])
        self.exitIDs  = set([])
        self.vertices = set([])
        stack = []
        stack.append(sese.entryID)
        while stack:
            vertexID = stack.pop()
            self.vertices.add(vertexID)
            v = currentCFG.getVertex(vertexID)
            for succID in v.getSuccessorIDs():
                if succID not in self.vertices:
                    stack.append(succID)

def createLoopComponent (sese):
    global currentCFG
    loopComponent = LoopComponent(sese)
    loopComponent.headerID = sese.entryID
    loopComponent.tailIDs.add(sese.exitID)
    if bool(random.getrandbits(1)) and currentCFG.getVertex(sese.entryID).numberOfSuccessors() == 1:
        loopComponent.exitIDs.add(sese.entryID)
    else:
        loopComponent.exitIDs.add(sese.exitID) 
    for tailID in loopComponent.tailIDs:
        currentCFG.addEdge(tailID, loopComponent.headerID)   
    return loopComponent

def connectRemainingVertices ():
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
        predID   = Vertices.dummyVertexID
        vertexID = Vertices.dummyVertexID
        while singleVertices:
            singleVertices -= 1
            vertexID = freeVertices.pop()
            if predID != Vertices.dummyVertexID:
                currentCFG.addEdge(predID, vertexID)
            else:
                masterSese.entryID = vertexID
            predID = vertexID
        masterSese.exitID = vertexID
    assert masterSese.entryID, "SESE entry ID not set"
    assert masterSese.exitID, "SESE exit ID not set"
    return masterSese

def findMergeVerticesToRemove ():
    pass

def connectDisconnectedComponents ():
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

def createSESEComponent ():
    global currentCFG
    global freeVertices    
    global singleVertices
    sese    = SESEComponent()
    startID = freeVertices.pop()
    sese.entryID = startID
    sese.exitID  = startID
    if singleVertices and bool(random.getrandbits(1)):
        singleVertices -= 1
        vertexID = freeVertices.pop()
        currentCFG.addEdge(sese.exitID, vertexID)
        sese.exitID = vertexID
    return sese

def createSwitch ():
    global currentCFG
    global freeVertices
    branchID = freeVertices.pop()
    mergeID  = freeVertices.pop()
    numberOfSwitchArms = 2 + int(random.uniform(maxFanOut - 2) + 1)
    for i in xrange(1, numberOfSwitchArms+1):
        switchArm = SESEComponent()
        currentCFG.addEdge(branchID, switchArm.entryID)
        currentCFG.addEdge(switchArm.exitID, mergeID)
    sese         = SESEComponent()
    sese.entryID = branchID
    sese.exitID  = mergeID
    return sese

def createShortCircuit ():
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

def createIfThen ():
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

def createIfThenElse ():
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
    
def addAcyclicComponents (numberOfIfThenElses, numberOfIfThens, numberOfShortCircuits, numberOfSwitches):
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
        
def setNumberOfComponents (sizeOfComponent, verticesInLoop):
    numberOfComponents = 0
    if verticesInLoop >= sizeOfComponent:
        numberOfComponents = random.randint(1,math.floor(verticesInLoop/sizeOfComponent))
        verticesInLoop -= sizeOfComponent * numberOfComponents
    return verticesInLoop, numberOfComponents

def decideWhichAcyclicComponents (verticesInLoop):
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
    if maxFanOut > 2 and bool(random.getrandbits(1)):
        verticesInLoop, numberOfSwitches = setNumberOfComponents(2 + maxFanOut, verticesInLoop)
    return verticesInLoop, numberOfIfThenElses, numberOfIfThens, numberOfShortCircuits, numberOfSwitches 

def generateLNT (loops, nestingDepth):
    lnt = Trees.Tree()
    # Add vertices to the tree, including one extra for the dummy outer loop
    vertexToLevel = {}
    for i in xrange(1,loops+2):
        lnt.addVertex(i)
        lnt.setRootID(i)
        vertexToLevel[i] = 0
    # Add edges to the tree
    parentID = lnt.getRootID()
    for v in lnt:
        vertexID = v.getVertexID()
        if vertexID != lnt.getRootID():
            newLevel = vertexToLevel[parentID] + 1
            if newLevel <= nestingDepth:
                lnt.addEdge(parentID, vertexID)
                vertexToLevel[vertexID] = newLevel
                parentID = vertexID
            else:
                # The height of the LNT now exceeds the maximum depth
                # Backtrack to an arbitrary proper ancestor
                ancestorID = parentID
                while True:
                    ancestorID = lnt.getVertex(ancestorID).getParentID()
                    if bool(random.getrandbits(1)) or ancestorID == lnt.getRootID():
                        break
                parentID = ancestorID
                lnt.addEdge(parentID, vertexID)
                vertexToLevel[vertexID] = vertexToLevel[parentID] + 1
                parentID = vertexID
    return lnt

def connectNestedLoops (lnt, treev, loopRegions):
    global currentCFG
    firstLoop  = None
    secondLoop = None
    if treev.numberOfSuccessors() > 1:
        succIDs  = treev.getSuccessorIDs()
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
        succID     = treev.getSuccessorIDs()[0] 
        succv      = lnt.getVertex(succID)
        firstLoop  = loopRegions[succv]
        secondLoop = loopRegions[succv]
    assert firstLoop, "The first loop component has not been set"
    assert secondLoop, "The second loop component has not been set"
    parentLoop   = loopRegions[treev]
    sourcev      = None
    destinationv = None 
    for vertexID in parentLoop.vertices:
        v = currentCFG.getVertex(vertexID) 
        if v.numberOfSuccessors() == 1 and v.getSuccessorIDs()[0] != parentLoop.headerID:
            succID       = v.getSuccessorIDs()[0]
            sourcev      = v
            destinationv = currentCFG.getVertex(succID)
            break
    if not sourcev:
        sourcev = currentCFG.getVertex(parentLoop.headerID)
        succIDs = sourcev.getSuccessorIDs()
        for succID in succIDs:
            currentCFG.removeEdge(parentLoop.headerID, succID)
        destinationID = succIDs[0]
        for succID in succIDs:
            if destinationID != succID:
                currentCFG.addEdge(destinationID, succID)
        destinationv = currentCFG.getVertex(destinationID)
    currentCFG.addEdge(sourcev.getVertexID(), firstLoop.headerID)
    for exitID in secondLoop.exitIDs:
        currentCFG.addEdge(exitID, destinationv.getVertexID())

def generateCFG (cfgVertices, 
                 loops, 
                 selfLoops, 
                 nestingDepth,
                 breaks, 
                 continues):
    global currentCFG
    global freeVertices
    global singleVertices
    lnt        = generateLNT(loops, nestingDepth)
    currentCFG = CFGs.CFG()
    for i in xrange(1,cfgVertices+1):
        vertexID = currentCFG.getNextVertexID()
        currentCFG.addVertex(Vertices.BasicBlock(vertexID))
        freeVertices.append(vertexID)
    loopRegions = {}
    for level, vertices in lnt.levelIterator(True):
        for treev in vertices:
            verticesInLoop = 0
            if level == 0:
                verticesInLoop = len(freeVertices)
            else:
                maxLoopSize    = (cfgVertices - 2)/loops
                verticesInLoop = 2 + int(random.uniform(0,1) * (maxLoopSize - 1))
            singleVertices, numberOfIfThenElses, numberOfIfThens, numberOfShortCircuits, numberOfSwitches = decideWhichAcyclicComponents(verticesInLoop)
            addAcyclicComponents(numberOfIfThenElses, numberOfIfThens, numberOfShortCircuits, numberOfSwitches)
            connectDisconnectedComponents()
            findMergeVerticesToRemove()
            sese = connectRemainingVertices()
            loopRegions[treev] = createLoopComponent(sese)
            if level == 0:
                currentCFG.setEntryID(sese.entryID)
                currentCFG.setExitID(sese.exitID)
    for level, vertices in lnt.levelIterator(True):
        for treev in vertices:
            if treev.numberOfSuccessors() > 0:
                connectNestedLoops(lnt, treev, loopRegions)

def isConnected ():
    global currentCFG
    visited = set([])
    stack   = []
    stack.append(currentCFG.getEntryID())
    while stack:
        vertexID = stack.pop()
        visited.add(vertexID)
        for succID in currentCFG.getVertex(vertexID).getSuccessorIDs():
            if succID not in visited:
                stack.append(succID)
    assert not currentCFG.getVertexIDs().difference(visited), "The CFG is not connected"
    return visited

def generate (subprograms, 
              cfgVertices, 
              fanOut, 
              loops, 
              selfLoops, 
              nestingDepth,
              breaks, 
              continues):
    maxFanOut          = fanOut
    candidateCallSites = {}
    levelInCallgraph   = {}
    program            = Programs.Program()
    for subprogramID in xrange(1, subprograms+1):
        subprogramName                     = "F%d" % subprogramID
        candidateCallSites[subprogramName] = set([])
        levelInCallgraph[subprogramName]   = 0
        Debug.debugMessage("Generating subprogram %s" % subprogramName, 1)
        generateCFG (cfgVertices, loops, selfLoops, nestingDepth, breaks, continues)
        program.addICFG(currentCFG, subprogramName)
        UDrawGraph.makeUdrawFile(currentCFG, subprogramName)
        isConnected()
        for v in currentCFG:
            if v.numberOfSuccessors() == 1 and v.getVertexID() != currentCFG.getExitID():
                candidateCallSites[subprogramName].add(v)
    return program
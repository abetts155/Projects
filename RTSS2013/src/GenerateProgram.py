import Debug, Programs, Trees, CFGs, Vertices
import random, math

currentCFG   = None
maxFanOut    = 2
freeVertices = 0
freeSESEs    = []

class SESEComponent:
    def __init__(self):
        entryID = None
        exitID  = None

def connectRemainingVertices ():
    global freeVertices
    global freeSESEs
    masterSese = SESEComponent()
    if freeSESEs:
        lastSese = freeSESEs.pop()
        while freeVertices:
            freeVertices -= 1
            vertexID = currentCFG.getNextVertexID()
            currentCFG.addVertex(Vertices.BasicBlock(vertexID))
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
        while freeVertices:
            freeVertices -= 1
            vertexID = currentCFG.getNextVertexID()
            currentCFG.addVertex(Vertices.BasicBlock(vertexID))
            if predID != Vertices.dummyVertexID:
                currentCFG.addEdge(predID, vertexID)
            else:
                masterSese.entryID = vertexID
            predID = vertexID
        masterSese.exitID = vertexID
    return masterSese

def findMergeVerticesToRemove ():
    pass

def connectDisconnectedComponents ():
    global currentCFG
    global freeVertices
    global freeSESEs
    while len(freeSESEs) > 1:
        sese1 = freeSESEs.pop()
        sese2 = freeSESEs.pop()
        sourceID = sese1.exitID
        while freeVertices and bool(random.getrandbits(1)):
            vertexID = currentCFG.getNextVertexID()
            currentCFG.addVertex(Vertices.BasicBlock(vertexID))
            currentCFG.addEdge(sourceID, vertexID)
            sourceID = vertexID
            freeVertices -= 1
        currentCFG.addEdge(sourceID, sese2.entryID)
        sese1.exitID = sese2.exitID
        freeSESEs.append(sese1)

def createSESEComponent ():
    global freeVertices
    sese    = SESEComponent()
    startID = currentCFG.getNextVertexID()
    currentCFG.addVertex(Vertices.BasicBlock(startID))
    sese.entryID = startID
    sese.exitID  = startID
    if freeVertices > 0 and bool(random.getrandbits(1)):
        vertexID = currentCFG.getNextVertexID()
        currentCFG.addVertex(Vertices.BasicBlock(vertexID))
        currentCFG.addEdge(sese.exitID, vertexID)
        sese.exitID = vertexID
        freeVertices -= 1
    return sese

def createSwitch ():
    branchID = currentCFG.getNextVertexID()
    currentCFG.addVertex(Vertices.BasicBlock(branchID))
    mergeID = currentCFG.getNextVertexID()
    currentCFG.addVertex(Vertices.BasicBlock(mergeID))
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
    outerBranchID = currentCFG.getNextVertexID()
    currentCFG.addVertex(Vertices.BasicBlock(outerBranchID))
    innerBranchID = currentCFG.getNextVertexID()
    currentCFG.addVertex(Vertices.BasicBlock(innerBranchID))
    thenBranch = createSESEComponent()
    elseBranch = createSESEComponent()
    currentCFG.addEdge(outerBranchID, innerBranchID)
    currentCFG.addEdge(outerBranchID, elseBranch.entryID)
    currentCFG.addEdge(innerBranchID, thenBranch.entryID)
    currentCFG.addEdge(innerBranchID, elseBranch.entryID)
    mergeID = currentCFG.getNextVertexID()
    currentCFG.addVertex(Vertices.BasicBlock(mergeID))
    currentCFG.addEdge(thenBranch.exitID, mergeID)
    currentCFG.addEdge(elseBranch.exitID, mergeID)
    sese         = SESEComponent()
    sese.entryID = outerBranchID
    sese.exitID  = mergeID
    return sese

def createIfThen ():
    branchID = currentCFG.getNextVertexID()
    currentCFG.addVertex(Vertices.BasicBlock(branchID))
    thenBranch = createSESEComponent()
    currentCFG.addEdge(branchID, thenBranch.entryID)
    mergeID = currentCFG.getNextVertexID()
    currentCFG.addVertex(Vertices.BasicBlock(mergeID))
    currentCFG.addEdge(thenBranch.exitID, mergeID)
    currentCFG.addEdge(branchID, mergeID)
    sese         = SESEComponent()
    sese.entryID = branchID
    sese.exitID  = mergeID
    return sese

def createIfThenElse ():
    branchID = currentCFG.getNextVertexID()
    currentCFG.addVertex(Vertices.BasicBlock(branchID))
    thenBranch = createSESEComponent()
    elseBranch = createSESEComponent()
    currentCFG.addEdge(branchID, thenBranch.entryID)
    currentCFG.addEdge(branchID, elseBranch.entryID)
    mergeID = currentCFG.getNextVertexID()
    currentCFG.addVertex(Vertices.BasicBlock(mergeID))
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
    print verticesInLoop, numberOfIfThenElses, numberOfIfThens, numberOfShortCircuits, numberOfSwitches
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

def generateCFG (cfgVertices, 
                 loops, 
                 selfLoops, 
                 nestingDepth,
                 breaks, 
                 continues):
    global currentCFG
    currentCFG         = CFGs.CFG()
    remainingVertices  = cfgVertices
    maxLoopSize        = (cfgVertices - 2)/loops
    lnt                = generateLNT(loops, nestingDepth)
    acyclicLoopRegions = {}
    for level, vertices in lnt.levelIterator(True):
        for v in vertices:
            verticesInLoop = 0
            if level == 0:
                verticesInLoop = remainingVertices
            else:
                verticesInLoop     = 2 + int(random.uniform(0,1) * (maxLoopSize - 1))
                remainingVertices -= verticesInLoop
            freeVertices, numberOfIfThenElses, numberOfIfThens, numberOfShortCircuits, numberOfSwitches = decideWhichAcyclicComponents(verticesInLoop)
            addAcyclicComponents(numberOfIfThenElses, numberOfIfThens, numberOfShortCircuits, numberOfSwitches)
            connectDisconnectedComponents()
            findMergeVerticesToRemove()
            sese = connectRemainingVertices()
            acyclicLoopRegions[v] = sese

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
        print currentCFG
        for v in currentCFG:
            if v.numberOfSuccessors() == 1 and v.getVertexID() != currentCFG.getExitID():
                candidateCallSites[subprogramName].add(v)
    return program
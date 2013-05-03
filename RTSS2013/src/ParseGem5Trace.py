import Debug, ARM, UDrawGraph, SuperBlocks, Vertices

class SuperBlockParser ():
    def __init__ (self, program, traceFiles):
        self.__program          = program
        self.__allruns          = set([])
        self.__observedCFGs     = set([])
        self.__firstAddr        = None
        self.__lastAddr         = None
        self.__rootCFG          = None
        self.__rootsuperg       = None
        self.__currentCFG       = None
        self.__currentBB        = None
        self.__currentSuperg    = None
        self.__stack            = []
        self.__superblockToRuns = {}
        self.__buildAddressInformation()
        self.__parse(traceFiles)
        self.__computePathInformation()
        
    def __buildAddressInformation (self):
        self.__rootCFG    = self.__program.getRootICFG()
        self.__rootsuperg = self.__program.getRootSuperBlockCFG()
        self.__firstAddr  = self.__rootCFG.getFirstInstruction().getAddress()
        lastbb            = self.__rootCFG.getVertex(self.__rootCFG.getExitID())
        for instruction in reversed(lastbb.getInstructions()):
            if instruction.getOp() not in ARM.ARMInstructionSet.Nops:
                self.__lastAddr = instruction.getAddress()
                break
        assert self.__lastAddr, "Unable to find last address"
        Debug.debugMessage("Start address of root function '%s' is %s" % (self.__rootCFG.getName(), hex(self.__firstAddr)), 1)    
        Debug.debugMessage("End address of root function '%s' is %s" % (self.__rootCFG.getName(), hex(self.__lastAddr)), 1)
        self.__observedCFGs.add(self.__rootCFG)
        
    def __parse (self, traceFiles):
        import shlex
        self.__runID = 0
        for filename in traceFiles:
            parsing = False 
            with open(filename, 'r') as f:
                self.__runID += 1
                self.__allruns.add(self.__runID)
                Debug.debugMessage("Analysing gem5 trace file '%s'" % filename, 1)
                for line in f:
                    lexemes  = shlex.split(line)
                    PCLexeme = lexemes[-1]
                    assert len(PCLexeme) == 11, "Unable to parse program counter %s" % PCLexeme
                    try:
                        PCLexeme = PCLexeme[5:]
                        PC       = int(PCLexeme, 16)
                        if PC == self.__firstAddr:
                            parsing              = True
                            self.__currentCFG    = self.__rootCFG
                            self.__currentBB     = self.__currentCFG.getVertex(self.__currentCFG.getEntryID())
                            self.__currentSuperg = self.__rootsuperg 
                        if parsing:
                            self.__parseAddress (PC)
                        if PC == self.__lastAddr:
                            parsing              = False
                            self.__currentCFG    = None
                            self.__currentBB     = None
                            self.__currentSuperg = None
                    except ValueError:
                        Debug.exitMessage("Cannot cast %s into an integer: it is not a hexadecimal string" % PCLexeme)

    def __getCFGWithAddress (self, address):
        for cfg in self.__program.getICFGs():
            firstAddress = cfg.getFirstInstruction().getAddress()
            if firstAddress == address:
                return cfg
        assert False, "Unable to find CFG with start address %s" % hex(address)
    
    def __parseAddress (self, address):
        oldBB = self.__currentBB
        if not self.__currentBB.hasAddress(address):
            for succID in self.__currentBB .getSuccessorIDs():
                succv = self.__currentCFG.getVertex(succID)
                if succv.hasAddress(address):
                    self.__currentBB = succv
                    break
            # Unable to find a successor of the current basic block with that address
            # It must be a call or a return
            if oldBB == self.__currentBB:
                if self.__currentCFG.getExitID() ==  self.__currentBB.getVertexID() and self.__currentCFG != self.__rootCFG:
                    (self.__currentCFG, self.__currentBB, self.__currentSuperg) = self.__stack.pop()
                else:
                    callerFrame = (self.__currentCFG, self.__currentBB, self.__currentSuperg)
                    self.__stack.append(callerFrame)
                    self.__currentCFG    = self.__getCFGWithAddress(address)
                    self.__currentBB     = self.__currentCFG.getVertex(self.__currentCFG.getEntryID())
                    self.__currentSuperg = self.__program.getSuperBlockCFG(self.__currentCFG.getName())
                    self.__observedCFGs.add(self.__currentCFG)
                    assert  self.__currentBB.hasAddress(address), "Calling into '%s' because of address %s but basic block does not contain an instruction with that address" % (self.__currentCFG.getName(), hex(address))      
        superv = self.__currentSuperg.getSuperBlock(self.__currentBB.getVertexID()) 
        if self.__currentSuperg.isMonitoredSuperBlock(superv) and self.__currentBB.getVertexID() == superv.getRepresentativeID(): 
            if superv.getVertexID() not in self.__superblockToRuns:
                self.__superblockToRuns[superv.getVertexID()] = set([])
            self.__superblockToRuns[superv.getVertexID()].add(self.__runID)
        elif self.__currentSuperg.isMonitoredEdge(oldBB.getVertexID(), self.__currentBB.getVertexID()):
            superv = self.__currentSuperg.getMonitoredEdgeSuperBlock(oldBB.getVertexID(), self.__currentBB.getVertexID())
            if superv.getVertexID() not in self.__superblockToRuns:
                self.__superblockToRuns[superv.getVertexID()] = set([])
            self.__superblockToRuns[superv.getVertexID()].add(self.__runID)
        Debug.debugMessage("Now in CFG '%s' at basic block %d" % (self.__currentCFG.getName(),  self.__currentBB.getVertexID()), 100)    
        
    def __computePathInformation (self):        
        for cfg in self.__observedCFGs:    
            functionName = cfg.getName()      
            superg       = self.__program.getSuperBlockCFG(functionName)  
            pathg        = SuperBlocks.PathInformationGraph()
            vertices     = superg.getBottomLayer()
            for pathv in vertices:
                pathg.vertices[pathv.getVertexID()] = pathv
                for theSet in pathv.setsToRuns.keys():
                    assert len(theSet) == 1
                    superVertexID = list(theSet)[0]
                    superv        = superg.getVertex(superVertexID)
                    if superv in self.__superblockToRuns:
                        pathv.setsToRuns[theSet].update(self.__superblockToRuns[superv])
                
            bottomLayer = True
            while len(vertices) > 1:
                newVertices = []
                i = 0
                while i < len(vertices) - 1: 
                    newVertexID = pathg.getNextVertexID()
                    predv       = Vertices.PathInformationVertex(newVertexID)
                    pathg.vertices[newVertexID] = predv
                    newVertices.append(predv)
                    succv1 = vertices[i]
                    succv2 = vertices[i+1]
                    pathg.addEdge(predv.getVertexID(), succv1.getVertexID())
                    pathg.addEdge(predv.getVertexID(), succv2.getVertexID())
                    for set1 in succv1.setsToRuns.keys():
                        for set2 in succv2.setsToRuns.keys():
                            newset = set([])
                            newset.update(set1)
                            newset.update(set2)
                            if bottomLayer or set1.intersection(set2):
                                runs = succv1.setsToRuns[set1].intersection(succv2.setsToRuns[set2])
                                predv.setsToRuns[frozenset(newset)] = runs
                                if not runs:
                                    print "{%s} is in EXCLUSIVE" % (', '.join(str(vertexID) for vertexID in newset))
                                if self.__allruns.intersection(runs) == self.__allruns:
                                    print "{%s} is in INCLUSIVE" % (', '.join(str(vertexID) for vertexID in newset))
                    i += 1
                bottomLayer = False
                vertices = newVertices
            pathg.setRootID(vertices[0].getVertexID())
            UDrawGraph.makeUdrawFile(cfg, "%s.%s" % (functionName, "cfg"))
            superg = self.__program.getSuperBlockCFG(functionName)
            UDrawGraph.makeUdrawFile(superg, "%s.%s" % (functionName, "superg"))
            UDrawGraph.makeUdrawFile(pathg, "%s.%s" % (superg.getName(), "pathg"))
    



                    

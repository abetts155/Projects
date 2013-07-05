import Programs, CFGs, Trees, Vertices, SuperBlocks, Calculations

enabled  = False
basename = ""

beginAttributes = "["
beginGraph      = "[\n"
endAttibutes    = "],"
endEdge         = ")"
endGraph        = "]\n"
endVertex       = "])),"
fileNameSuffix  = ".udraw"
newEdge         = "\ne(\"tEdge\","
newLine         = "\\n"

class COLOR:
    RED    = "red"
    YELLOW = "yellow" 
    BLUE   = "blue"
    GREEN  = "green"
    BLACK  = "black"
    
class SHAPE:
    BOX      = "box" 
    CIRCLE   = "circle"
    ELLIPSE  = "ellipse" 
    RHOMBUS  = "rhombus" 
    TRIANGLE = "triangle"
    
class EDGESHAPE:
    SOLID  = "solid"
    DASHED = "dashed"
    DOTTED = "dotted"
    
class DIRECTION:
    DESTINATION = "last"
    SOURCE      = "first"
    BOTH        = "both"
    NONE        = "none"

def newVertex (vertexID):
    return "l(\"v" + str(vertexID) + "\",n(\"tVertex\","

def edgeLink (vertexID):
    return "r(\"v" + str(vertexID) + "\")"

def setName (name):
    return "a(\"OBJECT\", \"" + name + "\"),"

def setColor (color):
    return "a(\"COLOR\", \"" + color + "\"),"

def setShape (shape):
    return "a(\"_GO\", \"" + shape + "\"),"

def setEdgeDirection (direction):
    return "a(\"_DIR\", \"" + direction + "\"),"

def setToolTip (tooltip):
    return "a(\"INFO\", \"" + tooltip + "\"),"

def setEdgePattern (shape, width):
    return "a(\"EDGEPATTERN\", \"single;" + shape + ";" + str(width) + ";1\"),"

def setEdgeColor (color):
    return "a(\"EDGECOLOR\", \"" + color + "\"),"

def makeUdrawFile (g, fileNamePrefix):
    global basename
    if enabled:
        filename = "%s.%s" % (basename, fileNamePrefix + fileNameSuffix)
        with open(filename, 'w') as f:
            f.write(beginGraph)
            # CFG or Instrumented CFG
            if isinstance(g, CFGs.CFG):
                colors = ['#FFFFFF', '#FFAEB9', '#98FB98', '#CD919E', '#FFF8DC', '#FF83FA', '#00FA9A', '#9370DB', '#BCD2EE', '#E3A869','#FF4040','#DCDCDC','#A8A8A8']
                colorsIterator = iter(colors) 
                colorMapping   = {}
                writeCFGVertex(colorMapping, colorsIterator, g, g.getEntryID(), f)
                for v in g:
                    vertexID = v.getVertexID()
                    if vertexID != g.getEntryID():
                        writeCFGVertex(colorMapping, colorsIterator, g, vertexID, f)   
            elif isinstance(g, SuperBlocks.SuperBlockGraph):
                for v in g:
                    vertexID = v.getVertexID()
                    superv   = g.getVertex(vertexID)
                    writeSuperBlockVertex(superv, f)   
                    writeSuperBlockVertexLinks(superv, f)
            elif isinstance(g, SuperBlocks.SuperBlockPathInformationGraph):
                for v in g:
                    vertexID = v.getVertexID()
                    writeSuperBlockPathInformationVertex(g, vertexID, f)   
            elif isinstance(g, Programs.CallGraph) or isinstance(g, Programs.ContextGraph):
                writeCallGraphVertex(g, g.getRootID(), f)
                for v in g:
                    vertexID = v.getVertexID()
                    if vertexID != g.getRootID():
                        writeCallGraphVertex(g, vertexID, f) 
            # Loop-Nesting Tree
            elif isinstance(g, Trees.LoopNests):
                for v in g:
                    writeTreeVertex(g, v.getVertexID(), f)
            elif isinstance(g, Calculations.ArithmeticExpressionTree):
                for v in g:
                    writeArithmeticVertex(g, v.getVertexID(), f)
            else:
                for v in g:
                    writeVertex(g, v.getVertexID(), f)
            f.write(endGraph) 
            
def writeVertex (g, vertexID, f):
    v = g.getVertex(vertexID)        
    f.write(newVertex(vertexID))
    f.write(beginAttributes)
    f.write(setName(str(vertexID)))
    f.write(endAttibutes)
    
    f.write(beginAttributes)
    for succID in v.getSuccessorIDs():
        f.write(newEdge)
        f.write(beginAttributes)
        f.write(setName(str(succID)))
        f.write(endAttibutes)
        f.write(edgeLink(succID))
        f.write(endEdge + ",\n")
    f.write(endVertex + "\n") 
    
def writeCFGVertex (colorMapping, colorsIterator, cfg, vertexID, f):
    v = cfg.getVertex(vertexID)
    f.write(newVertex(vertexID))
    f.write(beginAttributes)
    tooltip = ""
    if isinstance(v, Vertices.CFGEdge):
        f.write(setName(str(v.edge)))
    else:
        name = "%d (original=%d)" % (vertexID, v.getOriginalVertexID())
        f.write(setName(name))
        if v.getName():
            if v.getName() not in colorMapping:
                colorMapping[v.getName()] = colorsIterator.next()
            f.write(setColor(colorMapping[v.getName()]))
            if v.getName() != cfg.getName():
                tooltip += "Callee basic block for %s%s" % (v.getName(), newLine)
        if v.hasInstructions():
            for instruction in v.getInstructions():
                tooltip += instruction.__str__() + newLine
            tooltip = tooltip.rstrip(newLine)
    f.write(setToolTip(tooltip))
    f.write(endAttibutes)
    
    f.write(beginAttributes)
    for succe in v.getSuccessorEdges():
        f.write(newEdge)
        f.write(beginAttributes)
        if succe.hasEdgeID():
            f.write(setName("%d" % succe.getEdgeID()))
        f.write(endAttibutes)
        f.write(edgeLink(succe.getVertexID()))
        f.write(endEdge + ",\n")
    f.write(endVertex + "\n")
    
def writeCallGraphVertex (callg, vertexID, f):
    v = callg.getVertex(vertexID)
    f.write(newVertex(vertexID))
    f.write(beginAttributes)
    f.write(setName("%s (vertex id = %d)" % (v.getName(), vertexID)))
    if vertexID == callg.getRootID():
        f.write(setColor(COLOR.RED))
        f.write(setToolTip("Root"))
    f.write(endAttibutes)
    
    f.write(beginAttributes)
    for succID in v.getSuccessorIDs():
        f.write(newEdge)
        f.write(beginAttributes)
        calle   = v.getSuccessorEdge(succID)
        tooltip = ', '.join(str(vertexID) for vertexID in calle.getCallSites())
        f.write(setToolTip(tooltip))
        f.write(endAttibutes)
        f.write(edgeLink(succID))
        f.write(endEdge + ",\n")
    f.write(endVertex + "\n")
    
def writeArithmeticVertex (arithmetict, vertexID, f):
    v = arithmetict.getVertex(vertexID)
    if isinstance(v, Vertices.SuperBlock):
        writeSuperBlockVertex(v, f)
    else:
        f.write(newVertex(vertexID))
        f.write(beginAttributes)
        op   = v.getOperator()
        name = ""
        name += "op        = " + op + newLine
        name += "WCET  = %s" % sorted(v.getWCETs())
        if op == '+':
            f.write(setColor('#05EDFF'))
        elif op == 'max':            
            f.write(setColor('#D44942'))
        else:
            name += newLine + "bound = %d" % v.getBound()
            f.write(setColor('#EEC900'))
        f.write(setName(name))
        f.write(endAttibutes)
    
    f.write(beginAttributes)
    for succID in v.getSuccessorIDs():
        f.write(newEdge)
        f.write(beginAttributes)
        f.write(endAttibutes)
        f.write(edgeLink(succID))
        f.write(endEdge + ",\n")
    f.write(endVertex + "\n")
    
def writeSuperBlockVertex (superv, f):
    f.write(newVertex(superv.getVertexID()))
    f.write(beginAttributes)
    name = "super ID = %d%s" % (superv.getVertexID(), newLine)
    if superv.getBasicBlockIDs():
        name += "{%s}" % ', '.join(str(vertexID) for vertexID in superv.getBasicBlockIDs())
    if superv.getEdges ():
        name += newLine 
        name += "{%s}" % ', '.join(str(edge) for edge in superv.getEdges())
    f.write(setName(name))
    f.write(endAttibutes)
    
def writeSuperBlockVertexLinks (superv, f):
    colors     = ['#05EDFF', '#D44942', '#003F87', '#CD3700', '#EEC900']
    colorIndex = -1
    f.write(beginAttributes)
    for branchID, succEdges in superv.getBranchPartitions().iteritems():
        colorIndex += 1
        color      = colors[colorIndex]
        for succe in succEdges:
            f.write(newEdge)
            f.write(beginAttributes)
            succe = superv.getSuccessorEdge(succe.getVertexID())
            f.write(setName(str(branchID)))
            f.write(setEdgeColor(color))
            f.write(endAttibutes)
            f.write(edgeLink(succe.getVertexID()))
            f.write(endEdge + ",\n")
    f.write(endVertex + "\n")
    
def writeSuperBlockPathInformationVertex (partitiong, vertexID, f):
    v = partitiong.getVertex(vertexID)
    f.write(newVertex(vertexID))
    f.write(beginAttributes)
    name  = "Superv    = %d%s" % (vertexID, newLine)
    name += "Partition = %d" % v.partitionID
    f.write(setName(name))
    if v.acyclicPartition:
        f.write(setColor(COLOR.RED))
        f.write(setToolTip("Acyclic partition"))
    f.write(endAttibutes)
    
    f.write(beginAttributes)
    for succID in v.getSuccessorIDs():
        f.write(newEdge)
        f.write(beginAttributes)
        f.write(setEdgeDirection(DIRECTION.NONE))
        f.write(endAttibutes)
        f.write(edgeLink(succID))
        f.write(endEdge + ",\n")
    f.write(endVertex + "\n")

def writeTreeVertex (tree, vertexID, f):
    v = tree.getVertex(vertexID)
    f.write(newVertex(vertexID))
    f.write(beginAttributes)
    f.write(setName(str(vertexID)))
    if isinstance(tree, Trees.LoopNests):
        if isinstance(v, Vertices.HeaderVertex):
            f.write(setShape(SHAPE.TRIANGLE))
            f.write(setColor(COLOR.RED))
            f.write(setToolTip("Header ID = %s" % v.getHeaderID()))
        elif tree.isLoopExit(vertexID):
            f.write(setShape(SHAPE.ELLIPSE))
            f.write(setColor(COLOR.YELLOW))
            f.write(setToolTip("Loop Exit"))
    f.write(endAttibutes)
    
    f.write(beginAttributes)
    for succID in v.getSuccessorIDs():
        f.write(newEdge)
        f.write(beginAttributes)
        f.write(setName(str(succID)))
        f.write(endAttibutes)
        f.write(edgeLink(succID))
        f.write(endEdge + ",\n")
    f.write(endVertex + "\n")   

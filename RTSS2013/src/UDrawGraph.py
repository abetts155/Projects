import Programs, CFGs, Trees, Vertices, SuperBlocks, Edges

enabled = False

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
    if enabled:
        with open(fileNamePrefix + fileNameSuffix, 'w') as f:
            f.write(beginGraph)
            # CFG or Instrumented CFG
            if isinstance(g, CFGs.CFG):
                writeCFGVertex(g, g.getEntryID(), f)
                for v in g:
                    vertexID = v.getVertexID()
                    if vertexID != g.getEntryID():
                        writeCFGVertex(g, vertexID, f)   
            elif isinstance(g, SuperBlocks.SuperBlockGraph):
                for v in g:
                    vertexID = v.getVertexID()
                    writeSuperBlockVertex(g, vertexID, f)   
            elif isinstance(g, Programs.CallGraph):
                writeCallGraphVertex(g, g.getRootID(), f)
                for v in g:
                    vertexID = v.getVertexID()
                    if vertexID != g.getRootID():
                        writeCallGraphVertex(g, vertexID, f) 
            # Loop-Nesting Tree
            elif isinstance(g, Trees.LoopNests):
                for v in g:
                        writeTreeVertex(g, v.getVertexID(), f)
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
    
def writeCFGVertex (cfg, vertexID, f):
    v = cfg.getVertex(vertexID)
    f.write(newVertex(vertexID))
    f.write(beginAttributes)
    f.write(setName(str(vertexID)))
    if isinstance(v, Vertices.Ipoint):
        f.write(setShape(SHAPE.CIRCLE))
        f.write(setColor(COLOR.YELLOW))
    else:
        if v.hasInstructions():
            tooltip = ""
            for instruction in v.getInstructions():
                tooltip += instruction.__str__() + newLine
            tooltip = tooltip.rstrip(newLine)
            f.write(setToolTip(tooltip))
    f.write(endAttibutes)
    
    f.write(beginAttributes)
    for succID in v.getSuccessorIDs():
        f.write(newEdge)
        f.write(beginAttributes)
        f.write(endAttibutes)
        f.write(edgeLink(succID))
        f.write(endEdge + ",\n")
    f.write(endVertex + "\n")
    
def writeCallGraphVertex (callg, vertexID, f):
    v = callg.getVertex(vertexID)
    f.write(newVertex(vertexID))
    f.write(beginAttributes)
    f.write(setName(v.__str__()))
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
    
def writeSuperBlockVertex (superg, vertexID, f):
    v = superg.getVertex(vertexID)
    f.write(newVertex(vertexID))
    f.write(beginAttributes)
    name = ', '.join(str(vertexID) for vertexID in v.getBasicBlockIDs())
    name += "%ssuper ID = %d" % (newLine, vertexID)
    f.write(setName(name))
    f.write(endAttibutes)
    
    f.write(beginAttributes)
    for succID in v.getSuccessorIDs():
        f.write(newEdge)
        f.write(beginAttributes)
        succe = v.getSuccessorEdge(succID)
        f.write(setName(str(succe.getBasicBlockID())))
        if isinstance(succe, Edges.SuperBlockLoopEdge):
            f.write(setEdgePattern(EDGESHAPE.DOTTED, 4))
            f.write(setEdgeColor("#339999"))
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

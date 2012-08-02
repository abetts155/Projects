import ICFGs, Trees, CFGs, Vertices

fileNameSuffix = ".udraw"
beginGraph = "[\n"
endGraph = "]\n"
endVertex = "])),"
newEdge = "\ne(\"tEdge\","
endEdge = ")"
beginAttributes = "["
endAttibutes = "],"
newLine = "\\n"

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

def setToolTip (tooltip):
    return "a(\"INFO\", \"" + tooltip + "\"),"

def setEdgePattern (shape, width):
    return "a(\"EDGEPATTERN\", \"single;" + shape + ";" + str(width) + ";1\"),"

def setEdgeColor (color):
    return "a(\"EDGECOLOR\", \"" + color + "\"),"

def makeUdrawFile (g, fileNamePrefix):
    with open(fileNamePrefix + fileNameSuffix, 'w') as f:
        f.write(beginGraph)
        # Instrumented CFG
        if isinstance(g, ICFGs.ICFG):
            dfs = Trees.DepthFirstSearch(g, g.entryID)
            for vertexID in dfs.preorder:
                    writeICFGVertex(g, vertexID, f)
        # Loop-Nesting Tree
        elif isinstance(g, Trees.LoopNests):
            for v in g:
                    vertexID = v.getVertexID()
                    writeTreeVertex(g, vertexID, f)
        f.write(endGraph) 
    f.closed
    
def writeICFGVertex (icfg, vertexID, f):
    v = icfg.getVertex(vertexID)
        
    f.write(newVertex(vertexID))
    f.write(beginAttributes)
    f.write(setName(str(vertexID)))
    if isinstance(v, Vertices.Ipoint):
        f.write(setShape(SHAPE.CIRCLE))
        f.write(setColor(COLOR.YELLOW))
        f.write(setToolTip("Ipoint ID = %s" % v.getIpointID()))
    else:
        string = ""
        for address, instr in v.getInstructions ():
            string += instr.__str__() + newLine
        f.write(setToolTip(string[:-len(newLine)]))
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
    
def writeTreeVertex (tree, vertexID, f):
    v = tree.getVertex(vertexID)
        
    f.write(newVertex(vertexID))
    f.write(beginAttributes)
    f.write(setName(str(vertexID)))
    if isinstance(v, Vertices.HeaderVertex):
        f.write(setShape(SHAPE.CIRCLE))
        f.write(setColor(COLOR.YELLOW))
        f.write(setToolTip("Header ID = %s" % v.getHeaderID()))
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
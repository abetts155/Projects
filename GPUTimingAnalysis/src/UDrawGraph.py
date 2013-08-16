import CFGs, IPGs, Trees, Vertices, ICFGs
from Main import opts

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

def makeUdrawFile (g, basepath, fileNamePrefix):
    import os
    if opts.udraw: 
        filename = basepath + os.sep + fileNamePrefix + fileNameSuffix
        with open(filename, 'w') as f:
            f.write(beginGraph)
            # CFG or Instrumented CFG
            if isinstance(g, CFGs.CFG):
                for v in g:
                    writeCFGVertex(g, v.getVertexID(), f)           
            # Loop-Nesting Tree
            elif isinstance(g, Trees.LoopNests):
                for v in g:
                        writeTreeVertex(g, v.getVertexID(), f)
            # IPG
            elif isinstance(g, IPGs.IPG):
                for v in g:
                    writeIPGVertex(g, v.getVertexID(), f)
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
        f.write(setToolTip("Ipoint ID = 0x%04X" % v.getIpointID()))
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
        if isinstance(cfg, ICFGs.ICFG):
            if cfg.isBranchDivergentEdge(vertexID, succID):
                f.write(setEdgePattern(EDGESHAPE.DASHED, 4))
                f.write(setEdgeColor(COLOR.RED))
                f.write(setToolTip("Branch divergent edge"))
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
    if isinstance(tree, Trees.LoopNests):
        if tree.isLoopExit(vertexID):
            f.write(setShape(SHAPE.TRIANGLE))
            f.write(setColor(COLOR.RED))
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

def writeIPGVertex (ipg, vertexID, f): 
    v = ipg.getVertex(vertexID)
    f.write(newVertex(vertexID))
    f.write(beginAttributes)
    f.write(setName(str(vertexID)))
    f.write(setShape(SHAPE.CIRCLE))
    f.write(setColor(COLOR.YELLOW))
    f.write(setToolTip("Ipoint ID = 0x%04X" % v.getIpointID()))
    f.write(endAttibutes)
    
    f.write(beginAttributes)
    for succID in v.getSuccessorIDs():
        succe = v.getSuccessorEdge(succID)
        f.write(newEdge)
        f.write(beginAttributes)
        f.write(setName(str(succe.getEdgeID())))
        f.write(setToolTip(', '.join(str(v) for v in succe.getEdgeLabel())))
        if succe.isIterationEdge():
            f.write(setEdgePattern(EDGESHAPE.SOLID, 2))
        if ipg.isBranchDivergentEdge(vertexID, succID):
            f.write(setEdgePattern(EDGESHAPE.DASHED, 4))
            f.write(setEdgeColor(COLOR.RED))
            f.write(setToolTip("Branch divergent edge")) 
        f.write(endAttibutes)
        f.write(edgeLink(succID))
        f.write(endEdge + ",\n")
    f.write(endVertex + "\n")

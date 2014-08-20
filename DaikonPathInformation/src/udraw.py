import programs
import cfgs
import trees
import edges
import vertices
import super_blocks
import config
import os

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

def makeUdrawFile (g, graph_name):
    if config.Arguments.udraw:
        filename = "%s%s%s.%s.%s" % (config.Arguments.basepath, os.sep, config.Arguments.basename, graph_name, "udraw")
        with open(filename, 'w') as f:
            f.write(beginGraph)
            # CFG or Instrumented CFG
            if isinstance(g, cfgs.CFG):
                colors = ['#FFFFFF', '#FFAEB9', '#98FB98', '#CD919E', '#FFF8DC', '#FF83FA', '#00FA9A', '#9370DB', '#BCD2EE', '#E3A869','#FF4040','#DCDCDC','#A8A8A8']
                colorsIterator = iter(colors) 
                colorMapping   = {}
                writeCFGVertex(colorMapping, colorsIterator, g, g.getEntryID(), f)
                for v in g:
                    vertexID = v.vertexID
                    if vertexID != g.getEntryID():
                        writeCFGVertex(colorMapping, colorsIterator, g, vertexID, f)   
            elif isinstance(g, super_blocks.PathInformationGraph):
                bidirectionaledges = set([])
                for v in g:
                    vertexID = v.vertexID
                    writePathInfoVertex(g, vertexID, bidirectionaledges, f)    
            elif isinstance(g, programs.CallGraph) or isinstance(g, programs.ContextGraph):
                writeCallGraphVertex(g, g.getRootID(), f)
                for v in g:
                    vertexID = v.vertexID
                    if vertexID != g.getRootID():
                        writeCallGraphVertex(g, vertexID, f) 
            elif isinstance(g, cfgs.EnhancedCFG):
                for v in g:
                    writeEnhancedCFGVertex(g, v.vertexID, f)
            elif isinstance(g, trees.LoopNests):
                for v in g:
                    writeTreeVertex(g, v.vertexID, f)
            else:
                for v in g:
                    writeVertex(g, v.vertexID, f)
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
    
def writeEnhancedCFGVertex (g, vertexID, f):
    v = g.getVertex(vertexID)        
    f.write(newVertex(vertexID))
    f.write(beginAttributes)
    if isinstance(v, vertices.CFGEdge):
        name = str(v.getEdge())
        name += "%sID = %s" % (newLine, vertexID)
    else:
        name = str(vertexID)
    f.write(setName(name))
    f.write(endAttibutes)
    
    f.write(beginAttributes)
    for succe in v.getSuccessoredges():
        f.write(newEdge)
        f.write(beginAttributes)
        if succe.hasEdgeID():
            f.write(setName("%d" % succe.edgeID))
        f.write(endAttibutes)
        f.write(edgeLink(succe.vertexID))
        f.write(endEdge + ",\n")
    f.write(endVertex + "\n")
    
def writePathInfoVertex (g, vertexID, bidirectionaledges, f):
    v = g.getVertex(vertexID)        
    f.write(newVertex(vertexID))
    f.write(beginAttributes)
    name = "%s%sID = %s" % (str(v.getProgramPoint()), newLine, vertexID)
    f.write(setName(name))
    f.write(endAttibutes)
    
    f.write(beginAttributes)
    for succe in v.getSuccessoredges(edges.PathInformationEdgeType.INCLUSION):
        succID = succe.vertexID
        succv  = g.getVertex(succID)
        if succv.hasSuccessorEdge(vertexID, edges.PathInformationEdgeType.INCLUSION):
            if (vertexID, succID, edges.PathInformationEdgeType.INCLUSION) not in bidirectionaledges \
            and (succID, vertexID, edges.PathInformationEdgeType.INCLUSION) not in bidirectionaledges:
                bidirectionaledges.add((vertexID, succID, edges.PathInformationEdgeType.INCLUSION))
                f.write(newEdge)
                f.write(beginAttributes)
                f.write(setEdgeDirection(DIRECTION.BOTH))
                f.write(setEdgeColor(COLOR.RED))
                f.write(endAttibutes)
                f.write(edgeLink(succID))
                f.write(endEdge + ",\n")
        else:
            f.write(newEdge)
            f.write(beginAttributes)
            f.write(setEdgeColor(COLOR.RED))
            f.write(endAttibutes)
            f.write(edgeLink(succID))
            f.write(endEdge + ",\n")
    for succe in v.getSuccessoredges(edges.PathInformationEdgeType.EXCLUSION):
        succID = succe.vertexID
        if (vertexID, succID, edges.PathInformationEdgeType.EXCLUSION) not in bidirectionaledges \
        and (succID, vertexID, edges.PathInformationEdgeType.EXCLUSION) not in bidirectionaledges:
            bidirectionaledges.add((vertexID, succID, edges.PathInformationEdgeType.EXCLUSION))
            f.write(newEdge)
            f.write(beginAttributes)
            f.write(setEdgeDirection(DIRECTION.BOTH))
            f.write(setEdgeColor(COLOR.BLUE))
            f.write(endAttibutes)
            f.write(edgeLink(succID))
            f.write(endEdge + ",\n")
    for succe in v.getSuccessoredges(edges.PathInformationEdgeType.LOOP_BOUNDS):    
        succID = succe.vertexID
        f.write(newEdge)
        f.write(beginAttributes)
        tooltip = "Upper = %d%sRelative = %d" % (succe.upper, newLine, succe.relative)
        f.write(setName(tooltip))
        f.write(setEdgeColor(COLOR.GREEN))
        f.write(endAttibutes)
        f.write(edgeLink(succID))
        f.write(endEdge + ",\n")
    for succe in v.getSuccessoredges(edges.PathInformationEdgeType.CAPACITY_BOUNDS):  
        succID = succe.vertexID  
        f.write(newEdge)
        f.write(beginAttributes)
        tooltip = "Lower = %d%sUpper = %d" % (succe.lower, newLine, succe.upper)
        f.write(setName(tooltip))
        f.write(endAttibutes)
        f.write(edgeLink(succID))
        f.write(endEdge + ",\n")
    f.write(endVertex + "\n") 
    
def writeCFGVertex (colorMapping, colorsIterator, cfg, vertexID, f):
    v = cfg.getVertex(vertexID)
    f.write(newVertex(vertexID))
    f.write(beginAttributes)
    tooltip = ""
    if isinstance(v, vertices.CFGEdge):
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
    for succe in v.getSuccessoredges():
        f.write(newEdge)
        f.write(beginAttributes)
        if succe.hasEdgeID():
            f.write(setName("%d" % succe.edgeID))
        f.write(endAttibutes)
        f.write(edgeLink(succe.vertexID))
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

def writeTreeVertex (tree, vertexID, f):
    v = tree.getVertex(vertexID)
    f.write(newVertex(vertexID))
    f.write(beginAttributes)
    f.write(setName(str(vertexID)))
    if isinstance(tree, trees.LoopNests):
        if isinstance(v, vertices.HeaderVertex):
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

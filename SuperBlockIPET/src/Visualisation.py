import os
import pydot
import Trees, CFGs, SuperBlocks, Programs, Vertices

enabled  = False
basepath = os.curdir
basename = ""

def generateGraphviz (g, fileNamePrefix):
    global basepath, basename
    if enabled:
        graph = None
        # CFG or Instrumented CFG
        if isinstance(g, CFGs.CFG):
            graph = handleCFG(g)
        elif isinstance(g, SuperBlocks.SuperBlockGraph):
            graph = handleSuperBlockCFG(g) 
        elif isinstance(g, Programs.CallGraph) or isinstance(g, Programs.ContextGraph):
            graph = generateCallGraph(g)
        elif isinstance(g, Trees.LoopNests):
            graph = generateLNT(g)
        assert graph
        filename = "%s.%s" % (basepath + os.sep + basename, fileNamePrefix + ".png")
        graph.write_png(filename)
    
def handleCFG (cfg):
    graph        = pydot.Dot(graph_type='digraph')
    vertexToNode = {}
    entryv = cfg.getVertex(cfg.getEntryID())
    node = pydot.Node(str(entryv.getVertexID()))
    graph.add_node(node)
    vertexToNode[entryv] = node
    for v in cfg:
        if v.getVertexID() != entryv.getVertexID():
            node = pydot.Node(str(v.getVertexID()))
            graph.add_node(node)
            vertexToNode[v] = node
    for v in cfg:
        for succID in v.getSuccessorIDs():
            succv = cfg.getVertex(succID)
            edge  = pydot.Edge(vertexToNode[v], vertexToNode[succv])
            graph.add_edge(edge)
    return graph

def handleSuperBlockCFG (superg):
    graph        = pydot.Dot(graph_type='digraph')
    vertexToNode = {}
    for v in superg:
        node = handleSuperBlock(v)
        graph.add_node(node)
        vertexToNode[v] = node
    for v in superg:
        for branchID, succEdges in v.getBranchPartitions().iteritems():
            for succe in succEdges:
                succv = superg.getVertex(succe.getVertexID())
                edge  = pydot.Edge(vertexToNode[v], vertexToNode[succv], label=str(branchID))
                graph.add_edge(edge)
    return graph

def handleSuperBlock (superv):
    label = "super ID = %d\n" % (superv.getVertexID())
    if superv.basicBlocks:
        label += "{%s}" % ', '.join(str(vertexID) for vertexID in superv.basicBlocks)
    if superv.outOfScope:
        label += "\n" 
        label += "other = {%s}" % ', '.join(str(vertexID) for vertexID in superv.outOfScope)
    if superv.edges:
        label += "\n" 
        label += "{%s}" % ', '.join(str(edge) for edge in superv.edges)
    node = pydot.Node(label)
    return node

def generateCallGraph (callg):
    graph        = pydot.Dot(graph_type='digraph')
    vertexToNode = {}
    for v in callg:
        label = "%s\nvertex id = %d" % (v.getName(), v.getVertexID())
        node  = pydot.Node(label)
        graph.add_node(node)
        vertexToNode[v] = node
    for v in callg:
        for succID in v.getSuccessorIDs():
            succv = callg.getVertex(succID)
            edge  = pydot.Edge(vertexToNode[v], vertexToNode[succv])
            graph.add_edge(edge)
    return graph

def generateLNT (lnt):
    graph        = pydot.Dot(graph_type='graph')
    vertexToNode = {}
    for v in lnt:
        if isinstance(v, Vertices.HeaderVertex):
            label = "%d\nHeader=%d" % (v.getVertexID(), v.getHeaderID())
            if lnt.isDoWhileLoop(v.getHeaderID()):
                label += "\nDo-while loop"
            else:
                label += "\nFor loop"
            node = pydot.Node(label, shape="box", fillcolor="red", style="filled")
        elif lnt.isLoopExitSource(v.getVertexID()):  
            node = pydot.Node(str(v.getVertexID()), shape="triangle", fillcolor="yellow", style="filled")
        else:
            node = pydot.Node(str(v.getVertexID()))
        graph.add_node(node)
        vertexToNode[v] = node
    for v in lnt:
        for succID in v.getSuccessorIDs():
            succv = lnt.getVertex(succID)
            edge  = pydot.Edge(vertexToNode[v], vertexToNode[succv])
            graph.add_edge(edge)
    return graph

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
    global basepath, basename
    if enabled:
        filename = "%s.%s" % (basepath + os.sep + basename, fileNamePrefix + fileNameSuffix)
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
    
def writeSuperBlockVertex (superv, f):
    f.write(newVertex(superv.getVertexID()))
    f.write(beginAttributes)
    name = "super ID = %d%s" % (superv.getVertexID(), newLine)
    if superv.basicBlocks:
        name += "blocks = {%s}" % ', '.join(str(vertexID) for vertexID in superv.basicBlocks)
    if superv.outOfScope:
        name += newLine
        name += "other = {%s}" % ', '.join(str(vertexID) for vertexID in superv.outOfScope)
    if superv.edges:
        name += newLine 
        name += "edges = {%s}" % ', '.join(str(edge) for edge in superv.edges)
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
    
    
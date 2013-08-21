import pydot
import Trees, CFGs, SuperBlocks, Programs, Vertices

enabled  = False
basename = ""

def generateGraphviz (g, fileNamePrefix):
    global basename
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
        elif isinstance(g, Trees.ArithmeticExpressionTree):
            graph = generateAET(g)
        assert graph
        filename = "%s.%s" % (basename, fileNamePrefix + ".png")
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
    if superv.getBasicBlockIDs():
        label += "{%s}" % ', '.join(str(vertexID) for vertexID in superv.getBasicBlockIDs())
    if superv.getEdges ():
        label += "\n" 
        label += "{%s}" % ', '.join(str(edge) for edge in superv.getEdges())
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

def generateAET (aet):
    graph        = pydot.Dot(graph_type='graph')
    vertexToNode = {}
    for v in aet:
        if isinstance(v, Vertices.SuperBlock):
            node = handleSuperBlock(v)
            graph.add_node(node)
            vertexToNode[v] = node
        else:
            op    = v.getOperator()
            label = "op %s (vertex=%d)" % (op, v.getVertexID())
            label += "\nbound = %d" % v.getBound()
            label += "\nWCET = %d" % v.getWCET()
            if v.isAcyclicRegion():
                label += "\nExit path"
            else:
                label += "\nIteration path"
            if op == '+':
                node = pydot.Node(label, shape="box", fillcolor="#05EDFF", style="filled")
            elif op == 'max':            
                node = pydot.Node(label, shape="box", fillcolor="#D44942", style="filled")
            elif op == 'union':
                node = pydot.Node(label, shape="box", fillcolor="#003F87", style="filled")
            else:
                node = pydot.Node(label, shape="box", fillcolor="#EEC900", style="filled")
            graph.add_node(node)
            vertexToNode[v] = node
                
    for v in aet:
        for succID in v.getSuccessorIDs():
            succv = aet.getVertex(succID)
            edge  = pydot.Edge(vertexToNode[v], vertexToNode[succv])
            graph.add_edge(edge)
    return graph

beginAttributes = "["
beginGraph      = "[\n"
endAttibutes    = "],"
endEdge         = ")"
endGraph        = "]\n"
endVertex       = "])),"
newEdge         = "\ne(\"tEdge\","
newLine         = "\\n"

class COLOR:
    RED    = "red"
    YELLOW = "yellow" 
    BLUE   = "blue"
    GREEN  = "green"
    BLACK  = "black"
    
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

def setToolTip (tooltip):
    return "a(\"INFO\", \"" + tooltip + "\"),"

def generateUdraw (g, fileNamePrefix):
    assert isinstance(g, CFGs.CFG)
    if enabled:
        filename = "%s.%s" % (basename, fileNamePrefix + ".udraw")
        with open(filename, 'w') as f:
            f.write(beginGraph)
            colors = ['#FFFFFF', '#FFAEB9', '#98FB98', '#CD919E', '#FFF8DC', '#FF83FA', '#00FA9A', '#9370DB', '#BCD2EE', '#E3A869','#FF4040','#DCDCDC','#A8A8A8']
            colorsIterator = iter(colors) 
            colorMapping   = {}
            writeCFGVertex(colorMapping, colorsIterator, g, g.getEntryID(), f)
            for v in g:
                vertexID = v.getVertexID()
                if vertexID != g.getEntryID():
                    writeCFGVertex(colorMapping, colorsIterator, g, vertexID, f) 
            f.write(endGraph)
            
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
    
    
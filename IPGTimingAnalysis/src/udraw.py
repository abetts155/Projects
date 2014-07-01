import config
import cfgs
import ipgs
import trees
import vertices
import ipg_calculations
import os

begin_graph = "[\n"
end_graph   = "]\n"
end_vertex  = "])),"
new_edge    = "\ne(\"tEdge\","
end_edge    = ")"
begin_attrs = "["
end_attrs   = "],"

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

def new_vertex (vertexID):
    return "l(\"v" + str(vertexID) + "\",n(\"tVertex\","

def edge_link (vertexID):
    return "r(\"v" + str(vertexID) + "\")"

def set_name (name):
    return "a(\"OBJECT\", \"" + name + "\"),"

def set_color (color):
    return "a(\"COLOR\", \"" + color + "\"),"

def set_shape (shape):
    return "a(\"_GO\", \"" + shape + "\"),"

def set_tool_tip (tooltip):
    return "a(\"INFO\", \"" + tooltip + "\"),"

def set_edge_pattern (shape, width):
    return "a(\"EDGEPATTERN\", \"single;" + shape + ";" + str(width) + ";1\"),"

def set_edge_color (color):
    return "a(\"EDGECOLOR\", \"" + color + "\"),"

def make_file (g, graph_name):
    if config.Arguments.udraw:
        filename = "%s%s%s.%s.%s" % (config.Arguments.basepath, os.sep, config.Arguments.basename, graph_name, "udraw")
        with open(filename, 'w') as the_file:
            the_file.write(begin_graph)
            if isinstance(g, cfgs.CFG):
                writeCFGVertex(g, g.getEntryID(), the_file)
                for v in g:
                    if v.vertexID != g.getEntryID():
                        writeCFGVertex(g, v.vertexID, the_file)
            elif isinstance(g, trees.LoopNests):
                for v in g:
                        writeTreeVertex(g, v.vertexID, the_file)
            elif isinstance(g, ipgs.IPG) or isinstance(g, ipg_calculations.MiniIPG):
                writeIPGVertex(g, g.getEntryID(), the_file)
                for v in g:
                    if v.vertexID != g.getEntryID():
                        writeIPGVertex(g, v.vertexID, the_file)
            else:
                for v in g:
                    writeVertex(g, v.vertexID, the_file)
            the_file.write(end_graph) 
            
def writeVertex (g, vertexID, the_file):
    v = g.getVertex(vertexID)        
    the_file.write(new_vertex(vertexID))
    the_file.write(begin_attrs)
    the_file.write(set_name(str(vertexID)))
    the_file.write(end_attrs)
    
    the_file.write(begin_attrs)
    for succID in v.getSuccessorIDs():
        the_file.write(new_edge)
        the_file.write(begin_attrs)
        the_file.write(set_name(str(succID)))
        the_file.write(end_attrs)
        the_file.write(edge_link(succID))
        the_file.write(end_edge + ",\n")
    the_file.write(end_vertex + "\n")
    
def writeCFGVertex (cfg, vertexID, the_file):
    v = cfg.getVertex(vertexID)
    the_file.write(new_vertex(vertexID))
    the_file.write(begin_attrs)
    the_file.write(set_name(str(vertexID)))
    if isinstance(v, vertices.Ipoint):
        the_file.write(set_shape(SHAPE.CIRCLE))
        the_file.write(set_color(COLOR.YELLOW))
    the_file.write(end_attrs)
    
    the_file.write(begin_attrs)
    for succID in v.getSuccessorIDs():
        the_file.write(new_edge)
        the_file.write(begin_attrs)
        succe = v.getSuccessorEdge(succID)
        the_file.write(set_name(str(succe.getEdgeID())))
        the_file.write(end_attrs)
        the_file.write(edge_link(succID))
        the_file.write(end_edge + ",\n")
    the_file.write(end_vertex + "\n")
    
def writeTreeVertex (tree, vertexID, the_file):
    v = tree.getVertex(vertexID)
    the_file.write(new_vertex(vertexID))
    the_file.write(begin_attrs)
    the_file.write(set_name(str(vertexID)))
    if isinstance(tree, trees.LoopNests):
        if isinstance(v, vertices.HeaderVertex):
            the_file.write(set_shape(SHAPE.TRIANGLE))
            the_file.write(set_color(COLOR.RED))
            the_file.write(set_tool_tip("Header ID = %s" % v.headerID))
        elif tree.isLoopExit(vertexID):
            the_file.write(set_shape(SHAPE.ELLIPSE))
            the_file.write(set_color(COLOR.YELLOW))
            the_file.write(set_tool_tip("Loop Exit"))
    the_file.write(end_attrs)
    
    the_file.write(begin_attrs)
    for succID in v.getSuccessorIDs():
        the_file.write(new_edge)
        the_file.write(begin_attrs)
        the_file.write(set_name(str(succID)))
        the_file.write(end_attrs)
        the_file.write(edge_link(succID))
        the_file.write(end_edge + ",\n")
    the_file.write(end_vertex + "\n")   

def writeIPGVertex (ipg, vertexID, the_file): 
    v = ipg.getVertex(vertexID)
    the_file.write(new_vertex(vertexID))
    the_file.write(begin_attrs)
    the_file.write(set_name(str(v.getRealID())))
    if v.isGhost():
        the_file.write(set_shape(SHAPE.CIRCLE))
        the_file.write(set_color(COLOR.RED))
        the_file.write(set_tool_tip("GHOST"))
    else:
        the_file.write(set_shape(SHAPE.CIRCLE))
        the_file.write(set_color(COLOR.YELLOW))
        the_file.write(set_tool_tip("Ipoint ID = 0x%04X" % v.ipointID))
    the_file.write(end_attrs)
    
    the_file.write(begin_attrs)
    for succID in v.getSuccessorIDs():
        succe = v.getSuccessorEdge(succID)
        the_file.write(new_edge)
        the_file.write(begin_attrs)
        the_file.write(set_name(str(succe.getEdgeID())))
        toolTip = ', '.join(str(v) for v in succe.getEdgeLabel())
        if succe.isIterationEdge():
            the_file.write(set_edge_pattern(EDGESHAPE.SOLID, 2))
        elif succe.isDummyEdge():
            the_file.write(set_edge_pattern(EDGESHAPE.DASHED, 2))
            toolTip += "DUMMY EDGE"
        the_file.write(set_tool_tip(toolTip))
        the_file.write(end_attrs)
        the_file.write(edge_link(succID))
        the_file.write(end_edge + ",\n")
    the_file.write(end_vertex + "\n")

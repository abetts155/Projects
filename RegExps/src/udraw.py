import os
import config
import directed_graphs
import vertices

begin_attrs = "["
end_attrs   = "],"
begin_graph = "[\n"
end_graph   = "]\n"
end_vertex  = "])),"
new_edge    = "\ne(\"tEdge\","
end_edge    = ")"
new_line    = "\\n"

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

def new_vertex(vertexID):
    return "l(\"v" + str(vertexID) + "\",n(\"tVertex\","

def edge_link(vertexID):
    return "r(\"v" + str(vertexID) + "\")"

def set_name(name):
    return "a(\"OBJECT\", \"" + name + "\"),"

def set_color(color):
    return "a(\"COLOR\", \"" + color + "\"),"

def set_shape(shape):
    return "a(\"_GO\", \"" + shape + "\"),"

def set_tool_tip(tooltip):
    return "a(\"INFO\", \"" + tooltip + "\"),"

def set_edge_pattern(shape, width):
    return "a(\"EDGEPATTERN\", \"single;" + shape + ";" + str(width) + ";1\"),"

def set_edge_color(color):
    return "a(\"EDGECOLOR\", \"" + color + "\"),"

def set_edge_direction(direction):
    return "a(\"_DIR\", \"" + direction + "\"),"

def make_file(g, graph_name):
    global basepath, basename
    if config.Arguments.udraw:
        filename = "%s%s%s.%s.%s" % (config.Arguments.basepath, os.sep, config.Arguments.basename, graph_name, "udraw")
        with open(filename, 'w') as the_file:
            the_file.write(begin_graph)
            if isinstance(g, directed_graphs.CFG):
                write_CFG_vertex(g, g.get_vertex(g.get_entryID()), the_file)
                for v in g:
                    if v.vertexID != g.get_entryID():
                        write_CFG_vertex(g, v, the_file)   
            elif isinstance(g, directed_graphs.StateTransitionGraph) \
            or isinstance(g, directed_graphs.StateTransitionComponentDAG):
                for v in g:
                    writeStateTransitionVertex(g, v, the_file)
            elif isinstance(g, directed_graphs.LoopNests):
                for v in g:
                    writeTreeVertex(g, v, the_file)
            else:
                for v in g:
                    writeVertex(g, v, the_file)
            the_file.write(end_graph) 
            
def writeVertex(g, v, the_file):
    the_file.write(new_vertex(v.vertexID))
    the_file.write(begin_attrs)
    if isinstance(v, vertices.ProgramPoint):
        the_file.write(set_name(str(v.the_program_point)))
    elif isinstance(v, vertices.RegExpVertex):
        the_file.write(set_shape(SHAPE.TRIANGLE))
        the_file.write(set_name(v.operator))
        if v.operator == vertices.RegExpVertex.ALTERNATIVE:
            the_file.write(set_color(COLOR.BLUE))
        elif v.operator == vertices.RegExpVertex.FOR_LOOP:
            the_file.write(set_color(COLOR.RED))
        else:
            the_file.write(set_color(COLOR.YELLOW))
    else:
        the_file.write(set_name(str(v.vertexID)))
    the_file.write(end_attrs)
    
    the_file.write(begin_attrs)
    for succID in v.successors.keys():
        the_file.write(new_edge)
        the_file.write(begin_attrs)
        the_file.write(set_name(str(succID)))
        the_file.write(end_attrs)
        the_file.write(edge_link(succID))
        the_file.write(end_edge + ",\n")
    the_file.write(end_vertex + "\n") 
    
def write_CFG_vertex(cfg, v, the_file):
    the_file.write(new_vertex(v.vertexID))
    the_file.write(begin_attrs)
    the_file.write(set_name(str(v.vertexID)))
    if v.dummy:
        the_file.write(set_color(COLOR.RED))
        the_file.write(set_tool_tip("Dummy vertex"))
    the_file.write(end_attrs)
    
    the_file.write(begin_attrs)
    for succe in v.successors.values():
        the_file.write(new_edge)
        the_file.write(begin_attrs)
        the_file.write(set_name("%d" % succe.edgeID))
        the_file.write(end_attrs)
        the_file.write(edge_link(succe.vertexID))
        the_file.write(end_edge + ",\n")
    the_file.write(end_vertex + "\n")   

def writeTreeVertex(tree, v, the_file):
    the_file.write(new_vertex(v.vertexID))
    the_file.write(begin_attrs)
    if isinstance(v, vertices.ProgramPoint):
        the_file.write(set_name(str(v.the_program_point)))
    elif isinstance(v, vertices.HeaderVertex):
        the_file.write(set_shape(SHAPE.TRIANGLE))
        the_file.write(set_color(COLOR.RED))
        the_file.write(set_tool_tip("Header ID = %s" % v.headerID))
    else:
        the_file.write(set_shape(SHAPE.CIRCLE))
        the_file.write(set_color(COLOR.YELLOW))
        the_file.write(set_name(str(v.vertexID)))
    the_file.write(end_attrs)
    
    the_file.write(begin_attrs)
    for succID in v.successors.keys():
        the_file.write(new_edge)
        the_file.write(begin_attrs)
        the_file.write(set_name(str(succID)))
        the_file.write(end_attrs)
        the_file.write(edge_link(succID))
        the_file.write(end_edge + ",\n")
    the_file.write(end_vertex + "\n")   
    
def writeStateTransitionVertex(graph, v, the_file):
    the_file.write(new_vertex(v.vertexID))
    the_file.write(begin_attrs)
    the_file.write(set_color(COLOR.YELLOW))
    the_file.write(set_shape(SHAPE.CIRCLE))
    the_file.write(set_name(str(v.vertexID)))
    the_file.write(end_attrs)
    
    the_file.write(begin_attrs)
    for succe in v.successors.values():
        the_file.write(new_edge)
        the_file.write(begin_attrs)
        edge_str = succe.__str__()
        if edge_str.startswith("set"):
            the_file.write(set_edge_pattern(EDGESHAPE.SOLID, 4))
            the_file.write(set_tool_tip(edge_str))
        else:
            the_file.write(set_name(edge_str))
        the_file.write(end_attrs)
        the_file.write(edge_link(succe.vertexID))
        the_file.write(end_edge + ",\n")
    the_file.write(end_vertex + "\n")
    
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
                write_cfg_vertex(g, g.get_vertex(g.get_entryID()), the_file)
                for v in g:
                    if v.vertexID != g.get_entryID():
                        write_cfg_vertex(g, v, the_file)   
            elif isinstance(g, directed_graphs.EnhancedCFG):
                write_enhanced_cfg_vertex(g, g.get_vertex(g.get_entryID()), the_file)
                for v in g:
                    if v.vertexID != g.get_entryID():
                        write_enhanced_cfg_vertex(g, v, the_file)
            else:
                assert False
            the_file.write(end_graph) 
            
def write_cfg_vertex(g, v, the_file):
    the_file.write(new_vertex(v.vertexID))
    the_file.write(begin_attrs)
    the_file.write(set_name(str(v.vertexID)))
    if v.instrumented:
        the_file.write(set_color(COLOR.YELLOW))
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
            
def write_enhanced_cfg_vertex(g, v, the_file):
    the_file.write(new_vertex(v.vertexID))
    the_file.write(begin_attrs)
    if isinstance(v, vertices.CFGEdge):
        the_file.write(set_name(str(v.edge)))
        the_file.write(set_color("#37FDFC"))
    else:
        the_file.write(set_name(str(v.vertexID)))
        the_file.write(set_color(COLOR.YELLOW))
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

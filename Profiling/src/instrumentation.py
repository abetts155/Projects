import udraw
import config
import vertices
import random

def remove_vertices(enhanced_cfg, to_remove):
    for v in to_remove:
        enhanced_cfg.remove_vertex(v)

def relink_vertex_predecessors_to_successors(enhanced_cfg, v):
    for predID in v.predecessors.keys():
        for succID in v.successors.keys():
            if not enhanced_cfg.has_edge(predID, succID):
                enhanced_cfg.add_edge(predID, succID)  

def eliminate_program_points_which_are_not_profiled(enhanced_cfg, program_points_to_profile):
    vertices_to_profile = set()
    edges_to_profile    = set()
    edge_endpoints      = set()
    for a_program_point in program_points_to_profile:
        if isinstance(a_program_point, tuple):
            edges_to_profile.add(a_program_point)
            edge_endpoints.add(a_program_point[0])
            edge_endpoints.add(a_program_point[1])
        else:
            vertices_to_profile.add(a_program_point)
    
    to_remove = set()
    for v in enhanced_cfg:
        if v.vertexID != enhanced_cfg.entryID \
        and v.vertexID != enhanced_cfg.exitID:
            if isinstance(v, vertices.CFGEdge):
                if v.edge[0] not in vertices_to_profile \
                and v.edge[1] not in vertices_to_profile \
                and v.edge not in edges_to_profile:
                    to_remove.add(v)
                    relink_vertex_predecessors_to_successors(enhanced_cfg, v)
            else:
                if v.vertexID not in vertices_to_profile \
                and v.vertexID not in edge_endpoints:
                    to_remove.add(v)
                    relink_vertex_predecessors_to_successors(enhanced_cfg, v)
    remove_vertices(enhanced_cfg, to_remove)
    
def eliminate_cfg_edge_vertices(enhanced_cfg):
    to_remove = set()
    for v in enhanced_cfg:
        if v.vertexID != enhanced_cfg.entryID \
        and v.vertexID != enhanced_cfg.exitID:
            if isinstance(v, vertices.CFGEdge):
                to_remove.add(v)
                relink_vertex_predecessors_to_successors(enhanced_cfg, v)
    remove_vertices(enhanced_cfg, to_remove)

def eliminate_cfg_vertex_vertices(enhanced_cfg):
    to_remove = set()
    for v in enhanced_cfg:
        if v.vertexID != enhanced_cfg.entryID \
        and v.vertexID != enhanced_cfg.exitID:
            if not isinstance(v, vertices.CFGEdge):
                to_remove.add(v)
                relink_vertex_predecessors_to_successors(enhanced_cfg, v)
    remove_vertices(enhanced_cfg, to_remove)

def reduce_enhanced_cfg(enhanced_cfg):
    vertex_list = enhanced_cfg.the_vertices.values()
    random.shuffle(vertex_list)
    for v in vertex_list:
        if v.vertexID != enhanced_cfg.entryID \
        and v.vertexID != enhanced_cfg.exitID:
            remove_vertex = True
            for predID in v.predecessors.keys():
                for succID in v.successors.keys():
                    if enhanced_cfg.has_edge(predID, succID):
                        remove_vertex = False
            if remove_vertex:
                relink_vertex_predecessors_to_successors(enhanced_cfg, v)
                enhanced_cfg.remove_vertex(v)

def do_instrumentation(cfg, program_points_to_profile):
    enhanced_cfg = cfg.get_enhanced_cfg()
    if config.Arguments.instrument == "vertices":
        eliminate_cfg_edge_vertices(enhanced_cfg)
    elif config.Arguments.instrument == "edges":
        eliminate_cfg_vertex_vertices(enhanced_cfg)
    eliminate_program_points_which_are_not_profiled(enhanced_cfg, program_points_to_profile)    
    reduce_enhanced_cfg(enhanced_cfg)
    udraw.make_file(enhanced_cfg, "%s.enhanced" % enhanced_cfg.name)

        

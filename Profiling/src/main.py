#!/usr/bin/env python

import os
import argparse
import config
import program_input_output
import debug
import instrumentation
import re
import ast

prompt_prefix = "->"

def get_program_points(cfg):
    vertices = set()
    edges    = set()
    for v in cfg:
        vertices.add(v.vertexID)
        for succID in v.successors.keys():
            edges.add((v.vertexID, succID))
    
    if config.Arguments.profile == "choose":        
        print("%s Vertices = {%s}" % (prompt_prefix, ','.join(str(vertexID) for vertexID in sorted(vertices))))
        the_input = raw_input("%s Enter vertices as a comma-separated list: " % prompt_prefix).lower()
        the_input = re.sub(r'\s+', '', the_input)
        selected = set()
        for a_value in re.findall(r'\d+', the_input):
            vertexID = ast.literal_eval(a_value)
            if vertexID in vertices:
                selected.add(vertexID)
            else:
                debug.warning_message("CFG %s does not have vertex %d. Ignoring." % (cfg.name, vertexID))
        print("%s Edges = {%s}" % (prompt_prefix, ','.join(str(edge) for edge in sorted(edges))))
        the_input = raw_input("%s Enter edges as a comma-separated list: " % prompt_prefix).lower()
        the_input = re.sub(r'\s+', '', the_input)
        for a_value in re.findall(r'\(\d+,\d+\)', the_input):
            an_edge = ast.literal_eval(a_value)
            if an_edge in edges:
                selected.add(an_edge)
            else:
                debug.warning_message("CFG %s does not have edge %s. Ignoring." % (cfg.name, an_edge))
        return selected
    elif config.Arguments.profile == "vertices":
        return vertices
    elif config.Arguments.profile == "edges":
        return edges
    elif config.Arguments.profile == "all":
        return vertices.union(edges)
    else:
        assert False, "Unknown profile choice"

def select_cfgs(program):
    print("%s CFGs = {%s}" % (prompt_prefix, ','.join(cfg.name for cfg in program.cfgs.values())))
    the_input = raw_input("%s Enter CFGs as a comma-separated list. Each element of the list should be a single CFG identifier or a range of CFG identifiers where applicable (e.g. f1,f2-f6,f10): " % prompt_prefix).lower()
    the_input = re.sub(r'\s+', '', the_input)
    the_input = the_input.split(',')
    cfg_names = set()
    for elem in the_input:
        if '-' in elem:
            the_range     = elem.split('-')
            start_matches = re.findall('\d+', the_range[0])
            end_matches   = re.findall('\d+', the_range[1])
            for i in range(int(start_matches[0]), int(end_matches[0])+1):
                cfg_names.add("f%d" % i)
        else:
            cfg_names.add(elem)
    return cfg_names
            
def instrument_cfgs(program):
    for cfg_name in select_cfgs(program):
        if cfg_name not in program.cfgs:
            debug.warning_message("Program does not have CFG %s" % cfg_name)
        else:
            cfg = program.cfgs[cfg_name]
            instrumentation.do_instrumentation(cfg, get_program_points(cfg))

def the_command_line(): 
    parser = argparse.ArgumentParser(description="Instrument CFGs to collect execution profiles at run time")
    
    parser.add_argument("program_file",
                        help="a file containing program information (with '.txt' extension)")
    
    parser.add_argument("-d",
                        "--debug",
                        type=int,
                        help="debug mode",
                        default=0)
    
    parser.add_argument("--instrument",
                        choices=["vertices", "edges", "mixed"],
                        required=True,
                        help="instrument vertices, edges or both")
    
    parser.add_argument("--profile",
                        choices=["vertices", "edges", "all", "choose"],
                        required=True,
                        help="instrument to profile all vertices, all edges, all vertices and edges or a choice of vertices and edges")
    
    parser.add_argument("-u",
                        "--udraw",
                        action="store_true",
                        help="generate uDraw files to visualise graphs",
                        default=False)

    parser.add_argument("-v",
                        "--verbose",
                        action="store_true",
                        help="be verbose",
                        default=False)
    
    parser.parse_args(namespace=config.Arguments)
    
    config.Arguments.basename = os.path.splitext(os.path.basename(config.Arguments.program_file))[0]
    config.Arguments.basepath = os.path.abspath(os.path.dirname(config.Arguments.program_file))

if __name__ == "__main__": 
    the_command_line()
    program = program_input_output.read_file(config.Arguments.program_file)
    instrument_cfgs(program)
    
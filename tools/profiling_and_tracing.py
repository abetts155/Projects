#!/usr/bin/env python

import os
import argparse

from tools.lib.utils import config
from tools.lib.system import program



def parse_the_command_line(): 
    parser = argparse.ArgumentParser(description=
                                     "Instrument program to collect execution"
                                     " profiles at run time")
    
    parser.add_argument("program_file",
                        help="a file containing program information"
                        " (with '.txt' extension)")
    
    parser.add_argument("-d",
                        "--debug",
                        type=int,
                        help="debug mode",
                        default=0)
    
    parser.add_argument("--dot",
                        action="store_true",
                        help="produce Graphviz dot files of graphs produced"
                        " during the analysis",
                        default=False)

    parser.add_argument("-v",
                        "--verbose",
                        action="store_true",
                        help="be verbose",
                        default=False)
    
    parser.add_argument("--instrument",
                        choices=["vertices", "edges", "mixed"],
                        required=True,
                        help="instrument vertices, edges or both")
    
    parser.add_argument("--profile",
                        choices=["vertices", "edges", "all", "choose"],
                        required=True,
                        help="instrument to profile all vertices, all edges," 
                        " all vertices and edges or a choice of vertices and"
                        " edges")
    
    parser.parse_args(namespace=config.Arguments)
    
    config.Arguments.basename =\
        os.path.splitext(os.path.basename(config.Arguments.program_file))[0]
    config.Arguments.basepath =\
        os.path.abspath(os.path.dirname(config.Arguments.program_file))

if __name__ == "__main__": 
    parse_the_command_line()
    the_program = program.read_program_information_from_file\
                            (config.Arguments.program_file)
    for control_flow_graph in the_program.control_flow_graph_iterator():
        pass
    
    
    
#!/usr/bin/env python

import argparse

from tools.lib.utils import dot
from tools.lib.utils import config
from tools.lib.system import program



def parse_the_command_line(): 
    parser = argparse.ArgumentParser(description=
                                     'Instrument program to collect execution '
                                     'profiles at run time')
    
    parser.add_argument('program_file',
                        help='a file containing program information'
                        ' (with .txt extension)')
    
    parser.add_argument('-d',
                        '--debug',
                        type=int,
                        help='debug mode',
                        default=0)
    
    parser.add_argument('--dot',
                        action='store_true',
                        help='produce Graphviz dot files of graphs produced'
                        ' during the analysis',
                        default=False)
    
    parser.add_argument('--purge-dot',
                        action='store_true',
                        help='remove all files created by dot in the target'
                        ' directory',
                        default=False)

    parser.add_argument('-v',
                        '--verbose',
                        action='store_true',
                        help='be verbose',
                        default=False)
    
    parser.add_argument('--instrument',
                        choices=['vertices', 'edges', 'mixed'],
                        required=True,
                        help='instrument vertices, edges, or both')
    
    parser.parse_args(namespace=config.Arguments)
    config.set_filename_prefix()

if __name__ == '__main__': 
    parse_the_command_line()
    config.purge_png_files()
    the_program = program.read_program_information_from_file\
                            (config.Arguments.program_file)
    for control_flow_graph in the_program.control_flow_graph_iterator():
        print('=====================>', control_flow_graph.name)
        basic_blocks, control_flow_edges\
            = control_flow_graph.split_program_points_into_basic_blocks_and_edges()
        if config.Arguments.instrument == 'vertices':
            control_flow_graph.reduce_but_maintain_path_reconstructibility\
                                    (control_flow_edges)
        elif config.Arguments.instrument == 'edges':
            control_flow_graph.reduce_but_maintain_path_reconstructibility\
                                    (basic_blocks)
        elif config.Arguments.instrument == 'mixed':
            control_flow_graph.reduce_but_maintain_path_reconstructibility\
                                    (set())
        else:
            assert False
                                    
        for vertex in control_flow_graph:
            print(vertex.program_point)
        dot.make_file(control_flow_graph)
        
        

        
    
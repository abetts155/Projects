#!/usr/bin/env python

import argparse

from lib.utils import config
from lib.system import environment



def parse_the_command_line(): 
    parser = argparse.ArgumentParser(description=
                                     'Instrument program to collect execution '
                                     'profiles at run time')
    
    parser.add_argument('program_file',
                        help='a file containing program information'
                        ' (with .txt extension)')
    
    parser.add_argument('-d',
                        '--debug',
                        action='store_true',
                        help='debug mode',
                        default=False)
    
    parser.add_argument('-v',
                        '--verbose',
                        action='store_true',
                        help='be verbose',
                        default=False)
    
    parser.add_argument('--graphviz',
                        action='store_true',
                        help='using Graphviz, produce PNG files of graphs'
                        ' produced during the analysis',
                        default=False)
    
    parser.add_argument('--purge-graphviz',
                        action='store_true',
                        help='remove all files created by Graphviz in the'
                        'target directory',
                        default=False)
    
    parser.add_argument('--instrument',
                        choices=['vertices', 'edges', 'mixed'],
                        required=True,
                        help='instrument vertices, edges, or both')
    
    parser.parse_args(namespace=config.Arguments)
    config.set_filename_prefix()
    config.purge_graphviz_files()


if __name__ == '__main__': 
    parse_the_command_line()
    program = environment.create_program_from_input_file()
    
    for control_flow_graph in program.control_flow_graph_iterator():
        print(control_flow_graph.name)
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
        
        

        
    
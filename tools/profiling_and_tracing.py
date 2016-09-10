#!/usr/bin/env python

import sys
assert sys.version_info >= (3,0), 'Script requires Python 3.0 or greater to run'

import argparse

from lib.utils import globals
from lib.system import environment


def parse_the_command_line(): 
    parser = argparse.ArgumentParser(description=
                                     'Instrument program to collect execution '
                                     'profiles at run time')
    
    parser.add_argument('program_file',
                        help='a file containing program information'
                        ' (with .txt extension)')
    
    parser.add_argument('--instrument',
                        choices=['vertices', 'edges', 'mixed'],
                        required=True,
                        help='instrument vertices, edges, or both')
    
    globals.add_common_command_line_arguments(parser)
    globals.args = vars(parser.parse_args())
    globals.set_filename_prefix(globals.args['program_file'])


if __name__ == '__main__': 
    parse_the_command_line()
    program = environment.create_program_from_input_file()
    
    for control_flow_graph in program.control_flow_graph_iterator():
        print(control_flow_graph.name)
        basic_blocks, control_flow_edges\
            = control_flow_graph.split_program_points_into_basic_blocks_and_edges()
        if globals.args['instrument'] == 'vertices':
            control_flow_graph.reduce_but_maintain_path_reconstructibility\
                                    (control_flow_edges)
        elif globals.args['instrument'] == 'edges':
            control_flow_graph.reduce_but_maintain_path_reconstructibility\
                                    (basic_blocks)
        elif globals.args['instrument'] == 'mixed':
            control_flow_graph.reduce_but_maintain_path_reconstructibility\
                                    (set())
        else:
            assert False
        
        

        
    
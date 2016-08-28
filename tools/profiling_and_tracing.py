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
    the_program = program.read_program_information_from_file\
                            (config.Arguments.program_file)
    for control_flow_graph in the_program.control_flow_graph_iterator():
        basic_blocks, control_flow_edges = control_flow_graph.\
                                            get_all_program_points()
        state_transition_graph = control_flow_graph.get_state_transition_graph()
        if config.Arguments.instrument == 'vertices':
            state_transition_graph.eliminate_states_of_unmonitored_program_points\
                                    (control_flow_edges)
        elif config.Arguments.instrument == 'edges':
            state_transition_graph.eliminate_states_of_unmonitored_program_points\
                                    (basic_blocks)
        dot.make_file(state_transition_graph)
        
    
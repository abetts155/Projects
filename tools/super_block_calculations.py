#!/usr/bin/env python

import argparse
import threading
import sys

from tools.lib.utils import config
from tools.lib.system import environment
from tools.lib.system import calculations


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

    parser.add_argument('-v',
                        '--verbose',
                        action='store_true',
                        help='be verbose',
                        default=False)
    
    parser.add_argument('--repeat',
                        type=int,
                        help='repeat the calculation this many times',
                        default=config.Arguments.repeat,
                        metavar='<INT>')
    
    parser.add_argument('--shuffle-constraints',
                        action='store_true',
                        help='before repeating the solve stage of a '
                        'constraint-based WCET calculation, shuffle '
                        'the constraints',
                        default=False)
    
    parser.parse_args(namespace=config.Arguments)
    config.set_filename_prefix()
    config.purge_graphviz_files()


if __name__ == '__main__': 
    threading.stack_size(67108864) # 64MB stack
    sys.setrecursionlimit(2**20)
    
    parse_the_command_line()
    program = environment.Program.create()
    for control_flow_graph in program.control_flow_graph_iterator():
        control_flow_graph.get_super_block_graph()
    calculations.calculate_wcet_using_integer_linear_programming(program, 
                                                                 config.Arguments.repeat)
    
    
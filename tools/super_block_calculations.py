#!/usr/bin/env python

import argparse
import threading
import sys

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
    
    parser.add_argument('--repeat-calculation',
                        type=int,
                        help='repeat the calculation this many times',
                        default=1,
                        metavar='<INT>')
    
    parser.add_argument('--shuffle-constraints',
                        action='store_true',
                        help='before repeating the solve stage of a '
                        'constraint-based WCET calculation, shuffle '
                        'the constraints',
                        default=False)
    
    parser.parse_args(namespace=config.Arguments)
    config.set_filename_prefix()
    config.purge_png_files()


if __name__ == '__main__': 
    threading.stack_size(67108864) # 64MB stack
    sys.setrecursionlimit(2**20)
    
    parse_the_command_line()
    the_program = program.Program.create()
    for control_flow_graph in the_program.control_flow_graph_iterator():
        control_flow_graph.get_super_block_graph()

    
    
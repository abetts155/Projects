#!/usr/bin/env python3

import sys
assert sys.version_info >= (3,0), 'Script requires Python 3.0 or greater to run'

import shutil
import os
assert shutil.which('lp_solve', mode=os.X_OK), 'Script requires lp_solve to be in your path'

import argparse
import threading

from lib.utils import globals
from lib.system import environment
from lib.system import calculations


def parse_the_command_line(): 
    parser = argparse.ArgumentParser(description=
                                     'Do WCET calculation using super blocks '
                                     'analysis')
    
    parser.add_argument('program_file',
                        help='a file containing program information'
                        ' (with .txt extension)')
    
    parser.add_argument('--output',
                        help='write results to this file')

    parser.add_argument('--repeat',
                        type=int,
                        help='repeat the calculation this many times',
                        default=1,
                        metavar='<INT>')
    
    parser.add_argument('--max-loop-bound',
                        type=int,
                        help='set the maximum possible value for automatically '
                        'generated loop bounds',
                        default=10,
                        metavar='<INT>')
    
    parser.add_argument('--folded',
                        action='store_true',
                        help='fold super blocks before constraint solving',
                        default=False)
    
    parser.add_argument('--functions',
                        nargs='*',
                        help='analyse these functions only',
                        metavar='<STR>')
    
    globals.add_common_command_line_arguments(parser)
    globals.args = vars(parser.parse_args())
    globals.set_filename_prefix(globals.args['program_file'])
    

if __name__ == '__main__': 
    threading.stack_size(2**6 * 2**20)
    sys.setrecursionlimit(2**20)
    parse_the_command_line()
    program = environment.create_program_from_input_file()
    calculations.calculate_wcet_using_integer_linear_programming(program)

            
    
    
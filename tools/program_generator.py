#!/usr/bin/env python3

import sys
assert sys.version_info >= (3,0), 'Script requires Python 3.0 or greater to run'

from argparse import Action, ArgumentError, ArgumentParser
import os
import re

from lib.utils import globals
from lib.utils import debug
from lib.system import environment
from lib.system import analysis


def create_program_filename(base_directory):
    base_directory = os.path.abspath(base_directory)
    files = [file for file in os.listdir(base_directory) 
             if os.path.isfile(os.path.join(base_directory,file))]
    taken_filename_prefixes = set()
    for filename in files:
        if re.match(r'[0-9]+\.txt', filename):
            numbers = re.findall(r'[0-9]+', filename)
            assert len(numbers) == 1
            taken_filename_prefixes.add(int(numbers[0]))
    
    i = 1
    while True:
        if i not in taken_filename_prefixes:
            filename = '{}.txt'.format(i)
            return filename
        i += 1
        

def parse_the_command_line(): 
    
    class CheckForPositiveValue(Action):
        def __call__(self, parser, namespace, value, option_string=None):
            if value <= 0: 
                raise ArgumentError('Argument {} requires a positive integer'.\
                                    format(option_string))
            setattr(namespace, self.dest, value)
    
    parser = ArgumentParser(description='Generate a random program')
    
    parser.add_argument('--directory',
                        help='write the program file to this directory',
                        default=os.path.abspath(os.curdir))
    
    parser.add_argument('--program-file',
                        help='write the program to this file',
                        default=None)
    
    parser.add_argument('--subprograms',
                        action=CheckForPositiveValue,
                        type=int,
                        help='number of subprograms',
                        metavar='<INT>',
                        default=1)
    
    parser.add_argument('--loops',
                        type=int,
                        help='maximum number of loops in a control flow graph',
                        metavar='<INT>',
                        default=0)
    
    parser.add_argument('--nesting-depth',
                        type=int,
                        help='maximum nesting depth of loops',
                        metavar='<INT>',
                        default=1)
    
    parser.add_argument('--fan-out',
                        action=CheckForPositiveValue,
                        type=int,
                        help='select maximum fan out of a basic block',
                        metavar='<INT>',
                        default=2)
    
    parser.add_argument('--vertices',
                        type=int,
                        action=CheckForPositiveValue,
                        help='maximum number of basic blocks in a control flow graph',
                        metavar='<INT>',
                        default=10)

    parser.add_argument('--instrument-branches',
                        action='store_true',
                        help='instrument branches (i.e., both edges of 2-way conditionals, and calls/returns)',
                        default=False)

    globals.add_common_command_line_arguments(parser)
    globals.args = vars(parser.parse_args())
    base_directory = os.path.abspath(globals.args['directory'])
    if globals.args['program_file'] is None:
        globals.args['program_file'] = create_program_filename(base_directory)
    globals.args['program_file'] = base_directory + os.sep + globals.args['program_file']
    globals.set_filename_prefix(globals.args['program_file']) 
    
    if globals.args['vertices'] < globals.args['loops'] * 2:
        debug.exit_message('The number of vertices in a control flow graph ' 
                           'must be at least twice the number of loops')
    
    
if __name__ == '__main__': 
    parse_the_command_line()
    program = environment.generate_program()
    if globals.args['instrument_branches']:
        analysis.instrument_branches(program)
    environment.write_program_to_file(program)

#!/usr/bin/env python3

import sys
import argparse

assert sys.version_info >= (3, 0), 'Script requires Python 3.0 or greater to run'

from lib.utils import globals
from lib.system import environment
from lib.system import calculations


def parse_the_command_line():
    parser = argparse.ArgumentParser(description='Do WCET calculation using instrumentation point graphs')

    parser.add_argument('program_file',
                        help='a file containing program information'
                             ' (with .txt extension)')

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

    parser.add_argument('--functions',
                        nargs='*',
                        help='analyse these functions only')

    globals.add_common_command_line_arguments(parser)
    globals.args = vars(parser.parse_args())
    globals.set_filename_prefix(globals.args['program_file'])


if __name__ == '__main__':
    parse_the_command_line()
    program = environment.create_program_from_input_file()
    calculations.calculate_wcet_using_instrumentation_point_graph(program)

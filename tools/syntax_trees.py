import argparse
import sys

from graphs import graphs
from system import program
from utils import messages


def main(**kwargs):
    the_program = program.IO.read(kwargs['filename'])

    for subprogram in the_program:
        messages.debug_message('Analysing CFG for {}'.format(subprogram.name))
        subprogram.cfg.dotify()
        ppg = graphs.ProgramPointGraph.create_from_control_flow_graph(subprogram.cfg)
        ppg.dotify()
        lnt = graphs.LoopNests(ppg)
        lnt.dotify()

        ast = graphs.SyntaxTree(ppg)
        ast.dotify()


def parse_the_command_line():
    parser = argparse.ArgumentParser(description='Construct an abstract syntax tree for each control flow graph')

    parser.add_argument('--filename',
                        help='read the program from this file',
                        required=True)

    return parser.parse_args()


if __name__ == '__main__':
    assert sys.version_info >= (3, 0), 'Script requires Python 3.0 or greater to run'
    main(**vars(parse_the_command_line()))

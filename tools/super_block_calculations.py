import argparse
import os
import shutil
import sys
import threading
import timeit

from calculations import calculations
from graphs import graph
from system import program, database
from utils import messages


def main(**kwargs):
    prog = program.IO.read(kwargs['filename'])
    prog.cleanup()
    with database.Database(kwargs['database']) as db:
        for subprogram in prog:
            subprogram.cfg.dotify()
            ppg = graph.ProgramPointGraph.create_from_control_flow_graph(subprogram.cfg)
            ppg.dotify()

            for i in range(0, kwargs['repeat']):
                lnt = graph.LoopNests(ppg)
                lnt.dotify()

                ilp_for_ppg = calculations.create_ilp_for_program_point_graph(ppg, lnt, db)
                ilp_for_ppg.solve('{}{}.ppg.ilp'.format(prog.basename(), ppg.name))

                start = timeit.default_timer()
                super_graph = graph.SuperBlockGraph(lnt)
                super_graph.dotify()
                end = timeit.default_timer()

                ilp_for_super = calculations.create_ilp_for_super_block_graph(super_graph, lnt, db)
                ilp_for_super.solve('{}{}.super.ilp'.format(prog.basename(), ppg.name))
                ilp_for_super.add_to_construction_time(end - start)

                messages.verbose_message('>>>>>', ppg.name, "PASSED" if ilp_for_ppg.wcet == ilp_for_super.wcet else "FAILED " * 10)
                messages.verbose_message(ilp_for_ppg)
                messages.verbose_message(ilp_for_super, new_lines=2)


def parse_the_command_line():
    parser = argparse.ArgumentParser(description='Do WCET calculation using super blocks')

    parser.add_argument('--filename',
                        help='read the program from this file',
                        required=True)

    parser.add_argument('--database',
                        help='use the WCET data in this file',
                        required=True)

    parser.add_argument('--repeat',
                        type=int,
                        help='repeat the calculation this many times',
                        default=1,
                        metavar='<INT>')

    parser.add_argument('--folded',
                        action='store_true',
                        help='fold super blocks before constraint solving',
                        default=False)

    return parser.parse_args()


if __name__ == '__main__':
    assert sys.version_info >= (3, 0), 'Script requires Python 3.0 or greater to run'
    assert shutil.which('lp_solve', mode=os.X_OK), 'Script requires lp_solve to be in your path'
    threading.stack_size(2 ** 26)
    sys.setrecursionlimit(2 ** 20)
    main(**vars(parse_the_command_line()))

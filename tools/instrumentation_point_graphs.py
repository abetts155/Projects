import argparse
import os
import shutil
import sys
import threading

from graphs import graphs
from system import (program, database, calculations)
from utils import messages


def main(**kwargs):
    messages.verbose_message("Reading program from '{}'".format(kwargs['filename']))
    prog = program.IO.read(kwargs['filename'])
    prog.cleanup()

    failures = set()
    with database.Database(kwargs['database']) as db:
        messages.verbose_message("Using database '{}'".format(kwargs['database']))
        for subprogram in prog:
            if not kwargs['subprograms'] or (kwargs['subprograms'] and subprogram.name in kwargs['subprograms']):
                subprogram.cfg.dotify()
                ppg = graphs.ProgramPointGraph.create_from_control_flow_graph(subprogram.cfg)
                ppg.dotify()
                lnt = graphs.LoopNests(ppg)
                lnt.dotify()

                ilp_for_ppg = calculations.create_ilp_for_program_point_graph(ppg, lnt, db)
                ilp_for_ppg.solve('{}.{}.ppg.ilp'.format(prog.basename(), ppg.name))

                ipg = graphs.InstrumentationPointGraph.create(ppg, lnt, db)
                ipg.dotify()

                ilp_for_ipg = calculations.create_ilp_for_instrumentation_point_graph(ipg, lnt, db)
                ilp_for_ipg.solve('{}.{}.ipg.ilp'.format(prog.basename(), ipg.name))

                if ilp_for_ppg.wcet != ilp_for_ipg.wcet:
                    messages.verbose_message('>>>>>', ppg.name, 'FAILED' * 2)
                    failures.add(ppg.name)
                    messages.verbose_message(ilp_for_ppg)
                    messages.verbose_message(ilp_for_ipg, new_lines=2)
                else:
                    messages.verbose_message('>>>>> {} passed {:.5f} {:.5f}'.format(ppg.name,
                                                                                    ilp_for_ppg.solve_time,
                                                                                    ilp_for_ipg.solve_time))

    if len(failures) > 0:
        messages.verbose_message('The following subprograms failed: {}'.format(', '.join(failures)))


def parse_the_command_line():
    parser = argparse.ArgumentParser(description='Do WCET calculation using instrumentation point graphs')

    parser.add_argument('--program',
                        help='read the program from this file',
                        required=True)

    parser.add_argument('--database',
                        help='use the data in this file',
                        required=True)

    parser.add_argument('--repeat',
                        type=int,
                        help='repeat the calculation this many times',
                        default=1,
                        metavar='<INT>')

    parser.add_argument('--subprograms',
                        nargs='+',
                        help='only do the calculation for these subprograms',
                        metavar='<NAME>')

    return parser.parse_args()


if __name__ == '__main__':
    assert sys.version_info >= (3, 0), 'Script requires Python 3.0 or greater to run'
    assert shutil.which('lp_solve', mode=os.X_OK), 'Script requires lp_solve to be in your path'
    threading.stack_size(2 ** 26)
    sys.setrecursionlimit(2 ** 20)
    main(**vars(parse_the_command_line()))

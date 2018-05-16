import argparse
import os
import shutil
import sys
import threading
import timeit

from graphs import graphs
from system import (program, database, calculations)
from utils import messages


def main(**kwargs):
    messages.verbose_message("Reading program from '{}'".format(kwargs['filename']))
    the_program = program.IO.read(kwargs['filename'])
    the_program.cleanup()

    failures = set()
    with database.Database(kwargs['database']) as db:
        messages.verbose_message("Using database '{}'".format(kwargs['database']))
        for subprogram in the_program:
            subprogram.cfg.dotify()
            ppg = graphs.ProgramPointGraph.create_from_control_flow_graph(subprogram.cfg)
            ppg.dotify()
            lnt = graphs.LoopNests(ppg)
            lnt.dotify()

            ilp_for_ppg = calculations.create_ilp_for_program_point_graph(ppg, lnt, db)
            ilp_for_ppg.solve('{}.{}.ppg.ilp'.format(the_program.basename(), ppg.name))

            start = timeit.default_timer()
            super_graph = graphs.SuperBlockGraph(ppg, lnt)
            super_graph.dotify()
            end = timeit.default_timer()

            ilp_for_super = calculations.create_ilp_for_super_block_graph(super_graph, lnt, db)
            ilp_for_super.solve('{}.{}.super.ilp'.format(the_program.basename(), ppg.name))
            ilp_for_super.add_to_construction_time(end - start)

            if ilp_for_ppg.wcet != ilp_for_super.wcet:
                messages.verbose_message('>>>>>', ppg.name, 'FAILED' * 2)
                failures.add(ppg.name)
                messages.verbose_message(ilp_for_ppg)
                messages.verbose_message(ilp_for_super, new_lines=2)
            else:
                ppg_solve_times = []
                super_solve_times = []

                for i in range(0, kwargs['repeat']):
                    ilp_for_ppg.solve('{}.{}.ppg.ilp'.format(the_program.basename(), ppg.name))
                    ppg_solve_times.append(ilp_for_ppg.solve_time)
                    ilp_for_super.solve('{}.{}.super.ilp'.format(the_program.basename(), ppg.name))
                    super_solve_times.append(ilp_for_super.solve_time)

                ppg_solve_time = sum(ppg_solve_times)/len(ppg_solve_times)
                super_solve_time = sum(super_solve_times)/len(super_solve_times)

                factor = ppg_solve_time/super_solve_time
                if factor > 1:
                    message = '{:.2}X speed up'.format(factor)
                else:
                    message = '{:.2}X slow down'.format(1/factor)
                messages.verbose_message('>>>>> {} passed {:.5f} {:.5f} {}'.format(ppg.name,
                                                                                   ppg_solve_time,
                                                                                   super_solve_time,
                                                                                   message))


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

import argparse
import os
import shutil
import sys
import threading
import typing

from graphs import graphs
from system import (program, database, calculations)
from utils import messages


def main(program_filename:       str,
         database_filename:      str,
         repeat:                 int,
         subprogram_names:       typing.List[str],
         fold_optimisation:      bool,
         dominator_optimisation: bool):
    the_program = program.IO.read(program_filename)
    the_program.cleanup()

    failures = set()
    with database.Database(database_filename) as db:
        messages.verbose_message("Using database '{}'".format(database_filename))
        db.load_into_memory()

        analysable_subprograms = [subprogram for subprogram in the_program
                                  if not subprogram_names or (subprogram_names and subprogram.name in subprogram_names)]
        all_ppg_solve_times = []
        all_super_solve_times = []
        for subprogram in analysable_subprograms:
            subprogram.cfg.dotify()
            ppg = graphs.ProgramPointGraph.create_from_control_flow_graph(subprogram.cfg)
            ppg.dotify()
            lnt = graphs.LoopNests(ppg)
            lnt.dotify()

            ilp_for_ppg = calculations.create_ilp_for_program_point_graph(ppg, lnt, db)
            ilp_for_ppg.solve('{}.{}.ppg.ilp'.format(the_program.basename(), ppg.name))

            ilp_for_super = calculations.create_ilp_for_super_block_graph(ppg,
                                                                          lnt,
                                                                          db,
                                                                          fold_optimisation,
                                                                          dominator_optimisation)
            ilp_for_super.solve('{}.{}.super.ilp'.format(the_program.basename(), ppg.name))

            messages.verbose_message('>>>>>', ppg.name)
            if ilp_for_ppg.wcet != ilp_for_super.wcet:
                messages.verbose_message('FAILED')
                messages.verbose_message(ilp_for_ppg)
                messages.verbose_message(ilp_for_super, new_lines=2)
                failures.add(ppg.name)
            else:
                messages.verbose_message('PASSED')
                ppg_construction_times = []
                ppg_solve_times = []
                super_construction_times = []
                super_solve_times = []

                for i in range(0, repeat):
                    ilp_for_ppg = calculations.create_ilp_for_program_point_graph(ppg, lnt, db)
                    ilp_for_ppg.solve('{}.{}.ppg.ilp'.format(the_program.basename(), ppg.name))
                    ppg_construction_times.append(ilp_for_ppg.construction_time)
                    ppg_solve_times.append(ilp_for_ppg.solve_time)

                    ilp_for_super = calculations.create_ilp_for_super_block_graph(ppg,
                                                                                  lnt,
                                                                                  db,
                                                                                  fold_optimisation,
                                                                                  dominator_optimisation)
                    ilp_for_super.solve('{}.{}.{}.super.ilp'.format(the_program.basename(), i, ppg.name))
                    super_construction_times.append(ilp_for_super.construction_time)
                    super_solve_times.append(ilp_for_super.solve_time)

                ppg_solve_time = sum(ppg_solve_times)/repeat
                ppg_construction_time = sum(ppg_construction_times)/repeat
                super_solve_time = sum(super_solve_times)/repeat
                super_construction_time = sum(super_construction_times)/repeat

                all_ppg_solve_times.extend(ppg_solve_times)
                all_super_solve_times.extend(super_solve_times)

                factor = ppg_solve_time/super_solve_time
                if factor > 1:
                    solve_message = '{:.2}X speed up'.format(factor)
                else:
                    solve_message = '{:.2}X slow down'.format(1/factor)

                factor = (ppg_construction_time+ppg_solve_time)/(super_solve_time+super_construction_time)
                if factor > 1:
                    total_message = '{:.2}X speed up'.format(factor)
                else:
                    total_message = '{:.2}X slow down'.format(1 / factor)

                messages.verbose_message('solve={}'.format(solve_message))
                messages.verbose_message('total={}'.format(total_message))
                messages.verbose_message('solve={:.5f} '
                                         'construction={:.5f} '
                                         'total={:.5f} '
                                         'variables={} '
                                         'constraints={} '
                                         '[PPG]'.format(ppg_solve_time,
                                                        ppg_construction_time,
                                                        ppg_solve_time + ppg_construction_time,
                                                        ilp_for_ppg.number_of_variables(),
                                                        ilp_for_ppg.number_of_constraints()))
                messages.verbose_message('solve={:.5f} '
                                         'construction={:.5f} '
                                         'total={:.5f} '
                                         'variables={} '
                                         'constraints={} '
                                         '[SUPER]'.format(super_solve_time,
                                                          super_construction_time,
                                                          super_solve_time + super_construction_time,
                                                          ilp_for_super.number_of_variables(),
                                                          ilp_for_super.number_of_constraints()))

        total_trials = repeat * len(analysable_subprograms)
        factor = sum(all_ppg_solve_times) / sum(all_super_solve_times)
        if factor > 1:
            solve_message = '{:.2}X speed up'.format(factor)
        else:
            solve_message = '{:.2}X slow down'.format(1 / factor)
        messages.verbose_message('Average solve [PPG]={:.5f}'.format(sum(all_ppg_solve_times) / total_trials))
        messages.verbose_message('Average solve [SUPER]={:.5f}'.format(sum(all_super_solve_times) / total_trials))
        messages.verbose_message('solve={}'.format(solve_message))


def parse_the_command_line():
    parser = argparse.ArgumentParser(description='Do WCET calculation using super blocks')

    parser.add_argument('--program',
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

    parser.add_argument('--subprograms',
                        nargs='+',
                        help='only do the calculation for these subprograms',
                        metavar='<NAME>')

    parser.add_argument('--fold-optimisation',
                        action='store_true',
                        help='fold super blocks before constraint solving',
                        default=False)

    parser.add_argument('--dominator-optimisation',
                        action='store_true',
                        help='use dominator information to squash redundant constraints between fork and join '
                             'program points',
                        default=False)

    return parser.parse_args()


if __name__ == '__main__':
    assert sys.version_info >= (3, 0), 'Script requires Python 3.0 or greater to run'
    assert shutil.which('lp_solve', mode=os.X_OK), 'Script requires lp_solve to be in your path'
    threading.stack_size(2 ** 26)
    sys.setrecursionlimit(2 ** 20)
    kwargs = vars(parse_the_command_line())
    main(kwargs['program'],
         kwargs['database'],
         kwargs['repeat'],
         kwargs['subprograms'],
         kwargs['fold_optimisation'],
         kwargs['dominator_optimisation'])

import argparse
import random
import sys
import threading

from graphs import graph
from system import program, database
from utils import messages


def add_automatic_wcet(db: database.Database, v, maximum_value):
    db.add_wcet(v.program_point, random.randint(0, maximum_value))


def add_automatic_wfreq(db: database.Database, v, lnt, maximum_value):
    if lnt.is_header(v):
        loop = lnt.find_loop(v)
        if lnt.is_outermost_loop(loop):
            db.add_wfreq(v.program_point, 1)
        else:
            db.add_wfreq(v.program_point, random.randint(1, maximum_value))


def main(**kwargs):
    the_program = program.IO.read(kwargs['filename'])
    properties = program.IO.read_properties(kwargs['filename'])

    with database.Database(kwargs['database']) as db:
        db.reset()
        for subprogram in the_program:
            messages.debug_message('Creating data for {}'.format(subprogram.name))
            ppg = graph.ProgramPointGraph.create_from_control_flow_graph(subprogram.cfg)
            ppg.dotify()
            lnt = graph.LoopNests(ppg)
            lnt.dotify()

            for v in ppg:
                if not kwargs['manual_properties'] or v.program_point not in properties:
                    if isinstance(v.program_point, graph.Vertex):
                        add_automatic_wcet(db, v, kwargs['max_wcet'])
                    else:
                        db.add_wcet(v.program_point, 0)
                    add_automatic_wfreq(db, v, lnt, kwargs['max_loop_bound'])
                else:
                    if properties[v.program_point].wcet is not None:
                        db.add_wcet(v.program_point, properties[v.program_point].wcet)
                    else:
                        add_automatic_wcet(db, v, kwargs['max_wcet'])

                    if properties[v.program_point].bound is not None:
                        db.add_wfreq(v.program_point, properties[v.program_point].bound)
                    else:
                        add_automatic_wfreq(db, v, lnt, kwargs['max_loop_bound'])

            if not kwargs['manual_properties']:
                ppg.choose_instrumentation()
                for v in ppg:
                    db.set_instrumentation(v.program_point)
            else:
                for v in ppg:
                    if v.program_point in properties:
                        if properties[v.program_point].instrumentation:
                            db.set_instrumentation(v.program_point)


def parse_the_command_line():
    parser = argparse.ArgumentParser(description='Create a database of values needed in the WCET calculation')

    parser.add_argument('--filename',
                        help='read the program from this file',
                        required=True)

    parser.add_argument('--database',
                        help='write the data to this file',
                        required=True)

    parser.add_argument('--max-wcet',
                        type=int,
                        help='set the maximum possible value for execution times of basic blocks',
                        default=20,
                        metavar='<INT>')

    parser.add_argument('--max-loop-bound',
                        type=int,
                        help='set the maximum possible value for execution frequency bounds on loop headers',
                        default=10,
                        metavar='<INT>')

    parser.add_argument('--manual-properties',
                        action='store_true',
                        help='use properties of program points as specified manually in the program file',
                        default=False)

    return parser.parse_args()


if __name__ == '__main__':
    assert sys.version_info >= (3, 0), 'Script requires Python 3.0 or greater to run'
    threading.stack_size(2 ** 6 * 2 ** 20)
    sys.setrecursionlimit(2 ** 20)
    main(**vars(parse_the_command_line()))

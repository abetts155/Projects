import argparse
import random
import sys
import threading

from graphs import (edges, graphs, vertices)
from system import program, database
from utils import messages


def add_automatic_wcet(db: database.Database, v, maximum_value):
    db.add_wcet(v, random.randint(0, maximum_value))


def add_automatic_local_wfreq(db: database.Database, v, lnt, maximum_value):
    if lnt.is_header(v):
        loop = lnt.find_loop(v)
        if lnt.is_outermost_loop(loop):
            db.add_local_wfreq(v, 1)
        else:
            db.add_local_wfreq(v, random.randint(1, maximum_value))


def add_automatic_global_wfreq(db: database.Database, v, lnt, maximum_value):
    if lnt.is_header(v):
        loop = lnt.find_loop(v)
        if lnt.is_outermost_loop(loop):
            db.add_global_wfreq(v, 1)
        else:
            (loop_transition,) = [predecessor_edge for predecessor_edge in lnt.predecessors(loop)
                                  if predecessor_edge.direction == edges.LoopTransition.Direction.ENTRY]

            local_wfreq = db.get_local_wfreq(v)
            if loop_transition.predecessor() == lnt.entry:
                db.add_global_wfreq(v, local_wfreq)
            else:
                db.add_global_wfreq(v, random.randint(local_wfreq, maximum_value))


def main(**kwargs):
    the_program = program.IO.read(kwargs['filename'])
    properties = program.IO.read_properties(kwargs['filename'])

    with database.Database(kwargs['database']) as db:
        db.reset()
        for subprogram in the_program:
            messages.debug_message('Creating data for {}'.format(subprogram.name))
            ppg = graphs.ProgramPointGraph.create_from_control_flow_graph(subprogram.cfg)
            ppg.dotify()
            lnt = graphs.LoopNests(ppg)
            lnt.dotify()

            for v in ppg:
                if not kwargs['manual_properties'] or v.program_point not in properties:
                    if isinstance(v.program_point, vertices.Vertex):
                        add_automatic_wcet(db, v, kwargs['max_wcet'])
                    else:
                        db.add_wcet(v, 0)
                    add_automatic_local_wfreq(db, v, lnt, kwargs['max_loop_bound'])
                    add_automatic_global_wfreq(db, v, lnt, kwargs['max_loop_bound'])
                else:
                    if properties[v.program_point].wcet is not None:
                        db.add_wcet(v, properties[v.program_point].wcet)
                    else:
                        add_automatic_wcet(db, v, kwargs['max_wcet'])

                    if properties[v].local_wfreq is not None:
                        db.add_local_wfreq(v, properties[v].local_wfreq)
                    else:
                        add_automatic_local_wfreq(db, v, lnt, kwargs['max_loop_bound'])

                    if properties[v].global_wfreq is not None:
                        db.add_global_wfreq(v, properties[v].global_wfreq)
                    else:
                        add_automatic_global_wfreq(db, v, lnt, kwargs['max_loop_bound'])

            if not kwargs['manual_properties']:
                ppg.choose_instrumentation()
                for v in ppg:
                    db.set_instrumentation(v)
            else:
                for v in ppg:
                    if v in properties:
                        if properties[v].instrumentation:
                            db.set_instrumentation(v)


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

import argparse
import random
import sys
import threading

from graphs import (edges, graphs, vertices)
from system import program, database
from utils import messages


def main(**kwargs):
    the_program = program.IO.read(kwargs['program'])
    the_program.call_graph.dotify()

    if kwargs['manual']:
        program.IO.read_properties(the_program, kwargs['manual'])

    with database.Database(kwargs['database']) as db:
        db.reset()
        for subprogram in the_program:
            messages.debug_message('Creating data for {}'.format(subprogram.name))
            subprogram.cfg.dotify()
            ppg = graphs.ProgramPointGraph.create_from_control_flow_graph(subprogram.cfg)
            ppg.dotify()
            lnt = graphs.LoopNests(ppg)
            lnt.dotify()

            for v in ppg:
                if isinstance(v.program_point, vertices.Vertex):
                    default = random.randint(0, kwargs['max_wcet'])
                else:
                    default = 0

                if kwargs['manual']:
                    db.add_wcet(v, getattr(v.program_point, program.IO.WCET, default))
                else:
                    db.add_wcet(v, default)

                if lnt.is_header(v):
                    loop = lnt.find_loop(v)
                    if lnt.is_outermost_loop(loop):
                        db.add_global_wfreq(v, 1)
                    else:
                        if kwargs['manual']:
                            local_bound = getattr(v.program_point,
                                                  program.IO.LOCAL_BOUND,
                                                  random.randint(1, kwargs['max_loop_bound']))
                        else:
                            local_bound = random.randint(1, kwargs['max_loop_bound'])

                        (loop_transition,) = [predecessor_edge for predecessor_edge in lnt.predecessors(loop)
                                              if predecessor_edge.direction == edges.LoopTransition.Direction.ENTRY]
                        if loop_transition.predecessor() == lnt.entry:
                            db.add_local_wfreq(v, local_bound)
                            db.add_global_wfreq(v, local_bound)
                        else:
                            if kwargs['manual']:
                                global_bound = getattr(v.program_point,
                                                       program.IO.GLOBAL_BOUND,
                                                       random.randint(local_bound, kwargs['max_loop_bound']))
                            else:
                                global_bound = random.randint(local_bound, kwargs['max_loop_bound'])
                            db.add_local_wfreq(v, local_bound)
                            db.add_global_wfreq(v, global_bound)


def parse_the_command_line():
    parser = argparse.ArgumentParser(description='Create a database of values needed in the WCET calculation')

    parser.add_argument('--program',
                        help='read the program from this file',
                        required=True)

    parser.add_argument('--database',
                        help='write the data to this file',
                        required=True)

    parser.add_argument('--manual',
                        help='use properties of program points as specified manually in this file')

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

    return parser.parse_args()


if __name__ == '__main__':
    assert sys.version_info >= (3, 0), 'Script requires Python 3.0 or greater to run'
    threading.stack_size(2 ** 6 * 2 ** 20)
    sys.setrecursionlimit(2 ** 20)
    main(**vars(parse_the_command_line()))

import argparse
import sys

from graphs import (edges, graphs, vertices)
from system import (traces, program)
from utils import messages


def filter_trace(ppg: graphs.ProgramPointGraph, ipg: graphs.InstrumentationPointGraph, trace_file):
    all_traces = traces.Traces()
    trace = traces.Trace()
    with open(trace_file, 'r') as rd:
        for line in rd:
            lexemes = line.split()
            if lexemes:
                if len(lexemes) == 2:
                    program_point = vertices.Vertex.id_pool[int(lexemes[0])]
                    v = ppg[program_point]
                else:
                    p = vertices.Vertex.id_pool[int(lexemes[0])]
                    s = vertices.Vertex.id_pool[int(lexemes[1])]
                    program_point = edges.Edge(p, s)
                    v = ppg[program_point]

                time = lexemes[-1]
                if v in ipg:
                    trace.append(traces.TraceElement(v, time))
            else:
                all_traces.append(trace)
                trace = traces.Trace()
    return all_traces


def main(**kwargs):
    the_program = program.IO.read(kwargs['program'])

    subprogram_trace = {}
    for trace_file in kwargs['traces']:
        name = traces.TraceFile.extract_subprogram(the_program, trace_file)
        subprogram_trace[name] = trace_file

    for subprogram in the_program:
        if subprogram.name in subprogram_trace:
            messages.debug_message('Filtering traces for {}'.format(subprogram.name))
            subprogram.cfg.dotify()
            ppg = graphs.ProgramPointGraph.create_from_control_flow_graph(subprogram.cfg)
            ppg.dotify()

            lnt = graphs.LoopNests(ppg)
            lnt.dotify()

            ipg = graphs.InstrumentationPointGraph.create(ppg, lnt)
            ipg.dotify()
            all_traces = filter_trace(ppg, ipg, subprogram_trace[subprogram.name])
            all_traces.write(ipg.trace_filename())


def parse_the_command_line():
    parser = argparse.ArgumentParser(description='Filter a set of traces to instrumented program points')

    parser.add_argument('--program',
                        help='read the program from this file',
                        required=True)

    parser.add_argument('--traces',
                        help='files containing traces',
                        required=True,
                        nargs='+')

    return parser.parse_args()


if __name__ == '__main__':
    assert sys.version_info >= (3, 0), 'Script requires Python 3.0 or greater to run'
    main(**vars(parse_the_command_line()))

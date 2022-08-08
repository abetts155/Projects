from argparse import ArgumentParser, Namespace
from graphs import vertices
from os.path import splitext
from utils.messages import verbose_message
from random import choice
from system import programs
from typing import Set


class StackFrame:
    __slots__ = ['call_vertex', 'cfg', 'cfg_vertex']

    def __init__(self, call_vertex, cfg, cfg_vertex):
        self.call_vertex = call_vertex
        self.cfg = cfg
        self.cfg_vertex = cfg_vertex


def generate_trace(program: programs.Program, root: vertices.CallVertex, uncovered: Set[vertices.BasicBlock]):
    clock = 0
    root_subprogram = program[root.name]
    stack = [StackFrame(root, root_subprogram.cfg, root_subprogram.cfg.entry)]
    trace = [(0, 0)]
    while stack:
        frame = stack.pop()
        uncovered.discard(frame.cfg_vertex)

        if frame.call_vertex == root and frame.cfg_vertex == root_subprogram.cfg.entry:
            clock = 0
        else:
            for instruction in frame.cfg_vertex.instructions:
                clock += instruction.random_latency()
        trace.append((frame.cfg_vertex.id_, clock))

        if frame.cfg_vertex == frame.cfg.exit:
            if frame.cfg_vertex != root_subprogram.cfg.exit:
                caller_frame = stack.pop()
                edge = choice(caller_frame.cfg.successors(caller_frame.cfg_vertex))
                next_vertex = edge.successor()
                stack.append(StackFrame(caller_frame.call_vertex, caller_frame.cfg, next_vertex))
        elif len(frame.cfg.successors(frame.cfg_vertex)) == 1:
            callee_vertex = program.call_graph.is_call_site(frame.call_vertex, frame.cfg_vertex)
            if callee_vertex:
                stack.append(StackFrame(frame.call_vertex, frame.cfg, frame.cfg_vertex))
                callee = program[callee_vertex.name]
                stack.append(StackFrame(callee_vertex, callee.cfg, callee.cfg.entry))
            else:
                edge = choice(frame.cfg.successors(frame.cfg_vertex))
                next_vertex = edge.successor()
                stack.append(StackFrame(frame.call_vertex, frame.cfg, next_vertex))
        else:
            candidates = [edge for edge in frame.cfg.successors(frame.cfg_vertex) if frame.cfg_vertex in uncovered]
            if not candidates:
                candidates = [edge for edge in frame.cfg.successors(frame.cfg_vertex)]

            edge = choice(candidates)
            next_vertex = edge.successor()
            stack.append(StackFrame(frame.call_vertex, frame.cfg, next_vertex))
    return trace


def main(args: Namespace):
    program = programs.IO.read(args.program)
    root = program.call_graph.get_root()
    verbose_message('Root is {}'.format(root.name))

    uncovered = set()
    for subprogram in program:
        for vertex in subprogram.cfg:
            uncovered.add(vertex)

    count = 1
    traces = []
    total = len(uncovered)
    attained = len(uncovered) / total
    while attained > args.coverage and count <= args.runs:
        if attained < args.coverage * 2:
            print('Tests = {}. Coverage attained = {:.2f}%.'.format(count, attained * 100))
        traces.append(generate_trace(program, root, uncovered))
        count += 1
        attained = len(uncovered) / total

    stem, _ = splitext(program.filename)
    with open('traces.{}.txt'.format(stem), 'w') as traces_file:
        traces_file.write('{}\n\n'.format(program.magic))
        for trace in traces:
            for x, y in trace:
                traces_file.write('{} {}\n'.format(x, y))
            traces_file.write('\n')


def parse_the_command_line():
    parser = ArgumentParser(description='Simulate program testing and attempt to find large end-to-end execution times')

    parser.add_argument('--program',
                        help='read the program from this file',
                        required=True)

    parser.add_argument('--runs',
                        type=int,
                        help='maximum number of runs',
                        metavar='<INT>',
                        default=20000)

    parser.add_argument('--coverage',
                        type=float,
                        help='basic block coverage stopping criterion',
                        metavar='<FLOAT>',
                        default=0.1)

    return parser.parse_args()


if __name__ == '__main__':
    args = parse_the_command_line()
    main(args)

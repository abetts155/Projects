from argparse import ArgumentParser, Namespace
from enum import Enum
from graphs import edges, graphs, vertices
from random import choice, randint
from system import calculations, programs
from typing import Dict, List, Set, Tuple
from utils.messages import error_message, verbose_message


def statically_analyse_cfg(cfg: graphs.ControlFlowGraph, execution_times: Dict[vertices.Vertex, int]):
    def create_objective_function():
        for vertex in cfg:
            variable = calculations.VertexVariable(vertex)
            ilp.add_variable(variable)
            term = calculations.Term(execution_times[vertex], variable)
            ilp.add_to_objective(term)

            for edge in cfg.successors(vertex):
                variable = calculations.EdgeVariable(edge)
                ilp.add_variable(variable)

    def create_structural_constraints():
        for vertex in cfg:
            flow_in_lhs = calculations.LinearExpr()
            for edge in cfg.predecessors(vertex):
                variable = calculations.EdgeVariable(edge)
                flow_in_lhs.append(variable)

            flow_in_rhs = calculations.LinearExpr()
            flow_in_rhs.append(calculations.VertexVariable(vertex))

            flow_in_constraint = calculations.Constraint(flow_in_lhs, flow_in_rhs, calculations.Constraint.EQUALITY)
            ilp.add_constraint(flow_in_constraint)

            flow_out_lhs = calculations.LinearExpr()
            for edge in cfg.predecessors(vertex):
                variable = calculations.EdgeVariable(edge)
                flow_out_lhs.append(variable)

            flow_out_rhs = calculations.LinearExpr()
            for edge in cfg.successors(vertex):
                variable = calculations.EdgeVariable(edge)
                flow_out_rhs.append(variable)

            flow_out_constraint = calculations.Constraint(flow_out_lhs, flow_out_rhs, calculations.Constraint.EQUALITY)
            ilp.add_constraint(flow_out_constraint)

    def create_loop_bound_constraints():
        lhs = calculations.LinearExpr()
        lhs.append(calculations.VertexVariable(cfg.entry))
        rhs = calculations.LinearExpr()
        rhs.append(calculations.Term(1))
        constraint = calculations.Constraint(lhs, rhs, calculations.Constraint.EQUALITY)
        ilp.add_constraint(constraint)

    ilp = calculations.IntegerLinearProgram()
    create_objective_function()
    create_structural_constraints()
    create_loop_bound_constraints()
    ilp.solve('{}.cfg.ilp'.format(cfg.name))
    return ilp.wcet


class MeasuredData:
    __slots__ = ['subprogram', 'times', 'fixed_counts', 'temporary_counts']

    def __init__(self, subprogram: programs.Subprogram):
        self.subprogram = subprogram
        self.times = {}
        self.fixed_counts = {}
        self.temporary_counts = {}
        for vertex in subprogram.ipg:
            for edge in subprogram.ipg.successors(vertex):
                if vertex != subprogram.ipg.exit:
                    self.times[edge] = 0

        for loop in subprogram.lnt:
            self.fixed_counts[loop] = 0
            self.temporary_counts[loop] = 0

    def reset_temporary_counts(self):
        for loop, count in self.temporary_counts.items():
            if count > self.fixed_counts[loop]:
                self.fixed_counts[loop] = count
            self.temporary_counts[loop] = 0


def statically_analyse_ipg(ipg: graphs.InstrumentationPointGraph,
                           lnt: graphs.LoopNest,
                           measured_data: MeasuredData,
                           vertex_times: Dict[vertices.Vertex, int]):
    def create_objective_function():
        for vertex in ipg:
            variable = calculations.VertexVariable(vertex)
            ilp.add_variable(variable)
            term = calculations.Term(vertex_times[vertex], variable)
            ilp.add_to_objective(term)

            if vertex != ipg.exit:
                for edge in ipg.successors(vertex):
                    variable = calculations.EdgeVariable(edge)
                    ilp.add_variable(variable)
                    term = calculations.Term(measured_data.times[edge], variable)
                    ilp.add_to_objective(term)

    def create_structural_constraints():
        for vertex in ipg:
            flow_in_lhs = calculations.LinearExpr()
            for edge in ipg.predecessors(vertex):
                variable = calculations.EdgeVariable(edge)
                flow_in_lhs.append(variable)

            flow_in_rhs = calculations.LinearExpr()
            flow_in_rhs.append(calculations.VertexVariable(vertex))

            flow_in_constraint = calculations.Constraint(flow_in_lhs, flow_in_rhs, calculations.Constraint.EQUALITY)
            ilp.add_constraint(flow_in_constraint)

            flow_out_lhs = calculations.LinearExpr()
            for edge in ipg.predecessors(vertex):
                variable = calculations.EdgeVariable(edge)
                flow_out_lhs.append(variable)

            flow_out_rhs = calculations.LinearExpr()
            for edge in ipg.successors(vertex):
                variable = calculations.EdgeVariable(edge)
                flow_out_rhs.append(variable)

            flow_out_constraint = calculations.Constraint(flow_out_lhs, flow_out_rhs, calculations.Constraint.EQUALITY)
            ilp.add_constraint(flow_out_constraint)

    def create_execution_count_constraints():
        for loop in lnt:
            if loop == lnt.root:
                lhs = calculations.LinearExpr()
                lhs.append(calculations.VertexVariable(ipg.entry))
                rhs = calculations.LinearExpr()
                rhs.append(calculations.Term(1))
                constraint = calculations.Constraint(lhs, rhs, calculations.Constraint.EQUALITY)
                ilp.add_constraint(constraint)
            else:
                for vertex in loop:
                    lhs = calculations.LinearExpr()
                    lhs.append(calculations.VertexVariable(vertex))
                    rhs = calculations.LinearExpr()
                    rhs.append(calculations.Term(measured_data.fixed_counts[loop]))
                    constraint = calculations.Constraint(lhs, rhs, calculations.Constraint.LESS_OR_EQUAL)
                    ilp.add_constraint(constraint)

    ilp = calculations.IntegerLinearProgram()
    create_objective_function()
    create_structural_constraints()
    create_execution_count_constraints()
    ilp.solve('{}.ipg.ilp'.format(ipg.name))
    return ilp.wcet


def static_analysis(program: programs.Program, root_vertex: vertices.SubprogramVertex, dfs: graphs.DepthFirstSearch):
    wcets = {}
    for call_vertex in dfs.post_order():
        subprogram = program[call_vertex.name]
        execution_times = {}
        for vertex in subprogram.cfg:
            execution_times[vertex] = sum([instruction.worst_latency() for instruction in vertex.instructions])
            callee_vertex = program.call_graph.is_call_site(call_vertex, vertex)
            if callee_vertex:
                execution_times[vertex] += wcets[callee_vertex]

        wcet = statically_analyse_cfg(subprogram.cfg, execution_times)
        wcets[call_vertex] = wcet

    print('Static WCET estimate: {}'.format(wcets[root_vertex]))


class InstrumentationPolicy(Enum):
    none = 'none'
    full = 'full'
    deterministic = 'deterministic'
    subprograms = 'subprograms'

    def __str__(self):
        return self.value


def create_call(vertex: vertices.BasicBlock):
    call_instruction = vertex.instructions[-1]
    call_id = vertices.Vertex.get_vertex_id()
    return vertices.CallVertex(call_id, call_instruction.target)
    return call


def create_instrumentation_point(vertex: vertices.BasicBlock):
    point_id = vertices.Vertex.get_vertex_id()
    point = vertices.InstrumentationVertex(point_id, vertex.id_)
    return point


def instrument_with_a_policy(program: programs.Program,
                             call_vertex: vertices.SubprogramVertex,
                             cfg: graphs.ControlFlowGraph,
                             policy: InstrumentationPolicy,
                             vertex_to_ipg: Dict[vertices.Vertex, List]):
    for vertex in cfg:
        if policy == InstrumentationPolicy.full:
            if program.call_graph.is_call_site(call_vertex, vertex):
                vertex_to_ipg[vertex] = (create_instrumentation_point(vertex), create_call(vertex))
            else:
                point = create_instrumentation_point(vertex)
                vertex_to_ipg[vertex] = (point, point)
        elif policy == InstrumentationPolicy.deterministic:
            if program.call_graph.is_call_site(call_vertex, vertex):
                vertex_to_ipg[vertex] = (create_instrumentation_point(vertex), create_call(vertex))
            elif vertex == cfg.entry or vertex == cfg.exit:
                point = create_instrumentation_point(vertex)
                vertex_to_ipg[vertex] = (point, point)
        elif policy == InstrumentationPolicy.subprograms:
            if vertex == cfg.entry:
                if program.call_graph.is_call_site(call_vertex, vertex):
                    vertex_to_ipg[vertex] = (create_instrumentation_point(vertex), create_call(vertex))
                else:
                    point = create_instrumentation_point(vertex)
                    vertex_to_ipg[vertex] = (point, point)
            elif vertex == cfg.entry or vertex == cfg.exit:
                point = create_instrumentation_point(vertex)
                vertex_to_ipg[vertex] = (point, point)
            elif program.call_graph.is_call_site(call_vertex, vertex):
                call = create_call(vertex)
                vertex_to_ipg[vertex] = (call, call)


def instrument_with_a_budget(program: programs.Program,
                             call_vertex: vertices.SubprogramVertex,
                             cfg: graphs.ControlFlowGraph,
                             budget: int,
                             vertex_to_ipg: Dict[vertices.Vertex, List]):
    point = create_instrumentation_point(cfg.entry)
    if program.call_graph.is_call_site(call_vertex, cfg.entry):
        vertex_to_ipg[cfg.entry] = (point, create_call(cfg.entry))
    else:
        vertex_to_ipg[cfg.entry] = (point, point)

    point = create_instrumentation_point(cfg.exit)
    vertex_to_ipg[cfg.exit] = (point, point)

    budget -= 2
    candidates = [vertex for vertex in cfg if vertex != cfg.entry and vertex != cfg.exit]
    while budget > 0:
        budget -= 1
        vertex = choice(candidates)
        candidates.remove(vertex)
        point = create_instrumentation_point(vertex)
        if program.call_graph.is_call_site(call_vertex, vertex):
            vertex_to_ipg[vertex] = (point, create_call(vertex))
        else:
            vertex_to_ipg[vertex] = (point, point)

    for vertex in cfg:
        if program.call_graph.is_call_site(call_vertex, vertex):
            if vertex not in vertex_to_ipg:
                call = create_call(vertex)
                vertex_to_ipg[vertex] = (call, call)


def determinise(ipg: graphs.FlowGraph):
    worklist = [vertex for vertex in ipg]
    for vertex in worklist:
        if vertex in ipg:
            partition = {}
            for edge in ipg.successors(vertex):
                successor = edge.successor()
                if isinstance(successor, vertices.CallVertex):
                    partition.setdefault(successor.callee, []).append(successor)

            for callee, subset in partition.items():
                if len(subset) > 1:
                    representative = subset.pop()
                    worklist.append(representative)
                    for dead in subset:
                        for edge in ipg.predecessors(dead):
                            predecessor = edge.predecessor()
                            if not ipg.has_edge(predecessor, representative):
                                ipg.add_edge(edges.Edge(predecessor, representative))

                        for edge in ipg.successors(dead):
                            successor = edge.successor()
                            if not ipg.has_edge(representative, successor):
                                ipg.add_edge(edges.Edge(representative, successor))

                    for dead in subset:
                        ipg.remove_vertex(dead)


def create_ipg(program: programs.Program, cfg: graphs.ControlFlowGraph, vertex_to_ipg: Dict[vertices.Vertex, Tuple]):
    ipg = graphs.FlowGraph(program, cfg.name)

    in_data = {}
    out_data = {}
    for vertex in cfg:
        in_data[vertex] = set()
        out_data[vertex] = set()

        if vertex in vertex_to_ipg:
            the_entry, the_exit = vertex_to_ipg[vertex]
            ipg.add_vertex(the_entry)

            if the_entry != the_exit:
                ipg.add_vertex(the_exit)
                ipg.add_edge(edges.Edge(the_entry, the_exit))

    (ipg.entry, _) = vertex_to_ipg[cfg.entry]
    (ipg.exit, _) = vertex_to_ipg[cfg.exit]

    changed = True
    dfs = graphs.DepthFirstSearch(cfg, cfg.entry)
    while changed:
        changed = False

        for vertex in reversed(dfs.post_order()):
            size = len(in_data[vertex])

            for edge in cfg.predecessors(vertex):
                in_data[vertex].update(out_data[edge.predecessor()])

            if vertex in vertex_to_ipg:
                out_data[vertex] = {vertex}
            else:
                out_data[vertex] = in_data[vertex]

            if size != len(in_data[vertex]):
                changed = True

    for successor, predecessors in in_data.items():
        if successor in vertex_to_ipg:
            for predecessor in predecessors:
                if predecessor in vertex_to_ipg:
                    ipg.add_edge(edges.Edge(vertex_to_ipg[predecessor][1],
                                            vertex_to_ipg[successor][0]))

    determinise(ipg)
    return ipg


def create_lnt(ipg: graphs.FlowGraph) -> graphs.LoopNest:
    alive = {}
    unexplored = set()
    for vertex in ipg:
        unexplored.add(vertex)
        alive[vertex] = True
        for edge in ipg.successors(vertex):
            if vertex == ipg.exit:
                alive[edge] = False
            else:
                alive[edge] = True

    loop_nest = graphs.LoopNest(ipg.name)
    iteration = 1
    surplus = set()
    while unexplored:
        sccs = graphs.StrongComponents(ipg, alive)

        for scc in sccs.non_trivial():
            loop = loop_nest.create_loop()
            headers = set()
            for vertex in scc:
                loop_nest.add_to_body(vertex, loop)

                for edge in ipg.predecessors(vertex):
                    if edge.predecessor() not in scc:
                        loop_nest.add_to_headers(vertex, loop)
                        headers.add(vertex)

                for edge in ipg.successors(vertex):
                    if edge.successor() not in scc:
                        alive[edge] = False

            for vertex in headers:
                for edge in ipg.predecessors(vertex):
                    alive[edge] = False

        for vertex in sccs.singletons:
            alive[vertex] = False
            unexplored.remove(vertex)

            if ipg.has_edge(vertex, vertex):
                loop = loop_nest.create_loop()
                loop_nest.add_to_body(vertex, loop)
                loop_nest.add_to_headers(vertex, loop)
            elif iteration == 1:
                surplus.add(vertex)

        iteration += 1

    outermost_loop = loop_nest.create_loop()
    for vertex in surplus:
        loop_nest.add_to_body(vertex, outermost_loop)
    loop_nest.add_to_headers(ipg.entry, outermost_loop)
    loop_nest.root = outermost_loop

    for loop_a in loop_nest:
        for vertex in loop_a:
            for edge in ipg.successors(vertex):
                loop_b = loop_nest.loop(edge.successor())
                if loop_a != loop_b:
                    if not loop_nest.has_edge(loop_a, loop_b):
                        loop_nest.add_edge(edges.Edge(loop_a, loop_b))

    loop_nest.set_levels()
    return loop_nest


def filter_traces(labels: Set[int], traces_filename: str):
    trace = []
    with open(traces_filename, 'r') as traces_file:
        for line_number, line in enumerate(traces_file):
            line = line.strip()
            if line_number > 0 and line:
                first, second = line.split()
                first, second = list(map(int, [first, second]))
                if first in labels:
                    trace.append((first, second))
    return trace


class ParsingPosition:
    __slots__ = ['ipg', 'vertex', 'lnt', 'state']

    def __init__(self, ipg, vertex, lnt, state):
        self.ipg = ipg
        self.vertex = vertex
        self.lnt = lnt
        self.state = state


def parse_traces(program: programs.Program,
                 root_vertex: vertices.SubprogramVertex,
                 traces_filename: str,
                 labels: Set[int],
                 call_table: Dict[int, str],
                 trace: List,
                 wcet_data: Dict[str, MeasuredData],
                 measured_times: Set[int]):
    sentinel = 0
    root_subprogram = program[root_vertex.name]
    origin = ParsingPosition(root_subprogram.ipg,
                             root_subprogram.ipg.entry,
                             root_subprogram.lnt,
                             root_subprogram.lnt.loop(root_subprogram.ipg.entry))
    call_stack = []
    position = origin
    for label, tick in trace:
        if label == sentinel and tick == sentinel:
            pass
        else:
            if label == root_subprogram.ipg.entry.label:
                assert position.vertex == root_subprogram.ipg.entry
                call_stack.append(position)
            else:
                intra_position_a = position
                chosen_edge = None
                for edge in position.ipg.successors(position.vertex):
                    candidate = edge.successor()
                    if isinstance(candidate, vertices.InstrumentationVertex):
                        if candidate.label == label:
                            position = ParsingPosition(position.ipg,
                                                       candidate,
                                                       position.lnt,
                                                       position.state)
                            intra_position_b = position
                            chosen_edge = edge
                    elif label in call_table:
                        callee = call_table[label]
                        if callee == candidate.callee:
                            return_position = ParsingPosition(position.ipg,
                                                              candidate,
                                                              position.lnt,
                                                              position.state)
                            call_stack.append(return_position)
                            intra_position_b = return_position

                            callee_subprogram = program[callee]
                            position = ParsingPosition(callee_subprogram.ipg,
                                                       callee_subprogram.ipg.entry,
                                                       callee_subprogram.lnt,
                                                       callee_subprogram.lnt.loop(callee_subprogram.ipg.entry))
                            chosen_edge = edge

                    if chosen_edge:
                        subprogram_data = wcet_data[intra_position_a.ipg.name]
                        lnt = intra_position_a.lnt
                        loop_a = lnt.loop(intra_position_a.vertex)
                        loop_b = lnt.loop(intra_position_b.vertex)
                        if loop_a != loop_b and lnt.level(loop_b) >= lnt.level(loop_a):
                            subprogram_data.temporary_counts[loop_b] += 1

                        elapsed = tick - before
                        subprogram_data.times[chosen_edge] = max(subprogram_data.times[chosen_edge], elapsed)
                        break

                if not chosen_edge:
                    error_message('Parsing error at position {} with label {}'.format(position.vertex.id_, label))

        if position.vertex == position.ipg.exit:
            subprogram_data = wcet_data[position.ipg.name]
            subprogram_data.reset_temporary_counts()
            position = call_stack.pop()

            if not call_stack:
                measured_times.add(tick)

        before = tick


def hybrid_analysis(program: programs.Program,
                    root_vertex: vertices.SubprogramVertex,
                    dfs: graphs.DepthFirstSearch,
                    traces_filename: str,
                    policy: InstrumentationPolicy,
                    total_budget: int):
    if policy == InstrumentationPolicy.none:
        minimum = 2
        candidates = []
        budgets = {}
        for subprogram in program:
            budgets[subprogram] = minimum
            candidates.append(subprogram)

        remaining_budget = total_budget - minimum * len(program)
        while remaining_budget > 0:
            subprogram = choice(candidates)
            max_budget = subprogram.cfg.number_of_vertices() - budgets[subprogram]
            budget = randint(1, min(remaining_budget, max_budget))
            budgets[subprogram] += budget
            remaining_budget -= budget
            if budgets[subprogram] == subprogram.cfg.number_of_vertices():
                candidates.remove(subprogram)

        assert remaining_budget == 0

        for call_vertex in dfs.post_order():
            subprogram = program[call_vertex.name]
            vertex_to_ipg = {}
            instrument_with_a_budget(program, call_vertex, subprogram.cfg, budgets[subprogram], vertex_to_ipg)
            subprogram.ipg = create_ipg(program, subprogram.cfg, vertex_to_ipg)
    else:
        for call_vertex in dfs.post_order():
            subprogram = program[call_vertex.name]
            vertex_to_ipg = {}
            instrument_with_a_policy(program, call_vertex, subprogram.cfg, policy, vertex_to_ipg)
            subprogram.ipg = create_ipg(program, subprogram.cfg, vertex_to_ipg)

    labels = {0}
    call_table = {}
    wcet_data = {}
    for subprogram in program:
        subprogram.lnt = create_lnt(subprogram.ipg)
        wcet_data[subprogram.name] = MeasuredData(subprogram)
        for vertex in subprogram.ipg:
            if isinstance(vertex, vertices.InstrumentationVertex):
                labels.add(vertex.label)

        assert isinstance(subprogram.ipg.entry, vertices.InstrumentationVertex)
        call_table[subprogram.ipg.entry.label] = subprogram.name

    trace = filter_traces(labels, traces_filename)
    measured_times = set()
    parse_traces(program, root_vertex, traces_filename, labels, call_table, trace, wcet_data, measured_times)
    print('Dynamic WCET estimate: {}'.format(max(measured_times)))

    wcets = {}
    instrumentation_points = set()
    uncovered_transitions = 0
    total_transitions = 0
    for call_vertex in dfs.post_order():
        subprogram = program[call_vertex.name]
        vertex_times = {}
        for vertex in subprogram.ipg:
            if isinstance(vertex, vertices.CallVertex):
                vertex_times[vertex] = wcets[vertex.callee]
            else:
                vertex_times[vertex] = 0
                instrumentation_points.add(vertex)

        subprogram_data = wcet_data[call_vertex.name]
        wcet = statically_analyse_ipg(subprogram.ipg, subprogram.lnt, subprogram_data, vertex_times)
        wcets[call_vertex.name] = wcet

        for edge, time in subprogram_data.times.items():
            total_transitions += 1
            if time == 0:
                uncovered_transitions += 1

    print('Hybrid WCET estimate: {}'.format(wcets[root_vertex.name]))
    coverage = 100 * (total_transitions - uncovered_transitions) // total_transitions
    print('{}% transition coverage achieved'.format(coverage))
    print('{} instrumentation points'.format(len(instrumentation_points)))


def main(args: Namespace):
    program = programs.IO.read(args.program)
    root = program.call_graph.get_root()

    if args.budget:
        if args.budget < 2 * len(program):
            error_message('Each subprogram requires at least two instrumentation points; '
                          'the given program thus needs at least {}.'.format(len(program) * 2))

        max_budget = sum([subprogram.cfg.number_of_vertices() for subprogram in program])
        if args.budget > max_budget:
            error_message('The maximum number of allowed instrumentation points is {}.'.format(max_budget))

    with open(args.traces, 'r') as traces_file:
        line = traces_file.readline()
        line = line.strip()
        if line != program.magic:
            error_message('Traces are not generated by the given program.')

    verbose_message('Root is {}'.format(root.name))
    dfs = graphs.DepthFirstSearch(program.call_graph, root)
    static_analysis(program, root, dfs)
    hybrid_analysis(program, root, dfs, args.traces, args.policy, args.budget)


def check_arguments(args: Namespace):
    if not args.budget and args.policy == InstrumentationPolicy.none:
        error_message('Either choose an instrumentation budget or policy.')


def parse_the_command_line():
    parser = ArgumentParser(description='Perform both static and hybrid WCET analysis')

    parser.add_argument('--program',
                        help='read the program from this file',
                        required=True)

    parser.add_argument('--traces',
                        help='process the traces from this file',
                        required=True)

    parser.add_argument('--policy',
                        help='choose the instrumentation policy',
                        type=InstrumentationPolicy,
                        choices=list(InstrumentationPolicy),
                        default=InstrumentationPolicy.none.name)

    parser.add_argument('--budget',
                        help='choose the instrumentation budget',
                        type=int,
                        default=0)

    return parser.parse_args()


if __name__ == '__main__':
    args = parse_the_command_line()
    check_arguments(args)
    main(args)

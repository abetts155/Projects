from argparse import ArgumentParser, Namespace
from system import graph_based_calculations
from enum import Enum
from graphs import edges, graphs, vertices
from random import choice, randint, shuffle
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
            if len(ipg.successors(vertex)) > 0:
                flow_in_lhs = calculations.LinearExpr()
                for edge in ipg.successors(vertex):
                    variable = calculations.EdgeVariable(edge)
                    flow_in_lhs.append(variable)

                flow_in_rhs = calculations.LinearExpr()
                flow_in_rhs.append(calculations.VertexVariable(vertex))

                flow_in_constraint = calculations.Constraint(flow_in_lhs, flow_in_rhs, calculations.Constraint.EQUALITY)
                ilp.add_constraint(flow_in_constraint)

            if len(ipg.predecessors(vertex)) > 0 and len(ipg.successors(vertex)) > 0:
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
                             policy: InstrumentationPolicy):
    vertex_to_ipg = {}
    for vertex in cfg:
        if policy == InstrumentationPolicy.full:
            point = create_instrumentation_point(vertex)
            vertex_to_ipg[vertex] = point
        elif policy == InstrumentationPolicy.subprograms:
            if vertex == cfg.entry or vertex == cfg.exit:
                point = create_instrumentation_point(vertex)
                vertex_to_ipg[vertex] = point
        elif policy == InstrumentationPolicy.deterministic:
            if vertex == cfg.entry or vertex == cfg.exit or program.call_graph.is_call_site(call_vertex, vertex):
                point = create_instrumentation_point(vertex)
                vertex_to_ipg[vertex] = point
        else:
            assert False

    return vertex_to_ipg


def instrument_with_a_budget(program: programs.Program,
                             call_vertex: vertices.SubprogramVertex,
                             cfg: graphs.ControlFlowGraph,
                             budget: int,
                             randomise: bool) -> graphs.ControlFlowGraph:
    if budget < 2 or budget > cfg.number_of_vertices():
        error_message('Subprogram: {}, CFG size: {}, budget: {}'.format(call_vertex.name,
                                                                        cfg.number_of_vertices(),
                                                                        budget))

    def instrument(vertex_to_ipg: Dict, order: List[vertices.BasicBlock]):
        nonlocal budget

        if randomise:
            shuffle(order)

        while budget > 0 and order:
            vertex = order.pop(0)
            budget -= 1
            point = create_instrumentation_point(vertex)
            vertex_to_ipg[vertex] = point

    high_priority = []
    medium_priority = []
    low_priority = []
    for vertex in cfg:
        if vertex in [cfg.entry, cfg.exit]:
            high_priority.append(vertex)
        else:
            if program.call_graph.is_call_site(call_vertex, vertex):
                medium_priority.append(vertex)
            else:
                low_priority.append(vertex)

    vertex_to_ipg = {}
    instrument(vertex_to_ipg, high_priority)
    instrument(vertex_to_ipg, medium_priority)
    instrument(vertex_to_ipg, low_priority)

    return vertex_to_ipg


def create_instrumented_cfg(program: programs.Program, cfg: graphs.ControlFlowGraph, vertex_to_ipg: Dict):
    instrumented_cfg = graphs.ControlFlowGraph(program, cfg.name)
    for vertex in cfg:
        instrumented_cfg.add_vertex(vertex)
        if vertex in vertex_to_ipg:
            instrumented_cfg.add_vertex(vertex_to_ipg[vertex])

    if cfg.entry in vertex_to_ipg:
        instrumented_cfg.entry = vertex_to_ipg[cfg.entry]
    else:
        instrumented_cfg.entry = cfg.entry

    if cfg.exit in vertex_to_ipg:
        instrumented_cfg.exit = vertex_to_ipg[cfg.exit]
    else:
        instrumented_cfg.exit = cfg.exit

    for vertex in cfg:
        if vertex in vertex_to_ipg:
            if vertex == cfg.exit:
                instrumented_cfg.add_edge(edges.Edge(vertex, vertex_to_ipg[vertex]))
            else:
                instrumented_cfg.add_edge(edges.Edge(vertex_to_ipg[vertex], vertex))

        for edge in cfg.successors(vertex):
            if vertex == cfg.exit and vertex in vertex_to_ipg:
                true_predecessor = vertex_to_ipg[vertex]
            else:
                true_predecessor = vertex

            if edge.successor() in vertex_to_ipg:
                if edge.successor() == cfg.exit:
                    true_successor = edge.successor()
                else:
                    true_successor = vertex_to_ipg[edge.successor()]
            else:
                true_successor = edge.successor()

            instrumented_cfg.add_edge(edges.Edge(true_predecessor, true_successor))

    return instrumented_cfg


def transform_type_of_call_sites(program: programs.Program,
                                 call_vertex: vertices.SubprogramVertex,
                                 cfg: graphs.ControlFlowGraph):
    vertex_to_call = {}
    for vertex in cfg:
        callee = program.call_graph.is_call_site(call_vertex, vertex)
        if callee:
            call_site = vertices.CallVertex(vertices.Vertex.get_vertex_id(), callee.name)
            vertex_to_call[vertex] = call_site

    for call_site in vertex_to_call.values():
        cfg.add_vertex(call_site)

    for vertex, call_site in vertex_to_call.items():
        for predecessor_edge in cfg.predecessors(vertex):
            predecessor = predecessor_edge.predecessor()
            if predecessor in vertex_to_call:
                predecessor = vertex_to_call[predecessor]
            cfg.add_edge(edges.Edge(predecessor, call_site))

        for successor_edge in cfg.successors(vertex):
            successor = successor_edge.successor()
            if successor in vertex_to_call:
                successor = vertex_to_call[successor]
            cfg.add_edge(edges.Edge(call_site, successor))

        cfg.remove_vertex(vertex)


def copy_callee(callee_cfg: graphs.ControlFlowGraph):
    vertex_to_copy = {}
    vertex_set = set()
    edge_set = set()
    for vertex in callee_cfg:
        if isinstance(vertex, vertices.CallVertex):
            cloned_vertex = vertices.CallVertex(vertices.Vertex.get_vertex_id(), vertex.callee)
        else:
            cloned_vertex = vertices.BasicBlock(vertices.Vertex.get_vertex_id())

        vertex_to_copy[vertex] = cloned_vertex

    the_entry = None
    the_exit = None
    for vertex in callee_cfg:
        cloned_vertex = vertex_to_copy[vertex]
        vertex_set.add(cloned_vertex)

        if vertex != callee_cfg.exit:
            for successor_edge in callee_cfg.successors(vertex):
                successor = successor_edge.successor()
                successor = vertex_to_copy[successor]
                edge_set.add(edges.Edge(cloned_vertex, successor))

        if vertex == callee_cfg.entry:
            the_entry = cloned_vertex

        if vertex == callee_cfg.exit:
            the_exit = cloned_vertex

    return vertex_set, edge_set, the_entry, the_exit


def simplify(program: programs.Program,
             call_vertex: vertices.SubprogramVertex,
             instrumented_cfgs: Dict):
    cfg: graphs.ControlFlowGraph = instrumented_cfgs[call_vertex.name]
    inline = {}
    for vertex in cfg:
        if isinstance(vertex, vertices.CallVertex):
            callee_cfg = instrumented_cfgs[vertex.callee]
            instrumentation_points = 0
            chained_calls = 0
            for callee_vertex in callee_cfg:
                if isinstance(callee_vertex, vertices.InstrumentationVertex):
                    instrumentation_points += 1
                elif isinstance(callee_vertex, vertices.CallVertex):
                    chained_calls += 1

            if instrumentation_points == 0:
                if chained_calls == 0:
                    inline[vertex] = None
                else:
                    inline[vertex] = callee_cfg

    for dead, callee_cfg in inline.items():
        if callee_cfg is None:
            for predecessor_edge in cfg.predecessors(dead):
                predecessor = predecessor_edge.predecessor()
                for successor_edge in cfg.successors(dead):
                    successor = successor_edge.successor()
                    if not cfg.has_edge(predecessor, successor):
                        cfg.add_edge(edges.Edge(predecessor, successor))
        else:
            vertex_set, edge_set, the_entry, the_exit = copy_callee(callee_cfg)
            for vertex in vertex_set:
                cfg.add_vertex(vertex)

            for edge in edge_set:
                cfg.add_edge(edge)

            for predecessor_edge in cfg.predecessors(dead):
                predecessor = predecessor_edge.predecessor()
                cfg.add_edge(edges.Edge(predecessor, the_entry))

            for successor_edge in cfg.successors(dead):
                successor = successor_edge.successor()
                cfg.add_edge(edges.Edge(the_exit, successor))

        cfg.remove_vertex(dead)


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


def create_ipg(program: programs.Program, instrumented_cfg: graphs.ControlFlowGraph):
    in_data = {}
    out_data = {}
    vertex_to_ipg = {}
    for vertex in instrumented_cfg:
        in_data[vertex] = set()
        out_data[vertex] = set()

        if isinstance(vertex, vertices.InstrumentationVertex):
            vertex_to_ipg[vertex] = vertex
        elif isinstance(vertex, vertices.CallVertex):
            vertex_to_ipg[vertex] = vertex

    changed = True
    dfs = graphs.DepthFirstSearch(instrumented_cfg, instrumented_cfg.entry)
    assert len(dfs.post_order()) == instrumented_cfg.number_of_vertices()
    while changed:
        changed = False

        for vertex in reversed(dfs.post_order()):
            size = len(in_data[vertex])

            for edge in instrumented_cfg.predecessors(vertex):
                in_data[vertex].update(out_data[edge.predecessor()])

            if vertex in vertex_to_ipg:
                out_data[vertex] = {vertex}
            else:
                out_data[vertex] = in_data[vertex]

            if size != len(in_data[vertex]):
                changed = True

    ipg = graphs.FlowGraph(program, instrumented_cfg.name)
    for vertex in vertex_to_ipg.values():
        ipg.add_vertex(vertex)

    ipg.entry = instrumented_cfg.entry
    ipg.exit = instrumented_cfg.exit

    for successor, predecessors in in_data.items():
        if successor in vertex_to_ipg:
            for predecessor in predecessors:
                if predecessor in vertex_to_ipg:
                    if predecessor != ipg.exit and successor != ipg.entry:
                        ipg.add_edge(edges.Edge(vertex_to_ipg[predecessor], vertex_to_ipg[successor]))
    return ipg


def create_lnt(ipg: graphs.FlowGraph) -> graphs.LoopNest:
    edges_to_restore = set()
    flat_forest = set()
    loop_nest = graphs.LoopNest(ipg.name)

    ipg.dotify('ipg')

    while not graphs.is_dag(ipg):
        killed_edges = set()
        sccs = graphs.StrongComponents(ipg)

        for scc in sccs.non_trivial():
            loop = loop_nest.create_loop()
            headers = set()
            for vertex in scc:
                flat_forest.add(vertex)
                loop_nest.add_to_body(vertex, loop)

                for edge in ipg.predecessors(vertex):
                    if edge.predecessor() not in scc:
                        loop_nest.add_to_headers(vertex, loop)
                        headers.add(vertex)

            for h in headers:
               print(h)

            for vertex in headers:
                for edge in ipg.predecessors(vertex):
                    killed_edges.add(edge)

        for edge in killed_edges:
            ipg.remove_edge(edge)
        edges_to_restore.update(killed_edges)

    for edge in edges_to_restore:
        ipg.add_edge(edge)

    outermost_loop = loop_nest.create_loop()
    for vertex in ipg:
        if vertex not in flat_forest:
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
                 call_table: Dict[int, str],
                 trace: List,
                 wcet_data: Dict[str, MeasuredData]):
    measured_times = set()
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

    return measured_times


def instrumentation_budget_pipeline(program: programs.Program,
                                    root_subprogram: programs.Subprogram,
                                    total_budget: int,
                                    randomise: bool,
                                    instrumented_cfgs: Dict):
    remaining_budget = total_budget
    budgets = {}
    minimum = 2

    if remaining_budget >= minimum * len(program):
        for subprogram in program:
            budgets[subprogram] = minimum
            remaining_budget -= minimum
    else:
        candidates = [subprogram for subprogram in program]
        root_index = candidates.index(root_subprogram)
        candidates[0], candidates[root_index] = candidates[root_index], candidates[0]
        while remaining_budget >= minimum and candidates:
            subprogram = candidates.pop(0)
            budgets[subprogram] = minimum
            remaining_budget -= minimum

        for subprogram in candidates:
            budgets[subprogram] = 0

    calls = {}
    for subprogram in program:
        calls[subprogram] = 0
        if budgets[subprogram]:
            for vertex in subprogram.cfg:
                if vertex != subprogram.cfg.entry and program.call_graph.is_call_site(subprogram.call_vertex, vertex):
                    calls[subprogram] += 1

    if remaining_budget >= sum(calls.values()):
        for subprogram in program:
            budgets[subprogram] += calls[subprogram]
            remaining_budget -= calls[subprogram]
    else:
        candidates = [subprogram for subprogram in program if calls[subprogram]]
        while remaining_budget > 0:
            if randomise:
                subprogram = choice(candidates)
            else:
                subprogram = candidates[0]

            upper_bound = min(remaining_budget, calls[subprogram])
            if randomise:
                budget = randint(1, upper_bound)
            else:
                budget = upper_bound

            budgets[subprogram] += budget
            calls[subprogram] -= budget
            remaining_budget -= budget

            if calls[subprogram] == 0:
                candidates.remove(subprogram)

    candidates = [subprogram for subprogram in program
                  if 0 < budgets[subprogram] < subprogram.cfg.number_of_vertices()]
    while remaining_budget > 0:
        if randomise:
            subprogram = choice(candidates)
        else:
            subprogram = candidates[0]

        max_budget = subprogram.cfg.number_of_vertices() - budgets[subprogram]
        upper_bound = min(remaining_budget, max_budget)
        if randomise:
            budget = randint(1, upper_bound)
        else:
            budget = upper_bound

        budgets[subprogram] += budget
        remaining_budget -= budget
        if budgets[subprogram] == subprogram.cfg.number_of_vertices():
            candidates.remove(subprogram)

    assert remaining_budget == 0, 'Remaining budget is {}'.format(remaining_budget)

    for call_vertex in program.call_graph:
        subprogram = program[call_vertex.name]
        budget = budgets[subprogram]
        vertex_to_ipg = {}
        if budget > 0:
            vertex_to_ipg = instrument_with_a_budget(program, call_vertex, subprogram.cfg, budget, randomise)
        instrumented_cfg = create_instrumented_cfg(program, subprogram.cfg, vertex_to_ipg)
        transform_type_of_call_sites(program, call_vertex, instrumented_cfg)
        instrumented_cfgs[subprogram.name] = instrumented_cfg


def instrumentation_policy_pipeline(program: programs.Program, policy: InstrumentationPolicy, instrumented_cfgs: Dict):
    for call_vertex in program.call_graph:
        subprogram = program[call_vertex.name]
        vertex_to_ipg = instrument_with_a_policy(program, call_vertex, subprogram.cfg, policy)
        instrumented_cfg = create_instrumented_cfg(program, subprogram.cfg, vertex_to_ipg)
        transform_type_of_call_sites(program, call_vertex, instrumented_cfg)
        instrumented_cfgs[subprogram.name] = instrumented_cfg


def do_hybrid_analysis_wcet_calculation(program: programs.Program,
                                        root_vertex: vertices.SubprogramVertex,
                                        dfs: graphs.DepthFirstSearch,
                                        wcet_data: Dict) -> int:
    wcets = {}
    for call_vertex in dfs.post_order():
        subprogram = program[call_vertex.name]
        if subprogram.ipg:
            vertex_times = {}
            for vertex in subprogram.ipg:
                if isinstance(vertex, vertices.CallVertex):
                    vertex_times[vertex] = wcets[vertex.callee]
                else:
                    vertex_times[vertex] = 0

            subprogram_data = wcet_data[call_vertex.name]
            wcet = statically_analyse_ipg(subprogram.ipg, subprogram.lnt, subprogram_data, vertex_times)
            wcets[call_vertex.name] = wcet
        else:
            wcets[call_vertex.name] = 0

    return wcets[root_vertex.name]


def calculate_coverage_and_instrumentation_stats(program: programs.Program, wcet_data: Dict):
    instrumentation_points = 0
    total_transitions = 0
    uncovered_transitions = 0
    for subprogram in program:
        if subprogram.ipg:
            for vertex in subprogram.ipg:
                if isinstance(vertex, vertices.InstrumentationVertex):
                    instrumentation_points += 1

            subprogram_data = wcet_data[subprogram.name]
            for edge, time in subprogram_data.times.items():
                total_transitions += 1
                if time == 0:
                    uncovered_transitions += 1

    coverage = 100 * (total_transitions - uncovered_transitions) // total_transitions
    return coverage, instrumentation_points


def hybrid_analysis(program: programs.Program,
                    root_vertex: vertices.SubprogramVertex,
                    dfs: graphs.DepthFirstSearch,
                    traces_filename: str,
                    policy: InstrumentationPolicy,
                    total_budget: int,
                    randomise: bool):
    verbose_message('Creating instrumented CFGs')
    instrumented_cfgs = {}
    if policy == InstrumentationPolicy.none:
        root_subprogram = program[root_vertex.name]
        instrumentation_budget_pipeline(program, root_subprogram, total_budget, randomise, instrumented_cfgs)
    else:
        instrumentation_policy_pipeline(program, policy, instrumented_cfgs)

    labels = {vertices.InstrumentationVertex.ghost_value()}
    call_table = {}
    wcet_data = {}
    verbose_message('Creating IPGs')
    for call_vertex in dfs.post_order():
        subprogram = program[call_vertex.name]
        simplify(program, call_vertex, instrumented_cfgs)
        instrumented_cfg = instrumented_cfgs[subprogram.name]

        instrumentation_points = len([vertex for vertex in instrumented_cfg
                                      if isinstance(vertex, vertices.InstrumentationVertex)])

        if instrumentation_points > 0:
            subprogram.ipg = create_ipg(program, instrumented_cfg)
            determinise(subprogram.ipg)
            subprogram.lnt = create_lnt(subprogram.ipg)
            wcet_data[subprogram.name] = MeasuredData(subprogram)
            for vertex in subprogram.ipg:
                if isinstance(vertex, vertices.InstrumentationVertex):
                    labels.add(vertex.label)

            assert isinstance(subprogram.ipg.entry, vertices.InstrumentationVertex)
            call_table[subprogram.ipg.entry.label] = subprogram.name

    trace = filter_traces(labels, traces_filename)
    measured_times = parse_traces(program, root_vertex, call_table, trace, wcet_data)
    print('Dynamic WCET estimate: {}'.format(max(measured_times)))

    wcet = do_hybrid_analysis_wcet_calculation(program, root_vertex, dfs, wcet_data)
    print('Hybrid WCET estimate: {}'.format(wcet))

    coverage, instrumentation_points = calculate_coverage_and_instrumentation_stats(program, wcet_data)
    print('{}% transition coverage achieved'.format(coverage))
    print('{} instrumentation points'.format(instrumentation_points))


def main(args: Namespace):
    program = programs.IO.read(args.program)
    program.call_graph.dotify()
    root = program.call_graph.get_root()

    calls = 0
    for subprogram in program:
        graph_based_calculations.identify_loops(subprogram.cfg)
        for vertex in subprogram.cfg:
            if program.call_graph.is_call_site(subprogram.call_vertex, vertex):
                calls += 1

    print('#Subprograms: {}'.format(len(program)))
    print('#Calls: {}'.format(calls))

    if args.budget:
        if args.budget < 2:
            error_message('The minimum number of allowed instrumentation points is 2.')

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
    hybrid_analysis(program, root, dfs, args.traces, args.policy, args.budget, args.randomise)


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

    parser.add_argument('--randomise',
                        action='store_true',
                        help='where a choice exists, pick randomly',
                        default=False)

    return parser.parse_args()


if __name__ == '__main__':
    args = parse_the_command_line()
    check_arguments(args)
    main(args)

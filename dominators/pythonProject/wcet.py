import argparse
import collections
import dataclasses
import logging
import re
import sys
import threading
import typing

import ortools.linear_solver.pywraplp as pywraplp
import rich.logging
import sympy

from graph import edges
from graph import graph_input
from graph import graphs
from graph import vertices
from utils import command_line
from utils import wcet_data

logging.basicConfig(
    level=logging.DEBUG,
    format="%(levelname)s:%(asctime)s:%(message)s",
    datefmt="%H:%M:%S",
    handlers=[rich.logging.RichHandler()]
)
logger = logging.getLogger(__name__)


def assign_costs_to_ipg(
        ipg: graphs.InstrumentationPointGraph,
        cfg_constraints: wcet_data.ContextData
) -> tuple[dict[vertices.Vertex, int], dict[edges.Edge, int]]:
    vertex_costs = dict()
    for vertex in ipg.its_vertices:
        vertex_costs[vertex] = 0

    edge_costs = dict()
    for ipg_edge in ipg.its_edges:
        cost = 0
        for vertex in ipg.path_expressions[ipg_edge]:
            if vertex not in [ipg_edge.point_a, ipg_edge.point_b]:
                cost += cfg_constraints.wcets[vertex]
        edge_costs[ipg_edge] = cost

    return vertex_costs, edge_costs


def calculate_using_ipg(cfg: graphs.ControlFlowGraph, cfg_constraints: wcet_data.ContextData) -> int:
    profile = graphs.create_minimal_path_reconstruction_profile(cfg)
    augmented = graphs.AugmentedControlFlowGraph(cfg, profile)
    forest = graphs.build_loop_forest(augmented, graphs.GraphDirection.Forwards)
    ipg = graphs.InstrumentationPointGraph(augmented)

    loop_iteration_edges: dict[vertices.Vertex, set[edges.Edge]] = dict()
    loop_entry_edges: dict[vertices.Vertex, set[edges.Edge]] = dict()
    headers = forest.headers()
    for header in headers:
        loop_entry_edges[header] = set()
        loop_iteration_edges[header] = set()

    cfg_edge_to_ipg_edges = dict()
    for edge in augmented.its_edges:
        cfg_edge_to_ipg_edges[edge] = set()

    for ipg_edge in ipg.its_edges:
        list_one = ipg.path_expressions[ipg_edge][:-1]
        list_two = ipg.path_expressions[ipg_edge][1:]
        for predecessor, successor in zip(list_one, list_two):
            cfg_edge = edges.Edge(predecessor, successor)
            cfg_edge_to_ipg_edges[cfg_edge].add(ipg_edge)

        for vertex in ipg.path_expressions[ipg_edge]:
            if vertex in headers:
                loop_iteration_edges[vertex].add(ipg_edge)

    loop: vertices.LoopVertex
    for loop in forest.its_vertices:
        if loop != forest.root:
            (header,) = loop.entries
            for cfg_edge in augmented.predecessors[header]:
                predecessor_loop = forest.vertex_to_loop[cfg_edge.point_a]
                if not forest.is_ancestor(loop, predecessor_loop):
                    loop_entry_edges[header].update(cfg_edge_to_ipg_edges[cfg_edge])

    linear_program = LinearProgram(ipg)
    for loop in forest.its_vertices:
        if loop == forest.root:
            linear_program.solver.Add(linear_program.vertex_variables[ipg.entry_vertex] == 1)
        else:
            (header,) = loop.entries
            ancestor_loop: vertices.LoopVertex = loop
            level = 1
            while forest.predecessors[ancestor_loop]:
                expr = cfg_constraints.get_bound_expr(header, level)

                if expr:
                    evaluated_expr = expr.subs(cfg_constraints.symbol_settings)
                    if not evaluated_expr.free_symbols:
                        (ancestor_header,) = ancestor_loop.entries
                        incoming_edges = set()
                        for edge in cfg.predecessors[ancestor_header]:
                            other_loop = forest.vertex_to_loop[edge.point_a]
                            if not forest.is_ancestor(ancestor_loop, other_loop):
                                incoming_edges.add(edge)

                        terms = [linear_program.edge_variables[edge] for edge in loop_iteration_edges[header]]
                        lhs = linear_program.solver.Sum(terms)
                        terms = [linear_program.edge_variables[edge] * evaluated_expr for edge in
                                 loop_entry_edges[ancestor_header]]
                        rhs = linear_program.solver.Sum(terms)
                        linear_program.solver.Add(lhs <= rhs)

                (ancestor_edge,) = forest.predecessors[ancestor_loop]
                ancestor_loop = ancestor_edge.point_a
                level += 1

    vertex_costs, edge_costs = assign_costs_to_ipg(ipg, cfg_constraints)
    linear_program.add_objective(vertex_costs, edge_costs)
    linear_program.solve()
    print(f'{cfg.name} [IPG]: WCET estimate = {linear_program.wcet:,}')
    return linear_program.wcet


CostMapping = dict[vertices.Vertex | edges.Edge, sympy.Expr]
BoundMapping = dict[tuple[vertices.Vertex, int], sympy.Expr]


class LinearProgram:
    def __init__(
            self,
            cfg: graphs.ControlFlowGraph,
            forest: graphs.LoopForest,
            cost_function: CostMapping,
            bound_function: BoundMapping
    ):
        self._vertex_variables = dict()
        self._edge_variables = dict()
        self._solver = pywraplp.Solver.CreateSolver('GLOP')
        self._add_variables(cfg)
        self._add_flow_constraints(cfg)
        self._add_relative_capacity_constraints(cfg, forest, bound_function)
        self._add_objective(cfg, cost_function)

    def _add_variables(self, cfg: graphs.ControlFlowGraph):
        for vertex in cfg.its_vertices:
            name = vertex.identifier
            variable = self._solver.NumVar(0, self._solver.infinity(), name)
            self._vertex_variables[vertex] = variable

        for edge in cfg.its_edges:
            name = f'e_{edge.point_a.identifier}_{edge.point_b.identifier}'
            variable = self._solver.NumVar(0, self._solver.infinity(), name)
            self._edge_variables[edge] = variable

    def _add_flow_constraints(self, cfg: graphs.ControlFlowGraph):
        for vertex in cfg.its_vertices:
            incoming = []
            for edge in cfg.predecessors[vertex]:
                incoming.append(self._edge_variables[edge])

            outgoing = []
            for edge in cfg.successors[vertex]:
                outgoing.append(self._edge_variables[edge])

            if not cfg.predecessors[vertex]:
                self._solver.Add(self._vertex_variables[vertex] == self._solver.Sum(outgoing))
            elif not cfg.successors[vertex]:
                self._solver.Add(self._vertex_variables[vertex] == self._solver.Sum(incoming))
            else:
                self._solver.Add(self._vertex_variables[vertex] == self._solver.Sum(outgoing))
                self._solver.Add(self._solver.Sum(incoming) == self._solver.Sum(outgoing))

    def _add_relative_capacity_constraints(
            self, cfg: graphs.ControlFlowGraph,
            forest: graphs.LoopForest,
            bound_function: BoundMapping
    ):
        loop: vertices.LoopVertex
        for loop in forest.its_vertices:
            if loop == forest.root:
                self._solver.Add(self._vertex_variables[cfg.entry_vertex] == 1)
            else:
                (header,) = loop.entries
                ancestor_loop: vertices.LoopVertex = loop
                level = 1
                while forest.predecessors[ancestor_loop]:
                    bound_key = (header, level)
                    if bound_key in bound_function:
                        (ancestor_header,) = ancestor_loop.entries
                        incoming_edges = set()
                        for edge in cfg.predecessors[ancestor_header]:
                            other_loop = forest.vertex_to_loop[edge.point_a]
                            if not forest.is_ancestor(ancestor_loop, other_loop):
                                incoming_edges.add(edge)

                        lhs = self._vertex_variables[header]
                        terms = [self._edge_variables[edge] * bound_function[bound_key] for edge in incoming_edges]
                        rhs = self._solver.Sum(terms)
                        self._solver.Add(lhs <= rhs)

                    (ancestor_edge,) = forest.predecessors[ancestor_loop]
                    ancestor_loop = ancestor_edge.point_a
                    level += 1

    def _add_objective(self, cfg: graphs.ControlFlowGraph, cost_function: CostMapping):
        vertices_expr = [self._vertex_variables[vertex] * cost_function[vertex] for vertex in cfg.its_vertices]
        edges_expr = [self._edge_variables[edge] * cost_function[edge] for edge in cfg.its_edges]
        objective = self._solver.Sum(vertices_expr + edges_expr)
        self._solver.Maximize(objective)

    def output_solution(self):
        for variable in self._solver.variables():
            value = round(variable.solution_value())
            if value > 0:
                print(f'{variable.name()} = {value}')

    def solve(self) -> typing.Optional[int]:
        # self._solver.EnableOutput()
        # print(self.solver.ExportModelAsLpFormat(False).replace('\\', '').replace(',_', ','), sep='\n')
        status = self._solver.Solve()
        if status == pywraplp.Solver.OPTIMAL:
            return round(self._solver.Objective().Value())
        elif status == pywraplp.Solver.FEASIBLE:
            print('Feasible solution found')
        else:
            print('The ILP does not have an optimal solution')
            assert False


class Network:
    def __init__(
            self,
            qualifier: wcet_data.ContextQualifier,
            cfg: graphs.ControlFlowGraph,
            data_model: wcet_data.TimingDataModel,
            cost_function: CostMapping
    ):
        self.qualifier = qualifier
        self.cfg = cfg
        self.data_model = data_model
        self.cost_function = cost_function
        self.wcet = None

    def _is_t2_reducible(self, vertex: vertices.Vertex) -> bool:
        return len(self.cfg.predecessors[vertex]) == 1 and not self.cost_function[vertex].free_symbols

    def _do_t2_reduction(self, slave: vertices.Vertex) -> set[vertices.Vertex]:
        changed = set()
        if len(self.cfg.its_edges) == 1:
            (edge,) = self.cfg.its_edges
            if slave == edge.point_a:
                master = edge.point_b
            else:
                master = edge.point_a
            self.cost_function[master] += self.cost_function[edge] + self.cost_function[slave]
            self.cfg.remove_vertex(slave)
        else:
            reroutes = {}
            (edge_master_slave,) = self.cfg.predecessors[slave]

            if self.cfg.successors[slave]:
                for other_edge in self.cfg.successors[slave]:
                    if len(self.cfg.predecessors[other_edge.point_b]) > 1:
                        changed.add(other_edge.point_b)

                    edge_b_c = edges.Edge(slave, other_edge.point_b)
                    edge_a_c = edges.Edge(edge_master_slave.point_a, other_edge.point_b)
                    reroutes[edge_a_c] = (self.cost_function[edge_master_slave] +
                                          self.cost_function[edge_b_c] +
                                          self.cost_function[slave])

                for edge, cost in reroutes.items():
                    if edge not in self.cost_function:
                        self.cfg.add_edge(edge)
                        self.cost_function[edge] = cost
                    else:
                        self.cost_function[edge] = max(self.cost_function[edge], cost)
            else:
                self.cost_function[edge_master_slave.point_a] += (
                        self.cost_function[edge_master_slave] +
                        self.cost_function[slave]
                )

            self.cfg.remove_vertex(slave)

        return changed

    def _attempt_t1_reduction(self, loop: vertices.LoopVertex):
        context_data = self.data_model.get_context_data(self.qualifier)
        (header,) = loop.entries
        loop_bounds = self.data_model.get_bounds(self.qualifier, header)
        self_edge = edges.Edge(header, header)
        if (
                not self.cost_function[header].free_symbols and
                len(loop_bounds) == 1 and
                self_edge in self.cost_function
        ):
            expr = self.data_model.get_bound(self.qualifier, header, 1)
            evaluated_expr = expr.subs(context_data.parameters)
            if not evaluated_expr.free_symbols:
                loop_cost = self.cost_function[self_edge] + self.cost_function[header]
                self.cost_function[header] += loop_cost * (int(evaluated_expr) - 1)
                self.cfg.remove_edge(self_edge)

    def _reduce(self, forest: graphs.LoopForest):
        logger.debug(f"Reducing {self.cfg.name}")
        loop_ordering = list(reversed(graphs.jail_and_free(forest)))
        loop: vertices.LoopVertex
        for loop in loop_ordering:
            t2_reductions: collections.deque[vertices.Vertex] = collections.deque()
            for vertex in loop.body:
                if self._is_t2_reducible(vertex):
                    t2_reductions.append(vertex)

            for edge in forest.successors[loop]:
                (other_header,) = edge.point_b.entries
                if self._is_t2_reducible(other_header):
                    t2_reductions.append(other_header)

            while t2_reductions:
                slave = t2_reductions.popleft()
                changed = self._do_t2_reduction(slave)

                for vertex in changed:
                    if self._is_t2_reducible(vertex):
                        other_loop = forest.vertex_to_loop[vertex]
                        if other_loop == loop or forest.is_parent(loop, other_loop):
                            t2_reductions.append(vertex)

            if len(loop.entries) == 1:
                self._attempt_t1_reduction(loop)

    def evaluate(self):
        forest = graphs.build_loop_forest(self.cfg, graphs.GraphDirection.Forwards)
        self._reduce(forest)
        if len(self.cfg.its_vertices) == 1:
            (remaining_vertex,) = self.cfg.its_vertices
            self.wcet = self.cost_function[remaining_vertex]
        else:
            free_expressions = False
            for vertex in self.cfg.its_vertices:
                if self.cost_function[vertex].free_symbols:
                    free_expressions = True

            bound_function = BoundMapping()
            context_data = self.data_model.get_context_data(self.qualifier)
            for header in forest.headers():
                bounds = self.data_model.get_bounds(self.qualifier, header)
                for bound in bounds:
                    evaluated_expr = sympy.sympify(bound.expr).subs(context_data.parameters)
                    if evaluated_expr.free_symbols:
                        free_expressions = True
                    else:
                        bound_function[(header, bound.level)] = evaluated_expr

            if not free_expressions:
                linear_program = LinearProgram(self.cfg, forest, self.cost_function, bound_function)
                self.wcet = linear_program.solve()


@dataclasses.dataclass(slots=True)
class Networks:
    cfg: graphs.ControlFlowGraph
    reduced: Network = None
    free: list[Network] = dataclasses.field(default_factory=list)

    def remember_network(self, network: Network):
        if network.wcet is None:
            self.free.append(network)
        else:
            if self.reduced is None:
                self.reduced = network
            elif network.wcet > self.reduced.wcet:
                self.reduced = network

    def copy(self):
        return Networks(cfg=self.cfg, reduced=self.reduced, free=list(self.free))


def output_status(networks: Networks, qualifier: wcet_data.ContextQualifier):
    if networks.free:
        logger.debug(f"WCET({qualifier}) is not yet known")
    else:
        logger.debug(f"WCET({qualifier}) = {networks.reduced.wcet}")


def get_call_site_variable_placeholder(vertex: vertices.Vertex) -> str:
    return f'_{vertex.identifier}'


def create_cost_function(
        cfg: graphs.ControlFlowGraph,
        call_graph: graphs.CallGraph,
        context_data: wcet_data.ContextData,
        callee_costs: CostMapping,
) -> CostMapping:
    call_vertex = call_graph.get_vertex(cfg.name)
    call_sites = call_graph.get_call_sites(call_vertex)

    cost_function = CostMapping()

    for vertex in cfg.its_vertices:
        basic_cost = context_data.wcets[vertex.identifier]
        if vertex in call_sites:
            if vertex in callee_costs:
                total_cost = basic_cost + callee_costs[vertex]
                cost_function[vertex] = sympy.sympify(f"{total_cost}")
            else:
                cost_function[vertex] = sympy.sympify(f"{basic_cost} + {get_call_site_variable_placeholder(vertex)}")
        else:
            cost_function[vertex] = sympy.sympify(f"{basic_cost}")

    for edge in cfg.its_edges:
        cost_function[edge] = sympy.sympify("0")

    return cost_function


def create_networks(
        qualifier: wcet_data.ContextQualifier,
        cfg: graphs.ControlFlowGraph,
        call_graph: graphs.CallGraph,
        data_model: wcet_data.TimingDataModel,
        callee_costs: CostMapping
) -> Networks:
    networks = Networks(cfg)
    context_data = data_model.get_context_data(qualifier)
    for identifier, blacklist in context_data.exclusive.items():
        vertex = cfg.get_vertex(identifier)
        dead = {cfg.get_vertex(other_identifier) for other_identifier in blacklist}
        subgraph = graphs.create_control_flow_subgraph(cfg, vertex)
        subgraph.remove_dead_vertices(dead)
        cost_function = create_cost_function(cfg, call_graph, context_data, callee_costs)
        network = Network(qualifier, subgraph, data_model, cost_function)
        network.evaluate()
        networks.remember_network(network)

    master_cfg = cfg.copy(f'{cfg.name}')
    dead = {
        cfg.get_vertex(identifier) for identifier in context_data.exclusive.keys() if context_data.exclusive[identifier]
    }
    if dead:
        master_cfg.remove_dead_vertices(dead)

    cost_function = create_cost_function(master_cfg, call_graph, context_data, callee_costs)
    network = Network(qualifier, master_cfg, data_model, cost_function)
    network.evaluate()
    networks.remember_network(network)

    return networks


@dataclasses.dataclass(slots=True, frozen=True)
class Program:
    cfgs: dict[str, graphs.ControlFlowGraph]
    call_graph: graphs.CallGraph
    data_model: wcet_data.TimingDataModel


def request_parameter_settings(
        cfg: graphs.ControlFlowGraph,
        static_data: wcet_data.StaticData,
        context_data: wcet_data.ContextData
):
    for parameter in static_data.get_free_parameters():
        while parameter not in context_data.parameters:
            try:
                given = input(f"Give a positive integer for the program variable '{parameter}' in '{cfg.name}': ")
                number = int(given)
                if number >= 1:
                    context_data.parameters[parameter] = number
                else:
                    print("The number must be an integer at least 1.")
            except ValueError:
                print("Enter a valid integer.")


def analyse_partially_evaluated_context(
        program: Program,
        qualifier: wcet_data.ContextQualifier,
        call_vertex: vertices.Vertex,
        context_to_networks: dict[wcet_data.ContextQualifier, Networks]
):
    cfg = program.cfgs[call_vertex.identifier]
    static_data = program.data_model.get_static_data(cfg)
    context_data = program.data_model.get_context_data(qualifier)

    if static_data is not None:
        request_parameter_settings(cfg, static_data, context_data)

    networks = context_to_networks[qualifier].copy()
    while networks.free:
        network = networks.free.pop()
        cfg_copy = network.cfg.copy()
        cost_function_copy = network.cost_function.copy()

        call_edge: edges.CallEdge
        for call_edge in program.call_graph.successors[call_vertex]:
            if cost_function_copy[call_edge.site].free_symbols:
                context_qualifier = wcet_data.ContextQualifier.from_call_edge(call_edge)
                if context_qualifier in context_to_networks:
                    callee_networks = context_to_networks[context_qualifier]
                else:
                    no_context_qualifier = wcet_data.ContextQualifier.from_call_vertex(call_edge.point_b)
                    callee_networks = context_to_networks[no_context_qualifier]

                free_expr = cost_function_copy[call_edge.site]
                placeholder = get_call_site_variable_placeholder(call_edge.site)
                cost_function_copy[call_edge.site] = free_expr.subs({placeholder: callee_networks.reduced.wcet})

        network_copy = Network(qualifier, cfg_copy, program.data_model, cost_function_copy)
        network_copy.evaluate()
        networks.remember_network(network_copy)

    output_status(networks, qualifier)
    context_to_networks[qualifier] = networks


def request_missing_information(program: Program, context_to_networks: dict[wcet_data.ContextQualifier, Networks]):
    ordering = graphs.jail_and_free(program.call_graph)
    for call_vertex in reversed(ordering):
        no_context_qualifier = wcet_data.ContextQualifier.from_call_vertex(call_vertex)
        if no_context_qualifier in program.data_model.contexts:
            networks = context_to_networks[no_context_qualifier]
            if networks.free:
                analyse_partially_evaluated_context(
                    program,
                    no_context_qualifier,
                    call_vertex,
                    context_to_networks
                )

        call_edge: edges.CallEdge
        for call_edge in program.call_graph.predecessors[call_vertex]:
            context_qualifier = wcet_data.ContextQualifier.from_call_edge(call_edge)
            if context_qualifier in program.data_model.contexts:
                networks = context_to_networks[context_qualifier]
                if networks.free:
                    analyse_partially_evaluated_context(
                        program,
                        context_qualifier,
                        call_vertex,
                        context_to_networks
                    )


def ask_if_the_user_wants_to_provide_missing_information(
        program: Program,
        context_to_networks: dict[wcet_data.ContextQualifier, Networks]
):
    yes_regex = re.compile(r'^(y(es)?|)$', re.IGNORECASE)
    no_regex = re.compile(r'^(n(o)?)$', re.IGNORECASE)

    while True:
        given = input("Do you want to provide additional settings to evaluate the WCET? "
                      "(Yes/Y, No/N, or press Enter for Yes): ")

        if yes_regex.match(given):
            original_parameter_settings = {
                qualifier: context_data.parameters.copy() for qualifier, context_data in
                program.data_model.contexts.items()
            }

            request_missing_information(program, context_to_networks.copy())

            for qualifier, context_data in program.data_model.contexts.items():
                context_data.parameters = original_parameter_settings[qualifier]

        elif no_regex.match(given):
            break
        else:
            print("Enter a valid option")


def collect_callee_costs(
        call_graph: graphs.CallGraph,
        call_vertex: vertices.Vertex,
        context_to_networks: dict[wcet_data.ContextQualifier, Networks],
) -> CostMapping:
    # Update the WCETs of call sites with context-specific WCETs, if possible, or just the WCET of the subprogram,
    # if there is no context-specific information provided. If there are free program variables in all contexts,
    # then the call site remains locked until the user binds the free program variables to values.
    callee_costs: CostMapping = CostMapping()
    call_edge: edges.CallEdge
    for call_edge in call_graph.successors[call_vertex]:
        edge_qualifier = wcet_data.ContextQualifier.from_call_edge(call_edge)
        if edge_qualifier in context_to_networks:
            callee_networks = context_to_networks[edge_qualifier]
            if not callee_networks.free:
                callee_costs[call_edge.site] = callee_networks.reduced.wcet

        if call_edge.site not in callee_costs:
            callee_qualifier = wcet_data.ContextQualifier.from_call_vertex(call_edge.point_b)
            if callee_qualifier in context_to_networks:
                callee_networks = context_to_networks[callee_qualifier]
                if not callee_networks.free:
                    callee_costs[call_edge.site] = callee_networks.reduced.wcet

    return callee_costs


def do_analysis(program: Program):
    context_to_networks: dict[wcet_data.ContextQualifier, Networks] = {}
    ordering = graphs.jail_and_free(program.call_graph)
    for call_vertex in reversed(ordering):
        callee_costs = collect_callee_costs(program.call_graph, call_vertex, context_to_networks)
        cfg = program.cfgs[call_vertex.identifier]

        caller_qualifier = wcet_data.ContextQualifier.from_call_vertex(call_vertex)
        if caller_qualifier in program.data_model.contexts:
            logger.debug(f"Analysing {cfg.name} in context {caller_qualifier}")
            networks = create_networks(caller_qualifier, cfg, program.call_graph, program.data_model, callee_costs)
            context_to_networks[caller_qualifier] = networks
            output_status(networks, caller_qualifier)

        call_edge: edges.CallEdge
        for call_edge in program.call_graph.predecessors[call_vertex]:
            edge_qualifier = wcet_data.ContextQualifier.from_call_edge(call_edge)
            if edge_qualifier in program.data_model.contexts:
                logger.debug(f"Analysing {cfg.name} in context {edge_qualifier}")
                networks = create_networks(edge_qualifier, cfg, program.call_graph, program.data_model, callee_costs)
                context_to_networks[edge_qualifier] = networks
                output_status(networks, edge_qualifier)

    root_qualifier = wcet_data.ContextQualifier.from_call_vertex(program.call_graph.root_vertex)
    root_networks = context_to_networks[root_qualifier]
    if root_networks.free:
        ask_if_the_user_wants_to_provide_missing_information(program, context_to_networks)


def parse_the_command_line() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description='Do WCET analysis')

    command_line.add_cfg_options(parser, True)

    parser.add_argument('-D',
                        '--data',
                        help='read the WCET data from this JSON file',
                        metavar='<FILE>.json',
                        required=True)

    parser.add_argument('-C',
                        '--calls',
                        help='read the call graph from this JSON file',
                        metavar='<FILE>.json',
                        required=True)

    return parser.parse_args()


def main():
    threading.stack_size(2 ** 26)
    sys.setrecursionlimit(2 ** 30)

    args = parse_the_command_line()
    cfgs = graph_input.read_cfgs(args.program, args.cfgs)
    call_graph = graph_input.read_call_graph(args.calls, cfgs)
    call_graph.dotify('calls')
    data_model = wcet_data.load_timing_analysis_data(args.data, cfgs)
    program = Program(cfgs, call_graph, data_model)
    do_analysis(program)


if __name__ == '__main__':
    main()

from argparse import ArgumentParser
from concurrent.futures import ProcessPoolExecutor, as_completed
from graphs import edges, graphs, vertices
from miscellaneous.helpful import error_message
from numpy import percentile
from random import shuffle
from sys import setrecursionlimit
from system import programs
from threading import stack_size
from time import sleep, time
from typing import Dict, List, Set, Tuple


def loop_body(cfg: graphs.ControlFlowGraph,
              header: vertices.Vertex,
              vertex: vertices.Vertex,
              containment: Dict[vertices.Vertex, vertices.Vertex],
              visited: Dict[vertices.Vertex, bool]):
    visited[vertex] = True
    if vertex != header:
        for edge in cfg.predecessors(vertex):
            where_next = containment[edge.predecessor()]
            if where_next not in visited:
                loop_body(cfg, header, where_next, containment, visited)


def compute_visit_times(tree: graphs.Tree, vertex: vertices.Vertex, start, finish, clock: int):
    start[vertex] = clock
    clock += 1

    for edge in tree.successors(vertex):
        clock = compute_visit_times(tree, edge.successor(), start, finish, clock)

    finish[vertex] = clock
    clock += 1
    return clock


def is_ancestor(left: vertices.Vertex, right: vertices.Vertex, start, finish):
    return start[left] <= start[right] and finish[right] <= finish[left]


def ramalingam_loops(cfg: graphs.ControlFlowGraph):
    containment = {}
    for vertex in cfg:
        containment[vertex] = vertex

    tarjan_tree = graphs.LengauerTarjan(cfg, cfg.entry)
    start = {}
    finish = {}
    clock = 0
    compute_visit_times(tarjan_tree, tarjan_tree.root, start, finish, clock)

    loop_nest = graphs.LoopNest()
    dfs = graphs.DepthFirstSearch(cfg, cfg.entry)
    for vertex in reversed(dfs.pre_order()):
        back_edges = dfs.back_edges(vertex)
        if back_edges:
            loop_id = loop_nest.create_loop()
            loop_nest.add_to_headers(vertex, loop_id)
            loop_nest.add_to_body(vertex, loop_id)
            visited = {}
            for edge in back_edges:
                if is_ancestor(edge.successor(), edge.predecessor(), start, finish):
                    loop_body(cfg, vertex, edge.predecessor(), containment, visited)

            for other in visited:
                if not loop_nest.is_header(other):
                    loop_nest.add_to_body(vertex, loop_id)
                containment[other] = vertex

    return loop_nest


def betts_loops(cfg: graphs.ControlFlowGraph):
    alive = {}
    for vertex in cfg:
        alive[vertex] = True
        for edge in cfg.successors(vertex):
            alive[edge] = True

    unexplored = cfg.number_of_vertices()
    loop_nest = graphs.LoopNest()
    while unexplored > 0:
        sccs = graphs.StrongComponents(cfg, alive)

        for scc in sccs.non_trivial():
            loop = loop_nest.create_loop()
            headers = set()
            for vertex in scc:
                loop_nest.add_to_body(vertex, loop)

                for edge in cfg.predecessors(vertex):
                    if edge.predecessor() not in scc:
                        loop_nest.add_to_headers(vertex, loop)
                        headers.add(vertex)

                for edge in cfg.successors(vertex):
                    if edge.successor() not in scc:
                        alive[edge] = False

            for vertex in headers:
                for edge in cfg.predecessors(vertex):
                    alive[edge] = False

        for vertex in sccs.singletons:
            alive[vertex] = False
            unexplored -= 1

    return loop_nest


def verify(cfg: graphs.ControlFlowGraph, betts_tree):
    tarjan_tree = graphs.LengauerTarjan(cfg, cfg.entry)
    differences = set()
    for vertex in cfg:
        if vertex != cfg.entry:
            (betts,) = betts_tree.predecessors(vertex)
            (tarjan,) = tarjan_tree.predecessors(vertex)
            if betts.predecessor() != tarjan.predecessor():
                print('{}: betts({}) = {}  tarjan({}) = {}'.format(cfg.name,
                                                                   vertex,
                                                                   betts.predecessor(),
                                                                   vertex,
                                                                   tarjan.predecessor()))
                differences.add((vertex, betts.predecessor(), tarjan.predecessor()))

    if differences:
        error_message('Verification failed')
    else:
        print('Verified')


class GraphRow:
    __slots__ = ['id_', 'vertices', 'successors', 'predecessors']

    def __init__(self, id_: int, vertices: Set[vertices.Vertex]):
        self.id_ = id_
        self.vertices = vertices
        self.successors = set()
        self.predecessors = set()

    def __str__(self):
        return ('ID={}\nV={}\nP={}\nS={}'.format(self.id_,
                                                 ','.join(str(x) for x in self.vertices),
                                                 ','.join(str(x) for x in self.predecessors),
                                                 ','.join(str(x) for x in self.successors)))


def compute_sccs(forest: Dict[vertices.Vertex, GraphRow], entry_row_id: int):
    pre_order = {}
    low_link = {}
    on_stack = {}
    stack = []
    unexplored = 0
    pre_id = 0
    non_trivial_sccs = set()

    def explore(row_id: int):
        nonlocal pre_id, unexplored, non_trivial_sccs
        pre_id += 1
        pre_order[row_id] = pre_id
        low_link[row_id] = pre_id
        on_stack[row_id] = True
        stack.append(row_id)

        row = forest[row_id]
        for predecessor_id in row.predecessors:
            if predecessor_id not in pre_order:
                explore(predecessor_id)
                low_link[row_id] = min(low_link[row_id], low_link[predecessor_id])
            elif predecessor_id in on_stack:
                low_link[row_id] = min(low_link[row_id], pre_order[predecessor_id])

        if low_link[row_id] == pre_order[row_id]:
            scc = set()
            done = False
            while not done:
                z = stack.pop()
                del on_stack[z]
                scc.add(z)
                done = z == row_id

            if len(scc) > 1:
                non_trivial_sccs.add(frozenset(scc))

    entry_row = forest[entry_row_id]
    starting_points = set()
    for successor_id in entry_row.successors:
        successor_row = forest[successor_id]
        starting_points.update(successor_row.predecessors)

    for row_id in starting_points:
        if row_id not in pre_order:
            explore(row_id)

    return non_trivial_sccs


def see_forest(forest: Dict[vertices.Vertex, GraphRow]):
    for row in forest.values():
        print('>' * 10)
        print(row)
    print()


def preliminary_search(cfg: graphs.ControlFlowGraph,
                       entry: vertices.Vertex,
                       entry_row_id: int,
                       tree: graphs.Tree,
                       data: Dict[vertices.Vertex, List[vertices.Vertex]],
                       forest: Dict[vertices.Vertex, GraphRow],
                       tentacles: Dict[Tuple[vertices.Vertex, vertices.Vertex], List[vertices.Vertex]]):
    vertex_to_row = {}
    next_row_id = entry_row_id
    tree.add_vertex(entry)
    forest[entry_row_id] = GraphRow(next_row_id, {entry})
    vertex_to_row[entry] = entry_row_id

    if entry == cfg.entry:
        forward_transitions = graphs.DirectedGraph.successors
        forward_transition = edges.Edge.successor
        backward_transitions = graphs.DirectedGraph.predecessors
        backward_transition = edges.Edge.predecessor
    else:
        forward_transitions = graphs.DirectedGraph.predecessors
        forward_transition = edges.Edge.predecessor
        backward_transitions = graphs.DirectedGraph.successors
        backward_transition = edges.Edge.successor

    roots = [entry]
    while roots:
        root = roots.pop()
        data[root] = []
        if len(forward_transitions(cfg, root)) > 1:
            data[root].append(root)

        explore = [root]
        while explore:
            predecessor = explore.pop()

            for edge in forward_transitions(cfg, predecessor):
                successor = forward_transition(edge)

                if len(backward_transitions(cfg, successor)) == 1:
                    tree.add_vertex(successor)
                    tree.add_edge(edges.Edge(predecessor, successor))

                    explore.append(successor)
                    data[successor] = data[predecessor][:]
                    if len(forward_transitions(cfg, successor)) > 1:
                        data[successor].append(successor)
                else:
                    if successor not in vertex_to_row:
                        roots.append(successor)
                        tree.add_vertex(successor)
                        next_row_id += 1
                        forest[next_row_id] = GraphRow(next_row_id, {successor})
                        vertex_to_row[successor] = next_row_id

                    if root != successor:
                        predecessor_row = forest[vertex_to_row[root]]
                        successor_row = forest[vertex_to_row[successor]]
                        predecessor_row.successors.add(successor_row.id_)
                        successor_row.predecessors.add(predecessor_row.id_)
                        forest_edge = (predecessor_row.id_, successor_row.id_)
                        tentacles.setdefault(forest_edge, set()).add(predecessor)

    return next_row_id


def update_dominator_tree(predecessor_row: GraphRow,
                          row: GraphRow,
                          data: Dict[vertices.Vertex, List[vertices.Vertex]],
                          forest: Dict[vertices.Vertex, GraphRow],
                          tentacles: Dict[Tuple[vertices.Vertex, vertices.Vertex], List[vertices.Vertex]],
                          tree: graphs.Tree):
    forest_edge = (predecessor_row.id_, row.id_)
    query = tentacles[forest_edge]

    if len(query) == 1:
        (tentacle,) = query
        dominator = data[tentacle][:] + [tentacle]
    else:
        (first, *rest) = query
        dominator = data[first][:]
        for tentacle in rest:
            if len(dominator) == 1:
                break

            min_length = min(len(data[tentacle]), len(dominator))
            dominator = dominator[:min_length]
            i = min_length - 1
            while dominator[i] != data[tentacle][i]:
                dominator.pop()
                i -= 1

    for vertex in row.vertices:
        tree.add_edge(edges.Edge(dominator[-1], vertex))

    return dominator


def update_forest(predecessor_row: GraphRow,
                  row: GraphRow,
                  data: Dict[vertices.Vertex, List[vertices.Vertex]],
                  forest: Dict[vertices.Vertex, GraphRow],
                  tentacles: Dict[Tuple[vertices.Vertex, vertices.Vertex], List[vertices.Vertex]],
                  tree: graphs.Tree,
                  dominator: List[vertices.Vertex]):
    predecessor_row.successors.remove(row.id_)
    changed = set(row.vertices)
    for successor_id in row.successors:
        successor_row = forest[successor_id]
        successor_row.predecessors.remove(row.id_)
        forest_edge = (row.id_, successor_row.id_)
        changed.update(tentacles[forest_edge])

        if predecessor_row.id_ != successor_row.id_:
            successor_row.predecessors.add(predecessor_row.id_)
            predecessor_row.successors.add(successor_row.id_)

            new_forest_edge = (predecessor_row.id_, successor_row.id_)
            tentacles.setdefault(new_forest_edge, set()).update(tentacles[forest_edge])

    for tentacle in changed:
        data[tentacle] = dominator + data[tentacle]

    del forest[row.id_]


def do_t0_reduction(next_row_id: int,
                    scc: Set[int],
                    forest: Dict[vertices.Vertex, GraphRow],
                    tentacles: Dict[Tuple[vertices.Vertex, vertices.Vertex], List[vertices.Vertex]],):
    entry_rows = set()
    entry_vertices = set()
    for row_id in scc:
        row = forest[row_id]
        for predecessor_id in row.predecessors:
            if predecessor_id not in scc:
                entry_vertices.update(row.vertices)
                entry_rows.add(row_id)

    new_row = GraphRow(next_row_id, entry_vertices)
    forest[next_row_id] = new_row

    for row_id in entry_rows:
        row = forest[row_id]
        for predecessor_id in row.predecessors:
            predecessor_row = forest[predecessor_id]
            predecessor_row.successors.discard(row_id)
            if predecessor_id not in scc:
                new_row.predecessors.add(predecessor_id)
                predecessor_row.successors.add(new_row.id_)

                forest_edge = (predecessor_row.id_, row.id_)
                new_forest_edge = (predecessor_row.id_, new_row.id_)
                tentacles.setdefault(new_forest_edge, set()).update(tentacles[forest_edge])

        for successor_id in row.successors:
            if successor_id not in entry_rows:
                successor_row = forest[successor_id]
                new_row.successors.add(successor_id)
                successor_row.predecessors.add(new_row.id_)
                successor_row.predecessors.discard(row_id)

                forest_edge = (row.id_, successor_row.id_)
                new_forest_edge = (new_row.id_, successor_row.id_)
                tentacles.setdefault(new_forest_edge, set()).update(tentacles[forest_edge])

    for row_id in entry_rows:
        del forest[row_id]


def t0_t1_t2_dominators(cfg: graphs.ControlFlowGraph, entry: vertices.Vertex):
    tree = graphs.Tree()
    forest = {}
    tentacles = {}
    data = {}
    entry_row_id = 0

    next_row_id = preliminary_search(cfg, entry, entry_row_id, tree, data, forest, tentacles)
    ready = [id_ for id_, row in forest.items() if len(row.predecessors) == 1]
    while len(forest) > 1:
        if ready:
            candidates = set()
            for id_ in ready:
                row = forest[id_]
                (predecessor_id,) = row.predecessors
                predecessor_row = forest[predecessor_id]
                dominator = update_dominator_tree(predecessor_row, row, data, forest, tentacles, tree)
                update_forest(predecessor_row, row, data, forest, tentacles, tree, dominator)
                candidates.update(row.successors)

            ready = [id_ for id_ in candidates if id_ in forest and len(forest[id_].predecessors) == 1]
        else:
            non_trivial_sccs = compute_sccs(forest, entry_row_id)
            for scc in non_trivial_sccs:
                next_row_id += 1
                do_t0_reduction(next_row_id, scc, forest, tentacles)

            entry_row = forest[entry_row_id]
            ready = [id_ for id_ in entry_row.successors if len(forest[id_].predecessors) == 1]

    #tree.dotify('{}.t0_t1_t2'.format(cfg.name))
    #verify(cfg, tree)


def strip_outliers(data: List[float]):
    q25, q75 = percentile(data, 25), percentile(data, 75)
    iqr = q75 - q25
    cut_off = iqr * 1.5
    lower, upper = q25 - cut_off, q75 + cut_off
    return [x for x in data if lower <= x <= upper]


def main(filename: str, subprogram_names: List[str], repeat: int):
    program = programs.IO.read(filename)
    program.cleanup()

    if subprogram_names:
        program.keep_only(subprogram_names)

    time_for_original = 0
    time_for_new = 0
    for subprogram in program:
        subprogram.cfg.dotify()
        print(subprogram.cfg.name)
        algorithms = [t0_t1_t2_dominators, graphs.LengauerTarjan, graphs.Cooper]
        total_time = {alg: [] for alg in algorithms}

        for i in range(repeat):
            subprogram.cfg.shuffle_edges()
            shuffle(algorithms)

            for alg in algorithms:
                begin = time()
                alg(subprogram.cfg, subprogram.cfg.exit)
                total_time[alg].append(time() - begin)

        algorithms = [t0_t1_t2_dominators, graphs.LengauerTarjan, graphs.Cooper]
        for alg in algorithms:
            total_time[alg] = strip_outliers(total_time[alg])
            print('{:.5f}'.format(sum(total_time[alg]) / len(total_time[alg])))

        print()


def parse_command_line():
    parser = ArgumentParser(description='Reduce CFGs to hierarchy of loops')

    parser.add_argument('--program',
                        help='read the program from this file',
                        required=True)

    parser.add_argument('--repeat',
                        type=int,
                        help='repeat the computations this many times',
                        default=1,
                        metavar='<INT>')

    parser.add_argument('-s',
                        '--subprograms',
                        nargs='+',
                        help='only apply the algorithms to these subprograms',
                        metavar='<NAME>')

    return parser.parse_args()


if __name__ == '__main__':
    stack_size(2 ** 26)
    setrecursionlimit(2 ** 30)
    args = parse_command_line()
    main(args.program, args.subprograms, args.repeat)

from argparse import ArgumentParser
from graphs import edges, graphs, vertices
from sys import setrecursionlimit
from system import programs
from threading import stack_size
from time import time
from typing import Dict, List, Set


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
    graphs.Cooper(cfg)
    differences = set()
    for vertex in cfg:
        if vertex != cfg.entry:
            (betts,) = betts_tree.predecessors(vertex)
            (tarjan,) = tarjan_tree.predecessors(vertex)
            if betts.predecessor() != tarjan.predecessor():
                print('Error {}: betts({}) = {}  tarjan({}) = {}'.format(cfg.name,
                                                                         vertex,
                                                                         betts.predecessor(),
                                                                         vertex,
                                                                         tarjan.predecessor()))
                differences.add((vertex, betts.predecessor(), tarjan.predecessor()))
                assert False
    print('VERIFIED')


class TableRow:
    __slots__ = ['roots', 'partition', 'broadcast']

    def __init__(self, root):
        self.roots = {root}
        self.partition = {}
        self.broadcast = set()

    def __str__(self):
        roots = '{{{}}}'.format(','.join(str(vertex) for vertex in self.roots))
        partition = ' '.join('{}:{{{}}}'.format(key, ','.join(str(vertex) for vertex in values))
                             for key, values in self.partition.items())
        broadcast = '{{{}}}'.format(','.join(str(vertex) for vertex in self.broadcast))
        delimiter = '-' * 80
        return '{}\n{}\n{}\nPartition:: {}\nBroadcast:: {}\n'.format(delimiter,
                                                                     roots,
                                                                     delimiter,
                                                                     partition,
                                                                     broadcast)


class RootTable:
    __slots__ = ['index', 'rows', 'root_key']

    def __init__(self):
        self.index = 0
        self.rows = {}
        self.root_key = {}

    def add(self, root: vertices.Vertex) -> int:
        if root not in self.root_key:
            self.index += 1
            self.rows[self.index] = TableRow(root)
            self.root_key[root] = self.index

    def merge(self, headers: Set[vertices.Vertex], rest: Set[vertices.Vertex]):
        (representative, *others) = headers
        representative_key = self.root_key[representative]
        representative_row = self.rows[representative_key]
        representative_row.roots.update(headers)

        for header in headers:
            key = self.root_key[header]
            row = self.rows[key]
            for indexer, values in row.partition.items():
                if indexer not in headers and indexer not in rest:
                    representative_row.partition.setdefault(indexer, set()).update(values)

            if header in representative_row.partition:
                del representative_row.partition[header]

            if header != representative:
                del self[key]

        representative_row.broadcast.difference_update(headers)

    def __getitem__(self, root: vertices.Vertex):
        key = self.root_key[root]
        return key, self.rows[key]

    def __contains__(self, root: vertices.Vertex):
        return root in self.root_key

    def __delitem__(self, key: int):
        del self.rows[key]

    def __iter__(self):
        for key, row in self.rows.items():
            yield key, row

    def __len__(self):
        return len(self.rows)

    def dump(self):
        for root_data in self.rows.values():
            print(root_data)


class DynamicSCC:
    __slots__ = ['sccs', 'vertex_to_scc']

    def __init__(self):
        self.sccs = set()
        self.vertex_to_scc = {}

    def populate(self, sccs: graphs.StrongComponents):
        for scc in sccs.non_trivial():
            self.sccs.add(scc)
            for vertex in scc:
                self.vertex_to_scc[vertex] = scc

    def __iter__(self):
        for scc in self.sccs:
            yield scc

    def __bool__(self):
        return len(self.sccs) > 0


def t0_t1_t2_dominators(cfg: graphs.ControlFlowGraph):
    tree = graphs.Tree()
    data = {}
    forest = {}
    table = RootTable()
    dynamic_sccs = DynamicSCC()
    worklist = []

    def alive(vertex: vertices.Vertex):
        return forest[vertex] in table

    def combine_dominator_information(query):
        if len(query) == 1:
            (predecessor,) = query
            return data[predecessor][:] + [predecessor]
        else:
            (first, *rest) = query
            dominator = data[first][:]
            for predecessor in rest:
                min_length = min(len(data[predecessor]), len(dominator))
                dominator = dominator[:min_length]
                i = min_length - 1
                while dominator[i] != data[predecessor][i]:
                    dominator.pop()
                    i -= 1
            return dominator

    def answer(row: TableRow):
        (query,) = list(row.partition.values())
        dominator = combine_dominator_information(query)

        base = forest[dominator[0]]
        for merge in row.roots:
            tree.add_edge(edges.Edge(dominator[-1], merge))
            forest[merge] = base

        collected = set()
        for receiver in row.broadcast:
            receiver_key, receiver_row = table[receiver]
            for merge in row.roots:
                if merge in receiver_row.partition:
                    tentacles = receiver_row.partition[merge]
                    collected.update(tentacles)

            before_size = len(receiver_row.partition)
            if base != receiver:
                receiver_row.partition.setdefault(base, set()).update(tentacles)

            for merge in row.roots:
                if merge in receiver_row.partition:
                    del receiver_row.partition[merge]

            after_size = len(receiver_row.partition)
            if after_size == 1 and before_size > 1:
                worklist.append((receiver_key, receiver_row))

        for vertex in collected:
            forest[vertex] = base
            data[vertex] = dominator + data[vertex]

        _, parent_row = table[base]
        parent_row.broadcast.update(row.broadcast)
        parent_row.broadcast.remove(merge)
        parent_row.broadcast.discard(base)

    roots = [cfg.entry]
    table.add(cfg.entry)
    while roots:
        root = roots.pop()
        tree.add_vertex(root)

        data[root] = []
        if len(cfg.successors(root)) > 1:
            data[root].append(root)

        explore = [root]
        while explore:
            predecessor = explore.pop()
            forest[predecessor] = root

            for edge in cfg.successors(predecessor):
                successor = edge.successor()

                if len(cfg.predecessors(successor)) == 1:
                    tree.add_vertex(successor)
                    tree.add_edge(edges.Edge(predecessor, successor))

                    explore.append(successor)
                    data[successor] = data[predecessor][:]
                    if len(cfg.successors(successor)) > 1:
                        data[successor].append(successor)
                elif successor != root:
                    if successor not in table:
                        table.add(successor)
                        roots.append(successor)

                    _, row = table[successor]
                    row.partition.setdefault(root, set()).add(predecessor)

                    _, row = table[root]
                    row.broadcast.add(successor)

    for key, row in table:
        if len(row.partition) == 1:
            worklist.append((key, row))

    while len(table) > 1:
        if worklist:
            while worklist:
                key, row = worklist.pop(0)
                answer(row)
                del table[key]
        else:
            if not dynamic_sccs:
                sccs = graphs.StrongComponents(cfg, alive)
                dynamic_sccs.populate(sccs)

            for scc in dynamic_sccs:
                headers = set()
                rest = set()
                for vertex in scc:
                    external_edges = {edge for edge in cfg.predecessors(vertex) if edge.predecessor() not in scc}
                    if external_edges:
                        headers.add(vertex)
                    else:
                        rest.add(vertex)

                table.merge(headers, rest)
                (header, *others) = headers
                key, row = table[header]
                worklist.append((key, row))
                table.dump()

    #tree.dotify('{}.t0_t1_t2'.format(cfg.name))
    #verify(cfg, tree)


def betts_dominators(cfg: graphs.ControlFlowGraph):
    t0 = time()
    loop_nest = betts_loops(cfg)
    t1 = time()
    print('Betts  {}'.format(t1 - t0))
    super_graph = graphs.SuperBlockLoopGraph(cfg, loop_nest)
    t2 = time()
    print('Betts  {}'.format(t2 - t1))

    dfs = graphs.DepthFirstSearch(super_graph, super_graph.entry)
    edge_dominance = {}
    tree = graphs.Tree()
    for super_block in reversed(dfs.post_order()):
        for vertex in super_block:
            tree.add_vertex(vertex)

        super_dominance = []
        for super_edge in super_graph.predecessors(super_block):
            if super_edge in edge_dominance:
                if not super_dominance:
                    super_dominance = edge_dominance[super_edge][:]
                else:
                    min_length = min(len(edge_dominance[super_edge]), len(super_dominance))
                    super_dominance = super_dominance[:min_length]
                    i = min_length - 1
                    while super_dominance[i] != edge_dominance[super_edge][i]:
                        super_dominance.pop()
                        i -= 1

            if len(super_dominance) == 1:
                break

        if super_block != super_graph.entry:
            for vertex in super_block:
                edge = edges.Edge(super_dominance[-1], vertex)
                tree.add_edge(edge)

        for super_edge in super_graph.successors(super_block):
            if len(super_edge) == 1:
                (source,) = super_edge
                edge_dominance[super_edge] = super_dominance[:] + [source]
            else:
                edge_dominance[super_edge] = super_dominance[:]

    t3 = time()
    print('Betts  {}'.format(t3 - t2))
    print('Betts  {}'.format(t3 - t0))
    verify(cfg, tree)


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
        time_for_new = 0
        time_for_lt = 0
        time_for_df = 0

        for i in range(repeat):
            subprogram.cfg.shuffle_edges()
            begin = time()
            t0_t1_t2_dominators(subprogram.cfg)
            time_for_new += time() - begin

            begin = time()
            graphs.LengauerTarjan(subprogram.cfg, subprogram.cfg.entry)
            time_for_lt += time() - begin

            begin = time()
            graphs.Cooper(subprogram.cfg)
            time_for_df += time() - begin

        print('New={}, LT={}, DF={}'.format(time_for_new/repeat, time_for_lt/repeat, time_for_df/repeat))
        print()

    print('Old={:.5f}  New={:.5f}'.format(time_for_original, time_for_new))


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


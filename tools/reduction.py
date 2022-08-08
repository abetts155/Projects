from argparse import ArgumentParser
from cProfile import Profile
from collections import deque
from concurrent.futures import ProcessPoolExecutor, as_completed
from enum import auto, Enum
from graphs import edges, graphs, vertices
from heapq import heapify, heappop, heappush
from miscellaneous.helpful import error_message
from multiprocessing import Manager
from numpy import percentile
from random import sample, shuffle
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


def verify_immediate_dominators(cfg: graphs.ControlFlowGraph,
                                idom: Dict[vertices.Vertex, vertices.Vertex]):
    tarjan_tree = graphs.LengauerTarjan(cfg, cfg.entry)
    differences = 0
    for vertex in cfg:
        if vertex != cfg.entry:
            betts = idom[vertex]
            tarjan = tarjan_tree.idom[vertex]

            if betts != tarjan:
                print('{}: betts({}) = {}  tarjan({}) = {}'.format(cfg.name,
                                                                   vertex,
                                                                   betts,
                                                                   vertex,
                                                                   tarjan))
                differences += 1

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


def dump_data(data: Dict[vertices.Vertex, List[vertices.Vertex]]):
    for vertex in sorted(data.keys()):
        print('{:<3}:  [{}]'.format(vertex.id_, '  '.join(str(x) for x in data[vertex])))


def see_forest(forest: Dict[vertices.Vertex, GraphRow]):
    for row in forest.values():
        print('>' * 10)
        print(row)
    print()


def preliminary_search(cfg: graphs.ControlFlowGraph,
                       entry: vertices.Vertex,
                       entry_row_id: int,
                       idom: Dict[vertices.Vertex, vertices.Vertex],
                       data: Dict[vertices.Vertex, List[vertices.Vertex]],
                       forest: Dict[vertices.Vertex, GraphRow],
                       tentacles: Dict[Tuple[vertices.Vertex, vertices.Vertex], List[vertices.Vertex]]):
    vertex_to_row = {}
    next_row_id = entry_row_id
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
                    idom[successor] = predecessor

                    explore.append(successor)
                    data[successor] = data[predecessor][:]
                    if len(forward_transitions(cfg, successor)) > 1:
                        data[successor].append(successor)
                else:
                    if successor not in vertex_to_row:
                        roots.append(successor)
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
                          tentacles: Dict[Tuple[vertices.Vertex, vertices.Vertex], List[vertices.Vertex]],
                          idom: Dict[vertices.Vertex, vertices.Vertex]):
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
        idom[vertex] = dominator[-1]

    return dominator


def update_forest(predecessor_row: GraphRow,
                  row: GraphRow,
                  data: Dict[vertices.Vertex, List[vertices.Vertex]],
                  forest: Dict[vertices.Vertex, GraphRow],
                  tentacles: Dict[Tuple[vertices.Vertex, vertices.Vertex], List[vertices.Vertex]],
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
                    tentacles: Dict[Tuple[vertices.Vertex, vertices.Vertex], List[vertices.Vertex]], ):
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
    idom = {}
    forest = {}
    tentacles = {}
    data = {}
    entry_row_id = 0

    s = time()
    next_row_id = preliminary_search(cfg, entry, entry_row_id, idom, data, forest, tentacles)
    t = time()
    # print('search', t-s)
    ready = [id_ for id_, row in forest.items() if len(row.predecessors) == 1]
    while len(forest) > 1:
        if ready:
            candidates = set()
            for id_ in ready:
                row = forest[id_]
                (predecessor_id,) = row.predecessors
                predecessor_row = forest[predecessor_id]
                dominator = update_dominator_tree(predecessor_row, row, data, tentacles, idom)
                update_forest(predecessor_row, row, data, forest, tentacles, dominator)
                candidates.update(row.successors)

            ready = [id_ for id_ in candidates if id_ in forest and len(forest[id_].predecessors) == 1]
        else:
            non_trivial_sccs = compute_sccs(forest, entry_row_id)
            for scc in non_trivial_sccs:
                next_row_id += 1
                do_t0_reduction(next_row_id, scc, forest, tentacles)

            entry_row = forest[entry_row_id]
            ready = [id_ for id_ in entry_row.successors if len(forest[id_].predecessors) == 1]
    u = time()
    # print('reduce', u-t)
    # tree.dotify('{}.t0_t1_t2'.format(cfg.name))
    # verify(cfg, tree)


class AuxiliaryVertex:
    def __init__(self, root: vertices.Vertex):
        self.root = root
        self.merges = {root}
        self.predecessors = set()
        self.successors = set()
        self.explorers = 0
        self.ready = False
        self.fixed = False


def answer_query(left: List[vertices.Vertex], right: List[vertices.Vertex]):
    i = min(len(left) - 1, len(right) - 1)
    while i >= 0 and left[i] != right[i]:
        i -= 1
    return left[:i + 1]


def add_edge(auxiliary_edges: Dict[Tuple[vertices.Vertex, vertices.Vertex], List[vertices.Vertex]],
             articulations: Set[vertices.Vertex],
             auxiliary_predecessor: AuxiliaryVertex,
             auxiliary_successor: AuxiliaryVertex,
             data: List[vertices.Vertex]):
    key = (auxiliary_predecessor.root.id_, auxiliary_successor.root.id_)
    if key not in auxiliary_edges:
        auxiliary_edges[key] = data
        auxiliary_successor.predecessors.add(auxiliary_predecessor.root.id_)
        auxiliary_predecessor.successors.add(auxiliary_successor.root.id_)
    elif auxiliary_edges[key][-1] not in articulations:
        auxiliary_edges[key] = answer_query(auxiliary_edges[key], data)


def remove_edge(auxiliary_vertices: Dict[vertices.Vertex, AuxiliaryVertex],
                auxiliary_edges: Dict[Tuple[vertices.Vertex, vertices.Vertex], List[vertices.Vertex]],
                predecessor: AuxiliaryVertex,
                successor: AuxiliaryVertex):
    successor.predecessors.discard(predecessor.root.id_)
    predecessor.successors.discard(successor.root.id_)
    del auxiliary_edges[(predecessor.root.id_, successor.root.id_)]


def to_string(auxiliary_vertices, auxiliary_edges):
    pad = 0
    for vertex_id in auxiliary_vertices.keys():
        pad = max(pad, len(str(vertex_id)))

    value = '{}\n'.format('*' * 40)
    for vertex_id in sorted(auxiliary_vertices.keys(), reverse=True):
        for successor_id in sorted(auxiliary_vertices[vertex_id].successors, reverse=True):
            key = (vertex_id, successor_id)
            value += '|{:>{pad}} => {:>{pad}}|  {}\n'.format(vertex_id,
                                                             successor_id,
                                                             ' '.join(str(x) for x in auxiliary_edges[key]),
                                                             pad=pad)
    value += '{}\n'.format('*' * 40)
    return value


def is_t1_or_t2_reducible(auxiliary_vertices: Dict[vertices.Vertex, AuxiliaryVertex],
                          auxiliary_edges: Dict[Tuple[vertices.Vertex, vertices.Vertex], List[vertices.Vertex]],
                          root_to_articulations: Dict[vertices.Vertex, Set[vertices.Vertex]],
                          auxiliary_origin: AuxiliaryVertex,
                          auxiliary_root: AuxiliaryVertex):
    if len(auxiliary_root.predecessors) == 1:
        auxiliary_root.ready = True
        (predecessor_id,) = auxiliary_root.predecessors
        return auxiliary_vertices[predecessor_id], auxiliary_root

    elif len(auxiliary_root.predecessors) == 2 and auxiliary_root.root.id_ in auxiliary_root.predecessors:
        auxiliary_root.ready = True
        predecessor_one, predecessor_two = auxiliary_root.predecessors
        if predecessor_one == auxiliary_root.root.id_:
            predecessor_id = predecessor_two
        else:
            predecessor_id = predecessor_one
        return auxiliary_vertices[predecessor_id], auxiliary_root

    elif auxiliary_origin.root.id_ in auxiliary_root.predecessors:
        key = (auxiliary_origin.root.id_, auxiliary_root.root.id_)
        if auxiliary_edges[key][-1] in root_to_articulations[auxiliary_origin.root.id_]:
            auxiliary_root.ready = True
            return auxiliary_origin, auxiliary_root


def reduce_by_t1_and_t2(idom: Dict[vertices.Vertex, vertices.Vertex],
                        auxiliary_vertices: Dict[vertices.Vertex, AuxiliaryVertex],
                        auxiliary_edges: Dict[Tuple[vertices.Vertex, vertices.Vertex], List[vertices.Vertex]],
                        root_to_articulations: Dict[vertices.Vertex, Set[vertices.Vertex]],
                        auxiliary_origin: AuxiliaryVertex,
                        auxiliary_predecessor: AuxiliaryVertex,
                        auxiliary_root: AuxiliaryVertex):
    key = (auxiliary_predecessor.root.id_, auxiliary_root.root.id_)
    prefix = auxiliary_edges[key]

    for merge in auxiliary_root.merges:
        idom[merge] = prefix[-1]

    while auxiliary_root.predecessors:
        predecessor_id = auxiliary_root.predecessors.pop()
        remove_edge(auxiliary_vertices, auxiliary_edges, auxiliary_vertices[predecessor_id], auxiliary_root)

    if auxiliary_predecessor.root.id_ == auxiliary_origin.root.id_ and len(auxiliary_predecessor.successors) == 0 and len(auxiliary_root.merges) == 1:
        root_to_articulations[auxiliary_origin.root.id_].update(root_to_articulations[auxiliary_root.root.id_])
        prefix = []

    ready = []
    while auxiliary_root.successors:
        successor_id = auxiliary_root.successors.pop()
        auxiliary_successor = auxiliary_vertices[successor_id]
        edge_data = prefix + auxiliary_edges[(auxiliary_root.root.id_, auxiliary_successor.root.id_)]
        add_edge(auxiliary_edges,
                 root_to_articulations[auxiliary_origin.root.id_],
                 auxiliary_predecessor,
                 auxiliary_successor,
                 edge_data)

        remove_edge(auxiliary_vertices, auxiliary_edges, auxiliary_root, auxiliary_successor)

        if not auxiliary_successor.ready:
            information = is_t1_or_t2_reducible(auxiliary_vertices,
                                                auxiliary_edges,
                                                root_to_articulations,
                                                auxiliary_origin,
                                                auxiliary_successor)
            if information:
                ready.append(information)

    del auxiliary_vertices[auxiliary_root.root.id_]
    return ready


def search(origin: vertices.Vertex,
           cfg: graphs.ControlFlowGraph,
           idom: Dict[vertices.Vertex, vertices.Vertex],
           auxiliary_vertices: Dict[vertices.Vertex, AuxiliaryVertex],
           auxiliary_edges: Dict[Tuple[vertices.Vertex, vertices.Vertex], List[vertices.Vertex]],
           root_to_articulations: Dict[vertices.Vertex, Set[vertices.Vertex]]):
    auxiliary_origin = AuxiliaryVertex(origin)
    auxiliary_vertices[origin.id_] = auxiliary_origin
    data = {}
    frontier = deque([origin.id_])
    while frontier:
        root_id = frontier.popleft()
        auxiliary_root = auxiliary_vertices[root_id]
        root_to_articulations[root_id] = set()

        ready = [auxiliary_root.root]
        while ready:
            vertex = ready.pop()

            if len(ready) == 0 and len(auxiliary_root.successors) == 0:
                root_to_articulations[root_id].add(vertex)
                data[vertex] = []

            if len(cfg.successors(vertex)) > 1:
                data[vertex] = data[vertex] + [vertex]

            for edge in cfg.successors(vertex):
                predecessor = edge.predecessor()
                successor = edge.successor()

                if len(cfg.predecessors(successor)) == 1:
                    data[successor] = data[predecessor]
                    idom[successor] = predecessor
                    ready.append(successor)
                elif root_id != successor.id_:
                    if len(cfg.successors(vertex)) == 1:
                        edge_data = data[vertex] + [vertex]
                    else:
                        edge_data = data[vertex]

                    if successor.id_ not in auxiliary_vertices:
                        auxiliary_vertices[successor.id_] = AuxiliaryVertex(successor)
                        auxiliary_successor = auxiliary_vertices[successor.id_]
                        auxiliary_successor.explorers = 1

                        add_edge(auxiliary_edges,
                                 root_to_articulations[root_id],
                                 auxiliary_root,
                                 auxiliary_successor,
                                 edge_data)
                    else:
                        auxiliary_successor = auxiliary_vertices[successor.id_]
                        add_edge(auxiliary_edges,
                                 root_to_articulations[root_id],
                                 auxiliary_root,
                                 auxiliary_successor,
                                 edge_data)

                        if len(auxiliary_successor.predecessors) == 1:
                            auxiliary_successor.explorers += 1

                            if auxiliary_successor.explorers == len(cfg.predecessors(successor)):
                                key = (root_id, auxiliary_successor.root.id_)
                                data[successor] = auxiliary_edges[key]
                                idom[successor] = data[successor][-1]
                                ready.append(successor)

                                remove_edge(auxiliary_vertices, auxiliary_edges, auxiliary_root, auxiliary_successor)
                                del auxiliary_vertices[auxiliary_successor.root.id_]

        for successor_id in auxiliary_root.successors:
            auxiliary_successor = auxiliary_vertices[successor_id]
            if len(auxiliary_successor.predecessors) == 1:
                frontier.append(successor_id)


class StrongComponentData:
    __slots__ = ['pre_id', 'low_id']

    def __init__(self, value):
        self.pre_id = value
        self.low_id = value


def find_strong_components(auxiliary_vertices: Dict[vertices.Vertex, AuxiliaryVertex],
                           auxiliary_origin: AuxiliaryVertex,
                           auxiliary_root: AuxiliaryVertex,
                           stack: List[AuxiliaryVertex],
                           data: Dict[int, StrongComponentData],
                           on_stack: Dict[int, bool],
                           strong_components: List[List[vertices.Vertex]],
                           vertex_to_component: Dict[vertices.Vertex, int]):
    root_id = auxiliary_root.root.id_
    data[root_id] = StrongComponentData(len(data) + 1)
    on_stack[root_id] = True
    stack.append(root_id)

    for predecessor_id in auxiliary_root.predecessors:
        auxiliary_predecessor = auxiliary_vertices[predecessor_id]
        if predecessor_id not in data:
            find_strong_components(auxiliary_vertices,
                                   auxiliary_origin,
                                   auxiliary_predecessor,
                                   stack,
                                   data,
                                   on_stack,
                                   strong_components,
                                   vertex_to_component)
            data[root_id].low_id = min(data[root_id].low_id, data[predecessor_id].low_id)
        elif predecessor_id in on_stack:
            data[root_id].low_id = min(data[root_id].low_id, data[predecessor_id].pre_id)

    #print('{}: pre={}  low={}'.format(root_id, data[root_id].pre_id, data[root_id].low_id))

    if data[root_id].pre_id == data[root_id].low_id:
        done = False
        scc = []
        strong_components.append(scc)
        scc_number = len(strong_components)
        while not done:
            vertex_id = stack.pop()
            del on_stack[vertex_id]
            scc.append(vertex_id)
            vertex_to_component[vertex_id] = scc_number
            done = vertex_id == root_id


def reduce_by_t0(auxiliary_origin: AuxiliaryVertex,
                 auxiliary_vertices: Dict[vertices.Vertex, AuxiliaryVertex],
                 auxiliary_edges: Dict[Tuple[vertices.Vertex, vertices.Vertex], List[vertices.Vertex]],
                 root_to_articulations: Dict[vertices.Vertex, Set[vertices.Vertex]]):
    #print(to_string(auxiliary_vertices, auxiliary_edges))
    stack = []
    data = {}
    on_stack = {}
    vertex_to_component = {}
    strong_components = []

    for successor_id in auxiliary_origin.successors:
        auxiliary_successor = auxiliary_vertices[successor_id]
        for predecessor_id in auxiliary_successor.predecessors:
            if predecessor_id not in data:
                auxiliary_predecessor = auxiliary_vertices[predecessor_id]
                find_strong_components(auxiliary_vertices,
                                       auxiliary_origin,
                                       auxiliary_predecessor,
                                       stack,
                                       data,
                                       on_stack,
                                       strong_components,
                                       vertex_to_component)

    ready = []
    for scc in strong_components:
        if len(scc) > 1:
            #print('  '.join(str(x) for x in sorted(scc)))

            scc_entries = set()
            for vertex_id in scc:
                auxiliary_vertex = auxiliary_vertices[vertex_id]
                for predecessor_id in auxiliary_vertex.predecessors:
                    if vertex_to_component[vertex_id] != vertex_to_component[predecessor_id]:
                        scc_entries.add(vertex_id)

            (representative_id,) = sample(scc_entries, 1)
            auxiliary_representative = auxiliary_vertices[representative_id]

            new_predecessors = {}
            new_successors = {}
            for vertex_id in scc_entries:
                auxiliary_vertex = auxiliary_vertices[vertex_id]
                auxiliary_representative.merges.update(auxiliary_vertex.merges)

                while auxiliary_vertex.predecessors:
                    predecessor_id = auxiliary_vertex.predecessors.pop()
                    auxiliary_predecessor = auxiliary_vertices[predecessor_id]

                    if vertex_to_component[vertex_id] != vertex_to_component[predecessor_id]:
                        key = (predecessor_id, vertex_id)
                        if predecessor_id not in new_predecessors:
                            new_predecessors[predecessor_id] = auxiliary_edges[key]
                        else:
                            new_predecessors[predecessor_id] = answer_query(new_predecessors[predecessor_id],
                                                                            auxiliary_edges[key])

                    remove_edge(auxiliary_vertices,
                                auxiliary_edges,
                                auxiliary_predecessor,
                                auxiliary_vertex)

                while auxiliary_vertex.successors:
                    successor_id = auxiliary_vertex.successors.pop()
                    auxiliary_successor = auxiliary_vertices[successor_id]

                    if successor_id not in scc_entries:
                        key = (vertex_id, successor_id)
                        if successor_id not in new_successors:
                            new_successors[successor_id] = auxiliary_edges[key]
                        else:
                            new_successors[successor_id] = answer_query(new_successors[successor_id],
                                                                        auxiliary_edges[key])

                    remove_edge(auxiliary_vertices,
                                auxiliary_edges,
                                auxiliary_vertex,
                                auxiliary_successor)

                if vertex_id != representative_id:
                    del auxiliary_vertices[vertex_id]

            for predecessor_id, edge_data in new_predecessors.items():
                add_edge(auxiliary_edges,
                         root_to_articulations[auxiliary_origin.root.id_],
                         auxiliary_vertices[predecessor_id],
                         auxiliary_representative,
                         edge_data)

            for successor_id, edge_data in new_successors.items():
                add_edge(auxiliary_edges,
                         root_to_articulations[auxiliary_origin.root.id_],
                         auxiliary_representative,
                         auxiliary_vertices[successor_id],
                         edge_data)

            if len(auxiliary_representative.predecessors) == 1:
                if auxiliary_origin.root.id_ in auxiliary_representative.predecessors:
                    auxiliary_representative.ready = True
                    ready.append((auxiliary_origin, auxiliary_representative))

    #print(to_string(auxiliary_vertices, auxiliary_edges))

    assert ready
    return ready


def betts(cfg: graphs.ControlFlowGraph, origin: vertices.Vertex):
    idom = {}
    auxiliary_vertices = {}
    auxiliary_edges = {}
    root_to_articulations = {}
    search(origin, cfg, idom, auxiliary_vertices, auxiliary_edges, root_to_articulations)

    auxiliary_origin = auxiliary_vertices[origin.id_]
    ready = deque([])
    for auxiliary_root in auxiliary_vertices.values():
        information = is_t1_or_t2_reducible(auxiliary_vertices,
                                            auxiliary_edges,
                                            root_to_articulations,
                                            auxiliary_origin,
                                            auxiliary_root)
        if information:
            ready.append(information)

    effective_predecessors = {}
    while len(auxiliary_vertices) > 1:
        if ready:
            while ready:
                auxiliary_predecessor, auxiliary_root = ready.popleft()

                while auxiliary_predecessor in effective_predecessors:
                    auxiliary_predecessor = effective_predecessors[auxiliary_predecessor]
                effective_predecessors[auxiliary_root] = auxiliary_predecessor

                pending = reduce_by_t1_and_t2(idom,
                                              auxiliary_vertices,
                                              auxiliary_edges,
                                              root_to_articulations,
                                              auxiliary_origin,
                                              auxiliary_predecessor,
                                              auxiliary_root)
                ready.extend(pending)
        else:
            pending = reduce_by_t0(auxiliary_origin,
                                   auxiliary_vertices,
                                   auxiliary_edges,
                                   root_to_articulations)
            ready.extend(pending)

    return idom


def strip_outliers(data: List[float]):
    q25, q75 = percentile(data, 25), percentile(data, 75)
    iqr = q75 - q25
    cut_off = iqr * 1.5
    lower, upper = q25 - cut_off, q75 + cut_off
    return [x for x in data if lower <= x <= upper]


def main(filename: str, subprogram_names: List[str], repeat: int, verify: bool):
    program = programs.IO.read(filename)
    program.cleanup()

    if subprogram_names:
        program.keep_only(subprogram_names)

    time_for_original = 0
    time_for_new = 0
    for subprogram in program:
        subprogram.cfg.remove_edge(edges.Edge(subprogram.cfg.exit, subprogram.cfg.entry))
        subprogram.cfg.dotify()

        subprogram.cfg.shuffle_edges()
        #graphs.StrongComponents(subprogram.cfg, subprogram.cfg.entry)

        print('{}: |V|={}  |E|={}'.format(subprogram.cfg.name,
                                          subprogram.cfg.number_of_vertices(),
                                          subprogram.cfg.number_of_edges()))
        algorithms = [betts, graphs.LengauerTarjan, graphs.Cooper]
        total_time = {alg: [] for alg in algorithms}

        for i in range(repeat):
            subprogram.cfg.shuffle_edges()
            mutated = algorithms[:]
            shuffle(mutated)

            for alg in mutated:
                begin = time()
                idom = alg(subprogram.cfg, subprogram.cfg.entry)
                end = time()
                total_time[alg].append(end - begin)
                if alg == betts and verify:
                    verify_immediate_dominators(subprogram.cfg, idom)

        for alg in algorithms:
            # print('  '.join(str(t) for t in sorted(total_time[alg])))
            # total_time[alg] = strip_outliers(total_time[alg])
            print('{:.8f}'.format(sum(total_time[alg]) / len(total_time[alg])))

        print()


def parse_command_line():
    parser = ArgumentParser(description='Compute immediate dominators')

    parser.add_argument('--program',
                        help='read the program from this file',
                        required=True)

    parser.add_argument('-R',
                        '--repeat',
                        type=int,
                        help='repeat the computations this many times',
                        default=1,
                        metavar='<INT>')

    parser.add_argument('-s',
                        '--subprograms',
                        nargs='+',
                        help='only apply the algorithms to these subprograms',
                        metavar='<NAME>')

    parser.add_argument('-V',
                        '--verify',
                        action='store_true',
                        help='verify the immediate dominators',
                        default=False)

    return parser.parse_args()


if __name__ == '__main__':
    stack_size(2 ** 26)
    setrecursionlimit(2 ** 30)
    args = parse_command_line()
    main(args.program, args.subprograms, args.repeat, args.verify)

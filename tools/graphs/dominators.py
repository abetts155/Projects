from collections import deque
from graphs.graphs import ControlFlowGraph
from graphs.vertices import Vertex
from typing import Dict, Set


class DominatorTree:
    def __init__(self):
        self.idom = {}
        self.level = {}
        self.children = {}

    def add_vertex(self, vertex: Vertex):
        self.idom[vertex] = None
        self.level[vertex] = 0
        self.children[vertex] = set()

    def add_edge(self, predecessor: Vertex, successor: Vertex):
        self.children[predecessor].add(successor)
        self.idom[successor] = predecessor
        self.level[successor] = self.level[predecessor] + 1

    def remove_edge(self, predecessor: Vertex, successor: Vertex):
        self.children[predecessor].remove(successor)
        self.idom[successor] = None

    def do_lca(self, left, right) -> Vertex:
        while self.level[left] != self.level[right]:
            if self.level[left] > self.level[right]:
                left = self.idom[left]
            else:
                right = self.idom[right]

        while left != right:
            left = self.idom[left]
            right = self.idom[right]

        return left


def update_join(predecessor: Vertex,
                join: Vertex,
                reduced: Dict[Vertex, Set],
                tree: DominatorTree,
                articulations: Set,
                fixed: Set):
    before = tree.idom[join]
    after = tree.do_lca(before, predecessor)

    if before != after:
        tree.remove_edge(before, join)
        tree.add_edge(after, join)

        if after in articulations:
            fixed.add(join)

        subtree = set()
        stack = [join]
        while stack:
            root = stack.pop()
            for child in tree.children[root]:
                tree.level[child] = tree.level[root] + 1
                stack.append(child)

            if root in reduced:
                subtree.add(root)

        for root in subtree:
            for successor in reduced[root]:
                if successor not in subtree:
                    update_join(root, successor, reduced, tree, articulations, fixed)


def is_dominator_tree_fixed(cfg: ControlFlowGraph, fixed: Set):
    return cfg.number_of_vertices() - 1 == len(fixed)


def search(origin: Vertex, cfg: ControlFlowGraph, tree: DominatorTree):
    reduced = {}
    explored = {}
    joins = set()
    articulations = set()
    fixed = set()

    frontier = deque([origin])
    while frontier and not is_dominator_tree_fixed(cfg, fixed):
        joins.clear()
        root = frontier.popleft()

        ready = [root]
        while ready and not is_dominator_tree_fixed(cfg, fixed):
            vertex = ready.pop()

            if not joins and not ready and root == origin:
                articulations.add(vertex)

            for edge in cfg.successors(vertex):
                predecessor = edge.predecessor()
                successor = edge.successor()

                if len(cfg.predecessors(successor)) == 1:
                    tree.add_vertex(successor)
                    tree.add_edge(predecessor, successor)
                    ready.append(successor)
                    fixed.add(successor)
                elif root != successor.id_:
                    joins.add(successor)
                    if successor not in explored:
                        explored[successor] = 1
                        tree.add_vertex(successor)
                        tree.add_edge(predecessor, successor)

                        if predecessor in articulations:
                            fixed.add(successor)
                    else:
                        if successor not in fixed:
                            update_join(predecessor, successor, reduced, tree, articulations, fixed)

                        explored[successor] += 1
                        if explored[successor] == len(cfg.predecessors(successor)) and successor not in reduced:
                            ready.append(successor)
                            joins.remove(successor)
                            fixed.add(successor)

        for join in joins:
            if join not in reduced:
                reduced[join] = set()
                if join in fixed:
                    frontier.appendleft(join)
                else:
                    frontier.append(join)

            if root != origin and join not in fixed:
                reduced[root].add(join)


def betts(cfg: ControlFlowGraph, origin: Vertex):
    tree = DominatorTree()
    tree.add_vertex(origin)
    search(origin, cfg, tree)
    return tree.idom

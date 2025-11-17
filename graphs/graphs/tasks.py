import json
import typing

import graphs.edges
import graphs.graphs
import graphs.vertices


class Task(graphs.graphs.ControlFlowGraph):
    def __init__(self, name: str):
        super().__init__(name)
        self._vertex_times: dict[graphs.vertices, int] = {}

    def set_time(self, vertex: graphs.vertices, time: int):
        assert time >= 1
        self._vertex_times[vertex] = time

    def get_time(self, vertex: graphs.vertices) -> int:
        return self._vertex_times[vertex]

    def copy(self) -> typing.Self:
        reflection: Task = super().copy(self.name)
        for vertex in self.its_vertices:
            reflection._vertex_times[vertex] = self._vertex_times[vertex]
        return reflection

    def canonicalise(self) -> typing.Self:
        canonical = self.copy()
        dead_vertices = set()
        for vertex in self.its_vertices:
            if len(self.predecessors[vertex]) == 1:
                (edge,) = self.predecessors[vertex]
                successor = edge.point_b
                if len(self.successors[successor]) == 1:
                    dead_vertices.add(vertex)

        for vertex in dead_vertices:
            dead_edges = set()
            (predecessor_edge,) = canonical.predecessors[vertex]
            dead_edges.add(predecessor_edge)
            predecessor = predecessor_edge.point_a
            canonical._vertex_times[predecessor] += canonical._vertex_times[vertex]

            for successor_edge in canonical.successors[vertex]:
                successor = successor_edge.point_b
                replacement_edge = graphs.edges.Edge(predecessor, successor)
                canonical.add_edge(replacement_edge)
                dead_edges.add(successor_edge)

            for edge in dead_edges:
                canonical.remove_edge(edge)
            canonical.remove_vertex(vertex)

            if vertex == canonical.exit:
                canonical.exit = predecessor

        return canonical

    def get_volume(self):
        return sum(self._vertex_times.values())

    def to_json(self) -> dict:
        time_dict = {vertex.identifier: self.get_time(vertex) for vertex in self.its_vertices}
        edge_dicts = []
        for edge in self.its_edges:
            edge_dicts.append(dict(a=edge.point_a.identifier, b=edge.point_b.identifier))
        return dict(name=self.name, edges=edge_dicts, times=time_dict)

    def __str__(self):
        return (f"{self.name}\n"
                f"{'\n'.join(f'{vertex.identifier}: {self._vertex_times[vertex]}' for vertex in self.its_vertices)}")

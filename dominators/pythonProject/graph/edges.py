import dataclasses
from graph import vertices


@dataclasses.dataclass(slots=True)
class Edge:
    point_a: vertices.Vertex
    point_b: vertices.Vertex

    def __hash__(self):
        return hash(self.point_a) + hash(self.point_b)

    def __str__(self):
        return f'{self.point_a} => {self.point_b}'

    def dotify(self) -> str:
        return f'{self.point_a.dot_identifier()}->{self.point_b.dot_identifier()};\n'


@dataclasses.dataclass
class CallEdge(Edge):
    site: vertices.Vertex

    def __hash__(self):
        return hash(self.point_a) + hash(self.point_b) + hash(self.site)

    def __str__(self):
        return f'{self.point_a.identifier:>4} => {self.point_b.identifier:>4}  @{self.site}'

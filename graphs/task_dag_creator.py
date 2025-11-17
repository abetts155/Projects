import argparse
import dataclasses
import logging
import pathlib
import random
import typing

from rich.logging import RichHandler

import graphs.edges
import graphs.graphs
import graphs.graph_input
import graphs.tasks
import graphs.vertices

logging.basicConfig(
    level=logging.DEBUG,
    format="%(levelname)s:%(asctime)s:%(message)s",
    datefmt="%H:%M:%S",
    handlers=[RichHandler()]
)
logger = logging.getLogger(__name__)


@dataclasses.dataclass
class TaskGenerator:
    min_vertices: int = 10
    max_vertices: int = 20
    min_time: int = 1
    max_time: int = 10
    tasks: int = 1
    unstructured: bool = False

    def __post_init__(self):
        if self.max_vertices < self.min_vertices:
            logger.error("Maximum number of vertices must be at least the minimum number of vertices")
        if self.min_vertices < 4:
            logger.error("Minimum number of vertices must be at least 4")
        if self.tasks < 0:
            logger.error("Number of tasks must be a positive integer")
        if self.max_time < self.min_time:
            logger.error("Maximum WCET must be at least the minimum WCET")
        if self.min_time < 1:
            logger.error("Minimum WCET must be at least 1")


class Region:
    def __init__(self, vertices: list[graphs.vertices.Vertex]):
        assert len(vertices) >= 3
        self.edges: list[graphs.edges.Edge] = []
        self.entries: list[graphs.vertices.Vertex] = []
        self.exits: list[graphs.vertices.Vertex] = []
        self.create(vertices)

    def create(self, vertices: list[graphs.vertices.Vertex]):
        random.shuffle(vertices)
        selected = random.random()
        if 1/2 < selected <= 1:
            # Single entry, single exit
            the_entry = vertices[0]
            the_exit = vertices[1]
            self.entries.append(the_entry)
            self.exits.append(the_exit)

            for vertex in vertices[2:]:
                edge_a = graphs.edges.Edge(the_entry, vertex)
                edge_b = graphs.edges.Edge(vertex, the_exit)
                self.edges.append(edge_a)
                self.edges.append(edge_b)

            if len(vertices) == 3:
                edge = graphs.edges.Edge(the_entry, the_exit)
                self.edges.append(edge)

        elif 1/4 < selected <= 1/2:
            # Single entry, multiple exits
            the_entry = vertices[0]
            self.entries.append(the_entry)
            self.exits.extend(vertices[1:])

            for vertex in self.exits:
                edge = graphs.edges.Edge(the_entry, vertex)
                self.edges.append(edge)

        else:
            # Multiple entries, single exit
            self.entries.extend(vertices[1:])
            the_exit = vertices[0]
            self.exits.append(the_exit)

            for vertex in self.entries:
                edge = graphs.edges.Edge(vertex, the_exit)
                self.edges.append(edge)

    def connect(self, other: typing.Self):
        if len(self.exits) > 1 and len(other.entries) > 1:
            (the_entry,) = self.entries
            (the_exit,) = other.exits
            for vertex in other.entries:
                edge = graphs.edges.Edge(the_entry, vertex)
                self.edges.append(edge)
            for vertex in self.exits:
                edge = graphs.edges.Edge(vertex, the_exit)
                self.edges.append(edge)
            self.exits = [the_exit]
        elif len(self.entries) > 1 and len(other.exits) > 1:
            (the_entry,) = other.entries
            (the_exit,) = self.exits
            for vertex in self.entries:
                edge = graphs.edges.Edge(the_entry, vertex)
                self.edges.append(edge)
            for vertex in other.exits:
                edge = graphs.edges.Edge(vertex, the_exit)
                self.edges.append(edge)
            self.entries = [the_entry]
        elif len(self.entries) > 1 and len(other.entries) > 1:
            (the_exit,) = self.exits
            (other_exit,) = other.exits
            edge = graphs.edges.Edge(other_exit, the_exit)
            self.edges.append(edge)
            self.entries.extend(other.entries)
        elif len(self.exits) > 1 and len(other.exits) > 1:
            (the_entry,) = self.entries
            (other_entry,) = other.entries
            edge = graphs.edges.Edge(the_entry, other_entry)
            self.edges.append(edge)
            self.exits.extend(other.exits)
        else:
            selected = random.random()
            if 2/3 < selected <= 1:
                for predecessor in self.exits:
                    for successor in other.entries:
                        edge = graphs.edges.Edge(predecessor, successor)
                        self.edges.append(edge)
                self.exits = list(other.exits)
            elif 1/3 < selected <= 2/3:
                for predecessor in other.exits:
                    for successor in self.entries:
                        edge = graphs.edges.Edge(predecessor, successor)
                        self.edges.append(edge)
                self.entries = list(other.entries)
            else:
                for predecessor in self.entries:
                    for successor in other.entries:
                        edge = graphs.edges.Edge(predecessor, successor)
                        self.edges.append(edge)

                for predecessor in other.exits:
                    for successor in self.exits:
                        edge = graphs.edges.Edge(predecessor, successor)
                        self.edges.append(edge)

        self.edges.extend(other.edges)


def add_vertices(generator: TaskGenerator, task: graphs.tasks.Task):
    number_of_vertices = random.randint(generator.min_vertices, generator.max_vertices)
    for i in range(1, number_of_vertices + 1):
        vertex = graphs.vertices.Vertex(f"V{i}")
        task.add_vertex(vertex)
        time = random.randint(generator.min_time, generator.max_time)
        task.set_time(vertex, time)

    task.entry_vertex, task.exit_vertex = random.sample(list(task.its_vertices), 2)


def add_edges(task: graphs.tasks.Task):
    vertices = [vertex for vertex in task.its_vertices if vertex not in [task.entry_vertex, task.exit_vertex]]
    partition = [[]]
    for i in range(len(vertices)):
        if len(partition[-1]) < 3 or random.random() > 0.5:
            partition[-1].append(vertices[i])
        else:
            partition.append([vertices[i]])

    if len(partition[-1]) < 3:
        surplus = partition.pop()
        partition[-1].extend(surplus)

    regions = []
    for vertex_set in partition:
        regions.append(Region(vertex_set))

    random.shuffle(regions)
    while len(regions) > 1 and random.random() > 0.05:
        left = regions.pop()
        right = regions.pop()
        if len(right.entries) == 1 and len(right.exits) == 1 and (len(left.entries) > 1 or len(left.exits) > 1):
            right.connect(left)
            regions.append(right)
        else:
            left.connect(right)
            regions.append(left)

    for region in regions:
        for edge in region.edges:
            task.add_edge(edge)

        for an_entry in region.entries:
            edge = graphs.edges.Edge(task.entry_vertex, an_entry)
            task.add_edge(edge)

        for an_exit in region.exits:
            edge = graphs.edges.Edge(an_exit, task.exit_vertex)
            task.add_edge(edge)


def break_structure(task: graphs.tasks.Task):
    ordering = graphs.graphs.jail_and_free(task)
    for i, vertex in enumerate(ordering):
        # The first vertex in the entry and the second vertex can only have one predecessor
        if i >= 2:
            candidates = list(range(0, i - 1))
            while candidates and random.random() > 0.5:
                random.shuffle(candidates)
                j = candidates.pop()
                candidate = ordering[j]
                edge = graphs.edges.Edge(candidate, vertex)
                if edge not in task.its_edges:
                    task.add_edge(edge)


def guarantee_life(task: graphs.tasks.Task):
    if len(task.its_vertices) > 2 and len(task.its_edges) == len(task.its_vertices) - 1:
        # The task is a straight line
        edge = graphs.edges.Edge(task.entry_vertex, task.exit_vertex)
        task.add_edge(edge)


def generate(generator: TaskGenerator, identifier: str) -> graphs.tasks.Task:
    task = graphs.tasks.Task(identifier)
    add_vertices(generator, task)
    add_edges(task)

    if generator.unstructured and random.random() > 0.5:
        break_structure(task)

    guarantee_life(task)

    assert graphs.graphs.is_dag(task)

    return task


def parse_the_command_line() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Generate DAG tasks")

    parser.add_argument(
        "-T", type=int, default=TaskGenerator.tasks, metavar="<INT>",  help="number of tasks"
    )

    parser.add_argument(
        "-LV", type=int, default=TaskGenerator.min_vertices, metavar="<INT>", help="minimum number of vertices"
    )

    parser.add_argument(
        "-UV", type=int, default=TaskGenerator.max_vertices, metavar="<INT>", help="maximum number of vertices"
    )

    parser.add_argument(
        "-LW", type=int, default=TaskGenerator.min_time, metavar="<INT>", help="minimum WCET of a vertex"
    )

    parser.add_argument(
        "-UW", type=int, default=TaskGenerator.max_time, metavar="<INT>", help="maximum WCET of a vertex"
    )

    parser.add_argument(
        "-U", action="store_true", default=TaskGenerator.unstructured, help="allow unstructured tasks"
    )

    parser.add_argument(
        "--file", type=pathlib.Path, required=True, metavar="<FILE>", help="write tasks JSON to this file"
    )

    return parser.parse_args()


def main():
    args = parse_the_command_line()
    generator = TaskGenerator(args.LV, args.UV, args.LW, args.UW, args.T, args.U)
    tasks = []
    for i in range(1, generator.tasks + 1):
        task = generate(generator, f"T{i}")
        tasks.append(task)

    graphs.graph_input.write_tasks(args.file, tasks)

if __name__ == '__main__':
    main()
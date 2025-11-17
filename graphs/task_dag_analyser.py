import argparse
import pathlib
import random

import graphs.graphs
import graphs.graph_input
import graphs.tasks
import graphs.vertices


class Core:
    def __init__(self, identifier: int):
        self.identifier = identifier
        self.execution = {}
        self.vertex = None
        self.start_times = {}

    def execute(self, clock: int):
        self.execution[clock] = self.vertex

    def set_vertex(self, vertex: graphs.vertices.Vertex, clock: int):
        self.vertex = vertex
        self.start_times[vertex] = clock

    def get_vertex(self, clock: int) -> graphs.vertices.Vertex:
        return self.execution.get(clock)


class Execution:
    def __init__(self, task: graphs.tasks.Task, number_of_cores: int):
        self.task = task
        self.cores = [Core(i) for i in range(number_of_cores)]
        self.clock = 0
        self.choices = 0

    def run(self, prioritize_critical: bool):
        execution_times = {v: self.task.get_time(v) for v in self.task.its_vertices}
        if not prioritize_critical:
            candidates = [v for v in self.task.its_vertices if execution_times[v] > 1]
            if candidates:
                v = random.choice(candidates)
                execution_times[v] -= 1

        critical = None
        if prioritize_critical:
            ordering = graphs.graphs.jail_and_free(self.task)
            critical = {}
            for v in reversed(ordering):
                if v == self.task.exit_vertex:
                    critical[v] = self.task.get_time(v)
                else:
                    most = 0
                    for e in self.task.successors[v]:
                        most = max(most, critical[e.point_b])
                    critical[v] = self.task.get_time(v) + most

        ready = [self.task.entry_vertex]
        jailed = {v: 0 for v in self.task.its_vertices}
        available = self.cores[:]
        executing = set()

        while ready or executing:
            if prioritize_critical:
                ready.sort(key=lambda v: critical[v])
                print("; ".join(f"{vertex}: {critical[vertex]}" for vertex in ready))
            else:
                random.shuffle(ready)

            n = min(len(ready), len(available))
            if n:
                for c, v in zip(available[-n:], ready[:n]):
                    c.set_vertex(v, self.clock)
                    executing.add(v)
                del available[-n:]
                del ready[:n]

            if ready and len(self.cores) > 1:
                self.choices += 1

            self.clock += 1
            for c in self.cores:
                c.execute(self.clock)

            finished = [
                c for c in self.cores
                if c.vertex is not None
                   and self.clock - c.start_times[c.vertex] >= execution_times[c.vertex]
            ]

            for c in finished:
                v = c.vertex
                available.append(c)
                executing.discard(v)
                for e in self.task.successors[v]:
                    s = e.point_b
                    jailed[s] += 1
                    if jailed[s] == len(self.task.predecessors[s]):
                        ready.append(s)
                c.vertex = None

    def __str__(self):
        header = ["Time"] + [f"{core.identifier:>3}" for core in self.cores]
        width = sum(map(len, header)) + len(header) - 1

        lines = [
            "-" * width,
            "Time|" + " ".join(f"{core.identifier:>3}" for core in self.cores),
            "-" * width
        ]

        for tick in range(1, self.clock + 1):
            row = []
            for core in self.cores:
                vertex = core.get_vertex(tick)
                if vertex is None:
                    row.append(f"{'':>3}")
                else:
                    row.append(f"{vertex.identifier:>3}")
            lines.append(f"{tick:>4}|{' '.join(row)}")

        return "\n".join(lines)


def parse_the_command_line() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Analyse DAG tasks")

    parser.add_argument(
        "--cores", type=int, default=2, metavar="<INT>", help="number of cores"
    )

    parser.add_argument(
        "--executions", type=int, default=1, metavar="<INT>", help="number of executions to try"
    )

    parser.add_argument(
        "--file", type=pathlib.Path, required=True, metavar="<FILE>", help="read tasks JSON from this file"
    )

    return parser.parse_args()


def main():
    args = parse_the_command_line()
    tasks = graphs.graph_input.read_tasks(args.file)

    for task in tasks:
        task.dotify(f"{task.name}")
        worst_e = None
        for _ in range(args.executions):
            e = Execution(task, args.cores)
            e.run(False)
            if worst_e is None:
                worst_e = e
            elif e.clock > worst_e.clock:
                worst_e = e

        critical_e = Execution(task, args.cores)
        critical_e.run(True)

        if worst_e.clock > critical_e.clock:
            print(task)
            print(worst_e)
            print(critical_e)

if __name__ == '__main__':
    main()
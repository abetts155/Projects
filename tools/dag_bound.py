from argparse import ArgumentParser, Namespace
from collections import deque
from concurrent.futures import ProcessPoolExecutor, as_completed
from enum import Enum, auto
from functools import partial
from glob import glob
from math import ceil, floor
from os import walk
from os.path import abspath, basename, join, dirname
from random import randint, random, shuffle, sample
from sys import exit
from typing import Dict, List, Tuple

import matplotlib.pyplot as plt
from matplotlib.ticker import FixedLocator, MaxNLocator

from utils import dot


def error_and_quit(message: str):
    print(message)
    exit(1)


class Generator:
    def __init__(self):
        self.min_vertices = 10
        self.max_vertices = 20
        self.min_wcet = 1
        self.max_wcet = 10
        self.tasks = 0
        self.unstructured = False

    def update(self, args: Namespace):
        self.min_wcet = args.min_wcet
        self.max_wcet = args.max_wcet
        self.min_vertices = args.min_vertices
        self.max_vertices = args.max_vertices
        self.tasks = args.tasks
        self.unstructured = args.unstructured
        self.check()

    def check(self):
        if self.max_vertices < self.min_vertices:
            error_and_quit('Wrong')
        if self.min_vertices < 4:
            error_and_quit('Wrong')
        if self.tasks < 0:
            error_and_quit('Wrong')
        if self.max_wcet < self.min_wcet:
            error_and_quit('Wrong')
        if self.min_wcet < 1:
            error_and_quit('Wrong')


generator = Generator()


def parse_command_line():
    parser = ArgumentParser(description='Compute bounds on DAG tasks')

    parser.add_argument('-F',
                        '--file',
                        nargs='+',
                        help='read hand-crafted tasks from this file',
                        metavar='<FILE>')

    parser.add_argument('-D',
                        '--directory',
                        help='read GML files from this directory',
                        metavar='<FILE>')

    parser.add_argument('-C',
                        '--cores',
                        type=int,
                        help='number of cores',
                        default=1,
                        metavar='<INT>')

    parser.add_argument('-T',
                        '--tasks',
                        type=int,
                        help='automatically generate this number of artificial tasks',
                        default=generator.tasks,
                        metavar='<INT>')

    parser.add_argument('-LV',
                        '--min-vertices',
                        type=int,
                        help='the minimum number of vertices in an artificial task',
                        default=generator.min_vertices,
                        metavar='<INT>')

    parser.add_argument('-UV',
                        '--max-vertices',
                        type=int,
                        help='the maximum number of vertices in an artificial task',
                        default=generator.max_vertices,
                        metavar='<INT>')

    parser.add_argument('-LW',
                        '--min-wcet',
                        type=int,
                        help='the minimum WCET of a vertex in an artificial task',
                        default=generator.min_wcet,
                        metavar='<INT>')

    parser.add_argument('-UW',
                        '--max-wcet',
                        type=int,
                        help='the maximum WCET of a vertex in an artificial task',
                        default=generator.max_wcet,
                        metavar='<INT>')

    parser.add_argument('-U',
                        '--unstructured',
                        action='store_true',
                        help='allow tasks to be unstructured',
                        default=False)

    return parser.parse_args()


class Path(Enum):
    LONGEST = auto()
    HEAVIEST = auto()


class Network:
    def __init__(self):
        self.vertices = []
        self.successors = {}
        self.predecessors = {}
        self.wcets = {}
        self.entry = None
        self.exit = None
        self.english_alphabet = True

    def add_vertex(self, vertex: int):
        if vertex not in self.vertices:
            self.vertices.append(vertex)
            self.predecessors[vertex] = set()
            self.successors[vertex] = set()
            self.wcets[vertex] = 0
            if vertex < ord('a') or ord('z') < vertex:
                self.english_alphabet = False

    def remove_vertex(self, vertex: int):
        self.vertices.remove(vertex)
        del self.predecessors[vertex]
        del self.successors[vertex]
        del self.wcets[vertex]

    def add_edge(self, predecessor: int, successor: int):
        self.add_vertex(predecessor)
        self.add_vertex(successor)
        self.successors[predecessor].add(successor)
        self.predecessors[successor].add(predecessor)

    def remove_edge(self, predecessor: int, successor: int):
        self.successors[predecessor].remove(successor)
        self.predecessors[successor].remove(predecessor)

    def set_entry(self):
        origins = {vertex for vertex in self.vertices if not self.predecessors[vertex]}
        (self.entry,) = origins

    def set_exit(self):
        reverse_origins = {vertex for vertex in self.vertices if not self.successors[vertex]}
        (self.exit,) = reverse_origins

    def set_wcet(self, vertex: int, wcet: int):
        assert vertex in self.vertices and wcet >= 1
        self.wcets[vertex] = wcet

    def get_wcet(self, vertex: int) -> int:
        return self.wcets[vertex]

    def topological_ordering(self):
        jailed = {vertex: 0 for vertex in self.vertices}
        order = []
        freed = deque([self.entry])
        while freed:
            vertex = freed.popleft()
            order.append(vertex)
            for successor in self.successors[vertex]:
                jailed[successor] += 1
                if jailed[successor] == len(self.predecessors[successor]):
                    freed.append(successor)
        return order

    def copy(self) -> "Network":
        reflection = Network()
        reflection.english_alphabet = self.english_alphabet

        for vertex in self.vertices:
            reflection.add_vertex(vertex)
            reflection.wcets[vertex] = self.wcets[vertex]

        for vertex in self.vertices:
            for successor in self.successors[vertex]:
                reflection.add_edge(vertex, successor)

        reflection.set_entry()
        reflection.set_exit()
        assert reflection.entry == self.entry and reflection.exit == self.exit
        return reflection

    def canonicalise(self) -> "Network":
        assert self.entry is not None and self.exit is not None
        canonical = self.copy()
        dead = set()
        for vertex in self.vertices:
            if len(self.predecessors[vertex]) == 1:
                (predecessor,) = self.predecessors[vertex]
                if len(self.successors[predecessor]) == 1:
                    dead.add(vertex)

        for vertex in dead:
            (predecessor,) = canonical.predecessors[vertex]
            canonical.successors[predecessor].remove(vertex)
            canonical.wcets[predecessor] += canonical.wcets[vertex]
            for successor in canonical.successors[vertex]:
                canonical.successors[predecessor].add(successor)
                canonical.predecessors[successor].add(predecessor)
                canonical.predecessors[successor].remove(vertex)

            canonical.remove_vertex(vertex)

            if vertex == canonical.exit:
                canonical.exit = predecessor

        return canonical

    def compute_immediate_dominators(self, root: int) -> Dict[int, int]:
        assert root in [self.entry, self.exit]
        if root == self.entry:
            forwards = self.successors
            backwards = self.predecessors
        else:
            forwards = self.predecessors
            backwards = self.successors

        def answer_query(left: List[int], right: List[int]):
            i = min(len(left) - 1, len(right) - 1)
            while i >= 0 and left[i] != right[i]:
                i -= 1
            return left[:i + 1]

        data = {}
        idom = {}
        explored = {vertex: 0 for vertex in self.vertices}
        queue = deque([root])
        joins = set()
        while queue:
            vertex = queue.popleft()
            if not queue and not joins:
                data[vertex] = [vertex]
            elif len(forwards[vertex]) > 1:
                data[vertex].append(vertex)

            for forward in forwards[vertex]:
                explored[forward] += 1

                if explored[forward] == len(backwards[forward]):
                    queue.append(forward)

                    if len(backwards[forward]) == 1:
                        (backward,) = backwards[forward]
                        idom[forward] = backward
                        data[forward] = data[backward][:]
                    else:
                        for backward in backwards[forward]:
                            if forward not in data:
                                data[forward] = data[backward][:]
                            else:
                                data[forward] = answer_query(data[forward], data[backward])

                        idom[forward] = data[forward][-1]
                        joins.remove(forward)
                elif len(backwards[forward]) > 1:
                    joins.add(forward)

        return idom

    def get_volume(self):
        return sum(self.wcets.values())

    def evaluate_path(self, path: List[int], path_type: Path) -> int:
        return sum([self.wcets[vertex] if path_type == Path.HEAVIEST else 1 for vertex in path])

    def find_path(self, path_type: Path) -> List[int]:
        paths = {}
        for vertex in self.topological_ordering():
            if vertex == self.entry:
                paths[vertex] = [vertex]
            else:
                worst = None
                for predecessor in self.predecessors[vertex]:
                    if worst is None:
                        worst = predecessor
                    elif self.evaluate_path(paths[predecessor], path_type) > self.evaluate_path(paths[worst], path_type):
                        worst = predecessor
                paths[vertex] = paths[worst] + [vertex]
        return paths[self.exit]

    def jail_and_free(self):
        explored = {vertex: 0 for vertex in self.vertices}
        parents = {}
        children = {vertex: 0 for vertex in self.vertices}
        levels = {self.entry: 0}
        ordering = {0: {self.entry}}
        articulations = set()
        joins = set()

        queue = deque([self.entry])
        while queue:
            vertex = queue.popleft()

            if len(self.predecessors[vertex]) > 1:
                joins.remove(vertex)

            if not queue and not joins:
                articulations.add(vertex)

            for successor in self.successors[vertex]:
                explored[successor] += 1

                if len(self.predecessors[successor]) > 1:
                    joins.add(successor)

                if explored[successor] == len(self.predecessors[successor]):
                    queue.append(successor)
                    candidates = [predecessor for predecessor in self.predecessors[successor]
                                  if levels[predecessor] == levels[vertex]]
                    candidates.sort(key=lambda vertex: children[vertex], reverse=True)
                    last = candidates[-1]
                    parents[successor] = last
                    children[last] += 1
                    levels[successor] = levels[last] + 1
                    ordering.setdefault(levels[successor], set()).add(successor)

        return ordering, parents, articulations

    def reduce(self) -> Tuple[bool, List[int]]:
        reflection = self.copy()
        pending = {}
        worklist = deque()
        for vertex in reflection.vertices:
            if len(reflection.predecessors[vertex]) == 1 and len(reflection.successors[vertex]) == 1:
                worklist.append(vertex)
                pending[vertex] = True
            else:
                pending[vertex] = False

        ordering = []
        while worklist:
            vertex = worklist.popleft()
            ordering.append(vertex)
            (predecessor,) = reflection.predecessors[vertex]
            (successor,) = reflection.successors[vertex]
            reflection.remove_edge(predecessor, vertex)
            reflection.remove_edge(vertex, successor)
            reflection.add_edge(predecessor, successor)
            reflection.vertices.remove(vertex)

            if len(reflection.predecessors[predecessor]) == 1 and len(reflection.successors[predecessor]) == 1:
                if not pending[predecessor]:
                    worklist.append(predecessor)
                    pending[predecessor] = True

            if len(reflection.predecessors[successor]) == 1 and len(reflection.successors[successor]) == 1:
                if not pending[successor]:
                    worklist.append(successor)
                    pending[successor] = True

        return len(reflection.vertices) == 2, ordering

    def dotify(self, filename):
        data = []
        for vertex in self.vertices:
            label = [dot.HTML.open_html,
                     dot.HTML.open_table,
                     dot.HTML.open_row,
                     dot.HTML.open_cell(),
                     '{} ({})'.format(chr(vertex) if self.english_alphabet else vertex, self.wcets[vertex]),
                     dot.HTML.close_cell,
                     dot.HTML.close_row,
                     dot.HTML.close_table,
                     dot.HTML.close_html]
            data.append('{} [label={}];\n'.format(vertex, ''.join(label)))

            for successor in self.successors[vertex]:
                data.append('{}->{};\n'.format(vertex, successor))

        dot.generate(filename + '.dot', data)

    def __iter__(self):
        for vertex in self.vertices:
            yield vertex

    def __str__(self):
        value = ''
        for vertex in self.vertices:
            for successor in self.successors[vertex]:
                value += '{} => {}\n'.format(chr(vertex) if self.english_alphabet else vertex,
                                             chr(successor) if self.english_alphabet else successor)
        value += '\n'
        for vertex in self.vertices:
            value += 'WCET({}): {}\n'.format(chr(vertex) if self.english_alphabet else vertex, self.wcets[vertex])
        value += '\n'
        return value


class Task:
    def __init__(self):
        self.filename = None
        self.identifier = None
        self.original = Network()
        self.canonical = None
        self.dead = False

    def set_identifier(self, identifier: int):
        self.identifier = identifier

    def prepare(self):
        self.original.set_entry()
        self.original.set_exit()

        for vertex in self.original.vertices:
            if vertex in [self.original.entry, self.original.exit]:
                self.original.set_wcet(vertex, 1)
            elif self.original.get_wcet(vertex) == 0:
                wcet = randint(generator.min_wcet, generator.max_wcet)
                self.original.set_wcet(vertex, wcet)

        self.canonical = self.original.canonicalise()
        if len(self.canonical.vertices) == 1:
            print('Task {} is dead: it only has one vertex in canonical form'.format(self.identifier))
            self.dead = True


class Core:
    def __init__(self, identifier: int):
        self.identifier = identifier
        self.execution = {}
        self.vertex = None
        self.start_times = {}

    def execute(self, clock: int):
        self.execution[clock] = self.vertex

    def get_vertex(self, clock: int):
        if clock in self.execution:
            return self.execution[clock]
        else:
            return None

    def set_vertex(self, vertex: int, clock: int):
        self.vertex = vertex
        self.start_times[vertex] = clock


class Execution:
    def __init__(self, task: Task, number_of_cores: int, use_wcets: bool = True):
        self.task = task
        self.number_of_cores = number_of_cores
        self.cores = [Core(core_id) for core_id in range(number_of_cores)]
        self.clock = 0
        self.execution_times = {}
        self.jailed = {}
        self.choices = 0
        for vertex in self.task:
            self.jailed[vertex] = 0
            self.execution_times[vertex] = self.task.get_wcet(vertex)

        if not use_wcets:
            candidates = [vertex for vertex in self.task if self.execution_times[vertex] > 1]
            if candidates:
                (vertex,) = sample(candidates, 1)
                self.execution_times[vertex] -= 1

    def go(self):
        ready = [self.task.entry]
        available = [core for core in self.cores]
        executing = set()
        while ready or executing:
            if self.number_of_cores == 0 and len(ready) > len(available):
                core_id = len(self.cores) - 1
                needed = len(ready) - len(available)
                while needed > 0:
                    needed -= 1
                    core_id += 1
                    core = Core(core_id)
                    available.append(core)
                    self.cores.append(core)

            shuffle(available)
            while available and ready:
                core = available.pop()
                vertex = ready.pop()
                core.set_vertex(vertex, self.clock)
                executing.add(vertex)

            if ready and len(self.cores) > 1:
                self.choices += 1

            self.clock += 1
            changed = False
            for core in self.cores:
                #core.execute(self.clock)
                if core.vertex is not None:
                    if self.clock - core.start_times[core.vertex] == self.execution_times[core.vertex]:
                        available.append(core)
                        executing.remove(core.vertex)
                        for successor in self.task.successors[core.vertex]:
                            self.jailed[successor] += 1
                            if self.jailed[successor] == len(self.task.predecessors[successor]):
                                ready.append(successor)
                                changed = True
                        core.vertex = None

            if changed:
                shuffle(ready)

    def __str__(self):
        time_label = 'Time'
        core_labels = ['{:>3}'.format(core.identifier) for core in self.cores]
        column_labels = [time_label] + core_labels
        width = sum([len(column) for column in column_labels]) + len(column_labels) - 1
        value = '-' * width + '\n'
        value += '-' * width + '\n'
        value += 'Time|{}'.format(' '.join(core_labels)) + '\n'
        value += '-' * width + '\n'
        for tick in range(1, self.clock + 1):
            core_executions = ['{:>3}'.format('{}'.format(chr(core.get_vertex(tick)) if self.task.english_alphabet else core.get_vertex(tick)) if core.get_vertex(tick) else '')
                               for core in self.cores]
            value += '{:>4}|{}'.format(tick, ' '.join(core_executions)) + '\n'
        return value



class TaskGenerator:
    def __init__(self, identifier: int):
        self.task = Task()
        self.task.set_identifier(identifier)
        self.add_vertices()
        self.regions = []
        self.parition_vertices()
        self.connect_regions()
        self.make_good()
        if generator.unstructured:
            self.break_structure()
        self.guarantee_life()

    def add_vertices(self):
        number_of_vertices = randint(generator.min_vertices, generator.max_vertices)
        base = ord('a')
        for i in range(base, base + number_of_vertices + 1):
            self.task.original.add_vertex(i)
            wcet = randint(generator.min_wcet, generator.max_wcet)
            self.task.original.set_wcet(i, wcet)

    def parition_vertices(self):
        shuffle(self.task.original.vertices)
        self.task.original.entry = self.task.original.vertices[0]
        self.task.original.exit = self.task.original.vertices[1]
        partition = [[]]
        for i in range(2, len(self.task.original.vertices)):
            if len(partition[-1]) < 3 or random() > 0.5:
                partition[-1].append(self.task.original.vertices[i])
            else:
                partition.append([self.task.original.vertices[i]])

        if len(partition[-1]) < 3:
            surplus = partition.pop()
            partition[-1].extend(surplus)

        for vertex_set in partition:
            self.regions.append(Region(vertex_set))

    def connect_regions(self):
        shuffle(self.regions)
        while len(self.regions) > 1 and random() > 0.05:
            left = self.regions.pop()
            right = self.regions.pop()
            if len(right.entries) == 1 and len(right.exits) == 1 and (len(left.entries) > 1 or len(left.exits) > 1):
                right.connect(left)
                self.regions.append(right)
            else:
                left.connect(right)
                self.regions.append(left)

    def make_good(self):
        for region in self.regions:
            for (predecessor, successor) in region.edges:
                self.task.original.add_edge(predecessor, successor)

            for an_entry in region.entries:
                self.task.original.add_edge(self.task.original.entry, an_entry)
            for an_exit in region.exits:
                self.task.original.add_edge(an_exit, self.task.original.exit)

    def break_structure(self):
        ordering = self.task.original.topological_ordering()
        for i, vertex in enumerate(ordering):
            # The first vertex in the entry and the second vertex can only have one predecessor
            if i >= 2:
                candidates = list(range(0, i - 1))
                while candidates and random() > 0.25:
                    shuffle(candidates)
                    j = candidates.pop()
                    candidate = ordering[j]
                    if candidate not in self.task.original.predecessors[vertex]:
                        self.task.original.add_edge(candidate, vertex)

    def guarantee_life(self):
        if len(self.regions) == 1:
            if self.task.original.exit not in self.task.original.successors[self.task.original.entry]:
                self.task.original.add_edge(self.task.original.entry, self.task.original.exit)


def analyse(task: Task, number_of_cores: int):
    longest_path = task.canonical.find_path(Path.LONGEST)
    longest = task.canonical.evaluate_path(longest_path, Path.LONGEST)
    heaviest_path = task.canonical.find_path(Path.HEAVIEST)
    heaviest = task.canonical.evaluate_path(heaviest_path, Path.HEAVIEST)
    volume = task.canonical.get_volume()
    worst = Execution(task.canonical, number_of_cores, True)
    worst.go()
    assert worst.choices == 0

    task.canonical.dotify('task.{}.canonical'.format(task.identifier))

    graham = ceil(heaviest + (volume - heaviest) / number_of_cores)
    proportion = floor(100 * worst.clock / graham)
    of_volume = floor(100 * graham / volume)

    print('Vertices: {}'.format(len(task.canonical.vertices)))
    print('Longest:  {}'.format(longest))
    print('Heaviest: {}'.format(heaviest))
    print('Volume:   {}'.format(volume))
    print('WCET:     {}'.format(worst.clock))
    print('Graham:   {}'.format(graham))
    print('WCET:     {}% within Graham'.format(proportion))
    print('Graham:   {:.2f}% of volume'.format(of_volume))

    choiceless = Execution(task.canonical, 0, True)
    choiceless.go()

    assert len(choiceless.cores) <= number_of_cores
    if len(choiceless.cores) == number_of_cores:
        print('Same: {} == {}'.format(number_of_cores, len(choiceless.cores)))
    else:
        print('Diff: {} > {}'.format(number_of_cores, len(choiceless.cores)))

    for _ in range(100):
        other = Execution(task.canonical, number_of_cores, False)
        other.go()
        if other.clock > worst.clock:
            print('{} > {}'.format(other.clock, worst.clock))
            task.original.dotify('task.{}'.format(task.identifier))
            task.canonical.dotify('task.{}.canonical'.format(task.identifier))
            assert False


def reduce(network: Network) -> Tuple[Network, Dict[Tuple[int, int], List[int]]]:
    reduction = network.copy()
    labels = {}
    pending = {}
    worklist = deque()
    for vertex in reduction.vertices:
        if len(reduction.predecessors[vertex]) == 1 and len(reduction.successors[vertex]) == 1:
            worklist.append(vertex)
            pending[vertex] = True
        else:
            pending[vertex] = False

        for successor in reduction.successors[vertex]:
            labels[(vertex, successor)] = set()

    while worklist:
        vertex = worklist.popleft()
        (predecessor,) = reduction.predecessors[vertex]
        (successor,) = reduction.successors[vertex]
        reduction.remove_edge(predecessor, vertex)
        reduction.remove_edge(vertex, successor)
        reduction.add_edge(predecessor, successor)
        reduction.vertices.remove(vertex)

        key_a_to_b = (predecessor, vertex)
        key_b_to_c = (vertex, successor)
        key_a_to_c = (predecessor, successor)

        options = [{vertex}, labels[key_a_to_b], labels[key_b_to_c]]
        options.sort(key=lambda x: len(x))
        if key_a_to_c not in labels:
            labels[key_a_to_c] = options[-1]
        else:
            labels[key_a_to_c].update(options[-1])

        del labels[key_a_to_b]
        del labels[key_b_to_c]

        if len(reduction.predecessors[predecessor]) == 1 and len(reduction.successors[predecessor]) == 1:
            if not pending[predecessor]:
                worklist.append(predecessor)
                pending[predecessor] = True

        if len(reduction.predecessors[successor]) == 1 and len(reduction.successors[successor]) == 1:
            if not pending[successor]:
                worklist.append(successor)
                pending[successor] = True

    return reduction, labels


def compute_minimum_cores(canonical: Network, reduction: Network, labels: Dict[Tuple[int, int], List[int]]) -> int:
    if len(reduction.vertices) == 2:
        master_key = (canonical.entry, canonical.exit)
        return len(labels[master_key])
    else:
        ordering, parents, articulations = canonical.jail_and_free()
        children = {vertex: [] for vertex in canonical.vertices}
        data = {}
        for level in reversed(ordering.keys()):
            for vertex in ordering[level]:
                if vertex != canonical.entry:
                    children[parents[vertex]].append(vertex)

                golden_children = [data[child] for child in children[vertex] if child not in articulations]
                if not golden_children:
                    data[vertex] = 1
                else:
                    data[vertex] = sum(golden_children)

        cores = 1
        for vertex in articulations:
            if vertex in reduction.vertices:
                if len(reduction.successors[vertex]) == 1:
                    (successor,) = reduction.successors[vertex]
                    key = (vertex, successor)
                    cores = max(cores, len(labels[key]))
                else:
                    cores = max(cores, data[vertex])
        return cores


def analyse_tasks(tasks: List[Task], number_of_cores: int):
    for task in tasks:
        print('*' * 80)
        print('Analysing task {}...'.format(task.identifier))
        task.prepare()

        if not task.dead:
            reduction, labels = reduce(task.canonical)
            minimum_cores = compute_minimum_cores(task.canonical, reduction, labels)
            print('Minimum cores is {}'.format(minimum_cores))
            analyse(task, minimum_cores)
        print()

    if False:
        bins = {x: 0 for x in range(1, 101)}
        alive = set()
        choiceless = set()

        with ProcessPoolExecutor(max_workers=8) as executor:
            worker = partial(analyse, number_of_cores=cores[task])
            futures = {executor.submit(worker, task): task for task in tasks}
            for future in as_completed(futures):
                task = futures[future]
                execution = future.result()
                if not task.dead:
                    alive.add(task)
                    if not execution.choices:
                        choiceless.add(task)

        proportion = round(100 * len(choiceless) / len(alive))
        print('On {} cores, {}% tasks have deterministic response times'.format(number_of_cores, proportion))


def set_up_matplotlib():
    plt.style.use('seaborn-paper')
    plt.rcParams['font.family'] = 'Helvetica'
    plt.rcParams['font.size'] = 10
    plt.rcParams['axes.labelsize'] = 10
    plt.rcParams['axes.labelweight'] = 'bold'
    plt.rcParams['axes.titlesize'] = 14
    plt.rcParams['xtick.labelsize'] = 8
    plt.rcParams['ytick.labelsize'] = 8
    plt.rcParams['legend.fontsize'] = 10
    plt.rcParams['figure.titlesize'] = 14
    plt.rcParams['figure.figsize'] = (9, 6)


def show(cores: int, tasks: List[Task], bins):
    fig, ax = plt.subplots(nrows=1, ncols=1, constrained_layout=True)
    x_values, y_values = zip(*bins.items())
    min_y = min(y_values)
    max_y = max(y_values)
    scaled = [(y - min_y) / (max_y - min_y) for y in y_values]
    cmap = plt.get_cmap('Wistia')
    ax.set_frame_on(False)
    ax.bar(x_values, y_values, align='edge', width=1, edgecolor='black', color=cmap(scaled))
    ax.xaxis.set_major_locator(FixedLocator([0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]))
    ax.yaxis.set_major_locator(MaxNLocator(integer=True))
    ax.set_ylim([0, max_y + 1])
    ax.set_ylabel('Frequency')
    ax.set_xlabel("Percentage of estimated bound to Graham's bound")
    ax.set_title('Analysing {} tasks assuming {} cores'.format(len(tasks), cores))
    plt.show()


def read_gml(filename: str):
    task = Task()
    task.filename = filename
    last_id_line = None
    last_source_line = None
    read_node = False
    read_edge = False
    with open(filename) as in_file:
        for line in in_file:
            line = line.strip().lower()

            if line.startswith('id') and not read_node and not read_edge:
                _, x = line.split()
                task.set_identifier(int(x))

            if line.startswith('node'):
                read_node = True
                read_edge = False

            if line.startswith('edge'):
                read_node = False
                read_edge = True

            if read_node:
                if line.startswith('id'):
                    last_id_line = line
                elif line.startswith('c'):
                    _, a = last_id_line.split()
                    if a.isalpha():
                        vertex = ord(a)
                    else:
                        vertex = int(a)
                    _, b = line.split()
                    wcet = int(b)
                    task.original.add_vertex(vertex)
                    task.original.set_wcet(vertex, wcet)
                    last_id_line = None
                    read_node = False

            if read_edge:
                if line.startswith('source'):
                    last_source_line = line
                elif line.startswith('target'):
                    _, c = last_source_line.split()
                    if c.isalpha():
                        predecessor = ord(c)
                    else:
                        predecessor = int(c)

                    _, d = line.split()
                    if d.isalpha():
                        successor = ord(d)
                    else:
                        successor = int(d)

                    task.original.add_edge(predecessor, successor)
                    last_source_line = None
                    read_edge = False

    return task


def read_gmls(gml_files: List[str]):
    tasks = []
    if len(gml_files) > 32:
        with ProcessPoolExecutor(max_workers=8) as executor:
            futures = [executor.submit(read_gml, gml_file) for gml_file in gml_files]
            for future in as_completed(futures):
                task = future.result()
                tasks.append(task)
    else:
        for gml_file in gml_files:
            tasks.append(read_gml(gml_file))

    for id, task in enumerate(tasks):
        if task.identifier is None:
            task.set_identifier(id)

    return tasks


def main():
    args = parse_command_line()
    generator.update(args)
    set_up_matplotlib()

    max_identifier = 1
    if args.file:
        tasks = read_gmls(args.file)
        analyse_tasks(tasks, args.cores)

    if args.directory:
        dir_to_gml_files = {}
        for root, _, _ in walk(abspath(args.directory)):
            gml_files = glob(join(root, '*.gml'))

            if gml_files:
                directory = None
                if basename(root).isdigit():
                    directory = dirname(root)
                else:
                    directory = root

                dir_to_gml_files.setdefault(directory, []).extend(gml_files)

        for directory, gml_files in dir_to_gml_files.items():
            tasks = read_gmls(gml_files)
            print("Gathered {} tasks from '{}'".format(len(tasks), basename(directory)))
            analyse_tasks(tasks, args.cores)

    if generator.tasks > 0:
        tasks = []
        for identifier in range(max_identifier, generator.tasks + max_identifier):
            task_generator = TaskGenerator(identifier)
            tasks.append(task_generator.task)

        analyse_tasks(tasks, args.cores)


if __name__ == '__main__':
    main()

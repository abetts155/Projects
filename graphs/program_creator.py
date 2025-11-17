import argparse
import logging
import pathlib

from rich.logging import RichHandler
import prompt_toolkit
import prompt_toolkit.completion

import graphs.edges
import graphs.graph_input
import graphs.graphs
import graphs.vertices

logging.basicConfig(
    level=logging.DEBUG,
    format="%(levelname)s:%(asctime)s:%(message)s",
    datefmt="%H:%M:%S",
    handlers=[RichHandler()]
)
logger = logging.getLogger(__name__)


def parse_the_command_line() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description='Construct a program by hand')

    parser.add_argument(
        '--cfgs',
        type=pathlib.Path,
        help='write the CFGs to this file',
        metavar='<FILE>.json',
        default=pathlib.Path("cfgs.json")
    )

    parser.add_argument(
        '--calls',
        type=pathlib.Path,
        help='write the call graph to this file',
        metavar='<FILE>.json',
        default=pathlib.Path("calls.json")
    )

    parser.add_argument(
        '-I',
        '--instrument',
        type=bool,
        help='instrument the program',
        default=False
    )

    return parser.parse_args()


def input_edge(cfg: graphs.graphs.ControlFlowGraph, vertices: dict[str, graphs.vertices.Vertex]) -> bool:
    line = input("Enter a new edge (an empty line means no more edges) ").strip()
    edge_set_size = len(cfg.its_edges)
    if line:
        identifiers = line.split()
        if len(identifiers) != 2:
            print("⚠️ Invalid input: please enter exactly two identifiers separated by spaces.")
            return False

        a, b = identifiers
        if not graphs.vertices.identifiers_regex.match(a) or not graphs.vertices.identifiers_regex.match(b):
            print(f"⚠️ Invalid input: identifiers must match the pattern {graphs.vertices.identifiers_pattern}.")
            return False

        if a not in vertices:
            vertices[a] = graphs.vertices.Vertex(a)

        if b not in vertices:
            vertices[b] = graphs.vertices.Vertex(b)

        vertex_a = vertices[a]
        if vertex_a not in cfg.its_vertices:
            cfg.add_vertex(vertex_a)

        vertex_b = vertices[b]
        if vertex_b not in cfg.its_vertices:
            cfg.add_vertex(vertex_b)

        edge = graphs.edges.Edge(vertex_a, vertex_b)
        if edge not in cfg.its_edges:
            cfg.add_edge(edge)

    return edge_set_size != len(cfg.its_edges)


def input_cfg(cfg_name: str, vertices: dict[str, graphs.vertices.Vertex]):
    cfg = graphs.graphs.ControlFlowGraph(cfg_name)
    print(f"Enter the edges of CFG {cfg_name}")
    finished = False
    while not finished:
        try:
            cfg.set_entry_and_exit()
        except graphs.graphs.EntryExitError:
            more_edges_required = True
        else:
            more_edges_required = False

        if more_edges_required:
            _ = input_edge(cfg, vertices)
        else:
            print(f"✅ The CFG is valid")
            finished = not input_edge(cfg, vertices)

    return cfg


def create_cfgs() -> list[graphs.graphs.ControlFlowGraph]:
    vertices = {}
    cfgs = []
    cfg_identifier = 1
    while True:
        cfg_name = f"C{cfg_identifier}"
        cfg_identifier += 1
        cfg = input_cfg(cfg_name, vertices)
        cfgs.append(cfg)
        cfg.dotify(f"{cfg_name}.cfg")

        answer = input("Another CFG? (Only 'y/Y' means Yes) ")
        if not answer.strip() in ['y', 'Y']:
            break

    return cfgs


class AutoSelectCompleter(prompt_toolkit.completion.Completer):
    def __init__(self, choices: list, prompt_text: str):
        self.choices = [str(choice) for choice in choices]
        self.choices.sort()
        self.prompt_text = prompt_text

    def get_completions(self, document, complete_event):
        text = document.text.strip().lower()
        matches = [c for c in self.choices if c.lower().startswith(text)]

        if matches:
            first_match = matches.pop(0)
            yield prompt_toolkit.completion.Completion(first_match, start_position=-len(text))

            for match in matches:
                yield prompt_toolkit.completion.Completion(match, start_position=-len(text))

    def choose(self):
        while True:
            selection = prompt_toolkit.prompt(f"{self.prompt_text}: ", completer=self)
            if selection in self.choices:
                return selection
            print("Invalid selection. Please select from the given options.")


def create_call_graph(cfgs: list[graphs.graphs.ControlFlowGraph]) -> graphs.graphs.CallGraph:
    call_graph = graphs.graphs.CallGraph()

    cfg_choices = []
    for cfg in cfgs:
        cfg_choices.append(cfg.name)
        call_vertex = graphs.vertices.Vertex(cfg.name)
        call_graph.add_vertex(call_vertex)

    cfg_completer = AutoSelectCompleter(cfg_choices, 'Select the caller')
    caller_name = cfg_completer.choose()
    (caller_cfg,) = [cfg for cfg in cfgs if cfg.name == caller_name]

    site_choices = [vertex for vertex in caller_cfg.its_vertices if len(caller_cfg.successors[vertex]) == 1]
    if site_choices:
        site_completer = AutoSelectCompleter(cfg_choices, 'Select the caller')
        site_id = site_completer.choose()

        cfg_completer = AutoSelectCompleter(cfg_choices, 'Select the callee')
        caller_name = cfg_completer.choose()

    return call_graph


def main(cfgs_file: pathlib.Path, call_graph_file: pathlib.Path, instrument: bool):
    cfgs = create_cfgs()
    graphs.graph_input.write_cfgs(cfgs_file, cfgs)

    if len(cfgs) > 1:
        call_graph = create_call_graph(cfgs)
        graphs.graph_input.write_call_graph(call_graph_file, call_graph)


if __name__ == '__main__':
    args = parse_the_command_line()
    main(args.cfgs, args.calls, args.instrument)

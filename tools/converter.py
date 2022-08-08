from argparse import ArgumentParser
from graphs import edges, graphs, vertices
from miscellaneous.helpful import error_message
from re import compile
from system import programs


def main(filename):
    subprogram_regex = compile(r'subprogram')
    edge_regex = compile(r'(?P<left>[a-z])\s+(?P<right>[a-z])')

    subprogram_data = {}
    subprograms = 0
    with open(filename, 'r') as in_file:
        for line in in_file:
            line = line.strip().lower()

            subprogram_match = subprogram_regex.match(line)
            if subprogram_match:
                subprograms += 1
                name = 's{}'.format(subprograms)
                subprogram_data[name] = set()

            edge_match = edge_regex.match(line)
            if edge_match:
                left = edge_match.group('left')
                right = edge_match.group('right')
                subprogram_data[name].add((ord(left), ord(right)))

    cfgs = []
    program = programs.Program(filename)
    offset = 0
    for name, edge_data in subprogram_data.items():
        if not edges:
            error_message('No edges found for subprogram {}'.format(name))
        else:
            cfg = graphs.ControlFlowGraph(program, name)
            cfgs.append(cfg)

            vertex_map = {}
            for left, right in edge_data:
                left_id = left + offset
                if left_id not in vertex_map:
                    vertex_map[left_id] = vertices.BasicBlock(left_id)
                    cfg.add_vertex(vertex_map[left_id])

                right_id = right + offset
                if right_id not in vertex_map:
                    vertex_map[right_id] = vertices.BasicBlock(right_id)
                    cfg.add_vertex(vertex_map[right_id])

                left_vertex = vertex_map[left_id]
                right_vertex = vertex_map[right_id]
                cfg.add_edge(edges.Edge(left_vertex, right_vertex))

            offset += 26

    for cfg in cfgs:
        call_vertex = vertices.SubprogramVertex(vertices.Vertex.get_vertex_id(), cfg.name)
        subprogram = programs.Subprogram(cfg, call_vertex)
        program.add_subprogram(subprogram)

    programs.IO.write(program, 'converted.' + filename)


def parse_command_line():
    parser = ArgumentParser(description='Convert a file into a valid program file')

    parser.add_argument('--file',
                        help='read the program from this file',
                        required=True)

    return parser.parse_args()


if __name__ == '__main__':
    args = parse_command_line()
    main(args.file)

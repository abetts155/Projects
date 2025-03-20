import json
import pydantic

from graph import edges
from graph import graphs
from graph import vertices


VertexField = pydantic.Field(min_length=1, pattern=vertices.identifiers_regex.pattern)


class SerialisedEdge(pydantic.BaseModel):
    a: str = VertexField
    b: str = VertexField


class SerialisedCFG(pydantic.BaseModel):
    cfg_name: str = pydantic.Field(min_length=1, pattern=vertices.identifiers_regex.pattern)
    edges: list[SerialisedEdge]


def read_cfgs(filename: str, name_filter: list[str] = None) -> dict[str, graphs.ControlFlowGraph]:
    vertex_map = {}
    type_adapter = pydantic.TypeAdapter(list[SerialisedCFG])
    cfgs = {}
    with open(filename, 'r') as in_file:
        json_data = json.load(in_file)
        program_data = type_adapter.validate_python(json_data)

        for cfg_data in program_data:
            if name_filter is None or cfg_data.name in name_filter:
                cfg = graphs.ControlFlowGraph(cfg_data.cfg_name)
                for edge_data in cfg_data.edges:
                    if edge_data.a not in vertex_map:
                        vertex_map[edge_data.a] = vertices.Vertex(edge_data.a)
                    if edge_data.b not in vertex_map:
                        vertex_map[edge_data.b] = vertices.Vertex(edge_data.b)

                    vertex_a = vertex_map[edge_data.a]
                    if vertex_a not in cfg.its_vertices:
                        cfg.add_vertex(vertex_a)

                    vertex_b = vertex_map[edge_data.b]
                    if vertex_b not in cfg.its_vertices:
                        cfg.add_vertex(vertex_b)

                    edge = edges.Edge(vertex_a, vertex_b)
                    cfg.add_edge(edge)

                cfg.set_entry_and_exit()

                if cfg.name not in cfgs:
                    cfgs[cfg.name] = cfg
                else:
                    raise ValueError(f"The CFG named '{cfg.name}' is not unique.")
    return cfgs


class SerialisedCallEdge(pydantic.BaseModel):
    caller: str = VertexField
    callee: str = VertexField
    site: str = VertexField


def read_call_graph(filename: str, cfgs: dict[str, graphs.ControlFlowGraph]) -> graphs.CallGraph:
    type_adapter = pydantic.TypeAdapter(list[SerialisedCallEdge])
    with open(filename, 'r') as in_file:
        json_data = json.load(in_file)
        call_edges_data = type_adapter.validate_python(json_data)

    vertex_map = dict()
    call_graph = graphs.CallGraph()
    for cfg in cfgs.values():
        call_vertex = vertices.Vertex(cfg.name)
        vertex_map[cfg.name] = call_vertex
        call_graph.add_vertex(call_vertex)

        for vertex in cfg.its_vertices:
            vertex_map[vertex.identifier] = vertex

    for call_edge_data in call_edges_data:
        if call_edge_data.caller not in cfgs:
            raise ValueError(f"There is no CFG named '{call_edge_data.a}' in the provided CFG list")

        if call_edge_data.callee not in cfgs:
            raise ValueError(f"There is no CFG named '{call_edge_data.b}' in the provided CFG list")

        caller = vertex_map[call_edge_data.caller]
        callee = vertex_map[call_edge_data.callee]
        site = vertex_map[call_edge_data.site]
        caller_cfg = cfgs[call_edge_data.caller]
        if site not in caller_cfg.its_vertices:
            raise ValueError(f"There is no call site '{site.identifier}' in the caller CFG {caller_cfg.name}")

        edge = edges.CallEdge(caller, callee, site)
        call_graph.add_edge(edge)

    call_graph.set_root()
    return call_graph

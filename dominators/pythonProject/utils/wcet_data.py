import dataclasses
import json
import pydantic
import re
import sympy
import typing

from graph import edges
from graph import graphs
from graph import vertices


def check_identifier_is_a_vertex(cfg: graphs.ControlFlowGraph, identifier: str):
    if cfg.get_vertex(identifier) is None:
        raise ValueError(f"There is no vertex identified by '{identifier}' in the CFG '{cfg.name}'.")


def check_identifier_is_neither_the_entry_nor_the_exit(cfg: graphs.ControlFlowGraph, identifier: str):
    if identifier == cfg.entry_vertex.identifier:
        raise ValueError(f"The entry vertex '{identifier}' of '{cfg.name} cannot be part of additional path "
                         f"information.")

    if identifier == cfg.exit_vertex.identifier:
        raise ValueError(f"The entry vertex '{identifier}' of '{cfg.name} cannot be part of additional path "
                         f"information.")


class LoopBound(pydantic.BaseModel):
    header: str
    level: int
    expr: str


class StaticData(pydantic.BaseModel):
    loop_bounds: list[LoopBound] = pydantic.Field(default_factory=list)

    def get_free_parameters(self):
        free_parameters = set()
        for bound in self.loop_bounds:
            free_parameters.update(sympy.sympify(bound.expr).free_symbols)
        return free_parameters


class ContextData(pydantic.BaseModel):
    wcets: dict[str, int] = pydantic.Field(default_factory=dict)
    loop_bounds: list[LoopBound] = pydantic.Field(default_factory=list)
    inclusive: dict[str, list[str]] = pydantic.Field(default_factory=dict)
    exclusive: dict[str, list[str]] = pydantic.Field(default_factory=dict)
    parameters: dict[str, int] = pydantic.Field(default_factory=dict)


def validate_path_constraints(cfg: graphs.ControlFlowGraph, context_data: ContextData):
    for identifier, blacklist in context_data.exclusive.items():
        vertex = cfg.get_vertex(identifier)
        subgraph = graphs.create_control_flow_subgraph(cfg, vertex)
        dead = set([cfg.get_vertex(other_identifier) for other_identifier in blacklist])
        subgraph.remove_dead_vertices(dead)

        if cfg.entry_vertex not in subgraph.its_vertices or cfg.exit_vertex not in subgraph.its_vertices:
            raise ValueError(f"The mutual exclusion relationships on {identifier} are not valid.")


def validate_vertices(cfg: graphs.ControlFlowGraph, context_data: ContextData):
    for identifier in context_data.wcets.keys():
        check_identifier_is_a_vertex(cfg, identifier)

    for bound in context_data.loop_bounds:
        check_identifier_is_a_vertex(cfg, bound.header)

    for identifier, whitelist in context_data.inclusive.items():
        check_identifier_is_a_vertex(cfg, identifier)
        check_identifier_is_neither_the_entry_nor_the_exit(cfg, identifier)
        for other_identifier in whitelist:
            check_identifier_is_a_vertex(cfg, other_identifier)
            check_identifier_is_neither_the_entry_nor_the_exit(cfg, other_identifier)

    for identifier, blacklist in context_data.exclusive.items():
        check_identifier_is_a_vertex(cfg, identifier)
        check_identifier_is_neither_the_entry_nor_the_exit(cfg, identifier)
        for other_identifier in blacklist:
            check_identifier_is_a_vertex(cfg, other_identifier)
            check_identifier_is_neither_the_entry_nor_the_exit(cfg, other_identifier)


class ContextQualifier(str):
    pattern: re.Pattern = re.compile(rf'^({vertices.identifiers_pattern})(\.{vertices.identifiers_pattern})*$')

    @classmethod
    def from_string(cls, value: str) -> "ContextQualifier":
        if not cls.pattern.fullmatch(value):
            raise ValueError(f"Invalid Context Qualifier '{value}'")
        return cls(value)

    @classmethod
    def from_call_vertex(cls, vertex: vertices.Vertex) -> "ContextQualifier":
        return cls(vertex.identifier)

    @classmethod
    def from_call_edge(cls, edge: edges.CallEdge) -> "ContextQualifier":
        return cls(f'{edge.point_a.identifier}.{edge.site.identifier}.{edge.point_b.identifier}')

    def last(self) -> str:
        parts = self.split('.')
        return parts[-1]


class TimingDataModel(pydantic.BaseModel):
    contexts: dict[str, ContextData]
    static: dict[str, StaticData]

    def get_context_bounds(self, qualifier: ContextQualifier, header: vertices.Vertex) -> typing.Optional[list[LoopBound]]:
        context_data = self.contexts.get(qualifier)
        if context_data is not None:
            return [bound for bound in context_data.loop_bounds if bound.header == header.identifier]

    def get_static_bounds(self, qualifier: ContextQualifier, header: vertices.Vertex) -> typing.Optional[list[LoopBound]]:
        static_data = self.static.get(qualifier)
        if static_data is not None:
            return [bound for bound in static_data.loop_bounds if bound.header == header.identifier]

    def get_bounds(self, qualifier: ContextQualifier, header: vertices.Vertex) -> list[LoopBound]:
        loop_bounds = []

        context_bounds = self.get_context_bounds(qualifier, header)
        if context_bounds is not None:
            loop_bounds.extend(context_bounds)

        no_context_qualifier = ContextQualifier(qualifier.last())
        static_bounds = self.get_static_bounds(no_context_qualifier, header)
        if static_bounds is not None:
            loop_bounds.extend(static_bounds)

        return loop_bounds

    @staticmethod
    def _get_bound(loop_bounds: list[LoopBound], header: vertices.Vertex, level: int):
        for bound in loop_bounds:
            if bound.header == header.identifier and bound.level == level:
                return sympy.sympify(bound.expr)

    def get_bound(self, qualifier: ContextQualifier, header: vertices.Vertex, level: int) -> typing.Optional[sympy.Expr]:
        context_data = self.contexts.get(qualifier)
        if context_data is not None:
            expr = TimingDataModel._get_bound(context_data.loop_bounds, header, level)
            if expr is not None:
                return expr

        static_data = self.static.get(qualifier.last())
        if static_data is not None:
            expr = TimingDataModel._get_bound(static_data.loop_bounds, header, level)
            if expr is not None:
                return expr

    def get_context_data(self, qualifier: ContextQualifier) -> typing.Optional[ContextData]:
        for key, context_data in self.contexts.items():
            if key == qualifier:
                return context_data

    def get_static_data(self, cfg: graphs.ControlFlowGraph) -> typing.Optional[StaticData]:
        for key, static_data in self.static.items():
            if key == cfg.name:
                return static_data

    def _validate_loop_bounds(self, qualifier: ContextQualifier, cfg: graphs.ControlFlowGraph):
        forest: graphs.LoopForest = graphs.build_loop_forest(cfg, graphs.GraphDirection.Forwards)
        loop: vertices.LoopVertex
        for loop, depth in forest.depths().items():
            if loop != forest.root:
                (header,) = loop.entries

                for bound in self.get_bounds(qualifier, header):
                    if bound.level < 1 or bound.level > depth:
                        raise ValueError(
                            f"In '{cfg.name}' the bound on the header {header.identifier} applies to loop-nesting "
                            f"level {bound.level} but that is outside the valid range of 1..{depth}"
                        )

                if self.get_bound(qualifier, header, 1) is None:
                    raise ValueError(
                        f"In '{cfg.name}' there is no bound on the header {header.identifier} at loop-nesting level "
                        f"level one. This bound is required: it is with respect to the loop's closest surrounding "
                        f"scope, which is either its parent loop or the function/procedure body."
                    )

    def validate(self, cfgs: dict[str, graphs.ControlFlowGraph]):
        for qualifier_string, context_data in self.contexts.items():
            qualifier = ContextQualifier.from_string(qualifier_string)
            cfg = cfgs[qualifier.last()]
            self._validate_loop_bounds(qualifier, cfg)
            validate_vertices(cfg, context_data)
            validate_path_constraints(cfg, context_data)


def load_timing_analysis_data(filename: str, cfgs: dict[str, graphs.ControlFlowGraph]) -> TimingDataModel:
    with open(filename, 'r') as in_file:
        json_data = json.load(in_file)
        data_model = TimingDataModel(**json_data)
        data_model.validate(cfgs)

    return data_model


def store_timing_analysis_data(filename: str, data_model: TimingDataModel):
    with open(filename, 'w') as out_file:
        out_file.write(data_model.model_dump_json(indent=4))
        out_file.write('\n' * 2)

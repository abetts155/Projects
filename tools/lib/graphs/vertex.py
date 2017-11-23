from . import edge


class InvalidIDError(ValueError):
    pass


class InvalidIndexError(ValueError):
    pass


class Vertex:
    def __init__(self, id: int):
        if not isinstance(id, int):
            raise InvalidIDError('Vertex ID {} is not an integer'.format(id))
        self._id = id

    @property
    def id(self):
        return self._id

    @id.setter
    def id(self, id):
        self._id = id

    def __hash__(self):
        return self.id


class SubprogramVertex(Vertex):
    def __init__(self, id, name):
        Vertex.__init__(self, id)
        self._name = name

    @property
    def name(self):
        return self._name


class ProgramPoint(Vertex):
    def __init__(self, id, e: edge.Edge):
        Vertex.__init__(self, id)
        self._edge = e

    @property
    def edge(self) -> edge.Edge:
        return self._edge


class SuperBlock(Vertex):
    def __init__(self, id):
        Vertex.__init__(self, id)
        self._vertices = set()

    @property
    def vertices(self) -> set:
        return self._vertices


class LoopBody(SuperBlock):
    def __init__(self, id, header: Vertex):
        SuperBlock.__init__(self, id)
        self._header = header

    @property
    def header(self):
        return self._header

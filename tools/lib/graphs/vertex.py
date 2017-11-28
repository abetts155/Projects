
class InvalidIDError(ValueError):
    pass


class InvalidIndexError(ValueError):
    pass


class Vertex:
    def __init__(self, id: int):
        if not isinstance(id, int):
            raise InvalidIDError('Vertex ID {} is not an integer'.format(id))
        self.id = id

    def __str__(self):
        return str(self.id)

    def __hash__(self):
        return self.id


class SubprogramVertex(Vertex):
    def __init__(self, id, name):
        Vertex.__init__(self, id)
        self.name = name


class ProgramPoint(Vertex):
    def __init__(self, id, program_point):
        Vertex.__init__(self, id)
        self.program_point = program_point
        self.inlined = False


class SuperBlock(Vertex):
    def __init__(self, id):
        Vertex.__init__(self, id)
        self.vertices = set()


class LoopBody(SuperBlock):
    def __init__(self, id, header: Vertex):
        SuperBlock.__init__(self, id)
        self.header = header

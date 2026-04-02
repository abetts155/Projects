import pyevmasm

import graphs.vertices


class Block(graphs.vertices.Vertex):
    def __init__(self, identifier: str):
        super().__init__(identifier)
        self.instructions: list[pyevmasm.Instruction] = []

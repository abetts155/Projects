class Instruction:
    def __init__(self, opcode: str):
        self._opcode = opcode
        self._address = None

    @property
    def opcode(self) -> str:
        return self._opcode

    @property
    def address(self) -> int:
        return self._address


class CallInstruction(Instruction):
    opcode = 'CALL'

    def __init__(self, target: str = ''):
        Instruction.__init__(self, CallInstruction.opcode)
        self._target = target

    @property
    def target(self):
        return self._target


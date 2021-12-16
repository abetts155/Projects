from random import choice, choices
from typing import List


class Instruction:
    def __init__(self, opcode: str, latencies: List[int]):
        self._opcode = opcode
        self._latencies = latencies
        self._weights = []
        self._set_weights()

    def _set_weights(self):
        probability_constant = 0.7
        probability_total = 1
        for time in self._latencies:
            if time == self._latencies[-1]:
                self._weights.append(probability_total)
            else:
                probability = probability_constant * probability_total
                self._weights.append(probability)
                probability_total -= probability

    @property
    def opcode(self) -> str:
        return self._opcode

    def random_latency(self):
        (time,) = choices(self._latencies, self._weights)
        return time

    def worst_latency(self):
        return max(self._latencies)

    def __str__(self):
        return self.opcode


class CallInstruction(Instruction):
    OPCODE = 'CALL'

    def __init__(self, target: str = ''):
        Instruction.__init__(self, CallInstruction.OPCODE, [1, 4, 10])
        self._target = target

    @property
    def target(self):
        return self._target


class BranchInstruction(Instruction):
    OPCODE = 'BRANCH'

    def __init__(self):
        Instruction.__init__(self, BranchInstruction.OPCODE, [1, 2, 5, 10])


class AddInstruction(Instruction):
    OPCODE = '+'

    def __init__(self):
        Instruction.__init__(self, AddInstruction.OPCODE, [1, 4, 10])


class SubtractInstruction(Instruction):
    OPCODE = '-'

    def __init__(self):
        Instruction.__init__(self, SubtractInstruction.OPCODE, [1, 4, 10])


class MultiplyInstruction(Instruction):
    OPCODE = '*'

    def __init__(self):
        Instruction.__init__(self, MultiplyInstruction.OPCODE, [3, 4, 12, 20])


class DivideInstruction(Instruction):
    OPCODE = '/'

    def __init__(self):
        Instruction.__init__(self, DivideInstruction.OPCODE, [3, 4, 12, 20])


class LoadInstruction(Instruction):
    OPCODE = 'LOAD'

    def __init__(self):
        Instruction.__init__(self, LoadInstruction.OPCODE, [1, 2, 20])


class StoreInstruction(Instruction):
    OPCODE = 'STORE'

    def __init__(self):
        Instruction.__init__(self, StoreInstruction.OPCODE, [1, 2, 20])

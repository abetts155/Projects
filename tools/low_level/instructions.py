import enum

from utils import dot


class ReviewStatus(enum.Enum):
    IDENTICAL = 0
    EQUIVALENT = 1
    DIFFERENT = 2
    UNKNOWN = 3


class Field:
    """The opcode or an operand of an instruction"""

    def __init__(self, value):
        self.__value = value
        self.status = ReviewStatus.UNKNOWN

    def __str__(self):
        return self.__value

    @property
    def value(self):
        return self.__value

    def color(self):
        if self.status == ReviewStatus.IDENTICAL:
            return dot.Colors.lawn_green
        elif self.status == ReviewStatus.EQUIVALENT:
            return dot.Colors.yellow
        elif self.status == ReviewStatus.DIFFERENT:
            return dot.Colors.red
        else:
            return dot.Colors.white


class Instruction:
    """Models an instruction for a specific ISA"""

    instruction_id = 0

    @staticmethod
    def get_id():
        Instruction.instruction_id += 1
        return Instruction.instruction_id

    def __init__(self, prog, isa, address, opcode, operands):
        self.__id = Instruction.get_id()
        self.__prog = prog
        self.__isa = isa
        self.__address = address
        self.__fields = []
        self.__fields.append(Field(opcode))
        for op in operands:
            self.__fields.append(Field(op))
        self.__opcode = self.__fields[0]
        self.__operands = self.__fields[1:]

    @property
    def prog(self):
        return self.__prog

    @property
    def address(self):
        return self.__address

    @property
    def opcode(self):
        return self.__opcode

    @property
    def operands(self):
        return self.__operands

    @property
    def id_(self):
        return self.__id

    def __iter__(self):
        for f in self.__fields:
            yield f

    def __eq__(self, other):
        if type(other) is type(self):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __len__(self):
        # One for the address and the remainder from the instruction opcode and operands
        return 1 + len(self.__fields)

    def __str__(self):
        return '{:08X}: {} {}'.format(self.address,
                                      self.__opcode,
                                      ','.join(str(op) for op in self.__operands))


class Instructions(list):
    def append(self, instruction):
        if instruction in self:
            raise ValueError("Instruction '{}' already exists".format(instruction))
        list.append(self, instruction)

import abc
import typing

class SymbolicValue:
    def __init__(self, value: typing.Optional[int] = None, expression: typing.Optional[str] = None):
        self.value = value
        self.expression = expression or (str(value) if value is not None else "UNKNOWN")

    def __repr__(self):
        return f"Sym({self.expression})"


class VMState:
    def __init__(self):
        self.stack: list[SymbolicValue] = []
        self.pc = 0
        self.gas_used = 0


class Instruction(abc.ABC):
    def __init__(self, name: str, base_gas: int):
        self.name = name
        self.base_gas = base_gas

    @abc.abstractmethod
    def execute(self, state: VMState, operand: typing.Optional[int] = None):
        state.gas_used += self.base_gas


class ArithmeticOp(Instruction):
    def execute(self, state: VMState, operand=None):
        super().execute(state)
        v1 = state.stack.pop()
        v2 = state.stack.pop()

        # Symbolic logic: calculate if possible, otherwise build expression
        if v1.value is not None and v2.value is not None:
            # You would map self.name to actual Python operators (+, -, *, etc.)
            res = SymbolicValue(value=eval(f"{v1.value} + {v2.value}"))  # simplified
        else:
            res = SymbolicValue(expression=f"({v1.expression} {self.name} {v2.expression})")
        state.stack.append(res)


class PushOp(Instruction):
    def execute(self, state: VMState, operand: int):
        super().execute(state)
        state.stack.append(SymbolicValue(value=operand))


class DupOp(Instruction):
    def execute(self, state: VMState, operand: int): # operand 1-16
        super().execute(state)
        state.stack.append(state.stack[-operand])


class SwapOp(Instruction):
    def execute(self, state: VMState, operand: int): # operand 1-16
        super().execute(state)
        state.stack[-1], state.stack[-operand-1] = state.stack[-operand-1], state.stack[-1]


class JumpOp(Instruction):
    def execute(self, state: VMState, operand=None):
        super().execute(state)
        target = state.stack.pop()
        if target.value is not None:
            state.pc = target.value
        else:
            # This triggers your 'Calculated Jump' logic
            logger.warning(f"Dynamic Jump detected to {target.expression}")


class JumpiOp(Instruction):
    def execute(self, state: VMState, operand=None):
        super().execute(state)
        target = state.stack.pop()
        condition = state.stack.pop()
        # In a WCET analyzer, this is where you 'FORK' the VMState
        return "FORK", target, condition
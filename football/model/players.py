import dataclasses
import enum


@dataclasses.dataclass(frozen=True, slots=True)
class Foot:
    strength: float


@dataclasses.dataclass(frozen=True, slots=True, kw_only=True)
class PlayerProfile:
    speed: float
    stamina: float
    strength: float
    passing: float
    crossing: float
    shooting: float
    tackling: float


class Position(enum.StrEnum):
    GoalKeeper = enum.auto()
    Defender = enum.auto()
    Midfielder = enum.auto()
    Forward = enum.auto()


@dataclasses.dataclass(frozen=True)
class Player:
    id_: int
    forename: str
    surname: str
    position: Position
    left_foot: Foot
    right_foot: Foot
    vitals: PlayerProfile

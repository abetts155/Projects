import dataclasses

from model import pitches
from model import players


@dataclasses.dataclass(frozen=True, slots=True)
class Stadium:
    name: str
    capacity: int
    pitch: pitches.Pitch


@dataclasses.dataclass(slots=True)
class Club:
    name: str
    stadium: Stadium
    players: list[players.Player] = dataclasses.field(default_factory=list)

import dataclasses

from model import teams


@dataclasses.dataclass
class League:
    country: str
    name: str
    clubs: list[teams.Club] = dataclasses.field(default_factory=list)
    
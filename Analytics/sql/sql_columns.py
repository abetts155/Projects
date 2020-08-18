import enum


class Affinity(enum.Enum):
    BLOB = enum.auto()
    INTEGER = enum.auto()
    NUMERIC = enum.auto()
    REAL = enum.auto()
    TEXT = enum.auto()


class ColumnNames(enum.Enum):
    Away_Bench = enum.auto()
    Away_ID = enum.auto()
    Away_Lineup = enum.auto()
    Away_Score_HT = enum.auto()
    Away_Score_FT = enum.auto()
    Away_Substitutions = enum.auto()
    Competition_ID = enum.auto()
    Date_Of_Birth = enum.auto()
    Event_ID = enum.auto()
    Foot = enum.auto()
    From_Position = enum.auto()
    Home_Bench = enum.auto()
    Home_ID = enum.auto()
    Home_Lineup = enum.auto()
    Home_Score_HT = enum.auto()
    Home_Score_FT = enum.auto()
    Home_Substitutions = enum.auto()
    ID = enum.auto()
    Match_Date = enum.auto()
    Name = enum.auto()
    Period = enum.auto()
    Player_ID = enum.auto()
    Position = enum.auto()
    Season_ID = enum.auto()
    Sub_Event_ID = enum.auto()
    Tags = enum.auto()
    Team_ID = enum.auto()
    Timestamp = enum.auto()
    To_Position = enum.auto()


class Column:
    def __init__(self, name: str, affinity: Affinity):
        self._name = name
        self._affinity = affinity

    @property
    def affinity(self):
        return self._affinity

    @property
    def name(self) -> str:
        return self._name

    def __hash__(self):
        return hash(self.name)

    def __eq__(self, other):
        if type(other) == type(self):
            return self.__dict__ == other.__dict__
        return NotImplemented


def id_column() -> Column:
    return Column(ColumnNames.ID.name, Affinity.INTEGER)


def name_column() -> Column:
    return Column(ColumnNames.Name.name, Affinity.TEXT)

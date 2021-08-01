from enum import auto, Enum


class Affinity(Enum):
    BLOB = auto()
    INTEGER = auto()
    NUMERIC = auto()
    REAL = auto()
    TEXT = auto()


class ColumnNames(Enum):
    Away_Bench = auto()
    Away_ID = auto()
    Away_Lineup = auto()
    Away_Score_HT = auto()
    Away_Score_FT = auto()
    Away_Substitutions = auto()
    Code = auto()
    Country = auto()
    Country_Code = auto()
    Competition_ID = auto()
    Current = auto()
    Date = auto()
    Date_Of_Birth = auto()
    Detail = auto()
    Extra_Time = auto()
    Event_ID = auto()
    Event_IDs = auto()
    Finished = auto()
    Fixture_ID = auto()
    Flag = auto()
    Foot = auto()
    From_Position = auto()
    Full_Time = auto()
    Half_Time = auto()
    Home_Bench = auto()
    Home_ID = auto()
    Home_Lineup = auto()
    Home_Score_HT = auto()
    Home_Score_FT = auto()
    Home_Substitutions = auto()
    ID = auto()
    Left_ID = auto()
    Logo = auto()
    Match_Date = auto()
    Match_ID = auto()
    Name = auto()
    Period = auto()
    Player_ID = auto()
    Position = auto()
    Right_ID = auto()
    Season_ID = auto()
    Sub_Event_ID = auto()
    Tags = auto()
    Team_ID = auto()
    Time = auto()
    Timestamp = auto()
    To_Position = auto()
    Updated = auto()
    Year = auto()


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

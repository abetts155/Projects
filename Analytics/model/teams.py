import datetime
import enum
import typing

from miscellaneous import wyscout
from sql.sql_columns import Affinity, Column, ColumnNames
from sql import sql_columns, sql_tables


class PlayerPosition(enum.Enum):
    Goalkeeper = enum.auto()
    Defender = enum.auto()
    Midfielder = enum.auto()
    Forward = enum.auto()


class Foot(enum.Enum):
    Left = 'left'
    Right = 'right'
    Both = 'both'
    Unknown = None


class Player:
    table = None
    inventory = {}

    def __init__(self, id_: int, name: str, date_of_birth: datetime.date, position: PlayerPosition, foot: Foot):
        assert id_ > 0
        self._id = id_
        self._name = name
        self._date_of_birth = date_of_birth
        self._position = position
        self._foot = foot

    @property
    def date_of_birth(self) -> datetime.date:
        return self._date_of_birth

    @property
    def foot(self) -> Foot:
        return self._foot

    @property
    def id(self) -> int:
        return self._id

    @property
    def name(self) -> str:
        return self._name

    @property
    def position(self) -> PlayerPosition:
        return self._position

    def sql_values(self):
        values = [self.id, self.name, self.date_of_birth.strftime('%Y-%m-%d'), self.position.value, self.foot.value]
        assert len(values) == len(self.__class__.sql_table().columns)
        return values

    @classmethod
    def sql_table(cls) -> sql_tables.Table:
        if cls.table is None:
            cls.table = sql_tables.Table(cls.__name__,
                                         sql_columns.id_column(),
                                         [sql_columns.id_column(),
                                          sql_columns.name_column(),
                                          Column(ColumnNames.Date_Of_Birth.name, Affinity.TEXT),
                                          Column(ColumnNames.Position.name, Affinity.INTEGER),
                                          Column(ColumnNames.Foot.name, Affinity.INTEGER)])
        return cls.table

    def __hash__(self):
        return hash(self.id)

    def __eq__(self, other):
        if type(other) == type(self):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __str__(self):
        return self.name


def create_player_from_json(data: typing.Dict):
    id_ = int(data[wyscout.JSON_Keys.wyId])
    name = wyscout.decode_json_string(data[wyscout.JSON_Keys.shortName])
    date_of_birth = datetime.datetime.strptime(data[wyscout.JSON_Keys.birthDate], '%Y-%m-%d')
    position = data[wyscout.JSON_Keys.role][wyscout.JSON_Keys.name]
    foot = data[wyscout.JSON_Keys.foot]
    if foot == 'null' or foot == '':
        foot = None
    Player.inventory[id_] = Player(id_,
                                   name,
                                   date_of_birth,
                                   PlayerPosition[position],
                                   Foot(foot))


class Team:
    table = None
    inventory = {}

    def __init__(self, id_: int, name: str):
        self._id = id_
        self._name = name

    @property
    def id(self) -> int:
        return self._id

    @property
    def name(self) -> str:
        return self._name

    def sql_values(self):
        values = [self.id, self.name]
        assert len(values) == len(self.__class__.sql_table().columns)
        return values

    @classmethod
    def sql_table(cls) -> sql_tables.Table:
        if cls.table is None:
            cls.table = sql_tables.Table(cls.__name__,
                                         sql_columns.id_column(),
                                         [sql_columns.id_column(),
                                          sql_columns.name_column()])
        return cls.table

    def __eq__(self, other):
        if type(other) == type(self):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __str__(self):
        return self.name


def create_team_from_json(data: typing.Dict):
    id_ = int(data[wyscout.JSON_Keys.wyId])
    name = wyscout.decode_json_string(data[wyscout.JSON_Keys.name])
    Team.inventory[id_] = Team(id_, name)


class Coach:
    table = None
    inventory = {}

    def __init__(self, id_: int, name: str, date_of_birth: datetime.date):
        self._id = id_
        self._name = name
        self._date_of_birth = date_of_birth

    @property
    def date_of_birth(self) -> datetime.date:
        return self._date_of_birth

    @property
    def id(self) -> int:
        return self._id

    @property
    def name(self) -> str:
        return self._name

    def sql_values(self):
        values = [self.id,
                  self.name,
                  self.date_of_birth.strftime('%Y-%m-%d') if self.date_of_birth else None]
        assert len(values) == len(self.__class__.sql_table().columns)
        return values

    @classmethod
    def sql_table(cls) -> sql_tables.Table:
        if cls.table is None:
            cls.table = sql_tables.Table(cls.__name__,
                                         sql_columns.id_column(),
                                         [sql_columns.id_column(),
                                          sql_columns.name_column(),
                                          Column(ColumnNames.Date_Of_Birth.name, Affinity.TEXT)])
        return cls.table

    def __eq__(self, other):
        if type(other) == type(self):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __str__(self):
        return self.name


def create_coach_from_json(data: typing.Dict):
    id_ = int(data[wyscout.JSON_Keys.wyId])
    name = wyscout.decode_json_string(data[wyscout.JSON_Keys.shortName])
    date_of_birth = None
    if data[wyscout.JSON_Keys.birthDate] is not None:
        date_of_birth = datetime.datetime.strptime(data[wyscout.JSON_Keys.birthDate], '%Y-%m-%d')
    Coach.inventory[id_] = Coach(id_, name, date_of_birth)

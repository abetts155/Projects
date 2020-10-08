from enum import Enum, auto
from lib import messages
from model.teams import Team
from sql import sql_columns, sql_tables
from sql.sql_columns import Affinity, Column, ColumnNames
from typing import Callable, Dict, List
import datetime


class Venue(Enum):
    any = auto()
    away = auto()
    home = auto()

    @staticmethod
    def from_string(string: str):
        try:
            return Venue[string.lower()]
        except KeyError:
            messages.error_message("Venue '{}' is not valid".format(string))


class Result:
    __slots__ = ['left', 'right']

    def __init__(self, left: int, right: int):
        self.left = left
        self.right = right

    def won(self):
        return self.left > self.right

    def not_won(self):
        return self.left <= self.right

    def lost(self):
        return self.left < self.right

    def not_lost(self):
        return self.left >= self.right

    def drawn(self):
        return self.left == self.right

    def not_drawn(self):
        return self.left != self.right

    def reverse(self) -> "Result":
        return Result(self.right, self.left)

    def __str__(self):
        return '{}-{}'.format(self.left, self.right)

    @staticmethod
    def event_name(function: Callable) -> str:
        tokens = function.__name__.split('_')
        tokens[0] = tokens[0].capitalize()
        return ' '.join(tokens)


class Fixture:
    table = None
    inventory = {}

    def __init__(self,
                 id_: int,
                 date: datetime.date,
                 season_id: int,
                 home_team: Team,
                 away_team: Team,
                 half_time: str,
                 full_time: str,
                 finished: bool):
        self._id = id_
        self._date = date
        self._season_id = season_id
        self._home_team = home_team
        self._away_team = away_team
        self._half_time = half_time
        self._full_time = full_time
        self._finished = finished

    @property
    def id(self) -> int:
        return self._id

    @property
    def date(self) -> datetime.date:
        return self._date

    @property
    def season_id(self) -> int:
        return self._season_id

    @property
    def home_team(self) -> Team:
        return self._home_team

    @property
    def away_team(self) -> Team:
        return self._away_team

    def first_half(self) -> Result or None:
        if self._half_time:
            left, right = self._half_time.split('-')
            if left and right:
                return Result(int(left), int(right))

    def full_time(self) -> Result or None:
        if self._full_time:
            left, right = self._full_time.split('-')
            if left and right:
                return Result(int(left), int(right))

    def second_half(self) -> Result or None:
        if self.first_half() and self.full_time():
            return Result(self.full_time().left - self.first_half().left,
                          self.full_time().right - self.first_half().right)

    @property
    def finished(self) -> int:
        return self._finished

    def sql_values(self):
        values = [self.id,
                  self.date,
                  self.season_id,
                  self.home_team.id,
                  self.away_team.id,
                  self._half_time,
                  self._full_time,
                  self.finished]
        assert len(values) == len(self.__class__.sql_table().columns)
        return values

    @classmethod
    def sql_table(cls) -> sql_tables.Table:
        if cls.table is None:
            cls.table = sql_tables.Table(cls.__name__,
                                         sql_columns.id_column(),
                                         [sql_columns.id_column(),
                                          Column(ColumnNames.Date.name, Affinity.TEXT),
                                          Column(ColumnNames.Season_ID.name, Affinity.INTEGER),
                                          Column(ColumnNames.Home_ID.name, Affinity.INTEGER),
                                          Column(ColumnNames.Away_ID.name, Affinity.INTEGER),
                                          Column(ColumnNames.Half_Time.name, Affinity.TEXT),
                                          Column(ColumnNames.Full_Time.name, Affinity.TEXT),
                                          Column(ColumnNames.Finished.name, Affinity.INTEGER)])
        return cls.table

    def __eq__(self, other):
        if type(other) == type(self):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __hash__(self):
        return self.id

    def __str__(self):
        return '{}: {} (First:{}) (Second:{}) {} {}'.format(self.date.strftime('%d-%m-%Y %H:%M'),
                                                            self.home_team.name if self.home_team else None,
                                                            self.first_half(),
                                                            self.second_half(),
                                                            self.full_time(),
                                                            self.away_team.name if self.away_team else None)


def create_fixture_from_json(data: Dict):
    id_ = int(data['fixture_id'])
    date = datetime.datetime.fromisoformat(data['event_date'])
    season_id = int(data['league_id'])
    home_id = int(data['homeTeam']['team_id'])
    home_team = Team.inventory[home_id]
    away_id = int(data['awayTeam']['team_id'])
    away_team = Team.inventory[away_id]
    half_time = data['score']['halftime']
    full_time = data['score']['fulltime']
    finished = True if data['status'] == 'Match Finished' else False
    fixture = Fixture(id_,
                      date,
                      season_id,
                      home_team,
                      away_team,
                      half_time,
                      full_time,
                      finished)
    Fixture.inventory[fixture.id] = fixture


def create_fixture_from_row(row: List):
    id_ = int(row[0])
    date = datetime.datetime.fromisoformat(row[1])
    season_id = int(row[2])
    home_id = int(row[3])
    home_team = Team.inventory[home_id] if home_id in Team.inventory else None
    away_id = int(row[4])
    away_team = Team.inventory[away_id] if away_id in Team.inventory else None
    half_time = row[5]
    full_time = row[6]
    finished = bool(row[7])
    fixture = Fixture(id_,
                      date,
                      season_id,
                      home_team,
                      away_team,
                      half_time,
                      full_time,
                      finished)
    Fixture.inventory[fixture.id] = fixture
    return fixture

from enum import Enum, auto
from functools import partial
from lib import messages
from model.teams import Team
from sql import sql_columns, sql_tables
from sql.sql_columns import Affinity, Column, ColumnNames
from time import time
from typing import Callable, Dict, List

import datetime
import operator
import re


class Venue(Enum):
    anywhere = auto()
    away = auto()
    home = auto()

    @staticmethod
    def from_string(string: str):
        try:
            return Venue[string.lower()]
        except KeyError:
            messages.error_message("Venue '{}' is not valid".format(string))


class Half(Enum):
    first = '1st half'
    second = '2nd half'
    full = 'full time'

    @classmethod
    def from_string(cls, string: str):
        try:
            return Half[string.lower()]
        except KeyError:
            messages.error_message("Half '{}' is not valid".format(string))

    @staticmethod
    def to_string(halves: List["Half"]) -> str:
        return ', '.join([half.value for half in halves])


class Scoreline:
    __slots__ = ['left', 'right']

    def __init__(self, left: int, right: int):
        self.left = left
        self.right = right

    def reverse(self) -> "Scoreline":
        return Scoreline(self.right, self.left)

    def __str__(self):
        return '{}-{}'.format(self.left, self.right)

    def __eq__(self, other):
        if type(other) == type(self):
            return self.left == other.left and self.right == other.right
        return NotImplemented

    def __hash__(self):
        return self.left + self.right


def win(score: Scoreline):
    return operator.gt(score.left, score.right)


def loss(score: Scoreline):
    return operator.lt(score.left, score.right)


def draw(score: Scoreline):
    return operator.eq(score.left, score.right)


def bts(score: Scoreline):
    return operator.gt(score.left, 0) and operator.gt(score.right, 0)


def gf(op: Callable, bound: int, score: Scoreline):
    return op(score.left, bound)


def ga(op: Callable, bound: int, score: Scoreline):
    return op(score.right, bound)


def gfa(op: Callable, bound: int, score: Scoreline):
    return op(score.left + score.right, bound)


def gd(op: Callable, bound: int, score: Scoreline):
    if score.left > score.right:
        return op(score.left - score.right, bound)
    else:
        return op(score.right - score.left, bound)


class Event:
    inventory = {func.__name__: func for func in {win, draw, loss, bts}}
    stems = {func.__name__: func for func in {gf, ga, gfa, gd}}

    op_table = {(operator.eq, True): '!=',
                (operator.eq, False): '=',
                (operator.ne, True): '=',
                (operator.ne, False): '!=',
                (operator.gt, True): '<=',
                (operator.gt, False): '>',
                (operator.ge, True): '<',
                (operator.ge, False): '>=',
                (operator.lt, True): '>=',
                (operator.lt, False): '<',
                (operator.le, True): '>',
                (operator.le, False): '<='}

    long_table = {gf: 'Goals for',
                  ga: 'Goals against',
                  gfa: 'Goals scored',
                  gd: 'Goal difference',
                  win: 'Win',
                  draw: 'Draw',
                  loss: 'Loss',
                  bts: 'BTS'}

    short_table = {gf: 'GF',
                   ga: 'GA',
                   gfa: 'GFA',
                   gd: 'GD',
                   win: 'W',
                   draw: 'D',
                   loss: 'L',
                   bts: 'BTS'}

    @classmethod
    def encode(cls, event):
        for name, other in cls.inventory.items():
            if other == event:
                return name
        assert False

    @classmethod
    def decode(cls, name: str):
        lexemes = name.split('_')
        return lexemes[0], lexemes[1], int(lexemes[2])

    @classmethod
    def add(cls, name: str):
        if name not in cls.inventory:
            stem, op, bound = Event.decode(name)
            if stem in cls.stems:
                cls.inventory[name] = partial(cls.stems[stem], getattr(operator, op), bound)
            else:
                messages.error_message("Event '{}' is not valid".format(name))

    @classmethod
    def get(cls, name: str):
        if name not in cls.inventory:
            cls.add(name)
        return cls.inventory[name]

    @classmethod
    def name(cls, func: Callable or partial, negate: bool, short: bool = False) -> str:
        if type(func) is partial:
            try:
                if short:
                    func_name = Event.short_table[func.func]
                else:
                    func_name = Event.long_table[func.func]
                (op, arg) = func.args
                op_name = Event.op_table[(op, negate)]
                return '{} {} {}'.format(func_name, op_name, arg)
            except KeyError:
                messages.error_message("Unable to construct name for '{}'".format(func.func.__name__, op.__name__))
        else:
            if short:
                func_name = Event.short_table[func]
            else:
                func_name = Event.long_table[func]

            if negate:
                return 'No {}'.format(func_name)
            else:
                return func_name


class ContextualEvent:
    def __init__(self, func: Callable, negate: bool, venue: Venue, halves: List[Half]):
        self.func = func
        self.negate = negate
        self.venue = venue
        self.halves = halves


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
                 finished: bool,
                 updated: datetime.date):
        self._id = id_
        self._date = date
        self._season_id = season_id
        self._home_team = home_team
        self._away_team = away_team
        self._half_time = half_time
        self._full_time = full_time
        self._finished = finished
        self._updated = updated

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

    def first_half(self) -> Scoreline or None:
        if self._half_time:
            left, right = self._half_time.split('-')
            if left and right:
                return Scoreline(int(left), int(right))

    def full_time(self) -> Scoreline or None:
        if self._full_time:
            left, right = self._full_time.split('-')
            if left and right:
                return Scoreline(int(left), int(right))

    def second_half(self) -> Scoreline or None:
        if self.first_half() and self.full_time():
            return Scoreline(self.full_time().left - self.first_half().left,
                             self.full_time().right - self.first_half().right)

    @property
    def finished(self) -> int:
        return self._finished

    @property
    def updated(self) -> datetime.date:
        return self._updated

    def sql_values(self):
        values = [self.id,
                  self.date,
                  self.season_id,
                  self.home_team.id,
                  self.away_team.id,
                  self._half_time,
                  self._full_time,
                  self.finished,
                  self.updated]
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
                                          Column(ColumnNames.Finished.name, Affinity.INTEGER),
                                          Column(ColumnNames.Updated.name, Affinity.TEXT)])
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


def canonicalise_scoreline(fixture: Fixture, team: Team, score: Scoreline) -> Scoreline:
    if score:
        if fixture.home_team == team:
            return score
        else:
            return score.reverse()


round_regexes = [re.compile(r'Regular Season - \d+'),
                 re.compile(r'Regular Season'),
                 re.compile(r'Round is (Salzburg|Tirol|Southern Central)'),
                 re.compile(r'Girone [A-C] - \d+'),
                 re.compile(r'Group [A-D] - \d+'),
                 re.compile(r'Group [1-5] - \d+$'),
                 re.compile(r'ČFL (A|B) - \d+'),
                 re.compile(r'MSFL - \d+'),
                 re.compile(r'(North A|North B|South A|South B) - \d+$'),
                 re.compile(r'(Clausura|Apertura|Liguilla|Norra|Södra) - \d+'),
                 re.compile(r'Liga 1 - Round \d+'),
                 re.compile(r'(Clausura|Apertura)'),
                 re.compile(r'(1st|2nd) (Stage|Phase|Round) - \d+'),
                 re.compile(r'(South|North) - \d+'),
                 re.compile(r'(Isthmian|Northern|Southern|Southern South|Southern Central) - \d+'),
                 re.compile(r'(Südwest|Nordost|Nord|West|Bayern|Süd \/ Südwest|Nord \/ Nordost) - \d+'),
                 re.compile(r'(Vorarlberg|Mitte|Ost) - \d+'),
                 re.compile(r'(Kirmizi|Kırmızı|Beyaz) - \d+'),
                 re.compile(r'(Championship|Relegation) Round - \d+'),
                 re.compile(r'(Östra Götaland|'
                            r'Västra Götaland|'
                            r'Södra Svealand|'
                            r'Norrland|'
                            r'Norra Svealand|'
                            r'Norra Götaland) - \d+'),
                 re.compile(r'(National First Division|'
                            r'K-League Challenge|'
                            r'Women Bundesliga|'
                            r'Serie A Women|'
                            r'Feminine Division 1|'
                            r'Primera Division Women|'
                            r'W-League) - Round \d+'),
                 re.compile(r'FA WSL \(Women Super League\) - Round \d+')]


def is_regular_fixture(data: str):
    for regex in round_regexes:
        if regex.match(data):
            return True
    return False


def create_fixture_from_json(data: Dict):
    if is_regular_fixture(data['round']):
        id_ = int(data['fixture_id'])
        date = datetime.datetime.fromisoformat(data['event_date'])
        season_id = int(data['league_id'])
        home_id = int(data['homeTeam']['team_id'])
        home_name = data['homeTeam']['team_name']
        home_team = Team.find_team(home_id, home_name)
        away_id = int(data['awayTeam']['team_id'])
        away_name = data['awayTeam']['team_name']
        away_team = Team.find_team(away_id, away_name)
        half_time = data['score']['halftime']
        full_time = data['score']['fulltime']
        finished = True if data['status'] == 'Match Finished' else False

        if home_team and away_team:
            fixture = Fixture(id_,
                              date,
                              season_id,
                              home_team,
                              away_team,
                              half_time,
                              full_time,
                              finished,
                              datetime.datetime.now())
            Fixture.inventory[fixture.id] = fixture
    else:
        messages.warning_message('Ignoring fixture. Round is {}.'.format(data['round']))


def get_home_team_data(data: Dict):
    home_id = int(data['homeTeam']['team_id'])
    home_name = data['homeTeam']['team_name']
    return home_id, home_name


def get_away_team_data(data: Dict):
    away_id = int(data['awayTeam']['team_id'])
    away_name = data['awayTeam']['team_name']
    return away_id, away_name


def datetime_from_utc_to_local(utc_datetime):
    now_timestamp = time()
    offset = datetime.datetime.fromtimestamp(now_timestamp) - datetime.datetime.utcfromtimestamp(now_timestamp)
    return utc_datetime + offset


def create_fixture_from_row(row: List):
    id_ = int(row[0])
    date1 = datetime.datetime.fromisoformat(row[1])
    date2 = datetime_from_utc_to_local(date1)
    season_id = int(row[2])
    home_id = int(row[3])
    home_team = Team.inventory[home_id] if home_id in Team.inventory else None
    away_id = int(row[4])
    away_team = Team.inventory[away_id] if away_id in Team.inventory else None
    half_time = row[5]
    full_time = row[6]
    finished = bool(row[7])
    updated = datetime.datetime.fromisoformat(row[8])
    fixture = Fixture(id_,
                      date2,
                      season_id,
                      home_team,
                      away_team,
                      half_time,
                      full_time,
                      finished,
                      updated)
    Fixture.inventory[fixture.id] = fixture
    return fixture

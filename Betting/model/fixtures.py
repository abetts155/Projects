import collections
import datetime
import enum
import functools
import operator
import re
import time
import typing

import football_api.structure
import lib.messages
import model.competitions
import model.teams
import sql.sql
import sql.sql_tables

from sql.sql_columns import Affinity, Column, ColumnNames
from sql.sql_language import Keywords


class Venue(enum.StrEnum):
    ANYWHERE = 'Home and Away'
    HOME = 'Home'
    AWAY = 'Away'

    @staticmethod
    def from_string(value: str) -> "Venue":
        try:
            return Venue[value.upper()]
        except KeyError:
            lib.messages.error_message("Venue '{}' is not valid".format(value))


class Period(enum.StrEnum):
    FULL = 'FT'
    FIRST = '1st Half'
    SECOND = '2nd Half'
    EXTRA = 'ET'
    PENALTIES = 'Pens'

    @staticmethod
    def from_string(value: str) -> "Period":
        try:
            return Period[value.upper()]
        except KeyError:
            lib.messages.error_message("Period '{}' is not valid".format(value))


class Scoreline:
    __slots__ = ['left', 'right']

    def __init__(self, left: int, right: int):
        self.left = left
        self.right = right

    def reverse(self) -> "Scoreline":
        return Scoreline(self.right, self.left)

    def __str__(self):
        return f"{self.left}-{self.right}"

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


def gf(op: collections.abc.Callable, bound: int, score: Scoreline):
    return op(score.left, bound)


def ga(op: collections.abc.Callable, bound: int, score: Scoreline):
    return op(score.right, bound)


def gfa(op: collections.abc.Callable, bound: int, score: Scoreline):
    return op(score.left + score.right, bound)


def gd(op: collections.abc.Callable, bound: int, score: Scoreline):
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
                cls.inventory[name] = functools.partial(cls.stems[stem], getattr(operator, op), bound)
            else:
                lib.messages.error_message("Event '{}' is not valid".format(name))

    @classmethod
    def get(cls, name: str):
        if name not in cls.inventory:
            cls.add(name)
        return cls.inventory[name]

    @classmethod
    def name(cls, func: collections.abc.Callable or functools.partial, negate: bool, short: bool = False) -> str:
        if type(func) is functools.partial:
            try:
                if short:
                    func_name = Event.short_table[func.func]
                else:
                    func_name = Event.long_table[func.func]
                (op, arg) = func.args
                op_name = Event.op_table[(op, negate)]
                return '{} {} {}'.format(func_name, op_name, arg)
            except KeyError:
                lib.messages.error_message("Unable to construct name for '{}'".format(func.func.__name__, op.__name__))
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
    def __init__(self, func: collections.abc.Callable, negate: bool, venue: Venue, periods: list[Period]):
        self.func = func
        self.negate = negate
        self.venue = venue
        self.periods = periods


class Fixture:
    def __init__(
            self,
            id_: int,
            date: datetime.datetime,
            competition_id: int,
            season_year: int,
            home_team: model.teams.Team,
            away_team: model.teams.Team,
            finished: bool,
            referee: str,
            half_time: str,
            full_time: str,
    ):
        self.id = id_
        self.date = date
        self.competition_id = competition_id
        self.season_year = season_year
        self.home_team = home_team
        self.away_team = away_team
        self.finished = finished
        self.referee = referee
        self.half_time = half_time
        self.full_time = full_time

    def result(self, period: Period) -> typing.Optional[Scoreline]:
        if period == Period.FIRST:
            if self.half_time:
                a, b = self.half_time.split('-')
                if a and b:
                    return Scoreline(int(a), int(b))
        elif period == Period.SECOND:
            if self.half_time and self.full_time:
                a, b = self.half_time.split('-')
                c, d = self.full_time.split('-')
                if a and b and c and d:
                    return Scoreline(int(c) - int(a), int(d) - int(b))
        elif period == Period.FULL:
            if self.full_time:
                a, b = self.full_time.split('-')
                if a and b:
                    return Scoreline(int(a), int(b))

    def sql_values(self):
        values = [
            self.id,
            self.date,
            self.competition_id,
            self.season_year,
            self.home_team.id,
            self.away_team.id,
            self.half_time,
            self.full_time,
            self.finished,
            self.referee
        ]
        assert len(values) == len(self.__class__.sql_table().columns)
        return values

    @classmethod
    def sql_table(cls) -> sql.sql_tables.Table:
        id_col = Column(ColumnNames.ID.name, Affinity.INTEGER)
        return sql.sql_tables.Table(
            cls.__name__,
            [
                id_col
            ],
            [
                id_col,
                Column(ColumnNames.Date.name, Affinity.TEXT),
                Column(ColumnNames.Competition_ID.name, Affinity.INTEGER),
                Column(ColumnNames.Season_ID.name, Affinity.INTEGER),
                Column(ColumnNames.Home_ID.name, Affinity.INTEGER),
                Column(ColumnNames.Away_ID.name, Affinity.INTEGER),
                Column(ColumnNames.Half_Time.name, Affinity.TEXT),
                Column(ColumnNames.Full_Time.name, Affinity.TEXT),
                Column(ColumnNames.Finished.name, Affinity.INTEGER),
                Column(ColumnNames.Referee.name, Affinity.TEXT)
            ]
        )

    def __eq__(self, other):
        if type(other) == type(self):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __hash__(self):
        return self.id

    def __str__(self):
        return (
            f'{self.date.strftime('%d-%m-%Y %H:%M')}: '
            f'{self.home_team.name} '
            f'(First:{self.result(Period.FIRST)}) '
            f'(Second:{self.result(Period.SECOND)}) '
            f'{self.result(Period.FULL)} '
            f'{self.away_team.name}'
        )


class CupFixture(Fixture):
    def __init__(
            self,
            id_: int,
            date: datetime.date,
            competition_id: int,
            season_year: int,
            home_team: model.teams.Team,
            away_team: model.teams.Team,
            finished: bool,
            referee: str,
            half_time: str,
            full_time: str,
            extra_time: str,
            penalties: str,
            round: str
    ):
        Fixture.__init__(
            self,
            id_,
            date,
            competition_id,
            season_year,
            home_team,
            away_team,
            finished,
            referee,
            half_time,
            full_time
        )
        self.extra_time = extra_time
        self.penalties = penalties
        self.round = round

    def result(self, period: Period) -> typing.Optional[Scoreline]:
        if period in [Period.FIRST, Period.SECOND, Period.FULL]:
            return Fixture.result(self, period)
        elif period == Period.EXTRA:
            if self.extra_time:
                a, b = self.extra_time.split('-')
                if a and b:
                    return Scoreline(int(a), int(b))
        elif period == Period.PENALTIES:
            if self.penalties:
                a, b = self.half_time.split('-')
                if a and b:
                    return Scoreline(int(a), int(b))

    def sql_values(self):
        values = [
            self.id,
            self.date,
            self.competition_id,
            self.season_year,
            self.home_team.id,
            self.away_team.id,
            self.half_time,
            self.full_time,
            self.finished,
            self.referee,
            self.extra_time,
            self.penalties,
            self.round
        ]
        assert len(values) == len(self.__class__.sql_table().columns)
        return values

    @classmethod
    def sql_table(cls) -> sql.sql_tables.Table:
        id_col = Column(ColumnNames.ID.name, Affinity.INTEGER)
        return sql.sql_tables.Table(
            cls.__name__,
            [
                id_col
            ],
            [
                id_col,
                Column(ColumnNames.Date.name, Affinity.TEXT),
                Column(ColumnNames.Competition_ID.name, Affinity.INTEGER),
                Column(ColumnNames.Season_ID.name, Affinity.INTEGER),
                Column(ColumnNames.Home_ID.name, Affinity.INTEGER),
                Column(ColumnNames.Away_ID.name, Affinity.INTEGER),
                Column(ColumnNames.Half_Time.name, Affinity.TEXT),
                Column(ColumnNames.Full_Time.name, Affinity.TEXT),
                Column(ColumnNames.Finished.name, Affinity.INTEGER),
                Column(ColumnNames.Referee.name, Affinity.TEXT),
                Column(ColumnNames.Extra_Time.name, Affinity.TEXT),
                Column(ColumnNames.Penalties.name, Affinity.TEXT),
                Column(ColumnNames.Round.name, Affinity.TEXT)
            ]
        )


def sort_fixtures(fixtures: list[Fixture]):
    fixtures.sort(key=lambda fixture: fixture.date)


def canonicalise_scoreline(fixture: Fixture, team: model.teams.Team, score: Scoreline) -> Scoreline:
    if score:
        if fixture.home_team == team:
            return score
        else:
            return score.reverse()


round_regexes = [
    re.compile(r'Regular Season - \d+'),
    re.compile(r'Regular Season'),
    re.compile(r'Round is (Salzburg|Tirol|Southern Central)'),
    re.compile(r'Girone [A-C] - \d+'),
    re.compile(r'Group [A-D] - \d+'),
    re.compile(r'Group [1-5] - \d+$'),
    re.compile(r'ČFL ([AB]) - \d+'),
    re.compile(r'MSFL - \d+'),
    re.compile(r'(North A|North B|South A|South B) - \d+$'),
    re.compile(r'(Clausura|Apertura|Liguilla|Norra|Södra) - \d+'),
    re.compile(r'Liga 1 - Round \d+'),
    re.compile(r'(Clausura|Apertura)'),
    re.compile(r'(1st|2nd) (Stage|Phase|Round) - \d+'),
    re.compile(r'(South|North) - \d+'),
    re.compile(r'(Isthmian|Northern|Southern|Southern South|Southern Central) - \d+'),
    re.compile(r'(Südwest|Nordost|Nord|West|Bayern|Süd / Südwest|Nord / Nordost) - \d+'),
    re.compile(r'(Vorarlberg|Mitte|Ost) - \d+'),
    re.compile(r'(Kirmizi|Kırmızı|Beyaz) - \d+'),
    re.compile(r'(Promotion|Championship|Relegation|Lower Table) Round - \d+'),
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
    re.compile(r'FA WSL \(Women Super League\) - Round \d+'),
    re.compile(r'Promotion Group - \d+'),
    re.compile(r'Group Stage - \d+'),
    re.compile(r'1st Group Stage - \d+'),
    re.compile(r'2nd Group Stage (A|B) - \d+'),
    re.compile(r'(Opening|Closing) - \d+'),
    re.compile(r'League Stage - \d+'),
    re.compile(r'(1st|2nd|3rd) Round'),
    re.compile(r'Round of (16|32)'),
    re.compile(r'(Play-offs|8th Finals|Quarter-finals|Semi-finals|Final)')
]


def is_regular_fixture(data: str):
    for regex in round_regexes:
        if regex.match(data):
            return True
    return False


def extract_fixture_details(json_data: dict):
    fixture_id = int(json_data['fixture']['id'])
    date = datetime.datetime.fromisoformat(json_data['fixture']['date'])

    half_time_home = json_data['score']['halftime']['home']
    half_time_away = json_data['score']['halftime']['away']
    if half_time_home is not None and half_time_away is not None:
        half_time = f"{half_time_home}-{half_time_away}"
    else:
        half_time = None

    full_time_home = json_data['score']['fulltime']['home']
    full_time_away = json_data['score']['fulltime']['away']
    if full_time_home is not None and full_time_away is not None:
        full_time = f"{full_time_home}-{full_time_away}"
    else:
        full_time = None

    if json_data['fixture']['status']['long'] == 'Match Finished':
        finished = True
    else:
        finished = False

    referee: str = json_data['fixture']['referee']
    if referee:
        comma_index = referee.find(',')
        if comma_index > -1:
            referee = referee[:comma_index]

    return fixture_id, date, half_time, full_time, finished, referee


def create_fixture_from_json(
        competition_id: int,
        season_year: int,
        home_team: model.teams.Team,
        away_team: model.teams.Team,
        json_data: dict
) -> typing.Optional[Fixture]:
    if is_regular_fixture(json_data['league']['round']):
        fixture_id, date, half_time, full_time, finished, referee = extract_fixture_details(json_data)
        fixture = Fixture(
            fixture_id,
            date,
            competition_id,
            season_year,
            home_team,
            away_team,
            finished,
            referee,
            half_time,
            full_time
        )
        return fixture
    else:
        lib.messages.warning_message(f'Ignoring fixture. Round is {json_data['league']['round']}')


cup_regexes = [
    re.compile(r'(League|Group) Stage - \d+'),
    re.compile(r'(Qualifying|Elite) Round - \d+'),
    re.compile(r'Knockout Round Play-offs'),
    re.compile(r'Round of 16'),
    re.compile(r'Quarter-finals')
]


def is_accepted_cup_fixture(data: str):
    for regex in cup_regexes:
        if regex.match(data):
            return True
    return False


def create_cup_fixture_from_json(
        competition_id: int,
        season_year: int,
        home_team: model.teams.Team,
        away_team: model.teams.Team,
        json_data: dict
) -> typing.Optional[CupFixture]:
    if is_accepted_cup_fixture(json_data['league']['round']):
        fixture_id, date, half_time, full_time, finished, referee = extract_fixture_details(json_data)

        extra_time_home = json_data['score']['extratime']['home']
        extra_time_away = json_data['score']['extratime']['away']
        if extra_time_home is not None and extra_time_away is not None:
            extra_time = f"{extra_time_home}-{extra_time_away}"
        else:
            extra_time = None

        penalties_home = json_data['score']['penalty']['home']
        penalties_away = json_data['score']['penalty']['away']
        if penalties_home is not None and penalties_away is not None:
            penalties = f"{penalties_home}-{penalties_away}"
        else:
            penalties = None

        round = json_data['league']['round']

        fixture = CupFixture(
            fixture_id,
            date,
            competition_id,
            season_year,
            home_team,
            away_team,
            finished,
            referee,
            half_time,
            full_time,
            extra_time,
            penalties,
            round
        )
        return fixture


def datetime_from_utc_to_local(utc_datetime):
    now_timestamp = time.time()
    offset = datetime.datetime.fromtimestamp(now_timestamp) - datetime.datetime.utcfromtimestamp(now_timestamp)
    return utc_datetime + offset


def extract_fixture_details_from_row(row: list, teams: dict[int, model.teams.Team]):
    id_ = int(row[0])
    date1 = datetime.datetime.fromisoformat(row[1])
    date2 = datetime_from_utc_to_local(date1)
    competition_id = int(row[2])
    season_year = int(row[3])

    home_id = int(row[4])
    assert home_id in teams
    home_team = teams[home_id]

    away_id = int(row[5])
    assert away_id in teams
    away_team = teams[away_id]

    half_time = row[6]
    full_time = row[7]
    finished = bool(row[8])
    referee = row[9]

    return id_, date2, competition_id, season_year, home_team, away_team, finished, referee, half_time, full_time


def create_fixture_from_row(row: list, teams: dict[int, model.teams.Team]) -> Fixture:
    values = extract_fixture_details_from_row(row, teams)
    return Fixture(*values)


def create_cup_fixture_from_row(row: list, teams: dict[int, model.teams.Team]) -> CupFixture:
    values = extract_fixture_details_from_row(row, teams)
    extra_time = row[10]
    penalties = row[11]
    round = row[12]
    return CupFixture(*values, extra_time, penalties, round)


def teams(fixtures: list[Fixture]) -> set[model.teams.Team]:
    teams = set()
    for fixture in fixtures:
        teams.add(fixture.home_team)
        teams.add(fixture.away_team)
    return teams


def get_team_ids(fixture_rows: list[list]) -> set[int]:
    home_id_index = 4
    away_id_index = 5
    team_ids = set()
    for fixture_row in fixture_rows:
        team_ids.add(fixture_row[home_id_index])
        team_ids.add(fixture_row[away_id_index])
    return team_ids


def create_fixtures(competition: model.competitions.Competition, fixture_rows: list[list]) -> list[Fixture]:
    team_ids = get_team_ids(fixture_rows)
    teams = model.teams.load_teams(team_ids)

    fixtures = []
    for fixture_row in fixture_rows:
        if competition.type == model.competitions.CompetitionType.LEAGUE:
            fixture = create_fixture_from_row(fixture_row, teams)
        else:
            fixture = create_cup_fixture_from_row(fixture_row, teams)
        fixtures.append(fixture)

    sort_fixtures(fixtures)
    return fixtures


def fixtures_per_team(fixtures: list[Fixture]) -> dict[model.teams.Team, list[Fixture]]:
    sort_fixtures(fixtures)
    team_fixtures = {}
    for fixture in fixtures:
        team_fixtures.setdefault(fixture.home_team, [])
        team_fixtures.setdefault(fixture.away_team, [])
        team_fixtures[fixture.home_team].append(fixture)
        team_fixtures[fixture.away_team].append(fixture)
    return team_fixtures


def load_fixtures_within_window(competition: model.competitions.Competition, days: int) -> list[Fixture]:
    with sql.sql.Database(football_api.structure.database) as db:
        competition_constraint = f"{ColumnNames.Competition_ID.name}={competition.id}"
        window_constraint = f"{ColumnNames.Date.name} {Keywords.BETWEEN.name} datetime('now') {Keywords.AND.name} datetime('now', '+{days} day')"

        if competition.type == model.competitions.CompetitionType.LEAGUE:
            table = Fixture.sql_table()
        else:
            table = CupFixture.sql_table()

        fixture_rows = db.fetch_all_rows(table, [competition_constraint, window_constraint])
        return create_fixtures(competition, fixture_rows)


def load_head_to_head_fixtures(
        competition: model.competitions.Competition,
        home_team: model.teams.Team,
        away_team: model.teams.Team
) -> list[Fixture]:
    with sql.sql.Database(football_api.structure.database) as db:
        competition_constraint = f"{ColumnNames.Competition_ID.name}={competition.id}"

        home_vs_away = (
            f"{ColumnNames.Home_ID.name}={home_team.id} "
            f"{Keywords.AND.name} "
            f"{ColumnNames.Away_ID.name}={away_team.id}"
        )
        away_vs_home = (
            f"{ColumnNames.Home_ID.name}={away_team.id} "
            f"{Keywords.AND.name} "
            f"{ColumnNames.Away_ID.name}={home_team.id}"
        )
        home_away_constraint = f"({home_vs_away}) {Keywords.OR.name} ({away_vs_home})"

        constraints = [competition_constraint, home_away_constraint]

        if competition.type == model.competitions.CompetitionType.LEAGUE:
            table = Fixture.sql_table()
        else:
            table = CupFixture.sql_table()

        fixture_rows = db.fetch_all_rows(table, constraints)
        return create_fixtures(competition, fixture_rows)

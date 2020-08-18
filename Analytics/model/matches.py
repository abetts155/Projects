import datetime
import enum
import pickle
import typing

from . import competitions, events, teams
from miscellaneous import messages, wyscout
from sql.sql_columns import Affinity, Column, ColumnNames
from sql import sql_columns, sql_tables


class Referee:
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


def create_referee_from_json(data: typing.Dict):
    print(data)
    id_ = int(data[wyscout.JSON_Keys.wyId])
    name = wyscout.decode_json_string(data[wyscout.JSON_Keys.shortName])
    date_of_birth = None
    if data[wyscout.JSON_Keys.birthDate] is not None:
        date_of_birth = datetime.datetime.strptime(data[wyscout.JSON_Keys.birthDate], '%Y-%m-%d')
    Referee.inventory[id_] = Referee(id_, name, date_of_birth)


class Substitution:
    __slots__ = ['minute', 'player_off', 'player_on']

    def __init__(self, minute: int, player_off: teams.Player, player_on: teams.Player):
        self.minute = minute
        self.player_off = player_off
        self.player_on = player_on

    def __str__(self):
        return '{}:{} => {}'.format(self.minute, self.player_off, self.player_on)


class Side(enum.Enum):
    HOME = enum.auto()
    AWAY = enum.auto()


class Metric(enum.Enum):
    CORNERS = enum.auto()
    PASSES = enum.auto()
    SHOTS = enum.auto()


class MatchMetrics:
    def __init__(self):
        self._data = {(side, metric): 0 for side in Side for metric in Metric}

    def increment(self, side: Side, metric: Metric):
        self._data[(side, metric)] += 1

    def query(self, side: Side, metric: Metric) -> int:
        return self._data[(side, metric)]

    def possession(self, side: Side) -> float:
        away_passes = self._data[(Side.AWAY, Metric.PASSES)]
        home_passes = self._data[(Side.HOME, Metric.PASSES)]
        total_passes = away_passes + home_passes
        assert total_passes > 0
        if side == Side.AWAY:
            return (away_passes / total_passes) * 100
        else:
            return (home_passes / total_passes) * 100

    def __add__(self, other: "MatchMetrics"):
        if type(other) is MatchMetrics:
            combination = MatchMetrics()
            for side in Side:
                for metric in Metric:
                    key = (side, metric)
                    combination._data[key] = self._data[key] + other._data[key]
            return combination
        else:
            return NotImplemented

    def __radd__(self, other: "MatchMetrics"):
        return self.__add__(other)


class Match(list):
    table = None
    inventory = {}

    def __init__(self,
                 id_: int,
                 season_id: int,
                 competition: competitions.Competition,
                 match_date: datetime.date,
                 home_team: teams.Team,
                 home_score_ht: int,
                 home_score_ft: int,
                 home_lineup: typing.Set[teams.Player],
                 home_bench: typing.Set[teams.Player],
                 home_substitutions: typing.List[Substitution],
                 away_team: teams.Team,
                 away_score_ht: int,
                 away_score_ft: int,
                 away_lineup: typing.Set[teams.Player],
                 away_bench: typing.Set[teams.Player],
                 away_substitutions: typing.List[Substitution]):
        list.__init__(self)
        self._id = id_
        self._season_id = season_id
        self._competition = competition
        self._match_date = match_date
        self._home_team = home_team
        self._home_score_ht = home_score_ht
        self._home_score_ft = home_score_ft
        self._home_lineup = home_lineup
        self._home_bench = home_bench
        self._home_substitutions = home_substitutions
        self._away_team = away_team
        self._away_score_ht = away_score_ht
        self._away_score_ft = away_score_ft
        self._away_lineup = away_lineup
        self._away_bench = away_bench
        self._away_substitutions = away_substitutions
        self._events = []
        self._metrics = {period: MatchMetrics() for period in events.Period}

    @property
    def away_team(self) -> teams.Team:
        return self._away_team

    @property
    def away_lineup(self) -> typing.Set[teams.Player]:
        return self._away_lineup

    @property
    def away_bench(self) -> typing.Set[teams.Player]:
        return self._away_bench

    @property
    def away_score_ht(self) -> int:
        return self._away_score_ht

    @property
    def away_score_ft(self) -> int:
        return self._away_score_ft

    @property
    def away_substitutions(self) -> typing.List[Substitution]:
        return self._away_substitutions

    @property
    def competition(self) -> competitions.Competition:
        return self._competition

    @property
    def home_team(self) -> teams.Team:
        return self._home_team

    @property
    def home_lineup(self) -> typing.Set[teams.Player]:
        return self._home_lineup

    @property
    def home_bench(self) -> typing.Set[teams.Player]:
        return self._home_bench

    @property
    def home_score_ht(self) -> int:
        return self._home_score_ht

    @property
    def home_score_ft(self) -> int:
        return self._home_score_ft

    @property
    def home_substitutions(self) -> typing.List[Substitution]:
        return self._home_substitutions

    @property
    def id(self) -> int:
        return self._id

    @property
    def match_date(self) -> datetime.date:
        return self._match_date

    @property
    def season_id(self) -> int:
        return self._season_id

    def sql_values(self):
        values = [self.id,
                  self.season_id,
                  self.competition.id,
                  self.match_date.strftime('%Y-%m-%d'),
                  self.home_team.id,
                  self.home_score_ht,
                  self.home_score_ft,
                  pickle.dumps(self.home_lineup),
                  pickle.dumps(self.home_bench),
                  pickle.dumps(self.home_substitutions),
                  self.away_team.id,
                  self.away_score_ht,
                  self.away_score_ft,
                  pickle.dumps(self.away_lineup),
                  pickle.dumps(self.away_bench),
                  pickle.dumps(self.away_substitutions)]
        assert len(values) == len(self.__class__.sql_table().columns)
        return values

    @classmethod
    def sql_table(cls) -> sql_tables.Table:
        if cls.table is None:
            competition_column = Column(ColumnNames.Competition_ID.name, Affinity.INTEGER)
            home_team_column = Column(ColumnNames.Home_ID.name, Affinity.INTEGER)
            away_team_column = Column(ColumnNames.Away_ID.name, Affinity.INTEGER)
            cls.table = sql_tables.Table(cls.__name__,
                                         sql_columns.id_column(),
                                         [sql_columns.id_column(),
                                          Column(ColumnNames.Season_ID.name, Affinity.INTEGER),
                                          competition_column,
                                          Column(ColumnNames.Match_Date.name, Affinity.TEXT),
                                          home_team_column,
                                          Column(ColumnNames.Home_Score_HT.name, Affinity.INTEGER),
                                          Column(ColumnNames.Home_Score_FT.name, Affinity.INTEGER),
                                          Column(ColumnNames.Home_Lineup.name, Affinity.BLOB),
                                          Column(ColumnNames.Home_Bench.name, Affinity.BLOB),
                                          Column(ColumnNames.Home_Substitutions.name, Affinity.BLOB),
                                          away_team_column,
                                          Column(ColumnNames.Away_Score_HT.name, Affinity.INTEGER),
                                          Column(ColumnNames.Away_Score_FT.name, Affinity.INTEGER),
                                          Column(ColumnNames.Away_Lineup.name, Affinity.BLOB),
                                          Column(ColumnNames.Away_Bench.name, Affinity.BLOB),
                                          Column(ColumnNames.Away_Substitutions.name, Affinity.BLOB)])
            cls.table.add_foreign_key(competition_column, competitions.Competition.sql_table())
            cls.table.add_foreign_key(home_team_column, teams.Team.sql_table())
            cls.table.add_foreign_key(away_team_column, teams.Team.sql_table())
        return cls.table

    def __hash__(self):
        return self.id

    def __eq__(self, other):
        if type(other) == type(self):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def compute_metrics(self):
        for event in self:
            if event.team == self.home_team:
                side = Side.HOME
            else:
                side = Side.AWAY
            if events.is_shot(event):
                self._metrics[event.period].increment(side, Metric.SHOTS)
            if events.is_pass(event):
                self._metrics[event.period].increment(side, Metric.PASSES)
            if events.is_corner(event):
                self._metrics[event.period].increment(side, Metric.CORNERS)

    def metrics(self, period: events.Period):
        return self._metrics[period]

    def __str__(self):
        value = '{} {} ({}) {}\n'.format('=' * 5, self.competition.name, self.match_date.strftime('%d-%m-%Y'), '=' * 5)
        value += '{} {}-{} {}\n'.format(self.home_team.name,
                                        self.home_score_ft,
                                        self.away_score_ft,
                                        self.away_team.name)
        return value


def create_substitutions_from_json(data: typing.Dict) -> typing.List[Substitution]:
    substitutions = []
    if data != 'null':
        for substitution_data in data:
            minute = int(substitution_data[wyscout.JSON_Keys.minute])
            player_off_id = int(substitution_data[wyscout.JSON_Keys.playerOut])
            player_on_id = int(substitution_data[wyscout.JSON_Keys.playerIn])
            if player_on_id and player_off_id:
                substitution = Substitution(minute,
                                            teams.Player.inventory[player_off_id],
                                            teams.Player.inventory[player_on_id])
                substitutions.append(substitution)
    return substitutions


def create_players_from_json(data: typing.Dict) -> typing.Set[teams.Player]:
    selection = set()
    for player_data in data:
        player_id = int(player_data[wyscout.JSON_Keys.playerId])
        if player_id in teams.Player.inventory:
            player = teams.Player.inventory[player_id]
            selection.add(player)
        else:
            messages.warning_message('Unknown player {}'.format(player_id))
    return selection


def create_selection_from_json(data: typing.Dict) -> typing.Tuple[typing.Set[teams.Player],
                                                                  typing.Set[teams.Player],
                                                                  typing.List[Substitution]]:
    lineup = create_players_from_json(data[wyscout.JSON_Keys.lineup])
    bench = create_players_from_json(data[wyscout.JSON_Keys.bench])
    substitutions = create_substitutions_from_json(data[wyscout.JSON_Keys.substitutions])
    return lineup, bench, substitutions


def create_match_from_json(data: typing.Dict):
    id_ = int(data[wyscout.JSON_Keys.wyId])
    season_id = int(data[wyscout.JSON_Keys.seasonId])
    competition_id = int(data[wyscout.JSON_Keys.competitionId])
    competition = competitions.Competition.inventory[competition_id]
    match_date = datetime.datetime.strptime(data[wyscout.JSON_Keys.dateutc], '%Y-%m-%d %H:%M:%S')

    left_team_data, right_team_data = data[wyscout.JSON_Keys.teamsData].values()
    if left_team_data[wyscout.JSON_Keys.side] == Side.HOME.name.lower():
        assert right_team_data[wyscout.JSON_Keys.side] == Side.AWAY.name.lower()
        home_team_data = left_team_data
        away_team_data = right_team_data
    else:
        assert left_team_data[wyscout.JSON_Keys.side] == Side.AWAY.name.lower()
        assert right_team_data[wyscout.JSON_Keys.side] == Side.HOME.name.lower()
        away_team_data = left_team_data
        home_team_data = right_team_data

    home_team_id = int(home_team_data[wyscout.JSON_Keys.teamId])
    home_team = teams.Team.inventory[home_team_id]
    home_score_ht = int(home_team_data[wyscout.JSON_Keys.scoreHT])
    home_score_ft = int(home_team_data[wyscout.JSON_Keys.score])
    (home_lineup,
     home_bench,
     home_substitutions) = create_selection_from_json(home_team_data[wyscout.JSON_Keys.formation])

    away_team_id = int(away_team_data[wyscout.JSON_Keys.teamId])
    away_team = teams.Team.inventory[away_team_id]
    away_score_ht = int(away_team_data[wyscout.JSON_Keys.scoreHT])
    away_score_ft = int(away_team_data[wyscout.JSON_Keys.score])
    (away_lineup,
     away_bench,
     away_substitutions) = create_selection_from_json(away_team_data[wyscout.JSON_Keys.formation])

    Match.inventory[id_] = Match(id_,
                                 season_id,
                                 competition,
                                 match_date,
                                 home_team,
                                 home_score_ht,
                                 home_score_ft,
                                 home_lineup,
                                 home_bench,
                                 home_substitutions,
                                 away_team,
                                 away_score_ht,
                                 away_score_ft,
                                 away_lineup,
                                 away_bench,
                                 away_substitutions)

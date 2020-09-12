import datetime

from enum import auto, Enum
from model.competitions import Competition
from model.teams import Player, Team
from miscellaneous import messages, wyscout
from pickle import dumps, loads
from sql.sql_columns import id_column, Affinity, Column, ColumnNames
from sql.sql_tables import Table
from typing import Dict, List, Set, Tuple


class Substitution:
    __slots__ = ['minute', 'player_off', 'player_on']

    def __init__(self, minute: int, player_off: Player, player_on: Player):
        self.minute = minute
        self.player_off = player_off
        self.player_on = player_on

    def __str__(self):
        return '{}:{} => {}'.format(self.minute, self.player_off, self.player_on)


class Side(Enum):
    HOME = auto()
    AWAY = auto()


class Match:
    table = None
    inventory = {}

    def __init__(self,
                 id_: int,
                 season_id: int,
                 competition: Competition,
                 match_date: datetime.date,
                 home_team: Team,
                 home_score_ht: int,
                 home_score_ft: int,
                 home_lineup: Set[Player],
                 home_bench: Set[Player],
                 home_substitutions: List[Substitution],
                 away_team: Team,
                 away_score_ht: int,
                 away_score_ft: int,
                 away_lineup: Set[Player],
                 away_bench: Set[Player],
                 away_substitutions: List[Substitution]):
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
        Match.inventory[self.id] = self

    @property
    def away_team(self) -> Team:
        return self._away_team

    @property
    def away_lineup(self) -> Set[Player]:
        return self._away_lineup

    @property
    def away_bench(self) -> Set[Player]:
        return self._away_bench

    @property
    def away_score_ht(self) -> int:
        return self._away_score_ht

    @property
    def away_score_ft(self) -> int:
        return self._away_score_ft

    @property
    def away_substitutions(self) -> List[Substitution]:
        return self._away_substitutions

    @property
    def competition(self) -> Competition:
        return self._competition

    @property
    def home_team(self) -> Team:
        return self._home_team

    @property
    def home_lineup(self) -> Set[Player]:
        return self._home_lineup

    @property
    def home_bench(self) -> Set[Player]:
        return self._home_bench

    @property
    def home_score_ht(self) -> int:
        return self._home_score_ht

    @property
    def home_score_ft(self) -> int:
        return self._home_score_ft

    @property
    def home_substitutions(self) -> List[Substitution]:
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
                  dumps([player.id for player in self.home_lineup]),
                  dumps([player.id for player in self.home_lineup]),
                  dumps([(sub.minute, sub.player_off.id, sub.player_on.id) for sub in self.home_substitutions]),
                  self.away_team.id,
                  self.away_score_ht,
                  self.away_score_ft,
                  dumps([player.id for player in self.away_lineup]),
                  dumps([player.id for player in self.away_bench]),
                  dumps([(sub.minute, sub.player_off.id, sub.player_on.id) for sub in self.away_substitutions])]
        assert len(values) == len(self.__class__.sql_table().columns)
        return values

    @classmethod
    def sql_table(cls) -> Table:
        if cls.table is None:
            competition_column = Column(ColumnNames.Competition_ID.name, Affinity.INTEGER)
            home_team_column = Column(ColumnNames.Home_ID.name, Affinity.INTEGER)
            away_team_column = Column(ColumnNames.Away_ID.name, Affinity.INTEGER)
            cls.table = Table(cls.__name__,
                              id_column(),
                              [id_column(),
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
            cls.table.add_foreign_key(competition_column, Competition.sql_table())
            cls.table.add_foreign_key(home_team_column, Team.sql_table())
            cls.table.add_foreign_key(away_team_column, Team.sql_table())
        return cls.table

    def __hash__(self):
        return self.id

    def __eq__(self, other):
        if type(other) == type(self):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __str__(self):
        value = '{} {} ({}) {}\n'.format('=' * 5, self.competition.name, self.match_date.strftime('%d-%m-%Y'), '=' * 5)
        value += '{} {}-{} {}\n'.format(self.home_team.name,
                                        self.home_score_ft,
                                        self.away_score_ft,
                                        self.away_team.name)
        return value


def create_substitutions_from_json(data: Dict) -> List[Substitution]:
    substitutions = []
    if data != 'null':
        for substitution_data in data:
            minute = int(substitution_data[wyscout.JSON_Keys.minute])
            player_off_id = int(substitution_data[wyscout.JSON_Keys.playerOut])
            player_on_id = int(substitution_data[wyscout.JSON_Keys.playerIn])
            if player_on_id and player_off_id:
                substitution = Substitution(minute,
                                            Player.inventory[player_off_id],
                                            Player.inventory[player_on_id])
                substitutions.append(substitution)
    return substitutions


def create_players_from_json(data: Dict) -> Set[Player]:
    selection = set()
    for player_data in data:
        player_id = int(player_data[wyscout.JSON_Keys.playerId])
        if player_id in Player.inventory:
            player = Player.inventory[player_id]
            selection.add(player)
        else:
            messages.warning_message('Unknown player {}'.format(player_id))
    return selection


def create_selection_from_json(data: Dict) -> Tuple[Set[Player], Set[Player], List[Substitution]]:
    lineup = create_players_from_json(data[wyscout.JSON_Keys.lineup])
    bench = create_players_from_json(data[wyscout.JSON_Keys.bench])
    substitutions = create_substitutions_from_json(data[wyscout.JSON_Keys.substitutions])
    return lineup, bench, substitutions


def create_match_from_json(data: Dict):
    id_ = int(data[wyscout.JSON_Keys.wyId])
    season_id = int(data[wyscout.JSON_Keys.seasonId])
    competition_id = int(data[wyscout.JSON_Keys.competitionId])
    competition = Competition.inventory[competition_id]
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
    home_team = Team.inventory[home_team_id]
    home_score_ht = int(home_team_data[wyscout.JSON_Keys.scoreHT])
    home_score_ft = int(home_team_data[wyscout.JSON_Keys.score])
    (home_lineup,
     home_bench,
     home_substitutions) = create_selection_from_json(home_team_data[wyscout.JSON_Keys.formation])

    away_team_id = int(away_team_data[wyscout.JSON_Keys.teamId])
    away_team = Team.inventory[away_team_id]
    away_score_ht = int(away_team_data[wyscout.JSON_Keys.scoreHT])
    away_score_ft = int(away_team_data[wyscout.JSON_Keys.score])
    (away_lineup,
     away_bench,
     away_substitutions) = create_selection_from_json(away_team_data[wyscout.JSON_Keys.formation])

    Match(id_, season_id, competition, match_date, home_team, home_score_ht, home_score_ft, home_lineup, home_bench,
          home_substitutions, away_team, away_score_ht, away_score_ft, away_lineup, away_bench, away_substitutions)


def create_match_from_row(row: List) -> Match:
    id_ = int(row[0])
    season_id = int(row[1])
    competition_id = int(row[2])
    competition = Competition.inventory[competition_id]
    match_date = datetime.datetime.strptime(row[3], '%Y-%m-%d')
    home_team_id = int(row[4])
    home_team = Team.inventory[home_team_id]
    home_score_ht = int(row[5])
    home_score_ft = int(row[6])
    home_lineup = loads(row[7])
    home_bench = loads(row[8])
    home_substitutions = loads(row[9])
    away_team_id = int(row[10])
    away_team = Team.inventory[away_team_id]
    away_score_ht = int(row[11])
    away_score_ft = int(row[12])
    away_lineup = loads(row[13])
    away_bench = loads(row[14])
    away_substitutions = loads(row[15])

    home_lineup = {Player.inventory[id_] for id_ in home_lineup}
    home_bench = {Player.inventory[id_] for id_ in home_bench}
    home_substitutions = [Substitution(minute, Player.inventory[player_off_id], Player.inventory[player_on_id])
                          for (minute, player_off_id, player_on_id) in home_substitutions]
    away_lineup = {Player.inventory[id_] for id_ in away_lineup}
    away_bench = {Player.inventory[id_] for id_ in away_bench}
    away_substitutions = [Substitution(minute, Player.inventory[player_off_id], Player.inventory[player_on_id])
                          for (minute, player_off_id, player_on_id) in away_substitutions]

    match = Match(id_, season_id, competition, match_date, home_team, home_score_ht, home_score_ft, home_lineup,
                  home_bench, home_substitutions, away_team, away_score_ht, away_score_ft, away_lineup, away_bench,
                  away_substitutions)
    return match

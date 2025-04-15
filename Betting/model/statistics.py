import dataclasses
import pathlib
import typing

import model.fixtures
import model.players
import model.teams
import sql.sql
import sql.sql_tables
from sql.sql_columns import Affinity, Column, ColumnNames


@dataclasses.dataclass(slots=True)
class PlayerStats:
    fixture: model.fixtures.Fixture
    player: model.players.Player
    minutes: int
    offsides: int
    total_shots: int
    shots_on_goal: int
    goals: int
    assists: int
    passes: int
    accurate_passes: int
    total_duels: int
    duels_won: int
    fouls_drawn: int
    fouls_committed: int
    yellow_cards: int
    red_cards: int

    def sql_values(self):
        values = [
            self.fixture.id,
            self.player.id,
            self.minutes,
            self.offsides,
            self.total_shots,
            self.shots_on_goal,
            self.goals,
            self.assists,
            self.passes,
            self.accurate_passes,
            self.total_duels,
            self.duels_won,
            self.fouls_drawn,
            self.fouls_committed,
            self.yellow_cards,
            self.red_cards
        ]
        return values

    @classmethod
    def sql_table(cls) -> sql.sql_tables.Table:
        fixture_id_col = Column(ColumnNames.Fixture_ID.name, Affinity.INTEGER)
        player_id_col = Column(ColumnNames.Player_ID.name, Affinity.INTEGER)

        table = sql.sql_tables.Table(
            model.statistics.PlayerStats.__name__,
            [
                fixture_id_col,
                player_id_col
            ],
            [
                fixture_id_col,
                player_id_col,
                Column(ColumnNames.Minutes.name, Affinity.INTEGER),
                Column(ColumnNames.Offsides.name, Affinity.INTEGER),
                Column(ColumnNames.Total_Shots.name, Affinity.INTEGER),
                Column(ColumnNames.Shots_On_Goal.name, Affinity.INTEGER),
                Column(ColumnNames.Goals.name, Affinity.INTEGER),
                Column(ColumnNames.Assists.name, Affinity.INTEGER),
                Column(ColumnNames.Passes.name, Affinity.INTEGER),
                Column(ColumnNames.Accurate_Passes.name, Affinity.INTEGER),
                Column(ColumnNames.Total_Duels.name, Affinity.INTEGER),
                Column(ColumnNames.Duels_Won.name, Affinity.INTEGER),
                Column(ColumnNames.Fouls_Drawn.name, Affinity.INTEGER),
                Column(ColumnNames.Fouls_Committed.name, Affinity.INTEGER),
                Column(ColumnNames.Yellow_Cards.name, Affinity.INTEGER),
                Column(ColumnNames.Red_Cards.name, Affinity.INTEGER)
            ]
        )

        return table


def create_players_stats_from_json(
        fixture: model.fixtures.Fixture,
        player: model.players.Player,
        json_data: dict
) -> PlayerStats:
    values = [
        int(json_data['games']['minutes']) if json_data['games']['minutes'] is not None else 0,
        int(json_data['offsides']) if json_data['offsides'] is not None else 0,
        int(json_data['shots']['total']) if json_data['shots']['total'] is not None else 0,
        int(json_data['shots']['on']) if json_data['shots']['on'] is not None else 0,
        int(json_data['goals']['total']) if json_data['goals']['total'] is not None else 0,
        int(json_data['goals']['assists']) if json_data['goals']['assists'] is not None else 0,
        int(json_data['passes']['total']) if json_data['passes']['total'] is not None else 0,
        int(json_data['passes']['accuracy']) if json_data['passes']['accuracy'] is not None else 0,
        int(json_data['duels']['total']) if json_data['duels']['total'] is not None else 0,
        int(json_data['duels']['won']) if json_data['duels']['won'] is not None else 0,
        int(json_data['fouls']['drawn']) if json_data['fouls']['drawn'] is not None else 0,
        int(json_data['fouls']['committed']) if json_data['fouls']['committed'] is not None else 0,
        int(json_data['cards']['yellow']) if json_data['cards']['yellow'] is not None else 0,
        int(json_data['cards']['red']) if json_data['cards']['red'] is not None else 0
    ]
    return PlayerStats(fixture, player, *values)


def create_player_stats_from_row(
        fixture: model.fixtures.Fixture,
        player: model.players.Player,
        row: list
) -> PlayerStats:
    values = []
    for index in range(2, 16):
        value = int(row[index])
        values.append(value)
    return PlayerStats(fixture, player, *values)


def load_player_stats(
        database: pathlib.Path,
        fixture: model.fixtures.Fixture,
        player: model.players.Player
) -> typing.Optional[PlayerStats]:
    with sql.sql.Database(database) as db:
        fixture_constraint = f"{ColumnNames.Fixture_ID.name}={fixture.id}"
        player_constraint = f"{ColumnNames.Player_ID.name}={player.id}"
        stats_rows = db.fetch_all_rows(PlayerStats.sql_table(), [fixture_constraint, player_constraint])
        if stats_rows:
            (stats_row,) = stats_rows
            return create_player_stats_from_row(fixture, player, stats_row)


@dataclasses.dataclass(slots=True)
class TeamStats:
    fixture: model.fixtures.Fixture
    team: model.teams.Team
    shots_on_goal: typing.Optional[int]
    shots_off_goal: typing.Optional[int]
    total_shots: typing.Optional[int]
    blocked_shots: typing.Optional[int]
    shots_inside_box: typing.Optional[int]
    shots_outside_box: typing.Optional[int]
    fouls: typing.Optional[int]
    corner_kicks: typing.Optional[int]
    offsides: typing.Optional[int]
    yellow_cards: typing.Optional[int]
    red_cards: typing.Optional[int]
    saves: typing.Optional[int]
    passes: typing.Optional[int]
    accurate_passes: typing.Optional[int]
    expected_goals: typing.Optional[float]
    goals: typing.Optional[int] = None

    def __post_init__(self):
        result = self.fixture.result(model.fixtures.Period.FULL)
        if self.fixture.home_team == self.team:
            self.goals = result.left
        else:
            self.goals = result.right

    def sql_values(self):
        values = [
            self.fixture.id,
            self.team.id,
            self.shots_on_goal,
            self.shots_off_goal,
            self.total_shots,
            self.blocked_shots,
            self.shots_inside_box,
            self.shots_outside_box,
            self.fouls,
            self.corner_kicks,
            self.offsides,
            self.yellow_cards,
            self.red_cards,
            self.saves,
            self.passes,
            self.accurate_passes,
            self.expected_goals
        ]
        return values

    @classmethod
    def sql_table(cls) -> sql.sql_tables.Table:
        fixture_id_col = Column(ColumnNames.Fixture_ID.name, Affinity.INTEGER)
        team_id_col = Column(ColumnNames.Team_ID.name, Affinity.INTEGER)

        table = sql.sql_tables.Table(
            model.statistics.TeamStats.__name__,
            [
                fixture_id_col,
                team_id_col
            ],
            [
                fixture_id_col,
                team_id_col,
                Column(ColumnNames.Shots_On_Goal.name, Affinity.INTEGER),
                Column(ColumnNames.Shots_Off_Goal.name, Affinity.INTEGER),
                Column(ColumnNames.Total_Shots.name, Affinity.INTEGER),
                Column(ColumnNames.Blocked_Shots.name, Affinity.INTEGER),
                Column(ColumnNames.Shots_Inside_Box.name, Affinity.INTEGER),
                Column(ColumnNames.Shots_Outside_Box.name, Affinity.INTEGER),
                Column(ColumnNames.Fouls.name, Affinity.INTEGER),
                Column(ColumnNames.Corner_Kicks.name, Affinity.INTEGER),
                Column(ColumnNames.Offsides.name, Affinity.INTEGER),
                Column(ColumnNames.Yellow_Cards.name, Affinity.INTEGER),
                Column(ColumnNames.Red_Cards.name, Affinity.INTEGER),
                Column(ColumnNames.Saves.name, Affinity.INTEGER),
                Column(ColumnNames.Passes.name, Affinity.INTEGER),
                Column(ColumnNames.Accurate_Passes.name, Affinity.INTEGER),
                Column(ColumnNames.Expected_Goals.name, Affinity.REAL),
            ]
        )

        return table


def create_team_stats_from_json(fixture: model.fixtures.Fixture, team: model.teams.Team, json_data: dict) -> TeamStats:
    stats_dict = {item['type']: item['value'] for item in json_data["statistics"]}

    expected_fields = [
        ('Shots on Goal', int),
        ('Shots off Goal', int),
        ('Total Shots', int),
        ('Blocked Shots', int),
        ('Shots insidebox', int),
        ('Shots outsidebox', int),
        ('Fouls', int),
        ('Corner Kicks', int),
        ('Offsides', int),
        ('Yellow Cards', int),
        ('Red Cards', int),
        ('Goalkeeper Saves', int),
        ('Total passes', int),
        ('Passes accurate', int),
        ('expected_goals', float),
    ]

    values = []
    for field, field_type in expected_fields:
        if field in stats_dict and stats_dict[field] is not None:
            values.append(field_type(stats_dict[field]))
        else:
            values.append(None)

    return TeamStats(fixture, team, *values)


def create_team_stats_from_row(fixture: model.fixtures.Fixture, team: model.teams.Team, row: list) -> TeamStats:
    values = []
    for index in range(2, 17):
        value = int(row[index]) if row[index] else None
        values.append(value)
    return TeamStats(fixture, team, *values)


def load_team_stats(
        database: pathlib.Path,
        fixture: model.fixtures.Fixture,
        team: model.teams.Team
) -> typing.Optional[TeamStats]:
    with sql.sql.Database(database) as db:
        fixture_constraint = f"{ColumnNames.Fixture_ID.name}={fixture.id}"
        team_constraint = f"{ColumnNames.Team_ID.name}={team.id}"
        stats_rows = db.fetch_all_rows(TeamStats.sql_table(), [fixture_constraint, team_constraint])
        if stats_rows:
            (stats_row,) = stats_rows
            return create_team_stats_from_row(fixture, team, stats_row)


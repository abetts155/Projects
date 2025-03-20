import dataclasses
import typing

import football_api.structure
import model.fixtures
import model.teams
import sql.sql
import sql.sql_tables

from sql.sql_columns import Affinity, Column, ColumnNames


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


def create_stats_from_json(fixture: model.fixtures.Fixture, team: model.teams.Team, json_data: dict) -> TeamStats:
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
    for field, type in expected_fields:
        if field in stats_dict and stats_dict[field] is not None:
            values.append(type(stats_dict[field]))
        else:
            values.append(None)

    return TeamStats(fixture, team, *values)


def create_stats_from_row(fixture: model.fixtures.Fixture, team: model.teams.Team, row: list) -> TeamStats:
    return TeamStats(
        fixture,
        team,
        int(row[2]) if row[2] else None,
        int(row[3]) if row[3] else None,
        int(row[4]) if row[4] else None,
        int(row[5]) if row[5] else None,
        int(row[6]) if row[6] else None,
        int(row[7]) if row[7] else None,
        int(row[8]) if row[8] else None,
        int(row[9]) if row[9] else None,
        int(row[10]) if row[10] else None,
        int(row[11]) if row[11] else None,
        int(row[12]) if row[12] else None,
        int(row[13]) if row[13] else None,
        int(row[14]) if row[14] else None,
        int(row[15]) if row[15] else None,
        float(row[16]) if row[16] else None
    )


def load_stats(fixture: model.fixtures.Fixture, team: model.teams.Team) -> typing.Optional[TeamStats]:
    with sql.sql.Database(football_api.structure.database) as db:
        fixture_constraint = f"{ColumnNames.Fixture_ID.name}={fixture.id}"
        team_constraint = f"{ColumnNames.Team_ID.name}={team.id}"
        stats_rows = db.fetch_all_rows(TeamStats.sql_table(), [fixture_constraint, team_constraint])
        if stats_rows:
            (stats_row,) = stats_rows
            return create_stats_from_row(fixture, team, stats_row)

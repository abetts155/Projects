import dataclasses
import pathlib
import typing

import model.fixtures
import model.teams
import sql.sql
import sql.sql_tables

from sql.sql_columns import Affinity, Column, ColumnNames


player_id_separator = ' '

@dataclasses.dataclass(slots=True)
class Lineup:
    fixture: model.fixtures.Fixture
    team: model.teams.Team
    starting_11: list[int] = dataclasses.field(default_factory=list)
    substitutes: list[int] = dataclasses.field(default_factory=list)

    def sql_values(self):
        starting_11_str = [str(player_id) for player_id in self.starting_11]
        substitutes_str = [str(player_id) for player_id in self.substitutes]
        values = [
            self.fixture.id,
            self.team.id,
            f"{player_id_separator}".join(starting_11_str) if starting_11_str else None,
            f"{player_id_separator}".join(substitutes_str) if substitutes_str else None
        ]
        return values

    @classmethod
    def sql_table(cls) -> sql.sql_tables.Table:
        fixture_id_col = Column(ColumnNames.Fixture_ID.name, Affinity.INTEGER)
        team_id_col = Column(ColumnNames.Team_ID.name, Affinity.INTEGER)

        table = sql.sql_tables.Table(
            model.lineups.Lineup.__name__,
            [
                fixture_id_col,
                team_id_col
            ],
            [
                fixture_id_col,
                team_id_col,
                Column(ColumnNames.Starting_11.name, Affinity.TEXT),
                Column(ColumnNames.Substitutes.name, Affinity.TEXT)
            ]
        )

        return table


def create_lineup_from_json(fixture: model.fixtures.Fixture, team: model.teams.Team, json_data: dict) -> Lineup:
    starting_11 = []
    if 'startXI' in json_data:
        for player_json in json_data['startXI']:
            if player_json['player']['id'] is not None:
                player_id = int(player_json['player']['id'])
                starting_11.append(player_id)

    substitutes = []
    if 'substitutes' in json_data:
        for player_json in json_data['substitutes']:
            if player_json['player']['id'] is not None:
                player_id = int(player_json['player']['id'])
                substitutes.append(player_id)

    return Lineup(fixture, team, starting_11, substitutes)


def create_lineup_from_row(fixture: model.fixtures.Fixture, team: model.teams.Team, row: list) -> Lineup:
    if row[2] is not None:
        starting_11 = list(map(int, row[2].split(player_id_separator)))
    else:
        starting_11 = []

    if row[3] is not None:
        substitutes = list(map(int, row[3].split(player_id_separator)))
    else:
        substitutes = []

    return Lineup(fixture, team, starting_11, substitutes)


def load_lineup(
        database: pathlib.Path,
        fixture: model.fixtures.Fixture,
        team: model.teams.Team
) -> typing.Optional[Lineup]:
    with sql.sql.Database(database) as db:
        fixture_constraint = f"{ColumnNames.Fixture_ID.name}={fixture.id}"
        team_constraint = f"{ColumnNames.Team_ID.name}={team.id}"
        lineup_rows = db.fetch_all_rows(Lineup.sql_table(), [fixture_constraint, team_constraint])
        if lineup_rows:
            (lineup_row,) = lineup_rows
            return create_lineup_from_row(fixture, team, lineup_row)

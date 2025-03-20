import football_api.structure
import sql.sql
import sql.sql_columns
import sql.sql_tables

from sql.sql_columns import Affinity, Column, ColumnNames
from sql.sql_language import Keywords


class Team:
    def __init__(self, id_: int, name: str, country: str, venue_name: str):
        self.id = id_
        self.name = name
        self.country = country
        self.venue_name = venue_name

    def sql_values(self):
        values = [self.id, self.name, self.country, self.venue_name]
        assert len(values) == len(self.__class__.sql_table().columns)
        return values

    def get_team_logo(self) -> str:
        return f"https://media.api-sports.io/football/teams/{self.id}.png"

    @classmethod
    def sql_table(cls) -> sql.sql_tables.Table:
        id_col = Column(ColumnNames.ID.name, Affinity.INTEGER)
        name_col = Column(ColumnNames.Name.name, Affinity.TEXT)

        return sql.sql_tables.Table(
            cls.__name__,
            [
                id_col
            ],
            [
                id_col,
                name_col,
                Column(ColumnNames.Country.name, Affinity.TEXT),
                Column(ColumnNames.Venue_Name.name, Affinity.TEXT)
            ]
        )

    def __eq__(self, other):
        if type(other) == type(self):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __hash__(self):
        return self.id

    def __str__(self):
        return '{}={}'.format(self.id, self.name)


def create_team_from_json(json_data: dict) -> Team:
    id_ = int(json_data['team_id'])
    team = Team(id_, json_data['name'], json_data['country'], json_data['venue_name'])
    return team


def create_team_from_row(row: list) -> Team:
    id_ = int(row[0])
    name = row[1]
    country = row[2]
    venue_name = row[3]
    team = Team(id_, name, country, venue_name)
    return team


def load_teams(team_name: str) -> list[Team]:
    teams = []
    with sql.sql.Database(football_api.structure.database) as db:
        name_constraint = f"{ColumnNames.Name.name} {Keywords.LIKE.name} {team_name}"
        team_rows = db.fetch_all_rows(Team.sql_table(), [name_constraint])
        if team_rows:
            for row in team_rows:
                team = create_team_from_row(row)
                teams.append(team)
    return teams


def load_teams(team_ids: set[int]) -> dict[int, Team]:
    teams = {}
    with sql.sql.Database(football_api.structure.database) as db:
        id_constraint = f"{ColumnNames.ID.name} {Keywords.IN.name} ({', '.join(map(str, team_ids))})"
        team_rows = db.fetch_all_rows(Team.sql_table(), [id_constraint])
        for row in team_rows:
            team = create_team_from_row(row)
            teams[team.id] = team
    return teams




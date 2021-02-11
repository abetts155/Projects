from lib.messages import warning_message
from sql import sql_columns, sql_tables
from sql.sql_columns import Affinity, Column, ColumnNames
from typing import Dict, List


class Team:
    table = None
    inventory = {}

    def __init__(self, id_: int, name: str, country: str, logo: str):
        self._id = id_
        self._name = name
        self._country = country
        self._logo = logo

    @property
    def id(self) -> int:
        return self._id

    @property
    def name(self) -> str:
        return self._name

    @property
    def country(self) -> str:
        return self._country

    @property
    def logo(self) -> str:
        return self._logo

    def sql_values(self):
        values = [self.id, self.name, self.country, self.logo]
        assert len(values) == len(self.__class__.sql_table().columns)
        return values

    @classmethod
    def sql_table(cls) -> sql_tables.Table:
        if cls.table is None:
            cls.table = sql_tables.Table(cls.__name__,
                                         sql_columns.id_column(),
                                         [sql_columns.id_column(),
                                          sql_columns.name_column(),
                                          Column(ColumnNames.Country.name, Affinity.TEXT),
                                          Column(ColumnNames.Logo.name, Affinity.TEXT)])
        return cls.table

    def __eq__(self, other):
        if type(other) == type(self):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __hash__(self):
        return self.id

    def __str__(self):
        return '{}={}'.format(self.id, self.name)

    @classmethod
    def find_team(cls, id_: int, name: str) -> "Team" or None:
        try:
            return cls.inventory[id_]
        except KeyError:
            warning_message("No team '{}' with ID {} in the team inventory.".format(name, id_))


def create_team_from_json(data: Dict):
    id_ = int(data['team_id'])
    team = Team(id_, data['name'], data['country'], data['logo'])
    Team.inventory[team.id] = team


def create_team_from_row(row: List):
    id_ = int(row[0])
    name = row[1]
    country = row[2]
    logo = row[3]
    team = Team(id_, name, country, logo)
    Team.inventory[team.id] = team

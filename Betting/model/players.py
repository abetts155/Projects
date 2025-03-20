import datetime

import sql.sql
import sql.sql_columns
import sql.sql_tables

from sql.sql_columns import Affinity, Column, ColumnNames


class Player:
    def __init__(self, id_: int, name: str, firstname: str, lastname: str, country: str, dob: datetime.date):
        self.id = id_
        self.name = name
        self.firstname = firstname
        self.lastname = lastname
        self.country = country
        self.dob = dob

    def sql_values(self):
        values = [self.id, self.name, self.firstname, self.lastname, self.country, self.dob]
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
                Column(ColumnNames.Name.name, Affinity.TEXT),
                Column(ColumnNames.First_Name.name, Affinity.TEXT),
                Column(ColumnNames.Last_Name.name, Affinity.TEXT),
                Column(ColumnNames.Country.name, Affinity.TEXT),
                Column(ColumnNames.Date_Of_Birth.name, Affinity.TEXT)
            ]
        )

    def __eq__(self, other):
        if type(other) == type(self):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __hash__(self):
        return self.id

    def __str__(self):
        return f"{self.id} {self.firstname} {self.lastname}"


def create_player_from_json(json_data: dict) -> Player:
    if json_data['player']['birth']['date'] is not None:
        dob = datetime.datetime.strptime(json_data['player']['birth']['date'], "%Y-%m-%d")
    else:
        dob = None

    return Player(
        int(json_data['player']['id']),
        json_data['player']['name'],
        json_data['player']['firstname'],
        json_data['player']['lastname'],
        json_data['player']['nationality'],
        dob
    )


def create_player_from_row(row: list) -> Player:
    if row[5] is not None:
        dob = datetime.datetime.strptime(row[5], "%Y-%m-%d")
    else:
        dob = None

    return Player(
        int(row[0]),
        row[1],
        row[2],
        row[3],
        row[4],
        dob
    )

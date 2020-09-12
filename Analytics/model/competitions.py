import typing

from miscellaneous import wyscout
from sql import sql_columns, sql_tables


class Competition:
    table = None
    inventory = {}

    def __init__(self, id_: int, name: str):
        self._id = id_
        self._name = name
        Competition.inventory[self.id] = self

    @property
    def id(self) -> int:
        return self._id

    @property
    def name(self) -> str:
        return self._name

    def sql_values(self):
        values = [self.id, self.name]
        assert len(values) == len(self.__class__.sql_table().columns)
        return values

    @classmethod
    def sql_table(cls) -> sql_tables.Table:
        if cls.table is None:
            cls.table = sql_tables.Table(cls.__name__,
                                         sql_columns.id_column(),
                                         [sql_columns.id_column(), sql_columns.name_column()])
        return cls.table

    def __eq__(self, other):
        if type(other) == type(self):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __str__(self):
        return '{}::{}'.format(self.id, self.name)


def create_competition_from_json(data: typing.Dict):
    id_ = int(data[wyscout.JSON_Keys.wyId])
    name = wyscout.decode_json_string(data[wyscout.JSON_Keys.name])
    Competition(id_, name)


def create_competition_from_row(row: typing.List):
    id_ = int(row[0])
    name = row[1]
    Competition(id_, name)

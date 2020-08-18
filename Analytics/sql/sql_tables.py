import sqlite3
import typing

from miscellaneous import messages
from sql.sql_columns import Column
from sql.sql_language import Keywords


class Table:
    def __init__(self, name: str, primary_key: Column, columns: typing.List[Column]):
        assert primary_key in columns
        self._name = name
        self._primary_key = primary_key
        self._columns = columns
        self._rows = []
        self._foreign_keys = {}

    @property
    def columns(self) -> typing.List[Column]:
        return self._columns

    @property
    def name(self) -> str:
        return self._name

    @property
    def primary_key(self) -> Column:
        return self._primary_key

    @property
    def rows(self) -> typing.List:
        return self._rows

    def add_row(self, row: typing.List):
        assert len(self._columns) == len(row)
        self._rows.append(row)

    def add_foreign_key(self, column: Column, foreign_table: "Table"):
        assert column in self.columns and self.primary_key != column and column not in self._foreign_keys
        self._foreign_keys[column] = foreign_table

    def drop(self, cursor: sqlite3.Cursor):
        statement = '{} {} {} {} {}'.format(Keywords.DROP.name,
                                            Keywords.TABLE.name,
                                            Keywords.IF.name,
                                            Keywords.EXISTS.name,
                                            self.name)
        messages.verbose_message('SQL: {}'.format(statement))
        cursor.execute(statement)

    def create(self, cursor: sqlite3.Cursor):
        columns_text = []
        for column in self.columns:
            columns_text.append('{} {}'.format(column.name, column.affinity.name))
            if column == self._primary_key:
                columns_text[-1] += ' {} {}'.format(Keywords.PRIMARY.name, Keywords.KEY.name)

        for column, foreign_table in self._foreign_keys.items():
            columns_text.append('{} {}({}) {} {}({})'.format(Keywords.FOREIGN.name,
                                                             Keywords.KEY.name,
                                                             column.name,
                                                             Keywords.REFERENCES.name,
                                                             foreign_table.name,
                                                             foreign_table.primary_key.name))

        statement = '{} {} {} ({})'.format(Keywords.CREATE.name,
                                           Keywords.TABLE.name,
                                           self.name,
                                           ', '.join(columns_text))
        messages.verbose_message('SQL: {}'.format(statement))
        cursor.execute(statement)

    def insert_rows(self, cursor: sqlite3.Cursor):
        statement = '{} {} {} {} ({})'.format(Keywords.INSERT.name,
                                              Keywords.INTO.name,
                                              self.name,
                                              Keywords.VALUES.name,
                                              ','.join('?' for _ in self.columns))
        messages.verbose_message('SQL: {}'.format(statement))
        cursor.executemany(statement, self.rows)

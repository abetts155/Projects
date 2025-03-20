import sqlite3

import lib.messages

from sql.sql_columns import Column
from sql.sql_language import Keywords


class Table:
    def __init__(self, name: str, primary_key: list[Column], columns: list[Column]):
        self.name = name
        self.primary_key = primary_key
        self.columns = columns
        self.rows = []
        self.foreign_keys = {}

    def add_row(self, row: list):
        assert len(self.columns) == len(row)
        self.rows.append(row)

    def add_foreign_key(self, column: Column, foreign_table: "Table", foreign_column: Column):
        assert column in self.columns and self.primary_key != column and column not in self.foreign_keys
        self.foreign_keys[column] = foreign_table

    def drop(self, cursor: sqlite3.Cursor):
        statement = f"{Keywords.DROP.name} {Keywords.TABLE.name} {Keywords.IF.name} {Keywords.EXISTS.name} {self.name}"
        lib.messages.debug_message(statement)
        cursor.execute(statement)

    def create(self, cursor: sqlite3.Cursor):
        columns_text = []
        for column in self.columns:
            columns_text.append(f'{column.name} {column.affinity.name}')

        if self.primary_key:
            primary_columns =  ', '.join(column.name for column in self.primary_key)
            columns_text.append(f"{Keywords.PRIMARY.name} {Keywords.KEY.name} ({primary_columns})")

        for column, (foreign_table, foreign_column) in self.foreign_keys.items():
            columns_text.append(
                f"{Keywords.FOREIGN.name} {Keywords.KEY.name}({column.name}) {Keywords.REFERENCES.name} "
                f"{foreign_table.name}({foreign_column.name})"
            )

        statement = f"""
{Keywords.CREATE.name} {Keywords.TABLE.name} {Keywords.IF.name} {Keywords.NOT.name} {Keywords.EXISTS.name} 
{self.name} ({', '.join(columns_text)})
        """

        lib.messages.debug_message(statement)
        cursor.execute(statement)

    def insert_rows(self, cursor: sqlite3.Cursor):
        statement = f"""
{Keywords.INSERT.name} {Keywords.OR.name} {Keywords.REPLACE.name} {Keywords.INTO.name} {self.name} 
{Keywords.VALUES.name} ({','.join('?' for _ in self.columns)})
"""
        lib.messages.debug_message(statement)
        cursor.executemany(statement, self.rows)

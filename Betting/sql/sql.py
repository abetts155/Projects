import sqlite3

import lib.messages
import sql.sql_tables

from sql.sql_language import Characters, Keywords


class Database:
    def __init__(self, name: str):
        self.name = name

    def __enter__(self):
        self.connection = sqlite3.connect(self.name)
        self.cursor = self.connection.cursor()
        return self

    def __exit__(self, exception_type, exception_value, exception_traceback):
        self.connection.commit()
        self.connection.close()

    def _execute(self, statement: str):
        lib.messages.debug_message(statement)
        self.cursor.execute(statement)

    def create_rows(self, table: sql.sql_tables.Table, rows: list):
        table.rows.clear()
        for row in rows:
            table.add_row(row.sql_values())
        table.insert_rows(self.cursor)
        self.connection.commit()

    def drop_table(self, table: sql.sql_tables.Table):
        table.drop(self.cursor)
        self.connection.commit()

    def create_table(self, table: sql.sql_tables.Table):
        table.create(self.cursor)
        self.connection.commit()

    def fetch_all_rows(self, table: sql.sql_tables.Table, constraints=None):
        statement = f"{Keywords.SELECT.name} {Characters.STAR.value} {Keywords.FROM.name} {table.name}"

        if constraints:
            statement = f"{statement} {Keywords.WHERE.name} {f' {Keywords.AND.name} '.join(constraints)}"

        self._execute(statement)
        return self.cursor.fetchall()

    def count_rows(self, table: sql.sql_tables.Table):
        statement = f"{Keywords.SELECT.name} {Keywords.COUNT.name}(*) {Keywords.FROM.name} {table.name}"
        self._execute(statement)
        (value,) = self.cursor.fetchone()
        return int(value)

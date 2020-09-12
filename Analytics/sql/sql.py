import sqlite3

from miscellaneous.messages import debug_message
from model.competitions import Competition
from model.events import Event
from model.matches import Match
from model.teams import Coach, Player, Team
from sql.sql_columns import ColumnNames
from sql.sql_language import Characters, Keywords
from sql.sql_tables import Table


class Database:
    def __init__(self, name: str):
        self._name = name

    def __enter__(self):
        self._connection = sqlite3.connect(self.name)
        self._cursor = self._connection.cursor()
        return self

    def __exit__(self, exception_type, exception_value, exception_traceback):
        self._connection.commit()
        self._connection.close()

    def _execute(self, statement: str):
        debug_message(statement)
        self._cursor.execute(statement)

    @property
    def name(self) -> str:
        return self._name

    def create_rows(self):
        table = Competition.sql_table()
        table.rows.clear()
        for competition in Competition.inventory.values():
            table.add_row(competition.sql_values())
        table.insert_rows(self._cursor)
        self._connection.commit()

        table = Coach.sql_table()
        table.rows.clear()
        for coach in Coach.inventory.values():
            table.add_row(coach.sql_values())
        table.insert_rows(self._cursor)
        self._connection.commit()

        table = Player.sql_table()
        table.rows.clear()
        for player in Player.inventory.values():
            table.add_row(player.sql_values())
        table.insert_rows(self._cursor)
        self._connection.commit()

        table = Team.sql_table()
        table.rows.clear()
        for team in Team.inventory.values():
            table.add_row(team.sql_values())
        table.insert_rows(self._cursor)
        self._connection.commit()

        table = Event.sql_table()
        table.rows.clear()
        for event in Event.inventory.values():
            table.add_row(event.sql_values())
        table.insert_rows(self._cursor)
        self._connection.commit()

        table = Match.sql_table()
        table.rows.clear()
        for match in Match.inventory.values():
            table.add_row(match.sql_values())
        table.insert_rows(self._cursor)
        self._connection.commit()

    def create_tables(self):
        classes = [Competition, Coach, Player, Team, Event, Match]
        for cls in classes:
            table = cls.sql_table()
            table.drop(self._cursor)
            table.create(self._cursor)
        self._connection.commit()

    def fetch_all_rows(self, table: Table):
        statement = "{} {} {} {}".format(Keywords.SELECT.name,
                                         Characters.STAR.value,
                                         Keywords.FROM.name,
                                         table.name)
        self._execute(statement)
        return self._cursor.fetchall()

    def fetch_rows_with_likeness(self, table: Table, name: str):
        statement = "{} {} {} {} {} {} {} '%{}%'".format(Keywords.SELECT.name,
                                                         Characters.STAR.value,
                                                         Keywords.FROM.name,
                                                         table.name,
                                                         Keywords.WHERE.name,
                                                         ColumnNames.Name.name,
                                                         Keywords.LIKE.name,
                                                         name)

        self._execute(statement)
        return self._cursor.fetchall()

    def fetch_match_rows(self, team: Team):
        statement = "{} {} {} {} {} {}={} {} {}={}".format(Keywords.SELECT.name,
                                                           Characters.STAR.value,
                                                           Keywords.FROM.name,
                                                           Match.__name__,
                                                           Keywords.WHERE.name,
                                                           ColumnNames.Home_ID.name,
                                                           team.id,
                                                           Keywords.OR.name,
                                                           ColumnNames.Away_ID.name,
                                                           team.id)
        self._execute(statement)
        return self._cursor.fetchall()

    def fetch_event_rows(self, match: Match):
        statement = "{} {} {} {} {} {}={}".format(Keywords.SELECT.name,
                                                  Characters.STAR.value,
                                                  Keywords.FROM.name,
                                                  Event.__name__,
                                                  Keywords.WHERE.name,
                                                  ColumnNames.Match_ID.name,
                                                  match.id)
        self._execute(statement)
        return self._cursor.fetchall()

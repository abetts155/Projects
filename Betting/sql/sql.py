from lib import messages
from model.fixtures import Fixture, create_fixture_from_row
from model.leagues import League
from model.seasons import Season, create_season_from_row
from model.teams import Team, create_team_from_row
from pathlib import Path
from re import compile
from sqlite3 import connect
from sql.sql_columns import ColumnNames
from sql.sql_language import Characters, Keywords
from sql.sql_tables import Table
from typing import ClassVar, List


class Database:
    def __init__(self, name: str):
        self._name = name

    def __enter__(self):
        self._connection = connect(self.name)
        self._cursor = self._connection.cursor()
        return self

    def __exit__(self, exception_type, exception_value, exception_traceback):
        self._connection.commit()
        self._connection.close()

    def _execute(self, statement: str):
        messages.debug_message(statement)
        self._cursor.execute(statement)

    @property
    def name(self) -> str:
        return self._name

    def create_rows(self, cls: ClassVar):
        table = cls.sql_table()
        table.rows.clear()
        for registered in cls.inventory.values():
            table.add_row(registered.sql_values())
        table.insert_rows(self._cursor)
        self._connection.commit()

    def drop_table(self, cls: ClassVar):
        table = cls.sql_table()
        table.drop(self._cursor)
        self._connection.commit()

    def create_table(self, cls: ClassVar):
        table = cls.sql_table()
        table.create(self._cursor)
        self._connection.commit()

    def fetch_all_rows(self, table: Table, constraints=None):
        statement = "{} {} {} {} {}".format(Keywords.SELECT.name,
                                            Characters.STAR.value,
                                            Keywords.FROM.name,
                                            table.name,
                                            Keywords.WHERE.name)

        if constraints:
            for constraint in constraints:
                statement = '{} ({})'.format(statement, constraint)

                if constraint != constraints[-1]:
                    statement = '{} {}'.format(statement, Keywords.AND.name)
        else:
            statement = '{} {}'.format(statement, Characters.TRUE.value)

        self._execute(statement)
        return self._cursor.fetchall()


def check_database_exists(database_name: str):
    path = Path(database_name)
    if not path.exists():
        messages.error_message("Unable to find database '{}'".format(database_name))


def load_teams(filename: str):
    check_database_exists(filename)
    with Database(filename) as db:
        team_rows = db.fetch_all_rows(Team.sql_table())
        for row in team_rows:
            create_team_from_row(row)


def load_league(filename: str, league: League):
    check_database_exists(filename)
    with Database(filename) as db:
        name_constraint = "{}='{}' {} {}".format(ColumnNames.Code.name,
                                                 league.name,
                                                 Keywords.COLLATE.name,
                                                 Keywords.NOCASE.name)
        constraints = ["{}='{}'".format(ColumnNames.Country.name, league.country), name_constraint]
        season_rows = db.fetch_all_rows(Season.sql_table(), constraints)

        for season_row in season_rows:
            season = create_season_from_row(season_row)
            constraints = ['{}={}'.format(ColumnNames.Season_ID.name, season_row[0]),
                           '{}={}'.format(ColumnNames.Finished.name, Characters.TRUE.value)]
            fixture_rows = db.fetch_all_rows(Fixture.sql_table(), constraints)
            for fixture_row in fixture_rows:
                fixture = create_fixture_from_row(fixture_row)
                if fixture:
                    season.add_fixture(fixture)
            season.sort_fixtures()


def extract_picked_team(database_name: str, team_name: str, league: League = None, error: bool = True) -> List[str]:
    team_name = team_name.replace('*', '%')
    team_name = team_name.replace("'", "''")

    team_rows = []
    constraints = []
    team_constraint = "{} {} '{}' {} {}".format(ColumnNames.Name.name,
                                                Keywords.LIKE.name,
                                                team_name,
                                                Keywords.COLLATE.name,
                                                Keywords.NOCASE.name)
    constraints.append(team_constraint)

    with Database(database_name) as db:
        team_rows.extend(db.fetch_all_rows(Team.sql_table(), constraints))

    if (not team_rows or len(team_rows) > 1) and league:
        team_rows = []
        country_constaint = "{}='{}' {} {}".format(ColumnNames.Country.name,
                                                   league.country,
                                                   Keywords.COLLATE.name,
                                                   Keywords.NOCASE.name)
        constraints.append(country_constaint)

        with Database(database_name) as db:
            team_rows.extend(db.fetch_all_rows(Team.sql_table(), constraints))

    if not team_rows:
        if error:
            messages.error_message("No team '{}' found in the database.".format(team_name))
        else:
            all_rows = []
            with Database(database_name) as db:
                all_rows.extend(db.fetch_all_rows(Team.sql_table(), []))

            expr = compile(r'.*{}.*'.format(team_name.replace('%', '.*')))
            return [row for row in all_rows if expr.match(row[1])]
    elif len(team_rows) > 1:
        if error:
            options = ', '.join(row[1] for row in team_rows)
            messages.error_message("Too many teams match the name '{}' in the database: {}.".format(team_name, options))
        else:
            return team_rows
    else:
        return team_rows


def get_fixtures(db: Database, constraints: List[str]):
    fixture_rows = db.fetch_all_rows(Fixture.sql_table(), constraints)
    fixtures = []
    for row in fixture_rows:
        fixture = create_fixture_from_row(row)
        fixtures.append(fixture)
    return fixtures


def get_current_season(db: Database, league: League):
    country_constraint = "{}='{}' {} {}".format(ColumnNames.Country.name,
                                                league.country,
                                                Keywords.COLLATE.name,
                                                Keywords.NOCASE.name)

    name_constraint = "{}='{}' {} {}".format(ColumnNames.Code.name,
                                             league.name,
                                             Keywords.COLLATE.name,
                                             Keywords.NOCASE.name)

    current_constraint = "{}={}".format(ColumnNames.Current.name,
                                        Characters.TRUE.value)
    constraints = [country_constraint, name_constraint, current_constraint]
    season_rows = db.fetch_all_rows(Season.sql_table(), constraints)
    if season_rows:
        (season,) = season_rows
        return season

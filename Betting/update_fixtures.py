from argparse import ArgumentParser, Namespace
from cli.cli import add_database_option, add_logging_options, add_league_option, set_logging_options
from datetime import datetime
from football_api.football_api import get_fixtures
from football_api.structure import get_fixtures_json, store
from json import load
from lib import messages
from model.fixtures import Fixture, create_fixture_from_json, create_fixture_from_row
from model.leagues import league_register
from model.seasons import Season, create_season_from_row
from model.teams import Team, create_team_from_row
from os import EX_OK
from sql.sql_columns import ColumnNames
from sql.sql_language import Characters, Keywords
from sql.sql import Database, check_database_exists
from sys import exit
from typing import List, Tuple


def parse_command_line():
    parser = ArgumentParser(description='Update football results database with fixture information')
    add_database_option(parser)
    add_logging_options(parser)
    add_league_option(parser, False)

    parser.add_argument('--past',
                        action='store_true',
                        help="if selected, update all historical fixtures; otherwise, update this season's fixtures",
                        default=False)

    parser.add_argument('-f',
                        '--force',
                        action='store_true',
                        help='force an update',
                        default=False)

    return parser.parse_args()


def create_fixtures_json(season: int, force: bool):
    fixtures_json = get_fixtures_json(season)
    if not fixtures_json.exists() or force:
        messages.vanilla_message("Extracting fixtures JSON for '{}'".format(season))
        store(fixtures_json, get_fixtures(season))


def load_fixture_data(season: int):
    fixtures_json = get_fixtures_json(season)
    if not fixtures_json.exists():
        messages.warning_message("No fixtures available for season {}".format(season))
    else:
        with fixtures_json.open() as in_file:
            json_text = load(in_file)
            for data in json_text['api']['fixtures']:
                create_fixture_from_json(data)


def update_leagues(database: str, leagues: List[str], past: bool, force: bool):
    with Database(database) as db:
        team_rows = db.fetch_all_rows(Team.sql_table())
        for row in team_rows:
            create_team_from_row(row)

        for league_code in leagues:
            messages.vanilla_message('Updating {}...'.format(league_code))
            league = league_register[league_code]
            name_constraint = "{}='{}' {} {}".format(ColumnNames.Code.name,
                                                     league.name,
                                                     Keywords.COLLATE.name,
                                                     Keywords.NOCASE.name)
            country_constraint = "{}='{}' {} {}".format(ColumnNames.Country.name,
                                                        league.country,
                                                        Keywords.COLLATE.name,
                                                        Keywords.NOCASE.name)
            current_constraint = "{}={}".format(ColumnNames.Current.name,
                                                Characters.FALSE.value if past else Characters.TRUE.value)
            constraints = [name_constraint, country_constraint, current_constraint]
            season_rows = db.fetch_all_rows(Season.sql_table(), constraints)
            for row in season_rows:
                create_fixtures_json(row[0], force)
                load_fixture_data(row[0])
                db.create_table(Fixture)
                db.create_rows(Fixture)


def canonicalise(date: datetime.date) -> Tuple[int, int, int, int, int]:
    return date.day, date.month, date.year, date.hour, date.minute


def fixtures_played(database: str, season: Season) -> bool:
    played = False
    with Database(database) as db:
        constraints = ["{}='{}'".format(ColumnNames.Season_ID.name, season.id)]
        fixture_rows = db.fetch_all_rows(Fixture.sql_table(), constraints)
        for row in fixture_rows:
            fixture = create_fixture_from_row(row)
            if fixture.home_team is not None and fixture.away_team is not None and not fixture.finished:
                if canonicalise(fixture.updated) <= canonicalise(fixture.date) <= canonicalise(datetime.now()):
                    messages.debug_message('Must update: {}'.format(fixture))
                    played = True
    return played


def update_all(database: str, past: bool, force: bool):
    codes = []
    with Database(database) as db:
        team_rows = db.fetch_all_rows(Team.sql_table())
        for row in team_rows:
            create_team_from_row(row)

        for code, league in league_register.items():
            constraints = ["{}='{}'".format(ColumnNames.Country.name, league.country),
                           "{}='{}'".format(ColumnNames.Code.name, league.name)]
            season_rows = db.fetch_all_rows(Season.sql_table(), constraints)
            for season_row in season_rows:
                season = create_season_from_row(season_row)
                if season.current:
                    if force or fixtures_played(database, season):
                        codes.append(code)

    update_leagues(database, codes, past, force)


def main(arguments: Namespace):
    check_database_exists(arguments.database)

    if arguments.league:
        update_leagues(arguments.database, arguments.league, arguments.past, arguments.force)
    else:
        update_all(arguments.database, arguments.past, arguments.force)

    exit(EX_OK)


if __name__ == '__main__':
    arguments = parse_command_line()
    set_logging_options(arguments)
    main(arguments)

from argparse import ArgumentParser, Namespace
from cli.cli import add_database_option, add_logging_options, add_league_option, set_logging_options
from football_api.football_api import get_fixtures
from football_api.structure import get_fixtures_json, get_seasons_json, store
from json import load
from lib import messages
from model.fixtures import Fixture, create_fixture_from_json
from model.leagues import league_register
from model.seasons import Season, create_season_from_json
from model.teams import Team, create_team_from_row
from os import EX_OK
from sql.sql_columns import ColumnNames
from sql.sql_language import Characters, Keywords
from sql.sql import Database
from sys import exit


def parse_command_line():
    parser = ArgumentParser(description='Update football results database with fixture information')
    add_database_option(parser)
    add_logging_options(parser)
    add_league_option(parser)

    parser.add_argument('--past',
                        action='store_true',
                        help="if selected, update all historical fixtures; otherwise, update this season's fixtures",
                        default=False)

    return parser.parse_args()


def create_fixtures_json(season: int, force: bool):
    fixtures_json = get_fixtures_json(season)
    if not fixtures_json.exists() or force:
        messages.verbose_message("Extracting fixtures JSON for '{}'".format(season))
        store(fixtures_json, get_fixtures(season))


def load_season_data():
    with get_seasons_json().open() as in_file:
        json_text = load(in_file)
        for data in json_text['api']['leagues']:
            create_season_from_json(data)


def load_fixture_data(season: int):
    fixtures_json = get_fixtures_json(season)
    if not fixtures_json.exists():
        messages.warning_message("No fixtures available for season {}".format(season))
    else:
        with fixtures_json.open() as in_file:
            json_text = load(in_file)
            for data in json_text['api']['fixtures']:
                create_fixture_from_json(data)


def main(arguments: Namespace):
    load_season_data()

    with Database(arguments.database) as db:
        db.drop_table(Season)
        db.create_table(Season)
        db.create_rows(Season)
        team_rows = db.fetch_all_rows(Team.sql_table())
        for row in team_rows:
            create_team_from_row(row)

        for league_code in arguments.league:
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
                                                Characters.FALSE.value if arguments.past else Characters.TRUE.value)
            constraints = [name_constraint, country_constraint, current_constraint]
            season_rows = db.fetch_all_rows(Season.sql_table(), constraints)
            for row in season_rows:
                create_fixtures_json(row[0], not arguments.past)
                load_fixture_data(row[0])
                db.create_table(Fixture)
                db.create_rows(Fixture)
    exit(EX_OK)


if __name__ == '__main__':
    arguments = parse_command_line()
    set_logging_options(arguments)
    main(arguments)

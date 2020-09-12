from argparse import ArgumentParser, Namespace
from football_api.frontmatter import get_fixtures_json, get_seasons_json, get_teams_json
from json import load
from lib import messages
from model.competitions import (Fixture,
                                Season,
                                Team,
                                create_fixture_from_json,
                                create_season_from_json,
                                create_team_from_json,
                                create_team_from_row)
from sql.sql import Database


def parse_command_line():
    parser = ArgumentParser(description='Update football results database')

    parser.add_argument('--database',
                        help='update this database',
                        metavar='<DATABASE>',
                        required=True)

    parser.add_argument('--teams',
                        nargs='+',
                        help='update the database with team information for these countries',
                        metavar='<COUNTRY>')

    parser.add_argument('--fixtures',
                        nargs='+',
                        help='update the database with fixture information for these seasons',
                        metavar='<SEASON>')

    parser.add_argument('-d',
                        '--debug',
                        action='store_true',
                        help='print debug messages',
                        default=False)

    parser.add_argument('-v',
                        '--verbose',
                        action='store_true',
                        help='print verbose messages',
                        default=False)

    return parser.parse_args()


def create_table_for_seasons():
    with get_seasons_json().open() as in_file:
        json_text = load(in_file)
        for data in json_text['api']['leagues']:
            create_season_from_json(data)


def create_table_for_teams(country: str):
    teams_json = get_teams_json(country)
    if not teams_json.exists():
        messages.warning_message("No teams available for '{}'".format(country))
    else:
        with teams_json.open() as in_file:
            json_text = load(in_file)
            for data in json_text['api']['teams']:
                create_team_from_json(data)


def create_table_for_fixtures(season: int):
    fixtures_json = get_fixtures_json(season)
    if not fixtures_json.exists():
        messages.warning_message("No fixtures available for season {}".format(season))
    else:
        with fixtures_json.open() as in_file:
            json_text = load(in_file)
            for data in json_text['api']['fixtures']:
                create_fixture_from_json(data)


def main(arguments: Namespace):
    messages.verbose = arguments.verbose
    messages.debug = arguments.debug

    create_table_for_seasons()
    with Database(arguments.database) as db:
        db.drop_table(Season)
        db.create_table(Season)
        db.create_rows(Season)

    if arguments.teams:
        for country in arguments.teams:
            country = country.lower()
            country = country.capitalize()
            create_table_for_teams(country)

        with Database(arguments.database) as db:
            db.create_table(Team)
            db.create_rows(Team)
    else:
        with Database(arguments.database) as db:
            team_rows = db.fetch_all_rows(Team.sql_table())
            for row in team_rows:
                create_team_from_row(row)

    if arguments.fixtures:
        for season in arguments.fixtures:
            create_table_for_fixtures(season)

        with Database(arguments.database) as db:
            db.create_table(Fixture)
            db.create_rows(Fixture)


if __name__ == '__main__':
    main(parse_command_line())

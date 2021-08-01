from argparse import ArgumentParser, Namespace
from cli.cli import add_database_option, add_logging_options, add_country_option, set_logging_options
from football_api.football_api import get_teams
from football_api.structure import get_teams_json, store
from json import load
from lib import messages
from model.teams import Team, create_team_from_json
from os import EX_OK
from sql.sql import Database
from sys import exit


def parse_command_line():
    parser = ArgumentParser(description='Update football results database with team information')
    add_database_option(parser)
    add_logging_options(parser)
    add_country_option(parser)

    parser.add_argument('-f',
                        '--force',
                        action='store_true',
                        help='force an update',
                        default=False)

    return parser.parse_args()


def create_teams_json(country: str, force: bool):
    teams_json = get_teams_json(country)
    if not teams_json.exists() or force:
        messages.verbose_message("Extracting teams JSON for '{}'".format(country))
        store(teams_json, get_teams(country))


def load_team_data(country: str):
    teams_json = get_teams_json(country)
    with teams_json.open() as in_file:
        json_text = load(in_file)
        for data in json_text['api']['teams']:
            create_team_from_json(data)


def main(args: Namespace):
    for country in args.country:
        create_teams_json(country, args.force)
        load_team_data(country)

    with Database(args.database) as db:
        db.create_table(Team)
        db.create_rows(Team)


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)
    exit(EX_OK)

from argparse import ArgumentParser, Namespace
from cli.cli import add_database_option, add_logging_options, set_logging_options
from football_api.football_api import get_seasons
from football_api.structure import get_seasons_json, store
from lib import messages
from model.seasons import Season
from sql.sql import Database


def parse_command_line():
    parser = ArgumentParser(description='Update football results database with season information')
    add_database_option(parser)
    add_logging_options(parser)
    return parser.parse_args()


def create_season_json():
    seasons_json = get_seasons_json()
    messages.verbose_message("Extracting season JSON")
    store(seasons_json, get_seasons())


def main(arguments: Namespace):
    create_season_json()

    with Database(arguments.database) as db:
        db.create_table(Season)
        db.create_rows(Season)


if __name__ == '__main__':
    arguments = parse_command_line()
    set_logging_options(arguments)
    main(arguments)

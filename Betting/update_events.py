from argparse import ArgumentParser, Namespace
from cli.cli import add_database_option, add_logging_options, set_logging_options
from football_api.football_api import get_events
from football_api.structure import get_events_json, store
from lib import messages
from os import EX_OK
from sys import exit


def parse_command_line():
    parser = ArgumentParser(description='Update football results database with events information')
    add_database_option(parser)
    add_logging_options(parser)

    parser.add_argument('-f',
                        '--fixtures',
                        nargs='+',
                        type=int,
                        help='the fixture IDs',
                        required=True)

    return parser.parse_args()


def create_events_json(fixture_id: int):
    events_json = get_events_json(fixture_id)
    if not events_json.exists():
        messages.verbose_message("Extracting events JSON for '{}'".format(fixture_id))
        store(events_json, get_events(fixture_id))


def main(arguments: Namespace):
    for fixture_id in arguments.fixtures:
        create_events_json(fixture_id)

    exit(EX_OK)


if __name__ == '__main__':
    arguments = parse_command_line()
    set_logging_options(arguments)
    main(arguments)

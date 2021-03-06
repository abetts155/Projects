from argparse import ArgumentParser, Namespace
from lib import messages
from model.fixtures import Half, Venue, Event
from model.leagues import country_register, league_register
from typing import List


def add_database_option(parser: ArgumentParser):
    parser.add_argument('--database',
                        help='read from this database',
                        metavar='<DATABASE>',
                        required=True)


def add_venue_option(parser: ArgumentParser):
    parser.add_argument('-V',
                        '--venue',
                        choices=Venue,
                        type=Venue.from_string,
                        metavar='{{{}}}'.format(','.join(venue.name for venue in Venue)),
                        help='filter fixtures according to the venue',
                        default=Venue.any)


def add_half_option(parser: ArgumentParser):
    parser.add_argument('--half',
                        choices=Half,
                        type=Half.from_string,
                        metavar='{{{}}}'.format(','.join(half.name for half in Half)),
                        help='filter fixtures according to the half',
                        default=Half.both)


def add_league_option(parser: ArgumentParser, required: bool = True):
    parser.add_argument('-L',
                        '--league',
                        help='choose the league to analyse',
                        metavar='<NAME>',
                        nargs='+',
                        choices=league_register.keys(),
                        type=str.upper,
                        required=required)


def get_country(value: str):
    delimiter = '-'
    lexemes = value.split(delimiter)
    return delimiter.join(lex.lower() for lex in lexemes)


def add_country_option(parser: ArgumentParser):
    parser.add_argument('-C',
                        '--country',
                        help='choose the country to analyse',
                        metavar='<NAME>',
                        nargs='+',
                        choices=list(map(str.lower, country_register)),
                        type=get_country,
                        required=True)


def add_history_option(parser: ArgumentParser):
    parser.add_argument('-H',
                        '--history',
                        help='only consider this number of completed seasons',
                        metavar='<INT>',
                        type=int)


def add_team_option(parser: ArgumentParser, required: bool = False):
    parser.add_argument('-T',
                        '--team',
                        help='choose the team to analyse',
                        metavar='<NAME>',
                        type=str,
                        required=required)


def add_event(value: str):
    value = value.lower()
    Event.add(value)
    return value


def add_events_option(parser: ArgumentParser, required: bool = True, number: int = 2):
    parser.add_argument('-E',
                        '--event',
                        nargs='+' if number > 1 else 1,
                        type=add_event,
                        help='choose event to analyse',
                        required=required)

    parser.add_argument('-n',
                        '--negate',
                        action='store_true',
                        help='negate the event',
                        default=False)


def add_minimum_option(parser: ArgumentParser):
    parser.add_argument('--minimum',
                        type=int,
                        help='the minimum sequence length threshold',
                        default=1)


def add_chunk_option(parser: ArgumentParser):
    parser.add_argument('-C',
                        '--chunks',
                        help='divide the table into chunks of this size',
                        metavar='<INT>',
                        type=int,
                        default=0)


def add_logging_options(parser: ArgumentParser):
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

    parser.add_argument('--no-warnings',
                        action='store_true',
                        help='suppress warning messages',
                        default=False)


def set_logging_options(arguments: Namespace):
    messages.verbose = arguments.verbose
    messages.debug = arguments.debug
    if arguments.no_warnings:
        messages.warnings = False


def add_block_option(parser: ArgumentParser):
    parser.add_argument('--block',
                        action='store_true',
                        help='block when the application generates charts and graphs',
                        default=True)


def get_unique_league(arguments: Namespace) -> str:
    if len(arguments.league) > 1:
        messages.error_message("This tool only supports a single league. "
                               "You selected: '{}'".format(','.join(arguments.league)))
    else:
        (league_code,) = arguments.league
        return league_code


def get_multiple_teams(arguments: Namespace) -> List[str]:
    team_names = arguments.team.split(':')
    return team_names


def get_unique_team(arguments: Namespace) -> str:
    team_names = get_multiple_teams(arguments)
    if len(team_names) > 1:
        messages.error_message("This tool only supports a single team. "
                               "You selected: '{}'".format(','.join(team_names)))
    else:
        (team_name,) = team_names
        return team_name


def get_unique_event(arguments: Namespace) -> str:
    if len(arguments.event) > 1:
        messages.error_message("This tool only supports a single event. "
                               "You selected: '{}'".format(','.join(arguments.event)))
    else:
        (event,) = arguments.event
        return event

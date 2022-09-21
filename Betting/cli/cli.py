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
                        default=Venue.anywhere)


def add_half_option(parser: ArgumentParser):
    parser.add_argument('--half',
                        choices=Half,
                        type=Half.from_string,
                        nargs='+',
                        metavar='{{{}}}'.format(','.join(half.name for half in Half)),
                        help='filter fixtures according to the half',
                        default=[Half.full])


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


def add_country_option(parser: ArgumentParser, required: bool = True):
    parser.add_argument('-C',
                        '--country',
                        help='choose the country to analyse',
                        metavar='<NAME>',
                        nargs='+',
                        choices=list(map(str.lower, country_register)),
                        type=get_country,
                        required=required)


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


def add_events_option(parser: ArgumentParser, required: bool = True, allowed_events: int = 2):
    parser.add_argument('-E',
                        '--event',
                        nargs='+' if allowed_events > 1 else 1,
                        type=add_event,
                        help='choose event to analyse',
                        required=required)

    parser.add_argument('-n',
                        '--negate',
                        action='store_true',
                        help='negate the event',
                        default=False)


def add_minimum_option(parser: ArgumentParser, required: bool = True):
    parser.add_argument('--minimum',
                        type=int,
                        help='the minimum sequence length threshold',
                        required=required,
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


def set_logging_options(args: Namespace):
    messages.verbose = args.verbose
    messages.debug = args.debug
    if args.no_warnings:
        messages.warnings = False


def add_block_option(parser: ArgumentParser):
    parser.add_argument('--block',
                        action='store_true',
                        help='block when the application generates charts and graphs',
                        default=True)


def add_past_option(parser: ArgumentParser):
    parser.add_argument('--past',
                        action='store_true',
                        help="if selected, update historical data; otherwise, update this season's data",
                        default=False)


def add_force_option(parser: ArgumentParser):
    parser.add_argument('-f',
                        '--force',
                        action='store_true',
                        help='force an update',
                        default=False)


def get_unique_league(args: Namespace) -> str:
    if len(args.league) > 1:
        messages.error_message("This tool only supports a single league. "
                               "You selected: '{}'".format(','.join(args.league)))
    else:
        (league_code,) = args.league
        return league_code


def get_multiple_teams(args: Namespace) -> List[str]:
    team_names = args.team.split(':')
    return team_names


def get_unique_team(args: Namespace) -> str:
    team_names = get_multiple_teams(args)
    if len(team_names) > 1:
        messages.error_message("This tool only supports a single team. "
                               "You selected: '{}'".format(','.join(team_names)))
    else:
        (team_name,) = team_names
        return team_name


def get_unique_event(args: Namespace) -> str:
    if len(args.event) > 1:
        messages.error_message("This tool only supports a single event. "
                               "You selected: '{}'".format(','.join(args.event)))
    else:
        (event,) = args.event
        return event


def get_unique_half(args: Namespace) -> str:
    if len(args.half) > 1:
        messages.error_message("This tool only supports a single half. "
                               "You selected: '{}'".format(','.join(args.half)))
    else:
        (half,) = args.half
        return half

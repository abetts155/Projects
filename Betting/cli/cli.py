from argparse import ArgumentParser, Namespace
from lib import messages
from model.fixtures import Half, Result, Venue
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
                        help='filter fixtures according to the half')


def add_league_option(parser: ArgumentParser):
    parser.add_argument('-L',
                        '--league',
                        help='choose the league to analyse',
                        metavar='<NAME>',
                        nargs='+',
                        choices=league_register.keys(),
                        type=str.upper,
                        required=True)


def add_country_option(parser: ArgumentParser):
    parser.add_argument('-C',
                        '--country',
                        help='choose the country to analyse',
                        metavar='<NAME>',
                        nargs='+',
                        choices=country_register,
                        type=str.capitalize,
                        required=True)


def add_history_option(parser: ArgumentParser):
    parser.add_argument('-H',
                        '--history',
                        help='only consider this number of completed seasons',
                        metavar='<INT>',
                        type=int)


def add_team_option(parser: ArgumentParser):
    parser.add_argument('-T',
                        '--team',
                        help='choose the team to analyse',
                        metavar='<NAME>',
                        type=str)


def add_events_option(parser: ArgumentParser, required: bool = True):
    event_choices = [Result.draw.__name__,
                     Result.defeat.__name__,
                     Result.win.__name__,
                     Result.goals_for.__name__,
                     Result.goals_against.__name__,
                     Result.more_than_0.__name__,
                     Result.more_than_1.__name__,
                     Result.more_than_2.__name__,
                     Result.more_than_3.__name__,
                     Result.more_than_4.__name__,
                     Result.more_than_5.__name__,
                     Result.btts.__name__]

    parser.add_argument('-E',
                        '--event',
                        choices=event_choices,
                        nargs='+',
                        type=str.lower,
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

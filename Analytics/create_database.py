import miscellaneous.messages
import os

from argparse import ArgumentParser, Namespace
from json import load
from miscellaneous.files import check_file_or_directory_exists
from miscellaneous.messages import verbose_message
from model import competitions, events, matches, teams
from sql.sql import Database


def parse_command_line():
    parser = ArgumentParser(description='Create football analytics database')

    parser.add_argument(dest='data',
                        help='the directory where the data reside',
                        metavar='<DIR>')

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


def extract_directories(base_directory: str):
    check_file_or_directory_exists(base_directory)
    events_directory = base_directory + os.sep + 'events'
    check_file_or_directory_exists(events_directory)
    others_directory = base_directory + os.sep + 'others'
    check_file_or_directory_exists(others_directory)
    matches_directory = base_directory + os.sep + 'matches'
    check_file_or_directory_exists(matches_directory)
    return events_directory, others_directory, matches_directory


def fill_players_inventory(base_directory: str):
    players_file = base_directory + os.sep + 'players.json'
    check_file_or_directory_exists(players_file)
    with open(players_file, 'r') as in_file:
        players_data = load(in_file)
        for data in players_data:
            teams.create_player_from_json(data)


def fill_teams_inventory(base_directory: str):
    teams_file = base_directory + os.sep + 'teams.json'
    check_file_or_directory_exists(teams_file)
    with open(teams_file, 'r') as in_file:
        teams_data = load(in_file)
        for data in teams_data:
            teams.create_team_from_json(data)


def fill_competitions_inventory(base_directory: str):
    competitions_file = base_directory + os.sep + 'competitions.json'
    check_file_or_directory_exists(competitions_file)
    with open(competitions_file, 'r') as in_file:
        competitions_data = load(in_file)
        for data in competitions_data:
            competitions.create_competition_from_json(data)


def fill_coaches_inventory(base_directory: str):
    coaches_file = base_directory + os.sep + 'coaches.json'
    check_file_or_directory_exists(coaches_file)
    with open(coaches_file, 'r') as in_file:
        coaches_data = load(in_file)
        for data in coaches_data:
            teams.create_coach_from_json(data)


def fill_matches_inventory(filename: str):
    with open(filename, 'r') as in_file:
        match_data = load(in_file)
        for data in match_data:
            matches.create_match_from_json(data)


def fill_events_inventory(filename: str):
    with open(filename, 'r') as in_file:
        events_data = load(in_file)
        for data in events_data:
            events.create_event_from_json(data)


def main(arguments: Namespace):
    miscellaneous.messages.verbose = arguments.verbose
    miscellaneous.messages.debug = arguments.debug
    arguments.data = os.path.abspath(arguments.data)
    events_directory, others_directory, matches_directory = extract_directories(arguments.data)
    fill_players_inventory(others_directory)
    fill_teams_inventory(others_directory)
    fill_competitions_inventory(others_directory)
    fill_coaches_inventory(others_directory)

    matches_prefix = 'matches_'
    events_prefix = 'events_'
    suffix = '.json'
    matches_and_events = {}
    for root, _, files in os.walk(matches_directory):
        for file in files:
            competition = file[len(matches_prefix):-len(suffix)]
            matches_file = os.path.join(root, file)
            check_file_or_directory_exists(matches_file)
            matches_and_events[competition] = [matches_file]

    for root, _, files in os.walk(events_directory):
        for file in files:
            competition = file[len(events_prefix):-len(suffix)]
            events_file = os.path.join(root, file)
            check_file_or_directory_exists(events_file)
            matches_and_events[competition].append(events_file)

    for competition, (matches_file, events_file) in matches_and_events.items():
        verbose_message('Analysing {}'.format(competition))
        fill_matches_inventory(matches_file)
        fill_events_inventory(events_file)
        db_filename = '{}.db'.format(competition)
        with Database(db_filename) as db:
            db.create_tables()
            db.create_rows()


if __name__ == '__main__':
    main(parse_command_line())

import argparse
import json
import os

from miscellaneous import messages
from model import competitions, events, matches, teams
from sql import sql


def check_file_or_directory_exists(name: str):
    if not os.path.exists(name):
        messages.error_message("No such file or directory '{}'".format(name))


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
        players_data = json.load(in_file)
        for data in players_data:
            teams.create_player_from_json(data)


def fill_teams_inventory(base_directory: str):
    teams_file = base_directory + os.sep + 'teams.json'
    check_file_or_directory_exists(teams_file)
    with open(teams_file, 'r') as in_file:
        teams_data = json.load(in_file)
        for data in teams_data:
            teams.create_team_from_json(data)


def fill_competitions_inventory(base_directory: str):
    competitions_file = base_directory + os.sep + 'competitions.json'
    check_file_or_directory_exists(competitions_file)
    with open(competitions_file, 'r') as in_file:
        competitions_data = json.load(in_file)
        for data in competitions_data:
            competitions.create_competition_from_json(data)


def fill_coaches_inventory(base_directory: str):
    coaches_file = base_directory + os.sep + 'coaches.json'
    check_file_or_directory_exists(coaches_file)
    with open(coaches_file, 'r') as in_file:
        coaches_data = json.load(in_file)
        for data in coaches_data:
            teams.create_coach_from_json(data)


def fill_matches_inventory(filename: str):
    with open(filename, 'r') as in_file:
        match_data = json.load(in_file)
        for data in match_data:
            matches.create_match_from_json(data)


def fill_events_inventory(filename: str):
    with open(filename, 'r') as in_file:
        events_data = json.load(in_file)
        for data in events_data:
            match_id, event = events.create_event_from_json(data)
            match = matches.Match.inventory[match_id]
            match.append(event)


def parse_command_line():
    parser = argparse.ArgumentParser(description='Create football analytics database')

    parser.add_argument(dest='data',
                        help='the directory where the data reside',
                        metavar='<DIR>')

    parser.add_argument('--database',
                        help='write to this database',
                        metavar='<DATABASE>',
                        required=True)

    return parser.parse_args()


def main(arguments: argparse.Namespace):
    sql.create_database(arguments.database)
    arguments.data = os.path.abspath(arguments.data)
    events_directory, others_directory, matches_directory = extract_directories(arguments.data)
    fill_players_inventory(others_directory)
    fill_teams_inventory(others_directory)
    fill_competitions_inventory(others_directory)
    fill_coaches_inventory(others_directory)

    for root, _, files in os.walk(matches_directory):
        for file in files:
            matches_file = os.path.join(root, file)
            check_file_or_directory_exists(matches_file)
            fill_matches_inventory(matches_file)

    for root, _, files in os.walk(events_directory):
        for file in files:
            events_file = os.path.join(root, file)
            check_file_or_directory_exists(events_file)
            fill_events_inventory(events_file)

    sql.create_rows(arguments.database)


if __name__ == '__main__':
    main(parse_command_line())

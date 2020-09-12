import numpy as np
import miscellaneous.messages

from argparse import ArgumentParser, Namespace
from miscellaneous.analysis import compute_metrics, Metric
from miscellaneous.files import check_file_or_directory_exists
from model.competitions import create_competition_from_row, Competition
from model.events import create_event_from_row, Event, Period
from model.matches import create_match_from_row, Side
from model.teams import create_coach_from_row, create_player_from_row, create_team_from_row, Coach, Player, Team
from sql.sql import Database
from typing import List


def parse_command_line():
    parser = ArgumentParser(description='Do football analytics')

    parser.add_argument('--team',
                        help='choose the team to analyse',
                        metavar='<NAME>',
                        nargs='+',
                        type=str.lower)

    parser.add_argument('--country',
                        help='choose the team to analyse',
                        metavar='<NAME>',
                        nargs='+',
                        type=str.lower)

    parser.add_argument('--database',
                        help='read from this database',
                        metavar='<DATABASE>',
                        required=True)

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


def load_players_and_coaches_and_teams_and_competitions(arguments: Namespace):
    with Database(arguments.database) as db:
        player_rows = db.fetch_all_rows(Player.sql_table())
        for row in player_rows:
            create_player_from_row(row)

        team_rows = db.fetch_all_rows(Team.sql_table())
        for row in team_rows:
            create_team_from_row(row)

        competition_rows = db.fetch_all_rows(Competition.sql_table())
        for row in competition_rows:
            create_competition_from_row(row)

        coach_rows = db.fetch_all_rows(Coach.sql_table())
        for row in coach_rows:
            create_coach_from_row(row)


def analyse(team: Team, matches_viewed_as_events: List[List[Event]]):
    data = []
    for match_events in matches_viewed_as_events:
        match = match_events[0].match
        metrics = compute_metrics(match_events)
        first_half_metrics = metrics[Period.FIRST_HALF]
        second_half_metrics = metrics[Period.SECOND_HALF]
        match_metrics = first_half_metrics + second_half_metrics
        data.append(match_metrics.query(Side.HOME, Metric.CORNERS) + match_metrics.query(Side.AWAY, Metric.CORNERS))

    print('{:<25} {:#.6g} {:#.6g} {:#.6g} {:#.6g} {:#.6g} {}'.format(team.name,
                                                                     np.median(data),
                                                                     np.mean(data),
                                                                     np.std(data),
                                                                     np.min(data),
                                                                     np.max(data),
                                                                     ','.join(str(x) for x in data)))


def process_teams(arguments: Namespace):
    print('{:<25} {:<7} {:<7} {:<7} {:<7} {:<7}'.format('TEAM', 'MEDIAN', 'MEAN', 'STD', 'MIN', 'MAX'))
    with Database(arguments.database) as db:
        for team_name in arguments.team:
            lexemes = team_name.split('_')
            canonical_team_name = ' '.join(lexemes)
            (team_row,) = db.fetch_rows_with_likeness(Team.sql_table(), canonical_team_name)
            team = Team.inventory[team_row[0]]
            match_rows = db.fetch_match_rows(team)
            matches = []
            for match_row in match_rows:
                match = create_match_from_row(match_row)
                matches.append(match)
            matches.sort(key=lambda m: m.match_date)

            under_analysis = []
            for match in matches:
                event_rows = db.fetch_event_rows(match)
                match_events = []
                for event_row in event_rows:
                    event = create_event_from_row(event_row)
                    match_events.append(event)
                assert match_events
                match_events.sort(key=lambda e: (e.period, e.timestamp))
                under_analysis.append(match_events)

            analyse(team, under_analysis)


def main(arguments: Namespace):
    miscellaneous.messages.verbose = arguments.verbose
    miscellaneous.messages.debug = arguments.debug
    check_file_or_directory_exists(arguments.database)
    load_players_and_coaches_and_teams_and_competitions(arguments)
    process_teams(arguments)


if __name__ == '__main__':
    main(parse_command_line())

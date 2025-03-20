from argparse import ArgumentParser, Namespace
from cli.cli import (add_database_option,
                     add_league_option,
                     add_logging_options,
                     set_logging_options,
                     get_unique_league)
from lib import messages
from model.fixtures import Half
from model.competitions import league_register, League
from model.seasons import Season
from model.tables import LeagueTable
from sql.sql import load_league, load_teams
from typing import List


def parse_command_line():
    parser = ArgumentParser(description='Show matches that ended in one of the given results')
    add_database_option(parser)
    add_league_option(parser, True)
    add_logging_options(parser)

    parser.add_argument('--results',
                        help='use these results',
                        nargs='+',
                        metavar='<SCORE>',
                        type=str.strip,
                        required=True)

    return parser.parse_args()


def show_results(league: League, season: Season, results: List[str]):
    matched = []
    for fixture in season.fixtures():
        result = fixture.full_time()
        if result:
            for given_result in results:
                left, right = map(int, given_result.split('-'))
                if left == result.left and right == result.right:
                    matched.append(fixture)
        else:
            messages.warning_message('Ignoring {}'.format(fixture))

    if matched:
        table = LeagueTable(season, [Half.full])
        print('>' * 40, season.year, '<' * 40)
        team_count = {}
        for fixture in matched:
            if fixture.home_team not in team_count:
                team_count[fixture.home_team] = 0

            if fixture.away_team not in team_count:
                team_count[fixture.away_team] = 0

            team_count[fixture.home_team] += 1
            team_count[fixture.away_team] += 1

            print('{}: {:<2} vs {:<2} {} vs {}'.format(fixture.date.strftime('%d-%m-%y'),
                                                       table.team_position(fixture.home_team) + 1,
                                                       table.team_position(fixture.away_team) + 1,
                                                       fixture.home_team.name,
                                                       fixture.away_team.name))
        print()

        teams = list(team_count.keys())
        teams.sort(key=lambda team: team_count[team], reverse=True)
        max_name = max([len(team.name) for team in teams]) + 1
        for team in teams:
            print('{}{}: {}'.format(team.name, (max_name - len(team.name)) * ' ', team_count[team]))


def main(args: Namespace):
    load_teams(args.database)
    league = league_register[get_unique_league(args)]
    load_league(args.database, league)

    seasons = Season.seasons(league)
    if not seasons:
        messages.error_message("No season data found")

    for season in seasons:
        show_results(league, season, args.results)


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)

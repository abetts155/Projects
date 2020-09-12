from argparse import ArgumentParser, Namespace
from collections import Counter
from lib import messages
from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
from model.competitions import league_register, League, Season, Team, Venue
from sql.sql import load_database, extract_picked_team
from typing import List


def parse_command_line():
    parser = ArgumentParser(description='Betting predictions')

    parser.add_argument('--database',
                        help='read from this database',
                        metavar='<DATABASE>',
                        required=True)

    parser.add_argument('--venue',
                        choices=list(Venue),
                        type=Venue.from_string,
                        help='filter fixtures according to the venue',
                        default=Venue.any)

    parser.add_argument('--league',
                        help='choose the league to analyse',
                        metavar='<NAME>',
                        choices=league_register.keys(),
                        type=str.upper,
                        required=True)

    parser.add_argument('--team',
                        help='choose the team to analyse',
                        metavar='<NAME>',
                        type=str.capitalize,
                        required=True)

    parser.add_argument('--history',
                        help='only consider this number of completed seasons',
                        metavar='<INT>',
                        type=int)

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


class Statistics(object):
    __slots__ = ['wins', 'draws', 'losses', 'goals_for', 'goals_against', 'bts', 'goals']

    def __init__(self):
        self.wins = 0
        self.draws = 0
        self.losses = 0
        self.goals_for = 0
        self.goals_against = 0
        self.bts = 0
        self.goals = Counter()

    def __str__(self):
        return 'W={} D={} L={} GF={} GA={} BTS={}'.format(self.wins,
                                                          self.draws,
                                                          self.losses,
                                                          self.goals_for,
                                                          self.goals_against,
                                                          self.bts)


def compute_statistics(season: Season, team: Team, venue: Venue, stats: Statistics):
    fixtures = []
    for fixture in season.fixtures():
        if fixture.full_time_home is not None and fixture.full_time_away is not None:
            if venue == Venue.any:
                if fixture.home_team == team or fixture.away_team == team:
                    fixtures.append(fixture)
            elif venue == Venue.away:
                if fixture.away_team == team:
                    fixtures.append(fixture)
            elif venue == Venue.home:
                if fixture.home_team == team:
                    fixtures.append(fixture)

    for fixture in fixtures:
        if fixture.is_win(team):
            stats.wins += 1
        elif fixture.is_loss(team):
            stats.losses += 1
        else:
            stats.draws += 1

        if fixture.home_team == team:
            stats.goals_for += fixture.full_time_home
            stats.goals_against += fixture.full_time_away

        if fixture.away_team == team:
            stats.goals_for += fixture.full_time_away
            stats.goals_against += fixture.full_time_home

        if fixture.full_time_home > 0 and fixture.full_time_away > 0:
            stats.bts += 1

        total_goals = fixture.full_time_home + fixture.full_time_away
        stats.goals[total_goals] += 1


def display_bar_graph(team: Team, stats: Statistics):
    fig, ax = plt.subplots(1, figsize=(20, 10))
    x_values = ['wins', 'draws', 'losses', 'GF', 'GA', 'BTS', '0 goals', '1 goal', '2 goals', '3 goals', '4+ goals']
    y_values = [stats.wins,
                stats.draws,
                stats.losses,
                stats.goals_for,
                stats.goals_against,
                stats.bts,
                stats.goals[0],
                stats.goals[1],
                stats.goals[2],
                stats.goals[3],
                sum([stats.goals[k] for k, v in stats.goals.items() if k >= 4])]
    for x, y in zip(x_values, y_values):
        ax.text(x, y, str(y), ha='center', fontsize=8)
    ax.bar(x_values, y_values)
    plt.title('{}: {} games'.format(team.name, stats.wins + stats.draws + stats.losses))
    plt.show()


def main(arguments: Namespace):
    messages.verbose = arguments.verbose
    messages.debug = arguments.debug
    league = league_register[arguments.league]
    load_database(arguments.database, league)

    seasons = Season.seasons()
    if not seasons:
        messages.error_message("No season data found")

    if seasons[-1].current:
        this_season = seasons.pop()
    else:
        this_season = None

    if arguments.history:
        seasons = seasons[-arguments.history:]

    selected_team = extract_picked_team(arguments.database, arguments.team, league)
    stats = Statistics()
    for season in seasons:
        compute_statistics(season, selected_team, arguments.venue, stats)
    display_bar_graph(selected_team, stats)


if __name__ == '__main__':
    main(parse_command_line())

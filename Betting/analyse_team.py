from argparse import ArgumentParser, Namespace
from collections import Counter
from lib import messages
from matplotlib import pyplot as plt
from model.competitions import league_register, Fixture, Season, Team, Venue
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

    event_choices = [Fixture.is_draw.__name__, Fixture.is_loss.__name__, Fixture.is_win.__name__]
    parser.add_argument('--game-state',
                        help='analyse assuming this game state at half time',
                        choices=event_choices,
                        type=str.lower)

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


def compute_statistics(season: Season,
                       team: Team,
                       venue: Venue,
                       game_state: str,
                       first_half: Statistics,
                       second_half: Statistics,
                       both_halves: Statistics):
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
        analyse = True
        if fixture.home_team == team:
            if game_state:
                if game_state == Fixture.is_loss.__name__:
                    analyse = True if fixture.half_time_home < fixture.half_time_away else False
                elif game_state == Fixture.is_win.__name__:
                    analyse = True if fixture.half_time_home > fixture.half_time_away else False
                elif game_state == Fixture.is_draw.__name__:
                    analyse = True if fixture.half_time_home == fixture.half_time_away else False
                else:
                    assert False

            if analyse:
                first_half.goals_for += fixture.half_time_home
                first_half.goals_against += fixture.half_time_away
                second_half.goals_for += fixture.full_time_home - fixture.half_time_home
                second_half.goals_against += fixture.full_time_away - fixture.half_time_away
                both_halves.goals_for += fixture.full_time_home
                both_halves.goals_against += fixture.full_time_away

                if fixture.half_time_home > fixture.half_time_away:
                    first_half.wins += 1
                elif fixture.half_time_away > fixture.half_time_home:
                    first_half.losses += 1
                else:
                    first_half.draws += 1

                if fixture.full_time_home - fixture.half_time_home > fixture.full_time_away - fixture.half_time_away:
                    second_half.wins += 1
                elif fixture.full_time_away - fixture.half_time_away > fixture.full_time_home - fixture.half_time_home:
                    second_half.losses += 1
                else:
                    second_half.draws += 1

                if fixture.full_time_home > fixture.full_time_away:
                    both_halves.wins += 1
                elif fixture.full_time_away > fixture.full_time_home:
                    both_halves.losses += 1
                else:
                    both_halves.draws += 1

        if fixture.away_team == team:
            if game_state:
                if game_state == Fixture.is_loss.__name__:
                    analyse = True if fixture.half_time_home > fixture.half_time_away else False
                elif game_state == Fixture.is_win.__name__:
                    analyse = True if fixture.half_time_home < fixture.half_time_away else False
                elif game_state == Fixture.is_draw.__name__:
                    analyse = True if fixture.half_time_home == fixture.half_time_away else False
                else:
                    assert False

            if analyse:
                first_half.goals_for += fixture.half_time_away
                first_half.goals_against += fixture.half_time_home
                second_half.goals_for += fixture.full_time_away - fixture.half_time_away
                second_half.goals_against += fixture.full_time_home - fixture.half_time_home
                both_halves.goals_for += fixture.full_time_away
                both_halves.goals_against += fixture.full_time_home

                if fixture.half_time_away > fixture.half_time_home:
                    first_half.wins += 1
                elif fixture.half_time_home > fixture.half_time_away:
                    first_half.losses += 1
                else:
                    first_half.draws += 1

                if fixture.full_time_away - fixture.half_time_away > fixture.full_time_home - fixture.half_time_home:
                    second_half.wins += 1
                elif fixture.full_time_home - fixture.half_time_home > fixture.full_time_away - fixture.half_time_away:
                    second_half.losses += 1
                else:
                    second_half.draws += 1

                if fixture.full_time_away > fixture.full_time_home:
                    both_halves.wins += 1
                elif fixture.full_time_home > fixture.full_time_away:
                    both_halves.losses += 1
                else:
                    both_halves.draws += 1

        if analyse:
            if fixture.half_time_home > 0 and fixture.half_time_away > 0:
                first_half.bts += 1

            if fixture.full_time_home - fixture.half_time_home > 0 and fixture.full_time_away - fixture.half_time_away > 0:
                second_half.bts += 1

            if fixture.full_time_home > 0 and fixture.full_time_away > 0:
                both_halves.bts += 1

            first_half.goals[fixture.half_time_home + fixture.half_time_away] += 1
            second_half.goals[(fixture.full_time_home - fixture.half_time_home) +
                              (fixture.full_time_away - fixture.half_time_away)] += 1
            both_halves.goals[fixture.full_time_home + fixture.full_time_away] += 1


def reduce(stats: Statistics):
    return [stats.wins,
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


def display_bar_graphs(team: Team, first_half: Statistics, second_half: Statistics, both_halves: Statistics):
    fig, axes = plt.subplots(3, figsize=(20, 10))
    total_games = both_halves.wins + both_halves.draws + both_halves.losses
    fig.suptitle('{}: {} games'.format(team.name, total_games))
    x_values = ['wins', 'draws', 'losses', 'GF', 'GA', 'BTS', '0 goals', '1 goal', '2 goals', '3 goals', '4+ goals']

    y_values = reduce(first_half)
    for x, y in zip(x_values, y_values):
        axes[0].text(x, y, str(y), ha='center', fontsize=8)
    axes[0].bar(x_values, y_values)
    axes[0].set_title('First half')

    y_values = reduce(second_half)
    for x, y in zip(x_values, y_values):
        axes[1].text(x, y, str(y), ha='center', fontsize=8)
    axes[1].bar(x_values, y_values)
    axes[1].set_title('Second half')

    y_values = reduce(both_halves)
    for x, y in zip(x_values, y_values):
        axes[2].text(x, y, str(y), ha='center', fontsize=8)
    axes[2].bar(x_values, y_values)
    axes[2].set_title('Both halves')

    plt.subplots_adjust(hspace=0.5)
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
    first_half = Statistics()
    second_half = Statistics()
    both_halves = Statistics()
    for season in seasons:
        compute_statistics(season,
                           selected_team,
                           arguments.venue,
                           arguments.game_state,
                           first_half,
                           second_half,
                           both_halves)
    display_bar_graphs(selected_team, first_half, second_half, both_halves)


if __name__ == '__main__':
    main(parse_command_line())

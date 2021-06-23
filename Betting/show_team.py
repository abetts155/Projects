from argparse import ArgumentParser, Namespace
from cli.cli import (add_database_option,
                     add_history_option,
                     add_league_option,
                     add_team_option,
                     add_logging_options,
                     add_venue_option,
                     add_block_option,
                     set_logging_options,
                     get_unique_league,
                     get_unique_team)
from collections import Counter
from lib import messages
from matplotlib import pyplot as plt
from model.fixtures import Result, Venue, win, defeat, draw, bts
from model.leagues import league_register
from model.seasons import Season
from model.teams import Team
from numpy import arange, median
from sql.sql import extract_picked_team, load_league, load_teams
from typing import Callable, List


def parse_command_line():
    parser = ArgumentParser(description='Analyse individual team')
    add_database_option(parser)
    add_history_option(parser)
    add_league_option(parser)
    add_team_option(parser)
    add_venue_option(parser)
    add_block_option(parser)
    add_logging_options(parser)

    parser.add_argument('--game-states',
                        help='analyse assuming one of these game states at half time',
                        nargs='+',
                        metavar='<SCORE>',
                        type=str.strip)

    parser.add_argument('-A',
                        '--averages',
                        help='use seasonal averages',
                        action='store_true',
                        default=False)

    return parser.parse_args()


class Statistics:
    __slots__ = ['wins', 'draws', 'losses', 'goals_for', 'goals_against', 'bts', 'scored', 'conceded', 'goals',
                 'scores']

    def __init__(self):
        self.wins = 0
        self.draws = 0
        self.losses = 0
        self.goals_for = 0
        self.goals_against = 0
        self.bts = 0
        self.scored = 0
        self.conceded = 0
        self.goals = Counter()
        self.scores = Counter()


class MatchStatistics:
    __slots__ = ['first_half', 'second_half', 'both_halves']

    def __init__(self):
        self.first_half = Statistics()
        self.second_half = Statistics()
        self.both_halves = Statistics()


def update_stats(stats: Statistics, result: Result):
    stats.goals_for += result.left
    stats.goals_against += result.right

    if win(result):
        stats.wins += 1
    elif defeat(result):
        stats.losses += 1
    else:
        assert draw(result)
        stats.draws += 1

    if bts(result):
        stats.bts += 1

    if result.left > 0:
        stats.scored += 1

    if result.right > 0:
        stats.conceded += 1

    stats.goals[result.left + result.right] += 1
    stats.scores[(result.left, result.right)] += 1


def compute_statistics(season: Season, team: Team, venue: Venue, game_states: List[str]):
    fixtures = []
    for fixture in season.fixtures():
        if fixture.first_half() is not None and fixture.second_half() is not None:
            if venue == Venue.any:
                if fixture.home_team == team or fixture.away_team == team:
                    fixtures.append(fixture)
            elif venue == Venue.away:
                if fixture.away_team == team:
                    fixtures.append(fixture)
            elif venue == Venue.home:
                if fixture.home_team == team:
                    fixtures.append(fixture)

    stats = MatchStatistics()
    for fixture in fixtures:
        if fixture.home_team == team:
            first_half = fixture.first_half()
            second_half = fixture.second_half()
            full_time = fixture.full_time()
        else:
            first_half = fixture.first_half().reverse()
            second_half = fixture.second_half().reverse()
            full_time = fixture.full_time().reverse()

        analyse = True
        if game_states:
            analyse = False
            for state in game_states:
                left, right = map(int, state.split('-'))
                if not analyse:
                    analyse = left == first_half.left and right == first_half.right

        if analyse:
            update_stats(stats.first_half, first_half)
            update_stats(stats.second_half, second_half)
            update_stats(stats.both_halves, full_time)

    return stats


def attach_scorelines(ax,
                      x_values: List,
                      y_values: List,
                      stats: Statistics,
                      predicate: Callable,
                      max_colors: List = None):
    max_count = 0
    index = 0
    offset = len(y_values)
    for (left, right), count in sorted(stats.scores.items(), key=lambda x: x[1], reverse=True):
        if predicate(left, right):
            x_values.append('{}-{}'.format(left, right))
            y_values.append(count)
            effective_index = index + offset
            ax.text(effective_index, count, str(count), ha='center', fontsize=8)
            if count >= max_count:
                max_count = count
                if max_colors is not None:
                    max_colors.append(effective_index)
            index += 1


def create_dual_bars(ax, left: Statistics, right: Statistics, title: str):
    width = 0.3

    x_values = ['wins', 'draws', 'losses', 'GF', 'GA', 'BTS', 'Scored', 'Conceded',
                '0 goals', '1 goal', '2 goals', '3 goals', '4 goals', '5+ goals']

    y_values = [left.wins, left.draws, left.losses, left.goals_for, left.goals_against,
                left.bts, left.scored, left.conceded, left.goals[0], left.goals[1], left.goals[2], left.goals[3],
                left.goals[4], sum([left.goals[k] for k, v in left.goals.items() if k >= 5])]

    for x, y in enumerate(y_values):
        ax.text(x, y, str(y), ha='center', fontsize=8)

    attach_scorelines(ax, x_values, y_values, left, int.__gt__)
    ax.axvline((width + 2 * len(x_values) - 1) / 2, color='red', ls='-', lw=1)
    attach_scorelines(ax, x_values, y_values, left, int.__eq__)
    ax.axvline((width + 2 * len(x_values) - 1) / 2, color='red', ls='-', lw=1)
    attach_scorelines(ax, x_values, y_values, left, int.__lt__)

    ax.bar(x_values, y_values, width=width, align='center', color='lightskyblue', edgecolor='black')

    y_values = [right.wins, right.draws, right.losses, right.goals_for, right.goals_against,
                right.bts, right.scored, right.conceded, right.goals[0], right.goals[1],
                right.goals[2], right.goals[3], right.goals[4],
                sum([right.goals[k] for k, v in right.goals.items() if k >= 5])]

    for k, v in sorted(left.scores.items(), key=lambda x: x[1], reverse=True):
        if k in right.scores:
            y_values.append(right.scores[k])
        else:
            y_values.append(0)

    for x, y in enumerate(y_values):
        ax.text(x + width, y, str(y), ha='center', fontsize=8)

    indices = arange(len(x_values))
    ax.bar(indices + width, y_values, width=width, align='center', color='whitesmoke', edgecolor='black')

    boundaries = [(2, 3), (7, 8), (13, 14)]
    for pair in boundaries:
        ax.axvline((width + pair[0] + pair[1]) / 2, color='black', lw=4)

    ax.set_xticks(indices + width / 2)
    ax.set_xticklabels(x_values, rotation=30, ha='center')
    ax.set_title(title)
    ax.set_yticks([])


def display_averages(team: Team,
                     venue: Venue,
                     seasons: List[Season],
                     team_stats: MatchStatistics,
                     all_stats: MatchStatistics,
                     block: bool):
    fig, axes = plt.subplots(3, figsize=(25, 13), constrained_layout=True)

    if len(seasons) > 1:
        year_string = '{}-{}'.format(seasons[0].year, seasons[-1].year)
    else:
        year_string = str(seasons[0].year)

    if venue == Venue.any:
        venue_string = '{} or {}'.format(Venue.home.name, Venue.away.name)
    else:
        venue_string = '{} only'.format(venue.name)

    fig.suptitle('{}: Averages comparison for {} ({})'.format(year_string, team.name, venue_string),
                 fontweight='bold', fontsize=14)

    create_dual_bars(axes[0], team_stats.first_half, all_stats.first_half, 'First half')
    create_dual_bars(axes[1], team_stats.second_half, all_stats.second_half, 'Second half')
    create_dual_bars(axes[2], team_stats.both_halves, all_stats.both_halves, 'Both halves')
    plt.show(block=block)


def create_single_bar(ax, stats: Statistics, title: str):
    x_values = ['wins', 'draws', 'losses', 'GF', 'GA', 'BTS', 'Scored', 'Conceded']
    y_values = [stats.wins, stats.draws, stats.losses, stats.goals_for, stats.goals_against,
                stats.bts, stats.scored, stats.conceded]
    for x, y in enumerate(y_values):
        ax.text(x, y, str(y), ha='center', fontsize=8)

    x_values.extend(['0 goals', '1 goal', '2 goals', '3 goals', '4 goals', '5+ goals'])
    goals = [stats.goals[0], stats.goals[1], stats.goals[2], stats.goals[3], stats.goals[4],
             sum([stats.goals[k] for k, v in stats.goals.items() if k >= 5])]
    maximum = max(goals)
    max_colors = [len(y_values) + i for i, j in enumerate(goals) if j == maximum]
    denominator = sum(goals)
    for x, y in enumerate(goals):
        percentage = round(100 * (y / denominator))
        ax.text(x + len(y_values), y, '{} ({}%)'.format(y, percentage), ha='center', fontsize=8)
    y_values.extend(goals)

    attach_scorelines(ax, x_values, y_values, stats, int.__gt__, max_colors)
    ax.axvline((2 * len(x_values) - 1) / 2, color='red', ls='-', lw=1)
    attach_scorelines(ax, x_values, y_values, stats, int.__eq__, max_colors)
    ax.axvline((2 * len(x_values) - 1) / 2, color='red', ls='-', lw=1)
    attach_scorelines(ax, x_values, y_values, stats, int.__lt__, max_colors)

    width = 0.5
    bar = ax.bar(x_values, y_values, width=width, align='center', color='lightskyblue', edgecolor='black')
    for i in max_colors:
        bar[i].set_color('dodgerblue')
        bar[i].set_edgecolor('black')

    boundaries = [(2, 3), (7, 8), (13, 14)]
    for pair in boundaries:
        ax.axvline((pair[0] + pair[1]) / 2, color='black', lw=4)

    indices = arange(len(x_values))
    ax.set_xticks(indices)
    ax.set_xticklabels(x_values, rotation=30, ha='center')
    ax.set_title(title)
    ax.set_yticks([])


def display_summations(team: Team, venue: Venue, seasons: List[Season], team_stats: MatchStatistics, block: bool):
    fig, axes = plt.subplots(3, figsize=(25, 13), constrained_layout=True)

    if len(seasons) > 1:
        year_string = '{}-{}'.format(seasons[0].year, seasons[-1].year)
    else:
        year_string = str(seasons[0].year)

    if venue == Venue.any:
        venue_string = '{} or {}'.format(Venue.home.name, Venue.away.name)
    else:
        venue_string = '{} only'.format(venue.name)

    total_games = team_stats.both_halves.wins + team_stats.both_halves.draws + team_stats.both_halves.losses
    fig.suptitle('{}: {} over {} games ({})'.format(year_string, team.name, total_games, venue_string),
                 fontweight='bold', fontsize=14)

    create_single_bar(axes[0], team_stats.first_half, 'First half')
    create_single_bar(axes[1], team_stats.second_half, 'Second half')
    create_single_bar(axes[2], team_stats.both_halves, 'Both halves')
    plt.show(block=block)


def reduce(individuals: List[MatchStatistics], func: Callable, scale: int = 1) -> MatchStatistics:
    reduced_stats = MatchStatistics()
    for half in MatchStatistics.__slots__:
        for stat in Statistics.__slots__:
            if type(getattr(getattr(reduced_stats, half), stat)) is int:
                values = []
                for an_individual in individuals:
                    values.append(getattr(getattr(an_individual, half), stat))
                setattr(getattr(reduced_stats, half), stat, scale * int(func(values)))
            else:
                values = {}
                for an_individual in individuals:
                    for k, v in getattr(getattr(an_individual, half), stat).items():
                        values.setdefault(k, []).append(v)
                counter = Counter()
                for k, v in values.items():
                    counter[k] = scale * int(func(v))
                setattr(getattr(reduced_stats, half), stat, counter)
    return reduced_stats


def main(args: Namespace):
    load_teams(args.database)
    league = league_register[get_unique_league(args)]
    load_league(args.database, league)

    seasons = Season.seasons(league)
    if not seasons:
        messages.error_message("No season data found")

    if args.history:
        seasons = seasons[-args.history:]

    (row,) = extract_picked_team(args.database, get_unique_team(args), league)
    selected_team = Team.inventory[row[0]]

    if args.averages:
        if seasons[-1].current:
            seasons.pop()

        if seasons:
            team_season_stats = []
            collective_season_stats = []
            for season in seasons:
                stats = compute_statistics(season, selected_team, args.venue, args.game_states)
                team_season_stats.append(stats)

                teams = season.teams()
                if selected_team in teams:
                    this_season_stats = []
                    for team in teams:
                        if team != selected_team:
                            stats = compute_statistics(season, team, args.venue, args.game_states)
                            this_season_stats.append(stats)
                    collective_season_stats.append(reduce(this_season_stats, median))

            team_stats = reduce(team_season_stats, median, len(collective_season_stats))
            collective_stats = reduce(collective_season_stats, sum)
            display_averages(selected_team, args.venue, seasons, team_stats, collective_stats, args.block)
        else:
            messages.error_message('No historical data to analyse')
    else:
        team_season_stats = []
        for season in seasons:
            stats = compute_statistics(season, selected_team, args.venue, args.game_states)
            team_season_stats.append(stats)

        team_stats = reduce(team_season_stats, sum)
        display_summations(selected_team, args.venue, seasons, team_stats, args.block)


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)

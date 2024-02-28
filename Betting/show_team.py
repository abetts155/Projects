from argparse import ArgumentParser, Namespace
from collections import Counter
from re import compile
from typing import Callable, List

from matplotlib import pyplot as plt
from numpy import arange, median

from cli.cli import (add_database_option,
                     add_history_option,
                     add_league_option,
                     add_team_option,
                     add_logging_options,
                     add_venue_option,
                     add_block_option,
                     add_save_option,
                     set_logging_options,
                     get_unique_league,
                     get_unique_team)
from lib import messages
from lib.helpful import set_matplotlib_defaults
from model.fixtures import Half, Scoreline, Venue, win, loss, draw, bts
from model.leagues import league_register
from model.seasons import Season
from model.teams import Team
from sql.sql import extract_picked_team, load_league, load_teams


def parse_command_line():
    parser = ArgumentParser(description='Analyse individual team')
    add_database_option(parser)
    add_history_option(parser)
    add_league_option(parser)
    add_team_option(parser)
    add_venue_option(parser)
    add_block_option(parser)
    add_save_option(parser)
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
    goal_re = compile(r'(\d+)(\+)? goal(s)?')
    score_re = compile(r'(\d+|X)-(\d+|X)')

    __slots__ = ['wins',
                 'draws',
                 'losses',
                 'goals_for',
                 'goals_against',
                 'both_scored',
                 'scored',
                 'conceded',
                 'goals',
                 'scores']

    def __init__(self):
        self.wins = 0
        self.draws = 0
        self.losses = 0
        self.goals_for = 0
        self.goals_against = 0
        self.both_scored = 0
        self.scored = 0
        self.conceded = 0
        self.goals = Counter()
        self.scores = Counter()

    def get(self, name: str):
        if hasattr(self, name.lower()):
            return getattr(self, name.lower())
        elif name == 'for':
            return self.goals_for
        elif name == 'against':
            return self.goals_against
        elif name == 'both scored':
            return self.both_scored
        elif Statistics.goal_re.match(name):
            matches = Statistics.goal_re.findall(name)
            x, y, z = matches[0]
            base = int(x)
            if not y:
                return self.goals[base]
            else:
                total = 0
                for key in self.goals.keys():
                    if key >= base:
                        total += self.goals[key]
                return total
        elif Statistics.score_re.match(name):
            matches = Statistics.score_re.findall(name)
            x, y = matches[0]
            return self.scores[(int(x), int(y))]
        assert False


class MatchStatistics:
    __slots__ = ['first_half', 'second_half', 'both_halves']

    def __init__(self):
        self.first_half = Statistics()
        self.second_half = Statistics()
        self.both_halves = Statistics()


def update_stats(stats: Statistics, score: Scoreline):
    stats.goals_for += score.left
    stats.goals_against += score.right

    if win(score):
        stats.wins += 1
    elif loss(score):
        stats.losses += 1
    else:
        assert draw(score)
        stats.draws += 1

    if bts(score):
        stats.both_scored += 1

    if score.left > 0:
        stats.scored += 1

    if score.right > 0:
        stats.conceded += 1

    stats.goals[score.left + score.right] += 1
    stats.scores[(score.left, score.right)] += 1


def compute_statistics(season: Season, team: Team, venue: Venue, game_states: List[str]):
    fixtures = []
    for fixture in season.fixtures():
        if fixture.first_half() is not None and fixture.second_half() is not None:
            if venue == Venue.anywhere:
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
            ax.text(effective_index, count, str(count), ha='center')
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
        ax.text(x, y, str(y), ha='center')

    attach_scorelines(ax, x_values, y_values, left, int.__gt__)
    ax.axvline((width + 2 * len(x_values) - 1) / 2, ls='-', lw=1)
    attach_scorelines(ax, x_values, y_values, left, int.__eq__)
    ax.axvline((width + 2 * len(x_values) - 1) / 2, ls='-', lw=1)
    attach_scorelines(ax, x_values, y_values, left, int.__lt__)

    ax.bar(x_values, y_values, align='center')

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
        ax.text(x + width, y, str(y), ha='center')

    indices = arange(len(x_values))
    ax.bar(indices + width, y_values, width=width, align='center')

    boundaries = [(2, 3), (7, 8), (13, 14)]
    for pair in boundaries:
        ax.axvline((width + pair[0] + pair[1]) / 2, lw=4)

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
    fig, axes = plt.subplots(3, constrained_layout=True)

    if len(seasons) > 1:
        year_string = '{}-{}'.format(seasons[0].year, seasons[-1].year)
    else:
        year_string = str(seasons[0].year)

    fig.suptitle('{}: Averages comparison for {} ({})'.format(year_string, team.name, venue.name),
                 fontweight='bold')

    create_dual_bars(axes[0], team_stats.first_half, all_stats.first_half, 'First half')
    create_dual_bars(axes[1], team_stats.second_half, all_stats.second_half, 'Second half')
    create_dual_bars(axes[2], team_stats.both_halves, all_stats.both_halves, 'Both halves')
    plt.show(block=block)


class Frame:
    def __init__(self, x_values, cmap, color):
        self.x_values = x_values
        self.cmap = cmap
        self.color = color


def create_single_bar(ax, stats: Statistics, frame: Frame):
    y_values = []
    for field in frame.x_values:
        y_values.append(stats.get(field))

    min_y = min(y_values)
    max_y = max(y_values)
    scaled = [(y - min_y) / (max_y - min_y) for y in y_values]
    ax.bar(frame.x_values,
           y_values,
           align='center',
           color=frame.color(scaled) if frame.cmap else frame.color,
           zorder=1)

    for x, y in enumerate(y_values):
        ax.text(x, y, str(y), ha='center', zorder=2)

    ax.set_yticks([])
    ax.set_frame_on(False)


def create_bar_charts(team: Team, venue: Venue, seasons: List[Season], team_stats: MatchStatistics):
    fig, axs = plt.subplots(nrows=3,
                            ncols=5,
                            gridspec_kw={'width_ratios': [0.75, 0.5, 0.75, 1.5, 3.5]},
                            constrained_layout=True)

    if len(seasons) > 1:
        year_string = '{}-{}'.format(seasons[0].year, seasons[-1].year)
    else:
        year_string = str(seasons[0].year)

    total_games = team_stats.both_halves.wins + team_stats.both_halves.draws + team_stats.both_halves.losses
    title = '{}: {} ({}) over {} games'.format(year_string,
                                               team.name,
                                               '{} or {}'.format(Venue.home.name, Venue.away.name)
                                               if venue == Venue.anywhere else venue.name, total_games)
    fig.suptitle(title, fontweight='bold')

    frame_1 = Frame(['wins', 'draws', 'losses'], True,  plt.get_cmap('Wistia'))
    frame_2 = Frame(['for', 'against'], False, '#DA70D6')
    frame_3 = Frame(['both scored', 'scored', 'conceded'], False, '#DC143C')
    frame_4 = Frame(['0 goals', '1 goal', '2 goals', '3 goals', '4 goals', '5+ goals'], True, plt.get_cmap('Blues'))
    frames = [frame_1, frame_2, frame_3, frame_4]
    for half in Half:
        if half == Half.first:
            stats = team_stats.first_half
            i = 0
        elif half == Half.second:
            stats = team_stats.second_half
            i = 1
        else:
            stats = team_stats.both_halves
            i = 2

        for j, frame in enumerate(frames):
            ax = axs[i, j]
            create_single_bar(ax, stats, frame)

            if j == 0:
                ax.set_ylabel(half.value)

        sorted_scores = sorted(stats.scores.items(),
                               key=lambda x: (x[1], x[0][0] > x[0][1], x[0][0] == x[0][1], -sum(x[0])),
                               reverse=True)
        cutoff = 10
        x_values = []
        for score, count in sorted_scores:
            home, away = score
            x_values.append('{}-{}'.format(home, away))
        ax = axs[i, len(frames)]
        frame = Frame(x_values, True, plt.get_cmap('Reds'))
        create_single_bar(ax, stats, frame)


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
    set_matplotlib_defaults()
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
        if team_stats.both_halves.wins + team_stats.both_halves.draws + team_stats.both_halves.losses > 0:
            create_bar_charts(selected_team, args.venue, seasons, team_stats)
            if args.save:
                plt.savefig(args.save)
            else:
                plt.show(block=args.block)


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)

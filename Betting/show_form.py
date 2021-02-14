from argparse import ArgumentParser, Namespace
from collections import OrderedDict
from cli.cli import (add_database_option,
                     add_history_option,
                     add_league_option,
                     add_team_option,
                     add_half_option,
                     add_logging_options,
                     add_venue_option,
                     set_logging_options,
                     get_unique_league,
                     get_multiple_teams,
                     get_unique_team)
from lib.helpful import split_into_contiguous_groups, to_string
from lib.messages import error_message
from math import floor
from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
from model.fixtures import Half, Venue, win, defeat, draw
from model.leagues import league_register, League
from model.seasons import Season
from model.tables import LeagueTable, Position
from model.teams import Team
from statistics import mean
from sql.sql import load_database, extract_picked_team
from typing import List


def parse_command_line():
    parser = ArgumentParser(description='Compare team performance from this season against previous seasons')
    add_database_option(parser)
    add_history_option(parser)
    add_league_option(parser)
    add_team_option(parser)
    add_half_option(parser)
    add_venue_option(parser)
    add_logging_options(parser)

    parser.add_argument('-A',
                        '--average',
                        help='compare against averages over previous seasons',
                        action='store_true',
                        default=False)

    parser.add_argument('-r',
                        '--relative',
                        choices=Position,
                        type=Position.from_string,
                        metavar='{{{}}}'.format(','.join(position.name for position in Position)),
                        help='compare against relative table positions over previous seasons')

    parser.add_argument('-p',
                        '--position',
                        type=int,
                        nargs='+',
                        help='compare against absolute table positions over previous seasons')

    return parser.parse_args()


class Statistics:
    __slots__ = ['wins', 'draws', 'losses', 'goals_for', 'goals_against', 'points']

    def __init__(self):
        self.wins = 0
        self.draws = 0
        self.losses = 0
        self.goals_for = 0
        self.goals_against = 0
        self.points = 0

    def __str__(self):
        return 'W={}, D={}, L={}, GF={}, GA={} PTS={}'.format(self.wins,
                                                              self.draws,
                                                              self.losses,
                                                              self.goals_for,
                                                              self.goals_against,
                                                              self.points)


def combine(left: Statistics, right: Statistics) -> Statistics:
    combined = Statistics()
    combined.wins = left.wins + right.wins
    combined.draws = left.draws + right.draws
    combined.losses = left.losses + right.losses
    combined.goals_for = left.goals_for + right.goals_for
    combined.goals_against = left.goals_against + right.goals_against
    combined.points = left.points + right.points
    return combined


def compute_statistics(season: Season, team: Team, venue: Venue, half: Half, summary: List[Statistics]):
    fixtures = []
    season.sort_fixtures()
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

    for fixture in fixtures:
        if half is not None:
            if half == Half.first:
                result = fixture.first_half()
            else:
                result = fixture.second_half()
        else:
            result = fixture.full_time()

        if fixture.away_team == team:
            result = result.reverse()

        stats = Statistics()
        if win(result):
            stats.wins += 1
            stats.points += 3
        elif draw(result):
            stats.draws += 1
            stats.points += 1
        else:
            assert defeat(result)
            stats.losses += 1

        stats.goals_for = result.left
        stats.goals_against = result.right

        if not summary:
            summary.append(stats)
        else:
            summary.append(combine(stats, summary[-1]))


class Subplot:
    def __init__(self, ax, title):
        self.ax = ax
        self.title = title
        self.xlim = 0
        self.ylim = 0

    def add_line(self, x_values, y_values, label: str, color, line_width: int, marker_size: int):
        self.xlim = max(self.xlim, x_values[-1])
        self.ylim = max(self.ylim, y_values[-1])
        self.ax.plot(x_values,
                     y_values,
                     color=color,
                     linewidth=line_width,
                     label=label,
                     marker='o',
                     ms=marker_size)


class Datum:
    __slots__ = ['summary', 'label', 'color', 'line_width', 'marker_size']

    def __init__(self, summary: List[Statistics], label: str, color: str, line_width: float = 2, marker_size: int = 10):
        self.summary = summary
        self.label = label
        self.color = color
        self.line_width = line_width
        self.marker_size = marker_size


def display(title: str, data: List[Datum]):
    fig, axes = plt.subplots(2, 3, figsize=(20, 12))
    wins_ax, draws_ax, losses_ax = axes[0]
    goals_for_ax, goals_against_ax, points_ax = axes[1]

    wins_subplot = Subplot(wins_ax, 'Wins')
    draws_subplot = Subplot(draws_ax, 'Draws')
    losses_subplot = Subplot(losses_ax, 'Losses')
    goals_for_subplot = Subplot(goals_for_ax, 'GF')
    goals_against_subplot = Subplot(goals_against_ax, 'GA')
    points_subplot = Subplot(points_ax, 'PTS')

    subplots = [wins_subplot, draws_subplot, losses_subplot, goals_for_subplot, goals_against_subplot, points_subplot]
    for datum in data:
        wins_line = []
        draws_line = []
        losses_line = []
        goals_for_line = []
        goals_against_line = []
        points_line = []
        x_values = []

        for week, week_stats in enumerate(datum.summary, start=1):
            x_values.append(week)
            wins_line.append(week_stats.wins)
            draws_line.append(week_stats.draws)
            losses_line.append(week_stats.losses)
            goals_for_line.append(week_stats.goals_for)
            goals_against_line.append(week_stats.goals_against)
            points_line.append(week_stats.points)

        wins_subplot.add_line(x_values, wins_line, datum.label, datum.color, datum.line_width, datum.marker_size)
        draws_subplot.add_line(x_values, draws_line, datum.label, datum.color, datum.line_width, datum.marker_size)
        losses_subplot.add_line(x_values, losses_line, datum.label, datum.color, datum.line_width, datum.marker_size)
        goals_for_subplot.add_line(x_values, goals_for_line, datum.label, datum.color, datum.line_width,
                                   datum.marker_size)
        goals_against_subplot.add_line(x_values, goals_against_line, datum.label, datum.color, datum.line_width,
                                       datum.marker_size)
        points_subplot.add_line(x_values, points_line, datum.label, datum.color, datum.line_width, datum.marker_size)

    max_ylabels = 10
    for subplot in subplots:
        subplot.ax.set_title(subplot.title)
        subplot.ax.xaxis.set_major_locator(MaxNLocator(integer=True))
        subplot.ax.yaxis.set_major_locator(MaxNLocator(integer=True))

        subplot.ax.set_xlim(1, subplot.xlim)
        step = 5
        xticks = [1] + [i for i in range(1 + step, subplot.xlim, step)] + [subplot.xlim]
        subplot.ax.set_xticks(xticks)
        subplot.ax.set_xlabel('Matchday')

        subplot.ax.set_ylim(0, subplot.ylim)
        if subplot.ylim <= max_ylabels:
            yticks = [i for i in range(0, subplot.ylim)] + [subplot.ylim]
        else:
            step = subplot.ylim // 10
            yticks = [i for i in range(subplot.ylim, 0, -step)]
        subplot.ax.set_yticks(yticks)

        subplot.ax.legend()

    fig.suptitle(title, fontweight='bold', fontsize=14)
    plt.tight_layout()
    plt.show()


def compute_summary(team: Team, seasons: List[Season], venue: Venue, half: Half):
    team_summary = OrderedDict()
    for season in seasons:
        summary = []
        compute_statistics(season, team, venue, half, summary)
        team_summary[season] = summary
    return team_summary


def compute_average(combined_summary):
    average_summary = []
    for weekly_stats in combined_summary:
        average_stats = Statistics()
        average_summary.append(average_stats)
        for stat in Statistics.__slots__:
            values = [getattr(datum, stat) for datum in weekly_stats]
            average = floor(mean(values))
            setattr(average_stats, stat, average)
    return average_summary


def add_venue_and_half_to_title(title: str, venue: Venue, half: Half):
    if venue == Venue.any:
        title = '{} ({} or {})'.format(title, Venue.home.name, Venue.away.name)
    else:
        title = '{} ({} only)'.format(title, venue.name)

    if half is not None:
        title = '{} ({} half)'.format(title, half.name)

    return title


class Color:
    def __init__(self, gradient: bool = False):
        self.index = 0
        if gradient:
            self.choices = [(0.118, 0.565, 1), (1, 0, 0), (1, 0.39, 0.28), (1, 0.49, 0.31), (0.80, 0.36, 0.36),
                            (0.95, 0.50, 0.50), (0.91, 0.59, 0.48), (0.98, 0.50, 0.44), (1, 0.63, 0.48), (1, 0.27, 0),
                            (1, 0.55, 0), (1, 0.65, 0), (1, 0.84, 0)]
        else:
            self.choices = ['black', 'dodgerblue', 'lightblue', 'salmon', 'gold', 'darkorange', 'silver']

    def next(self) -> str:
        if self.index > len(self.choices) - 1:
            error_message('Out of options for next color')
        self.index += 1
        return self.choices[self.index - 1]


def compute_relative_performance(args: Namespace,
                                 league: League,
                                 seasons: List[Season],
                                 team_names: List[str],
                                 positions: List[int]):
    this_season = seasons[-1]
    team_summaries = {}
    for name in team_names:
        team = extract_picked_team(args.database, name, league)
        team_summaries[team] = compute_summary(team, [this_season], args.venue, args.half)

    summaries = []
    for season in seasons:
        if not season.current:
            table = LeagueTable(season)
            teams = table.teams_by_position(positions)
            for team in teams:
                summary = []
                summaries.append(summary)
                compute_statistics(season, team, args.venue, args.half, summary)

    combined_summary = []
    for summary in summaries:
        for week, stat in enumerate(summary):
            if week >= len(combined_summary):
                combined_summary.append([])
            combined_summary[week].append(stat)

    color = Color()
    sublists = split_into_contiguous_groups(positions)
    label = 'Positions: {}'.format(to_string(sublists))
    label = '{} ({}-{})'.format(label, seasons[0].year, seasons[-2].year)
    average_summary = compute_average(combined_summary)
    data = [Datum(average_summary, label, color.next())]

    for name in team_names:
        team = extract_picked_team(args.database, name, league)
        summary = team_summaries[team][this_season]
        data.append(Datum(summary, team.name, color.next()))

    title = 'Relative performance comparison {}-{}'.format(seasons[0].year, seasons[-2].year)
    title = add_venue_and_half_to_title(title, args.venue, args.half)
    display(title, data)


def compute_average_performance(args: Namespace, league: League, seasons: List[Season], team_name: str):
    team = extract_picked_team(args.database, team_name, league)
    team_summary = compute_summary(team, seasons, args.venue, args.half)

    this_season = seasons[-1]
    combined_summary = []
    for season, summary in team_summary.items():
        if season != this_season and summary:
            for week, stat in enumerate(summary):
                if week >= len(combined_summary):
                    combined_summary.append([])
                combined_summary[week].append(stat)

    color = Color()
    average_summary = compute_average(combined_summary)
    data = [Datum(average_summary, 'Average', color.next()),
            Datum(team_summary[this_season], '{}:{}'.format(team.name, this_season.year), color.next())]

    title = 'Average performance comparison {}-{}'.format(seasons[0].year, seasons[-2].year)
    title = add_venue_and_half_to_title(title, args.venue, args.half)
    display(title, data)


def compute_individual_performance(args: Namespace, league: League, seasons: List[Season]):
    name = get_unique_team(args)
    team = extract_picked_team(args.database, name, league)
    team_summary = compute_summary(team, seasons, args.venue, args.half)

    color = Color(True)
    linewidth = 3
    marker_size = 10
    data = []
    for season in reversed(seasons):
        summary = team_summary[season]
        if summary:
            data.append(Datum(summary, str(season.year), color.next(), linewidth, marker_size))
            linewidth -= 0.3
            marker_size -= 1

    title = 'Team performance comparison {}-{}: {}'.format(seasons[0].year, seasons[-2].year, team.name)
    title = add_venue_and_half_to_title(title, args.venue, args.half)
    display(title, data)


def main(args: Namespace):
    league = league_register[get_unique_league(args)]
    load_database(args.database, league)

    seasons = Season.seasons()
    if args.history:
        seasons = seasons[-args.history:]

    if args.relative:
        team_names = get_multiple_teams(args)
        table = LeagueTable(seasons[-1])
        lower, upper = table.positions(args.relative)
        positions = [i for i in range(lower, upper)]
        compute_relative_performance(args, league, seasons, team_names, positions)
    elif args.position:
        team_names = get_multiple_teams(args)
        compute_relative_performance(args, league, seasons, team_names, args.position)
    elif args.average:
        team_name = get_unique_team(args)
        compute_average_performance(args, league, seasons, team_name)
    else:
        compute_individual_performance(args, league, seasons)


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)

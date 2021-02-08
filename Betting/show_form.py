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
                     get_unique_team)
from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
from model.fixtures import Half, Venue
from model.leagues import league_register
from model.seasons import Season
from model.teams import Team
from sql.sql import load_database, extract_picked_team
from typing import Dict, List


def parse_command_line():
    parser = ArgumentParser(description='Compare team performance from this season against previous seasons')
    add_database_option(parser)
    add_history_option(parser)
    add_league_option(parser)
    add_team_option(parser)
    add_half_option(parser)
    add_venue_option(parser)
    add_logging_options(parser)
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

    def combine(self, other: "Statistics"):
        self.wins += other.wins
        self.draws += other.draws
        self.losses += other.losses
        self.goals_for += other.goals_for
        self.goals_against += other.goals_against
        self.points += other.points

    def __str__(self):
        return 'W={}, D={}, L={}, GF={}, GA={} PTS={}'.format(self.wins,
                                                              self.draws,
                                                              self.losses,
                                                              self.goals_for,
                                                              self.goals_against,
                                                              self.points)


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
        if result.win():
            stats.wins += 1
            stats.points += 3
        elif result.draw():
            stats.draws += 1
            stats.points += 1
        else:
            stats.losses += 1

        stats.goals_for = result.left
        stats.goals_against = result.right

        if not summary:
            summary.append(stats)
        else:
            stats.combine(summary[-1])
            summary.append(stats)


class Subplot:
    def __init__(self, ax, title):
        self.ax = ax
        self.title = title
        self.xlim = 0
        self.ylim = 0

    def add_line(self, current: Season, season: Season, x_values, y_values):
        if season == current:
            color = 'dodgerblue'
            linewidth = 2.5
        else:
            difference = current.year - season.year - 1
            color = (1 - difference/10, difference/10, difference/10)
            linewidth = 2/(difference + 1)

        self.ax.plot(x_values, y_values, color=color, linewidth=linewidth, label=season.year)

        if x_values:
            self.xlim = max(self.xlim, x_values[-1])

        if y_values:
            self.ylim = max(self.ylim, y_values[-1])


def display(team: Team, venue: Venue, current: Season, season_summary: Dict[Season, List[Statistics]]):
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

    for season, summary in season_summary.items():
        wins_line = []
        draws_line = []
        losses_line = []
        goals_for_line = []
        goals_against_line = []
        points_line = []
        x_values = []

        if summary:
            for week, week_stats in enumerate(summary, start=1):
                x_values.append(week)
                wins_line.append(week_stats.wins)
                draws_line.append(week_stats.draws)
                losses_line.append(week_stats.losses)
                goals_for_line.append(week_stats.goals_for)
                goals_against_line.append(week_stats.goals_against)
                points_line.append(week_stats.points)

            wins_subplot.add_line(current, season, x_values, wins_line)
            draws_subplot.add_line(current, season, x_values, draws_line)
            losses_subplot.add_line(current, season, x_values, losses_line)
            goals_for_subplot.add_line(current, season, x_values, goals_for_line)
            goals_against_subplot.add_line(current, season, x_values, goals_against_line)
            points_subplot.add_line(current, season, x_values, points_line)

    max_ylabels = 10
    for subplot in subplots:
        subplot.ax.set_title(subplot.title)
        subplot.ax.xaxis.set_major_locator(MaxNLocator(integer=True))
        subplot.ax.yaxis.set_major_locator(MaxNLocator(integer=True))

        subplot.ax.set_xlim(1, subplot.xlim)
        step = 5
        xticks = [1] + [i for i in range(1 + step, subplot.xlim, step)] + [subplot.xlim]
        subplot.ax.set_xticks(xticks)

        subplot.ax.set_ylim(0, subplot.ylim)
        if subplot.ylim <= max_ylabels:
            yticks = [i for i in range(0, subplot.ylim)] + [subplot.ylim]
        else:
            step = subplot.ylim//10
            yticks = [i for i in range(subplot.ylim, 0, -step)]
        subplot.ax.set_yticks(yticks)

        subplot.ax.legend()

    if venue == Venue.any:
        venue_string = '{} or {}'.format(Venue.home.name, Venue.away.name)
    else:
        venue_string = '{} only'.format(venue.name)

    fig.suptitle('{} ({})'.format(team.name, venue_string), fontweight='bold', fontsize=14)
    plt.tight_layout()
    plt.show()


def main(arguments: Namespace):
    league = league_register[get_unique_league(arguments)]
    load_database(arguments.database, league)

    seasons = Season.seasons()
    if arguments.history:
        seasons = seasons[-arguments.history:]

    selected_team = extract_picked_team(arguments.database, get_unique_team(arguments), league)
    season_summary = OrderedDict()
    for season in seasons:
        summary = []
        compute_statistics(season, selected_team, arguments.venue, arguments.half, summary)
        season_summary[season] = summary

    display(selected_team, arguments.venue, seasons[-1], season_summary)


if __name__ == '__main__':
    arguments = parse_command_line()
    set_logging_options(arguments)
    main(arguments)

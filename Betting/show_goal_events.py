from argparse import ArgumentParser, Namespace
from cli.cli import (add_database_option,
                     add_league_option,
                     add_logging_options,
                     set_logging_options,
                     add_history_option,
                     add_team_option,
                     add_venue_option,
                     add_block_option,
                     get_unique_league,
                     get_unique_team)
from collections import Counter, OrderedDict
from concurrent.futures import ThreadPoolExecutor, as_completed
from lib.helpful import DisplayGrid, set_matplotlib_defaults
from lib.messages import error_message
from matplotlib import pyplot as plt
from model.events import Event, get_events_for_fixture, is_goal
from model.fixtures import Venue
from model.leagues import league_register
from model.seasons import Season
from model.teams import Team
from numpy import arange
from sql.sql_columns import ColumnNames
from sql.sql import Database, load_league, load_teams, extract_picked_team
from typing import List, Tuple


def parse_command_line():
    parser = ArgumentParser(description='Show minutes when goals are scored')
    add_database_option(parser)
    add_block_option(parser)
    add_history_option(parser)
    add_venue_option(parser)
    add_league_option(parser, True)
    add_team_option(parser, True)
    add_logging_options(parser)

    parser.add_argument('-I',
                        '--intervals',
                        help='specify the number of time intervals per half, excluding injury time',
                        type=int,
                        default=3)

    return parser.parse_args()


class Interval:
    __slots__ = ['lower', 'upper', 'injury']

    def __init__(self, lower: int, upper: int, injury: bool):
        self.lower = lower
        self.upper = upper
        self.injury = injury

    def __str__(self):
        if self.injury:
            return '{}+'.format(self.upper)
        else:
            return '{}-{}'.format(self.lower, self.upper)


def create_intervals(intervals: List[Interval], interval_size: int, offset: int):
    minutes_per_interval = 45 // interval_size
    slack = 45 % interval_size
    lower = 1
    upper = 45
    next = []
    for i in range(lower, upper + 1):
        if len(next) == minutes_per_interval:
            if slack == 0:
                intervals.append(Interval(next[0] + offset, next[-1] + offset, False))
                next = []
            else:
                slack -= 1
        elif len(next) == minutes_per_interval + 1:
            intervals.append(Interval(next[0] + offset, next[-1] + offset, False))
            next = []

        next.append(i)

    intervals.append(Interval(next[0] + offset, next[-1] + offset, False))
    intervals.append(Interval(upper + offset, upper + offset, True))


class Summary:
    def __init__(self, intervals: List[Interval]):
        self._interval_to_goals = Counter()
        self._first_half_injury = None
        self._second_half_injury = None
        self._indexer = {}
        for interval in intervals:
            self._interval_to_goals[interval] = 0
            if interval.injury:
                if interval.lower == 45 and interval.upper == 45:
                    self._first_half_injury = interval
                else:
                    self._second_half_injury = interval

    def add_goal(self, time: int, extra_time: int):
        if time > 0:
            if extra_time > 0:
                if time == 45:
                    self._interval_to_goals[self._first_half_injury] += 1
                else:
                    self._interval_to_goals[self._second_half_injury] += 1
            else:
                if time not in self._indexer:
                    for interval in self._interval_to_goals.keys():
                        if interval.lower <= time <= interval.upper:
                            self._indexer[time] = interval

                self._interval_to_goals[self._indexer[time]] += 1

    def __iter__(self):
        for interval, goals in self._interval_to_goals.items():
            yield interval, goals

    def __len__(self):
        return len(self._interval_to_goals)

    def __bool__(self):
        return sum(self._interval_to_goals.values()) > 0


def compute_summaries(season: Season,
                      intervals: List[Interval],
                      team: Team,
                      venue: Venue) -> Tuple[Summary]:
    for_team = Summary(intervals)
    against_team = Summary(intervals)

    time_since_goal = 0
    for fixture in season.fixtures():
        events = get_events_for_fixture(fixture)
        for event in events:
            if is_goal(event.detail):
                count_for_team = False
                if venue == Venue.any:
                    count_for_team = team in [fixture.home_team, fixture.away_team]
                elif venue == Venue.home and team == fixture.home_team:
                    count_for_team = True
                elif venue == Venue.away and team == fixture.away_team:
                    count_for_team = True

                if count_for_team:
                    if event.team == team:
                        for_team.add_goal(event.time, event.extra_time)
                        time_since_goal
                    else:
                        against_team.add_goal(event.time, event.extra_time)

    return for_team, against_team


def create_bar(ax, summary: Summary, bar_width: float, offset: int, color: str):
    indices = arange(len(summary))
    y_values = []
    for interval, goals in summary:
        y_values.append(goals)

    ax.bar(indices + bar_width * offset, y_values, width=bar_width, color=color)

    for k, v in zip(indices, y_values):
        ax.text(k + bar_width * offset, v, str(v), ha='center')


def show(title: str, season_to_summary, block: bool):
    set_matplotlib_defaults()
    display = DisplayGrid(len(season_to_summary), 2)
    fig, axs = plt.subplots(nrows=display.nrows,
                            ncols=display.ncols,
                            figsize=(20, 10),
                            constrained_layout=True)

    for i, season in enumerate(sorted(season_to_summary.keys())):
        for_team, against_team = season_to_summary[season]
        cell_x, cell_y = display.index(i)
        ax = axs[cell_x, cell_y]

        bar_width = 0.3
        create_bar(ax, for_team, bar_width, 1, 'gold')
        create_bar(ax, against_team, bar_width, 2, 'black')

        labels = []
        indices = arange(len(for_team))
        for interval, _ in for_team:
            labels.append(str(interval))

        ax.set_xticks(indices + bar_width * 1.5)
        ax.set_xticklabels(labels, ha='center')

        #ax.set_frame_on(False)
        ax.set_yticks([])
        ax.set_ylabel('{}'.format(season.year))

    for i in range(len(season_to_summary), display.nrows * display.ncols):
        cell_x, cell_y = display.index(i)
        ax = axs[cell_x][cell_y]
        fig.delaxes(ax)

    fig.suptitle(title, fontweight='bold')
    plt.show(block=block)


def main(args: Namespace):
    load_teams(args.database)
    league = league_register[get_unique_league(args)]
    load_league(args.database, league)

    seasons = Season.seasons(league)
    if not seasons:
        error_message("No season data found")

    if args.history:
        seasons = seasons[-args.history:]

    (row,) = extract_picked_team(args.database, get_unique_team(args), league)
    selected_team = Team.inventory[row[0]]

    intervals = []
    create_intervals(intervals, args.intervals, 0)
    create_intervals(intervals, args.intervals, 45)

    season_to_summary = OrderedDict()
    with ThreadPoolExecutor(max_workers=8) as executor:
        future_to_data = {executor.submit(compute_summaries,
                                          season,
                                          intervals,
                                          selected_team,
                                          args.venue): season for season in seasons}
        for future in as_completed(future_to_data):
            season = future_to_data[future]
            for_team, against_team = future.result()
            if for_team and against_team:
                season_to_summary[season] = (for_team, against_team)

    title = 'Goal times: {} ({})'.format(selected_team.name, league)
    if args.venue == Venue.any:
        title = '{} ({} or {})'.format(title, Venue.home.name, Venue.away.name)
    else:
        title = '{} ({} only)'.format(title, args.venue.name)

    show(title, season_to_summary, args.block)


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)

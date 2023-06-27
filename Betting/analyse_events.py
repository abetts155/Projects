from argparse import ArgumentParser, Namespace
from cli.cli import (add_database_option,
                     add_league_option,
                     add_logging_options,
                     set_logging_options,
                     add_history_option,
                     add_half_option,
                     add_team_option,
                     add_venue_option,
                     add_block_option,
                     get_unique_league,
                     get_unique_team)
from collections import Counter, OrderedDict
from enum import Enum, auto
from lib.helpful import set_matplotlib_defaults, DisplayGrid
from lib.messages import error_message
from matplotlib import pyplot as plt
from model.events import Event, EventDetail, get_events_for_fixture, is_goal
from model.fixtures import Half, Venue
from model.leagues import league_register, prettify
from model.seasons import Season
from model.teams import Team
from sql.sql_columns import ColumnNames
from sql.sql import Database, load_league, load_teams, extract_picked_team
from sys import maxsize
from typing import Dict, List, Tuple


class AnalysedEvent(Enum):
    GF = auto()
    GA = auto()
    GFA = auto()
    YELLOW = auto()
    RED = auto()

    @staticmethod
    def from_string(string: str):
        try:
            return AnalysedEvent[string.upper()]
        except KeyError:
            error_message("Event '{}' is not valid".format(string))


def parse_command_line():
    parser = ArgumentParser(description='Analyse times between goals')
    add_database_option(parser)
    add_block_option(parser)
    add_history_option(parser)
    add_team_option(parser, True)
    add_half_option(parser)
    add_venue_option(parser)
    add_league_option(parser, True)
    add_logging_options(parser)

    parser.add_argument('-e',
                        '--event',
                        required=True,
                        choices=AnalysedEvent,
                        type=AnalysedEvent.from_string,
                        metavar='{{{}}}'.format(','.join(event.name for event in AnalysedEvent)),
                        help='choose the event to analyse')

    parser.add_argument('-I',
                        '--intervals',
                        help='specify the number of time intervals',
                        type=int,
                        default=5)

    return parser.parse_args()


class Interval:
    __slots__ = ['lower', 'upper']

    def __init__(self, lower: int, upper: int):
        self.lower = lower
        self.upper = upper

    def __lt__(self, other):
        return self.upper <= other.lower

    def __str__(self):
        if self.upper == maxsize:
            return '{}+'.format(self.lower)
        else:
            return '{}-{}'.format(self.lower, self.upper)


class SeasonData:
    __slots__ = ['season', 'count', 'interval', 'time']

    def __init__(self, season):
        self.season = season
        self.count = Counter()
        self.interval = None
        self.time = None


def create_bar(ax, datum: SeasonData):
    ax.set_frame_on(False)
    ax.set_yticks([])
    ax.tick_params(axis='x', rotation=45)

    x_values = []
    y_values = []
    to_highlight = None
    for i, interval in enumerate(sorted(datum.count.keys())):
        x_values.append(str(interval))
        y_values.append(datum.count[interval])
        ax.text(i, datum.count[interval], str(datum.count[interval]), ha='center', zorder=2)
        if interval == datum.interval:
            to_highlight = i

    cmap = plt.get_cmap('Blues')
    min_y = min(y_values)
    max_y = max(y_values)
    scaled = [(y - min_y) / (max_y - min_y) for y in y_values]
    bar = ax.bar(x_values, y_values, color=cmap(scaled), zorder=1)

    if to_highlight is not None:
        bar[to_highlight].set_edgecolor('gold')
        bar[to_highlight].set_lw(2)


def show(league, team, season_to_data, what_to_analyse: AnalysedEvent):
    display = DisplayGrid(len(season_to_data))
    fig, axs = plt.subplots(nrows=display.nrows,
                            ncols=display.ncols,
                            figsize=(20, 10),
                            squeeze=False,
                            constrained_layout=True)
    for i, datum in enumerate(season_to_data.values()):
        row, col = display.index(i)
        ax = axs[row, col]
        ax.set_ylabel('{}'.format(datum.season.year))
        create_bar(ax, datum)

    for i in range(len(season_to_data), display.nrows * display.ncols):
        cell_x, cell_y = display.index(i)
        ax = axs[cell_x][cell_y]
        fig.delaxes(ax)

    title = '{} {}: {}'.format(prettify(league.country), league.name, team.name)
    if datum.time is not None:
        title += '\nLast {} {} minutes ago'.format(what_to_analyse.name, datum.time)

    fig.suptitle(title, fontweight='bold')
    plt.show()


def gather(seasons: List[Season], team: Team, venue: Venue, half: Half, what_to_analyse: AnalysedEvent):
    season_to_gaps = OrderedDict()
    max_gap = 0
    for season in seasons:
        print('>' * 10, season.year)
        team_fixtures = []
        for fixture in season.fixtures():
            if fixture.finished:
                if venue == Venue.anywhere and team in [fixture.home_team, fixture.away_team]:
                    team_fixtures.append(fixture)
                elif venue == Venue.away and fixture.away_team == team:
                    team_fixtures.append(fixture)
                elif venue == Venue.home and fixture.home_team == team:
                    team_fixtures.append(fixture)

        season_events = []
        minutes_per_half = 45
        first = 0
        with Database(args.database) as db:
            for fixture in team_fixtures:
                fixture_events = get_events_for_fixture(fixture)

                if half in [Half.first, Half.second]:
                    fixture_events = [event for event in fixture_events if
                                      (event.time <= minutes_per_half and half == Half.first) or
                                      (event.time > minutes_per_half and half == Half.second)]

                fixture_events.sort(key=lambda event: (event.time, event.extra_time))
                filtered_events = []
                scored = []
                for event in fixture_events:
                    if what_to_analyse == AnalysedEvent.GF and is_goal(event.detail) and team == event.team:
                        filtered_events.append(event)
                        if not scored:
                            scored.append(event.team)
                    elif what_to_analyse == AnalysedEvent.GA and is_goal(event.detail) and team != event.team:
                        filtered_events.append(event)
                        if not scored:
                            scored.append(event.team)
                    elif what_to_analyse == AnalysedEvent.GFA and is_goal(event.detail):
                        filtered_events.append(event)
                        if not scored:
                            scored.append(event.team)
                    elif what_to_analyse == AnalysedEvent.YELLOW and event.detail == EventDetail.yellow_card and team == event.team:
                        filtered_events.append(event)
                    elif what_to_analyse == AnalysedEvent.RED and event.detail == EventDetail.red_card and team == event.team:
                        filtered_events.append(event)

                #print(fixture)
                if scored:
                    (first_scorer,) = scored
                    print('{} scored first'.format(first_scorer.name))
                    if first_scorer == team:
                        first += 1

                if filtered_events:
                    season_events.append(filtered_events)
                else:
                    season_events.append([])

        print(season.year, first)
        if season_events:
            gaps = []
            season_to_gaps[season] = gaps
            clock = 0
            game_time = 90
            last_time = 0
            for game_number, filtered_fixture_events in enumerate(season_events):
                for event in filtered_fixture_events:
                    relative_time = clock + event.time
                    gaps.append(relative_time - last_time)
                    max_gap = max(max_gap, gaps[-1])
                    last_time = relative_time

                clock += game_time

            gaps.append(game_time * len(team_fixtures) - last_time)
            max_gap = max(max_gap, gaps[-1])

    return season_to_gaps, max_gap


def create_intervals(this_season: Season, season_to_gaps: Dict[Season, List], max_gap: int, number_of_intervals: int):
    intervals = []
    if max_gap >= 90:
        interval_span = max_gap // number_of_intervals
        lower = 0
        for i in range(0, number_of_intervals):
            if i == number_of_intervals - 1:
                upper = max_gap
            else:
                upper = lower + interval_span
            intervals.append(Interval(lower, upper))
            lower += interval_span
    else:
        intervals.append(Interval(0, 30))
        intervals.append(Interval(31, 60))
        intervals.append(Interval(61, 90))

    season_to_data = OrderedDict()
    for season, gaps in season_to_gaps.items():
        datum = SeasonData(season)
        season_to_data[season] = datum

        for interval in intervals:
            datum.count[interval] = 0

        for time in gaps:
            for interval in intervals:
                if interval.lower <= time <= interval.upper:
                    datum.count[interval] += 1
                    if time == gaps[-1] and season == this_season:
                        datum.interval = interval
                        datum.time = time

    return season_to_data


def main(args: Namespace):
    set_matplotlib_defaults()
    load_teams(args.database)
    league = league_register[get_unique_league(args)]
    load_league(args.database, league)

    seasons = Season.seasons(league)
    if not seasons:
        error_message("No season data found")

    if args.history:
        seasons = seasons[-args.history:]

    (row,) = extract_picked_team(args.database, get_unique_team(args), league)
    team = Team.inventory[row[0]]
    season_to_gaps, max_gap = gather(seasons, team, args.venue, args.half, args.event)
    season_to_data = create_intervals(seasons[-1], season_to_gaps, max_gap, args.intervals)
    show(league, team, season_to_data, args.event)


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)

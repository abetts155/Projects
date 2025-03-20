from argparse import ArgumentParser, Namespace
from collections import OrderedDict
from typing import Callable, Dict, List

from matplotlib import pyplot as plt
from matplotlib.patches import Circle
from matplotlib.ticker import MaxNLocator

from cli.cli import (add_database_option,
                     add_league_option,
                     add_team_option,
                     get_unique_team,
                     get_unique_league,
                     add_venue_option,
                     add_logging_options,
                     set_logging_options,
                     add_block_option)
from lib.helpful import set_matplotlib_defaults
from lib.messages import error_message
from model.fixtures import Event, Fixture, Half, bts, Team, Venue
from model.competitions import league_register
from model.seasons import Season
from sql.sql import load_league, load_teams, extract_picked_team


def parse_command_line():
    parser = ArgumentParser(description='Pick out teams with a tendency for goals')
    add_database_option(parser)
    add_logging_options(parser)
    add_league_option(parser, True)
    add_team_option(parser, True)
    add_venue_option(parser)
    add_block_option(parser)
    return parser.parse_args()


analyses = [(Half.first, Event.get('gf_gt_0')),
            (Half.first, Event.get('ga_gt_0')),
            (Half.first, Event.get('gfa_gt_0')),
            (Half.first, Event.get('gfa_le_1')),
            (Half.second, Event.get('gf_gt_0')),
            (Half.second, Event.get('ga_gt_0')),
            (Half.second, Event.get('gfa_gt_0')),
            (Half.second, Event.get('gfa_le_1')),
            (Half.full, Event.get('gf_gt_0')),
            (Half.full, Event.get('ga_gt_0')),
            (Half.full, Event.get('gfa_gt_2')),
            (Half.full, bts)]


class TeamEventRegister:
    def __init__(self):
        self.numerators = OrderedDict()
        self.denominators = OrderedDict()
        for key in analyses:
            self.numerators[key] = 0
            self.denominators[key] = 0

    def increment_numerator(self, half: Half, event: Callable):
        self.numerators[(half, event)] += 1

    def increment_denominator(self, half: Half, event: Callable):
        self.denominators[(half, event)] += 1

    def get_numerator(self, half: Half, event: Callable):
        return self.numerators[(half, event)]

    def get_denominator(self, half: Half, event: Callable):
        return self.denominators[(half, event)]


class Percentages:
    def __init__(self):
        self.bins = OrderedDict()
        for percentage in range(0, 101):
            self.bins[percentage] = 0

    def add(self, percentage: float):
        self.bins[percentage] += 1

    def coalesce(self):
        print(self.bins)
        ranges = [(0, 9), (10, 19), (20, 29), (30, 39), (40, 49), (50, 59), (60, 69), (70, 79), (80, 89), (90, 100)]
        coalesced = OrderedDict({bounds: 0 for bounds in ranges})
        index = 0
        for percentage in sorted(self.bins.keys()):
            _, upper = ranges[index]
            if percentage > upper:
                index += 1
            coalesced[ranges[index]] += self.bins[percentage]
        return coalesced


def analyse_fixtures(team: Team, fixtures: List[Fixture], venue: Venue):
    if venue == Venue.away:
        fixtures = [fixture for fixture in fixtures if fixture.away_team == team]
    elif venue == Venue.home:
        fixtures = [fixture for fixture in fixtures if fixture.home_team == team]

    total_for = 0
    total_against = 0
    register = TeamEventRegister()
    for fixture in fixtures:
        if fixture.finished:
            for half, event in analyses:
                if half == Half.first:
                    result = fixture.first_half()
                elif half == Half.second:
                    result = fixture.second_half()
                else:
                    result = fixture.full_time()

                if result:
                    if fixture.away_team == team:
                        result = result.reverse()

                    total_for += result.left
                    total_against += result.right

                    register.increment_denominator(half, event)
                    if event(result):
                        register.increment_numerator(half, event)

    return total_for, total_against, register


def show(title: str, percentages: Dict, team_percentages: Dict, block: bool):
    fig, axs = plt.subplots(nrows=len(Half),
                            ncols=len(analyses) // len(Half),
                            constrained_layout=True)
    i = 0
    j = 0
    for half, event in analyses:
        data = percentages[(half, event)].coalesce()
        x_values = []
        y_values = []
        for (lower, upper), value in data.items():
            x_values.append('{}-{}'.format(lower, upper))
            y_values.append(value)

        ax = axs[i, j]
        j += 1
        if j == len(analyses) // len(Half):
            i += 1
            j = 0

        cmap = plt.get_cmap('Blues')
        min_y = min(y_values)
        max_y = max(y_values)
        scaled = [(y - min_y) / (max_y - min_y) for y in y_values]

        bars = ax.bar(x_values, y_values, color=cmap(scaled))
        ax.xaxis.set_major_locator(MaxNLocator(integer=True))
        ax.yaxis.set_major_locator(MaxNLocator(integer=True))
        ax.set_xticks(x_values)
        ax.set_xlabel('% of games')
        ax.set_frame_on(False)
        ax.set_title('{} ({})'.format(Event.name(event, False), Half.to_string([half])))

        highlight = None
        for k, (lower, upper) in enumerate(data.keys()):
            if lower <= team_percentages[(half, event)] <= upper:
                highlight = k
        bars[highlight].set_hatch('XX')

    fig.suptitle(title, fontweight='bold')
    plt.show(block=block)


def main(args: Namespace):
    set_matplotlib_defaults()
    load_teams(args.database)
    league = league_register[get_unique_league(args)]
    load_league(args.database, league)

    seasons = Season.seasons(league)
    if not seasons:
        error_message("No season data found")

    (row,) = extract_picked_team(args.database, get_unique_team(args), league)
    selected_team = Team.inventory[row[0]]

    percentages = {}
    for key in analyses:
        percentages[key] = Percentages()

    for season in seasons[:-1]:
        for team, fixtures in season.fixtures_per_team().items():
            total_for, total_against, register = analyse_fixtures(team, fixtures, args.venue)
            for half, event in analyses:
                numerator = register.get_numerator(half, event)
                denominator = register.get_denominator(half, event)
                if denominator:
                    percentage = round(100 * numerator / denominator)
                    percentages[(half, event)].add(percentage)

    this_season = seasons.pop()
    fixtures = this_season.fixtures_per_team()[selected_team]
    total_for, total_against, register = analyse_fixtures(selected_team, fixtures, args.venue)
    team_percentages = {}
    for half, event in analyses:
        numerator = register.get_numerator(half, event)
        denominator = register.get_denominator(half, event)
        if denominator:
            percentage = round(100 * numerator / denominator)
            team_percentages[(half, event)] = percentage

    title = '{} goal statistics ({}) compared against season averages in period {}-{}'.format(selected_team.name,
                                                                                              args.venue.name,
                                                                                              seasons[0].year,
                                                                                              seasons[-1].year)
    show(title, percentages, team_percentages, args.block)


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)

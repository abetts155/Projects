from argparse import ArgumentParser, Namespace
from cli.cli import (add_database_option,
                     add_league_option,
                     add_logging_options,
                     set_logging_options,
                     add_block_option,
                     add_history_option,
                     add_half_option,
                     add_venue_option,
                     add_events_option,
                     add_team_option,
                     get_unique_league,
                     get_unique_event)
from collections import Counter, OrderedDict
from lib import messages
from lib.helpful import DisplayGrid, set_matplotlib_defaults
from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
from numpy import min, max
from model.fixtures import Half, Event, Venue
from model.leagues import league_register, prettify, League
from model.seasons import Season
from model.sequences import count_events, DataUnit
from model.teams import Team
from sql.sql import load_league, load_teams, extract_picked_team
from typing import Callable, Dict, List


def parse_command_line():
    parser = ArgumentParser(description='Show summary of seasons by sequences')
    add_database_option(parser)
    add_league_option(parser)
    add_logging_options(parser)
    add_history_option(parser)
    add_half_option(parser)
    add_venue_option(parser)
    add_events_option(parser, True, 1)
    add_team_option(parser)
    add_block_option(parser)
    return parser.parse_args()


def show(title: str, season_data: Dict[Season, DataUnit], x_limit: int, block: bool):
    set_matplotlib_defaults()
    display = DisplayGrid(len(season_data), 2)
    fig, axs = plt.subplots(nrows=display.nrows,
                            ncols=display.ncols,
                            constrained_layout=True)

    for i, (season, datum) in enumerate(season_data.items()):
        x_values = []
        y_values = []
        for x in range(0, x_limit + 1):
            x_values.append(x)
            if x in datum.counter:
                y_values.append(datum.counter[x])
            else:
                y_values.append(0)

        cell_x, cell_y = display.index(i)
        ax = axs[cell_x, cell_y]
        cmap = plt.get_cmap('Wistia')
        min_y = min(y_values)
        max_y = max(y_values)
        scaled = [(y - min_y) / (max_y - min_y) for y in y_values]

        ax.bar(x_values, y_values, color=cmap(scaled))
        ax.xaxis.set_major_locator(MaxNLocator(integer=True))
        ax.yaxis.set_major_locator(MaxNLocator(integer=True))
        ax.set_yticks([])
        ax.set_ylabel(season.year)
        ax.set_xticks(x_values)
        ax.set_frame_on(False)

        for k, v in datum.counter.items():
            ax.text(k, v, str(v), ha='center', fontsize=8, fontweight='bold')

    for i in range(len(season_data), display.nrows * display.ncols):
        cell_x, cell_y = display.index(i)
        ax = axs[cell_x][cell_y]
        fig.delaxes(ax)

    fig.suptitle(title, fontweight='bold')
    plt.show(block=block)


def construct_title(league: League, func: Callable, negate: bool, venue: Venue, halves: List[Half], team: Team):
    event = Event.name(func, negate)
    if venue == Venue.any:
        prologue = '{} ({} or {})'.format(event, Venue.home.name, Venue.away.name)
    else:
        prologue = '{} ({} only)'.format(event, venue.name)

    prologue += ' ({} results)'.format(', '.join([half.name for half in Half if half in halves]))
    prologue = '{} in {} {}'.format(prologue, prettify(league.country), league.name)

    if team:
        return '{} for {}'.format(prologue, team.name)
    else:
        return prologue


def main(args: Namespace):
    load_teams(args.database)
    league = league_register[get_unique_league(args)]
    load_league(args.database, league)

    seasons = Season.seasons(league)
    if not seasons:
        messages.error_message("No season data found")

    if args.history:
        seasons = seasons[-args.history:]

    func = Event.get(get_unique_event(args))

    if args.team:
        (row,) = extract_picked_team(args.database, args.team, league)
        selected_team = Team.inventory[row[0]]
    else:
        selected_team = None

    season_data = {}
    x_limit = 0
    for season in seasons:
        data = DataUnit(Counter(), season)
        if selected_team:
            count_events(season,
                         selected_team,
                         args.venue,
                         args.half,
                         func,
                         args.negate,
                         data)
        else:
            for team in season.teams():
                count_events(season,
                             team,
                             args.venue,
                             args.half,
                             func,
                             args.negate,
                             data)

        if data.counter:
            x_limit = max([x_limit] + list(data.counter.keys()))
            season_data[season] = data

    title = construct_title(league, func, args.negate, args.venue, args.half, selected_team)
    show(title, season_data, x_limit, args.block)


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)

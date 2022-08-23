from argparse import ArgumentParser, Namespace
from cli.cli import (add_database_option,
                     add_events_option,
                     add_half_option,
                     add_history_option,
                     add_league_option,
                     add_team_option,
                     add_logging_options,
                     add_venue_option,
                     add_chunk_option,
                     add_block_option,
                     set_logging_options,
                     get_unique_league,
                     get_unique_event)
from collections import Counter, OrderedDict
from lib import messages
from lib.helpful import DisplayGrid, set_matplotlib_defaults
from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
from model.fixtures import Half, Venue, Event
from model.leagues import League, league_register, prettify
from model.seasons import Season
from model.sequences import count_events, DataUnit
from model.tables import LeagueTable
from model.teams import Team
from sql.sql import extract_picked_team, load_league, load_teams
from typing import Callable, List

import numpy as np


def parse_command_line():
    parser = ArgumentParser(description='Show sequence data in bar charts')
    add_database_option(parser)
    add_half_option(parser)
    add_events_option(parser)
    add_history_option(parser)
    add_league_option(parser)
    add_team_option(parser)
    add_venue_option(parser)
    add_logging_options(parser)
    add_chunk_option(parser)
    add_block_option(parser)
    return parser.parse_args()


def compute_aggregated_data(seasons: List[Season],
                            selected_team: Team,
                            func: Callable,
                            negate: bool,
                            venue: Venue,
                            half: Half):
    if seasons[-1].current:
        this_season = seasons.pop()
    else:
        this_season = None

    charts = []
    aggregated_history = DataUnit(Counter(), seasons)
    charts.append(aggregated_history)
    for season in seasons:
        for team in season.teams():
            count_events(season, team, venue, half, func, negate, aggregated_history)

    if selected_team is not None:
        team_history = DataUnit(Counter(), seasons, team=selected_team)
        charts.append(team_history)
        for season in seasons:
            count_events(season, team_history.team, venue, half, func, negate, team_history)

    if this_season is not None:
        aggregated_now = DataUnit(Counter(), [this_season])
        charts.append(aggregated_now)
        for team in this_season.teams():
            count_events(this_season, team, venue, half, func, negate, aggregated_now)

        if selected_team is not None:
            team_now = DataUnit(Counter(), [this_season], team=selected_team, highlight=True)
            charts.append(team_now)
            count_events(this_season, selected_team, venue, half, func, negate, team_now)

    return charts


def compute_chunked_data(seasons: List[Season],
                         func: Callable,
                         negate: bool,
                         venue: Venue,
                         half: Half,
                         chunk_size: int):
    chunk_to_chart = OrderedDict()
    for season in seasons:
        table = LeagueTable(season, half)
        table_map = table.group(chunk_size)

        for chunk_id in range(table_map.number_of_chunks()):
            if chunk_id not in chunk_to_chart:
                chart = DataUnit(Counter(), seasons, positions=table_map.get_rows(chunk_id))
                chunk_to_chart[chunk_id] = chart

            chart = chunk_to_chart[chunk_id]
            for row_id in table_map.get_rows(chunk_id):
                team = table[row_id].TEAM
                count_events(season, team, venue, half, func, negate, chart)

    return list(chunk_to_chart.values())


def construct_title(league: League, func: Callable, negate: bool, venue: Venue, half: Half):
    event = Event.name(func, negate)
    if venue == Venue.any:
        prologue = '{} ({} or {})'.format(event, Venue.home.name, Venue.away.name)
    else:
        prologue = '{} ({} only)'.format(event, venue.name)

    if half != Half.both:
        prologue += ' ({} half)'.format(half.name)

    return 'Sequences: {} in {} {}'.format(prologue, prettify(league.country), league.name)


def find_limits(data: List[DataUnit]):
    x_limit = 0
    y_limit = 0
    for datum in data:
        if datum.counter:
            max_key = max(datum.counter.keys())
            x_limit = max(x_limit, max_key)
            max_value = max(datum.counter.values())
            y_limit = max(y_limit, max_value)
    return x_limit, y_limit


def plot(ax, datum: DataUnit, x_limit: int):
    x_values, y_values = datum.values(x_limit)
    ax.xaxis.set_major_locator(MaxNLocator(integer=True))
    ax.yaxis.set_major_locator(MaxNLocator(integer=True))
    ax.set_xticks(range(0, x_limit + 1))
    ax.set_title(datum.title(), fontstyle='italic')
    ax.set_frame_on(False)

    if sum(y_values):
        cmap = plt.get_cmap('RdPu')
        min_y = np.min(y_values)
        max_y = np.max(y_values)
        scaled = [(y - min_y) / (max_y - min_y) for y in y_values]
        ax.set_yticks([])
        offset = 0.5
        ax.set_xlim([-1 * offset, x_limit + offset])
        bar = ax.bar(x_values, y_values, color=cmap(scaled), zorder=1)

        if datum.team is not None and datum.last is not None and datum.highlight:
            bar[datum.last].set_color('gold')

    for k, v in datum.counter.items():
        ax.text(k, v, str(v), ha='center', zorder=2)


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

    if args.team:
        (row,) = extract_picked_team(args.database, args.team, league)
        selected_team = Team.inventory[row[0]]
    else:
        selected_team = None

    func = Event.get(get_unique_event(args))
    if args.chunks:
        data = compute_chunked_data(seasons,
                                    func,
                                    args.negate,
                                    args.venue,
                                    args.half,
                                    args.chunks)

        nrows = len(data)
        if selected_team is not None:
            ncols = 3
        else:
            ncols = 1

        fig, axes = plt.subplots(nrows=len(data),
                                 ncols=ncols,
                                 squeeze=False,
                                 constrained_layout=True)

        x_limit, _ = find_limits(data)
        for i, datum in enumerate(data):
            ax = axes[i, 0]
            plot(ax, datum, x_limit)
            if datum.positions:
                ax.set_ylabel(datum.y_label())

        if selected_team is not None:
            golden_season = seasons[-1]
            golden_table = LeagueTable(golden_season, args.half)
            golden_map = golden_table.group(args.chunks)

            chunk_to_seasons = OrderedDict()
            for chunk_id in range(golden_map.number_of_chunks()):
                if chunk_id not in chunk_to_seasons:
                    chunk_to_seasons[chunk_id] = []

            for season in seasons:
                if season != golden_season:
                    table = LeagueTable(season, args.half)
                    table_map = table.group(args.chunks)
                    if selected_team in season.teams():
                        position = table.team_position(selected_team)
                        chunk_id = table_map.get_chunk(position)
                        chunk_to_seasons[chunk_id].append(season)

            chunk_to_datum = OrderedDict()
            for chunk_id, chunk_seasons in chunk_to_seasons.items():
                if chunk_seasons:
                    datum = DataUnit(Counter(),
                                     chunk_seasons,
                                     team=selected_team,
                                     positions=golden_map.get_rows(chunk_id))
                    chunk_to_datum[chunk_id] = datum

            for season in seasons:
                if season == golden_season:
                    position = golden_table.team_position(selected_team)
                    datum = DataUnit(Counter(),
                                     [golden_season],
                                     team=selected_team,
                                     positions=[position],
                                     highlight=True)
                    golden_datum = datum
                else:
                    if selected_team in season.teams():
                        table = LeagueTable(season, args.half)
                        table_map = table.group(args.chunks)
                        position = table.team_position(selected_team)
                        chunk_id = table_map.get_chunk(position)
                        datum = chunk_to_datum[chunk_id]

                count_events(season,
                             selected_team,
                             args.venue,
                             args.half,
                             func,
                             args.negate,
                             datum)

            position = golden_table.team_position(selected_team)
            chunk_id = golden_map.get_chunk(position)
            ax = axes[chunk_id, 1]
            plot(ax, golden_datum, x_limit)
            used = {(chunk_id, 1)}

            for chunk_id, datum in chunk_to_datum.items():
                ax = axes[chunk_id, 2]
                plot(ax, datum, x_limit)
                used.add((chunk_id, 2))

            for i in range(0, nrows):
                for j in range(1, 3):
                    if (i, j) not in used:
                        fig.delaxes(axes[i, j])
    else:
        data = compute_aggregated_data(seasons,
                                       selected_team,
                                       func,
                                       args.negate,
                                       args.venue,
                                       args.half)
        x_limit, _ = find_limits(data)
        display = DisplayGrid(len(data), 2)
        fig, axes = plt.subplots(nrows=display.nrows, ncols=display.ncols, squeeze=False, constrained_layout=True)
        for i, datum in enumerate(data):
            cell_x, cell_y = display.index(i)
            ax = axes[cell_x, cell_y]
            plot(ax, datum, x_limit)

    title = construct_title(league, func, args.negate, args.venue, args.half)
    fig.suptitle(title, fontweight='bold', wrap=True)
    plt.show(block=args.block)


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)

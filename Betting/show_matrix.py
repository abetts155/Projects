from argparse import ArgumentParser, Namespace
from cli.cli import (add_database_option,
                     add_history_option,
                     add_league_option,
                     add_half_option,
                     add_logging_options,
                     add_venue_option,
                     set_logging_options,
                     add_events_option,
                     get_unique_event,
                     add_chunk_option,
                     add_block_option,
                     get_unique_league)
from lib.helpful import split_into_contiguous_groups, to_string, DisplayGrid
from lib.messages import verbose_message
from matplotlib import pyplot as plt
from model.fixtures import Half, Event
from model.competitions import league_register
from model.seasons import Season
from model.tables import LeagueTable, TableMap
from seaborn import heatmap
from sql.sql import load_league, load_teams
from typing import Callable

import numpy as np
import pandas as pd


def parse_command_line():
    parser = ArgumentParser(description='Show matrix of data')
    add_database_option(parser)
    add_history_option(parser)
    add_league_option(parser)
    add_half_option(parser)
    add_venue_option(parser)
    add_logging_options(parser)
    add_chunk_option(parser)
    add_events_option(parser, True, 1)
    add_block_option(parser)

    parser.add_argument('-S',
                        '--symmetry',
                        action='store_true',
                        help='assume the event applies to both teams equally',
                        default=False)

    return parser.parse_args()


def fill_matrix(func: Callable,
                symmetry: bool,
                matrix: np.ndarray,
                table_map: TableMap,
                table: LeagueTable,
                half: Half):
    verbose_message('Filling matrix for season {}'.format(table.season))
    for fixture in table.season.fixtures():
        home_team_position = table.team_position(fixture.home_team)
        home_team_chunk_id = table_map.get_chunk(home_team_position)
        away_team_position = table.team_position(fixture.away_team)
        away_team_chunk_id = table_map.get_chunk(away_team_position)

        if half == Half.both:
            result = fixture.full_time()
        elif half == Half.first:
            result = fixture.first_half()
        elif half == Half.second:
            result = fixture.second_half()
        else:
            assert False

        if result:
            if func(result):
                matrix[home_team_chunk_id][away_team_chunk_id] += 1
                if symmetry:
                    matrix[away_team_chunk_id][home_team_chunk_id] += 1


def create_chunk_labels(table_map: TableMap):
    row_names = []
    for chunk_id in range(table_map.number_of_chunks()):
        chunk = table_map.get_rows(chunk_id)
        if chunk[0] == chunk[-1]:
            row_names.append('{}'.format(chunk[0] + 1))
        else:
            row_names.append('{}-{}'.format(chunk[0] + 1, chunk[-1] + 1))
    return row_names


def populate_axis(table_map: TableMap, datum: pd.DataFrame, ax, title: str):
    row_names = create_chunk_labels(table_map)
    datum.columns = row_names
    datum.index = row_names
    heatmap(datum, cmap='coolwarm', linewidth=0.5, annot=True, fmt='d', ax=ax)
    ax.set_title(title, pad=20)
    ax.set_ylabel('Positions')
    ax.set_xlabel('Positions')


def main(args: Namespace):
    load_teams(args.database)
    league = league_register[get_unique_league(args)]
    load_league(args.database, league)

    seasons = Season.seasons(league)
    if args.history:
        seasons = seasons[-args.history:]

    event = Event.get(get_unique_event(args))

    head_season = seasons[-1]
    head_table = LeagueTable(head_season, args.half)
    if head_table.played() == 0:
        head_season = seasons[-2]
        head_table = LeagueTable(head_season, args.half)
        nrows = 1
    else:
        nrows = 2

    head_map = head_table.group(args.chunks)
    shape = (head_map.number_of_chunks(), head_map.number_of_chunks())

    historical_matrix = np.zeros(shape=shape, dtype=np.int32)
    historical_seasons = seasons[:-1]
    for season in historical_seasons:
        table = LeagueTable(season, args.half)
        fill_matrix(event, args.symmetry, historical_matrix, head_map, table, args.half)

    display = DisplayGrid(nrows, 1)
    fig, axs = plt.subplots(nrows=display.nrows,
                            ncols=display.ncols,
                            figsize=(15, 12),
                            squeeze=False,
                            constrained_layout=True)

    datum = pd.DataFrame(historical_matrix)
    sublists = split_into_contiguous_groups([season.year for season in historical_seasons])
    title = 'Seasons:{}'.format(to_string(sublists))
    cell_x, cell_y = display.index(0)
    ax = axs[cell_x, cell_y]
    populate_axis(head_map, datum, ax, title)

    if nrows == 2:
        now_matrix = np.zeros(shape=shape, dtype=np.int32)
        fill_matrix(event, args.symmetry, now_matrix, head_map, head_table, args.half)
        datum = pd.DataFrame(now_matrix)
        title = 'Season:{}'.format(head_season.year)
        cell_x, cell_y = display.index(1)
        ax = axs[cell_x, cell_y]
        populate_axis(head_map, datum, ax, title)

    title = '{} {}: {}'.format(league.country, league.name, Event.name(event, args.negate))
    if args.half != Half.both:
        title = '{} ({} half)'.format(title, args.half.name)
    fig.suptitle(title, fontweight='bold', fontsize=14)
    plt.show(block=args.block)


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)

from argparse import ArgumentParser, Namespace
from cli.cli import (add_database_option,
                     add_history_option,
                     add_league_option,
                     add_team_option,
                     add_half_option,
                     add_logging_options,
                     add_venue_option,
                     set_logging_options,
                     get_unique_league)
from collections import OrderedDict
from enum import auto, Enum
from lib.helpful import split_into_contiguous_groups, to_string
from lib.messages import error_message, warning_message
from math import ceil
from matplotlib import pyplot as plt
from model.fixtures import Half, Result
from model.leagues import league_register, League
from model.seasons import Season
from model.tables import LeagueTable, TableMap
from seaborn import heatmap
from sql.sql import load_database
from typing import List

import numpy as np
import pandas as pd


class Analysis(Enum):
    RESULT = auto()
    SCORED = auto()
    GOALS = auto()

    @staticmethod
    def from_string(string: str):
        try:
            return Analysis[string.upper()]
        except KeyError:
            error_message("Analysis '{}' is not valid".format(string))


class Predicates:
    __slots__ = ['functions', 'labels', 'mutual_exclusion']

    def __init__(self, functions, labels, mutual_exclusion: bool):
        assert len(functions) == len(labels)
        self.functions = functions
        self.labels = labels
        self.mutual_exclusion = mutual_exclusion


predicate_table = {
    Analysis.RESULT: Predicates([Result.win, Result.draw, Result.defeat],
                                ['W', 'D', 'L'],
                                False),

    Analysis.SCORED: Predicates([Result.bts, Result.goals_for, Result.goals_against, Result.blank],
                                ['BTS', 'GF', 'GA', 'Z'],
                                True),

    Analysis.GOALS: Predicates([Result.more_than_5, Result.exactly_5, Result.exactly_4, Result.exactly_3,
                                Result.exactly_2, Result.exactly_1, Result.exactly_0],
                               ['>5', '5', '4', '3', '2', '1', '0'],
                               False)
}


def parse_command_line():
    parser = ArgumentParser(description='Show heatmap of data')
    add_database_option(parser)
    add_history_option(parser)
    add_league_option(parser)
    add_team_option(parser)
    add_half_option(parser)
    add_venue_option(parser)
    add_logging_options(parser)

    parser.add_argument('-C',
                        '--chunks',
                        help='divide the table into chunks of this size',
                        metavar='<INT>',
                        type=int,
                        default=1)

    parser.add_argument('-A',
                        '--analysis',
                        choices=Analysis,
                        type=Analysis.from_string,
                        metavar='{{{}}}'.format(','.join(analysis.name for analysis in Analysis)),
                        help='choose type of analysis',
                        required=True)

    return parser.parse_args()


def create_numpy_matrix(rows: int, columns: int):
    array = np.zeros(shape=(rows, columns), dtype=np.int32)
    return array


def fill_matrix(matrix: np.ndarray, table_map: TableMap, tables: List[LeagueTable], predicates: Predicates, half: Half):
    for table in tables:
        team_fixtures = table.season.fixtures_per_team()

        for position, row in enumerate(table):
            fixtures = team_fixtures[row.TEAM]
            results = []
            for fixture in fixtures:
                if half:
                    if half == Half.first:
                        result = fixture.first_half()
                    else:
                        result = fixture.second_half()
                else:
                    result = fixture.full_time()

                if result:
                    result = fixture.canonicalise_result(row.TEAM, result)
                    results.append(result)
                else:
                    warning_message('Ignoring {}'.format(fixture))

            totals = {func: 0 for func in predicates.functions}
            for result in results:
                satisfied = False
                for func in predicates.functions:
                    if func(result):
                        totals[func] += 1
                        satisfied = True
                    if predicates.mutual_exclusion and satisfied:
                        break

            for j, func in enumerate(predicates.functions):
                i = table_map.get_chunk(position)
                matrix[i, j] += totals[func]


def create_chunk_labels(table_map: TableMap):
    row_names = []
    for chunk_id in range(table_map.number_of_chunks()):
        chunk = table_map.get_rows(chunk_id)
        if chunk[0] == chunk[-1]:
            row_names.append('{}'.format(chunk[0] + 1))
        else:
            row_names.append('{}-{}'.format(chunk[0] + 1, chunk[-1] + 1))
    return row_names


def filter_tables(seasons: List[Season], head_table: LeagueTable) -> List[LeagueTable]:
    tables = []
    for season in seasons:
        table = LeagueTable(season)
        if len(head_table) == len(table):
            tables.append(table)
        else:
            warning_message('Ignoring season {} because it had {} teams'.format(season.year, len(table)))
    return tables


def create_heatmap(tables: List[LeagueTable], chunk_size: int, analysis: Analysis, half: Half) -> pd.DataFrame:
    head_table = tables[0]
    table_map = head_table.group(chunk_size)
    predicates = predicate_table[analysis]
    matrix = create_numpy_matrix(table_map.number_of_chunks(), len(predicates.labels))
    fill_matrix(matrix, table_map, tables, predicates, half)
    data = pd.DataFrame(matrix)
    data.columns = predicates.labels
    data.index = create_chunk_labels(table_map)
    return data


class DataUnit:
    __slots__ = ['tables', 'heatmap']

    def __init__(self):
        self.tables = []

    def size(self):
        return len(self.tables[0])


def show(league: League, data: List[DataUnit], analysis: Analysis, half: Half):
    if len(data) <= 2:
        nrows = 1
        ncols = len(data)
    else:
        nrows = 2
        ncols = ceil(len(data) / nrows)

    fig, axes = plt.subplots(nrows=nrows, ncols=ncols, figsize=(10, 10))

    row_id = 0
    col_id = 0
    for datum in data:
        if nrows == 1:
            if ncols == 1:
                ax = axes
            else:
                ax = axes[col_id]
        else:
            ax = axes[row_id, col_id]

        heatmap(datum.heatmap, cmap='coolwarm', linewidth=0.5, annot=True, fmt='d', ax=ax)
        ax.set_ylabel('Positions')
        ax.set_xlabel(analysis.name.capitalize())
        sublists = split_into_contiguous_groups([table.season.year for table in datum.tables])
        ax.set_title('Seasons: {}'.format(to_string(sublists)))

        if col_id == ncols - 1:
            row_id += 1
            col_id = 0
        else:
            col_id += 1

    if 0 < col_id:
        for i in range(col_id, ncols):
            fig.delaxes(axes[row_id][col_id])

    title = '{} {}'.format(league.country, league.name)
    if half is not None:
        title = '{} ({} half)'.format(title, half.name)
    fig.suptitle(title, fontweight='bold', fontsize=14)

    plt.tight_layout()
    plt.show()


def main(arguments: Namespace):
    league = league_register[get_unique_league(arguments)]
    load_database(arguments.database, league)

    seasons = Season.seasons()
    if arguments.history:
        seasons = seasons[-arguments.history:]

    table_split = OrderedDict()
    for season in seasons:
        table = LeagueTable(season)
        table_size = len(table)
        table_split.setdefault(table_size, DataUnit())
        datum = table_split[table_size]
        datum.tables.append(table)

    for table_size, datum in table_split.items():
        datum.heatmap = create_heatmap(datum.tables, arguments.chunks, arguments.analysis, arguments.half)

    show(league, list(table_split.values()), arguments.analysis, arguments.half)


if __name__ == '__main__':
    arguments = parse_command_line()
    set_logging_options(arguments)
    main(arguments)

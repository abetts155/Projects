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
from lib.messages import error_message, warning_message
from matplotlib import pyplot as plt
from model.fixtures import Result
from model.leagues import league_register
from model.seasons import Season
from model.tables import LeagueTable
from seaborn import heatmap
from sql.sql import load_database
from typing import List

import numpy as np
import pandas as pd


class Analysis(Enum):
    RESULTS = auto()
    GOALS = auto()

    @staticmethod
    def from_string(string: str):
        try:
            return Analysis[string.upper()]
        except KeyError:
            error_message("Analysis '{}' is not valid".format(string))


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


class ResultsMatrix:
    def __init__(self):
        self._predicates = [Result.win, Result.draw, Result.defeat]

    def number_of_columns(self) -> int:
        return len(self._predicates)

    def column_ticks(self) -> List[str]:
        return ['{}'.format(p.__name__.capitalize()) for p in self._predicates]

    def fill_matrix(self, matrix: np.ndarray, row_to_chunk, tables: List[LeagueTable]):
        for table in tables:
            team_fixtures = table.season.fixtures_per_team()

            for position, row in enumerate(table):
                fixtures = team_fixtures[row.TEAM]
                results = []
                for fixture in fixtures:
                    results.append(fixture.canonicalise_result(row.TEAM))

                for j, predicate in enumerate(self._predicates):
                    total = [result for result in results if predicate(result)]
                    matrix[row_to_chunk[position], j] += len(total)


class GoalsMatrix:
    def __init__(self):
        self._predicates = [Result.blank, Result.goals_for, Result.goals_against, Result.bts]

    def number_of_columns(self) -> int:
        return len(self._predicates)

    def column_ticks(self) -> List[str]:
        ticks = []
        for p in self._predicates:
            lexemes = p.__name__.split('_')
            if len(lexemes) == 2:
                first, second = lexemes
                ticks.append('{}{}'.format(first[0].capitalize(), second[0].capitalize()))
            else:
                ticks.append(lexemes[0].upper())
        return ticks

    def fill_matrix(self, matrix: np.ndarray, row_to_chunk, tables: List[LeagueTable]):
        for table in tables:
            team_fixtures = table.season.fixtures_per_team()

            for position, row in enumerate(table):
                fixtures = team_fixtures[row.TEAM]
                results = []
                for fixture in fixtures:
                    results.append(fixture.canonicalise_result(row.TEAM))

                for j, predicate in enumerate(self._predicates):
                    total = [result for result in results if predicate(result)]
                    matrix[row_to_chunk[position], j] += len(total)


def create_chunk_labels(chunk_to_rows):
    row_names = []
    for chunk in chunk_to_rows.values():
        if chunk[0] == chunk[-1]:
            row_names.append('{}'.format(chunk[0] + 1))
        else:
            row_names.append('{}-{}'.format(chunk[0] + 1, chunk[-1] + 1))
    return row_names


def check_chunk_size(chunk_size: int, table_size: int):
    if chunk_size == 0 or table_size % chunk_size != 0:
        error_message('Chunk size {} is not a valid divisor for tables of size {}.'.format(chunk_size, table_size))


def create_indices(chunk_size: int, table_size: int):
    chunks = table_size//chunk_size
    row_to_chunk = {}
    chunk_to_rows = OrderedDict()
    for chunk in range(chunks):
        chunk_to_rows[chunk] = []
        for index in range(chunk_size):
            row = index + (chunk * chunk_size)
            row_to_chunk[row] = chunk
            chunk_to_rows[chunk].append(row)
    return row_to_chunk, chunk_to_rows


def filter_tables(seasons: List[Season], table_size: int) -> List[LeagueTable]:
    tables = []
    for season in seasons:
        table = LeagueTable(season)
        if table_size == len(table):
            tables.append(table)
        else:
            warning_message('Ignoring season {} because it had {} teams'.format(season.year, len(table)))
    return tables


def create_heatmap(tables: List[LeagueTable], row_to_chunk, chunk_to_rows, analysis: Analysis) -> pd.DataFrame:
    if analysis == Analysis.RESULTS:
        matrix = ResultsMatrix()
    elif analysis == Analysis.GOALS:
        matrix = GoalsMatrix()
    else:
        assert False

    raw_matrix = create_numpy_matrix(len(chunk_to_rows), matrix.number_of_columns())
    matrix.fill_matrix(raw_matrix, row_to_chunk, tables)
    data = pd.DataFrame(raw_matrix)
    data.columns = matrix.column_ticks()
    data.index = create_chunk_labels(chunk_to_rows)
    return data


def show(data: pd.DataFrame, title: str):
    fig, _ = plt.subplots(figsize=(8, 10))
    heatmap(data, cmap='coolwarm', linewidth=0.5, annot=True, fmt='d')
    plt.yticks(rotation=0)
    plt.ylabel('Positions')
    fig.suptitle(title, fontweight='bold', fontsize=14)
    plt.show()


def main(arguments: Namespace):
    league = league_register[get_unique_league(arguments)]
    load_database(arguments.database, league)

    seasons = Season.seasons()
    if arguments.history:
        seasons = seasons[-arguments.history:]

    table = LeagueTable(seasons[-1])
    table_size = len(table)
    check_chunk_size(arguments.chunks, table_size)
    row_to_chunk, chunk_to_rows = create_indices(arguments.chunks, table_size)
    tables = filter_tables(seasons, table_size)

    data = create_heatmap(tables, row_to_chunk, chunk_to_rows, arguments.analysis)
    if len(tables) == 1:
        (table,) = tables
        title = 'Season: {}'.format(table.season.year)
    else:
        title = 'Seasons: {}-{}'.format(tables[0].season.year, tables[-1].season.year)
    show(data, title)


if __name__ == '__main__':
    arguments = parse_command_line()
    set_logging_options(arguments)
    main(arguments)

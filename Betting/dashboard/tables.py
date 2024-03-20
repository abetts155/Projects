from abc import ABC
from dataclasses import dataclass
from datetime import date
from typing import Dict, List

from model.fixtures import Half, Venue


@dataclass
class TableColumn(ABC):
    display: str
    tooltip: str

    def __hash__(self):
        return hash(self.display)


class IntegerTableColumn(TableColumn):
    default: int = 0


class StringTableColumn(TableColumn):
    default: str = ''


COL_TEAM = StringTableColumn('Team', '')
COL_PLAYED = IntegerTableColumn('P', 'Played')
COL_WON = IntegerTableColumn('W', 'Won')
COL_DRAWN = IntegerTableColumn('D', 'Drawn')
COL_LOST = IntegerTableColumn('L', 'Lost')
COL_GOALS_FOR = IntegerTableColumn('GF', 'Goals For')
COL_GOALS_AGAINST = IntegerTableColumn('GA', 'Goals Against')
COL_GOAL_DIFFERENCE = IntegerTableColumn('GD', 'Goal Difference')
COL_GOAL_RATE = IntegerTableColumn('GR', 'Goal Rate (Goals per Game or Half)')
COL_SCORED = IntegerTableColumn('S', 'Scored (%Games or %Halves)')
COL_CONCEDED = IntegerTableColumn('C', 'Conceded (%Games or %Halves)')
COL_BTS = IntegerTableColumn('BTS', 'Both Teams Scored (%Games or %Halves)')
COL_POINTS = IntegerTableColumn('PTS', 'Points')

table_columns = [
    COL_TEAM,
    COL_PLAYED,
    COL_WON,
    COL_DRAWN,
    COL_LOST,
    COL_GOALS_FOR,
    COL_GOALS_AGAINST,
    COL_GOAL_DIFFERENCE,
    COL_GOAL_RATE,
    COL_SCORED,
    COL_CONCEDED,
    COL_BTS,
    COL_POINTS
]


class TableRow:
    def __init__(self):
        for column in table_columns:
            setattr(self, column.display, column.default)

    def set(self, column: TableColumn, value):
        setattr(self, column.display, value)

    def increase(self, column: IntegerTableColumn, value: int):
        setattr(self, column.display, getattr(self, column.display) + value)

    def get(self, column: TableColumn):
        return getattr(self, column.display)

    def __str__(self):
        return ' '.join(f'{column.display} {self.get(column)}' for column in table_columns)


def fill_table_with_collected_values(league_table: List[TableRow], team_fixtures: Dict, half: Half):
    for team, fixtures in team_fixtures.items():
        row = TableRow()
        row.set(COL_TEAM, team)
        league_table.append(row)

        for fixture_dict in fixtures:
            if fixture_dict[half.name] != '?-?':
                left, right = fixture_dict[half.name].split('-')
                if fixture_dict[Venue.away.name] == team:
                    left, right = right, left

                left, right = int(left), int(right)
                if left > right:
                    row.increase(COL_WON, 1)
                elif right > left:
                    row.increase(COL_LOST, 1)
                else:
                    row.increase(COL_DRAWN, 1)

                row.increase(COL_GOALS_FOR, left)
                row.increase(COL_GOALS_AGAINST, right)

                if left:
                    row.increase(COL_SCORED, 1)

                if right:
                    row.increase(COL_CONCEDED, 1)

                if left and right:
                    row.increase(COL_BTS, 1)


def fill_table_with_computed_values(league_table: List[TableRow]):
    for row in league_table:
        for column in table_columns:
            if column.display == COL_PLAYED.display:
                setattr(row, column.display, row.W + row.D + row.L)
            elif column.display == COL_GOAL_DIFFERENCE.display:
                setattr(row, column.display, row.GF - row.GA)
            elif column.display == COL_GOAL_RATE.display:
                setattr(row, column.display, round((row.GF + row.GA) / row.P, 1))
            elif column.display in [COL_SCORED.display, COL_CONCEDED.display, COL_BTS.display]:
                setattr(row, column.display, getattr(row, column.display) / row.P)
            elif column.display == COL_POINTS.display:
                setattr(row, column.display, 3 * row.W + row.D)


def create_cell_formatter(league_table: List[TableRow]):
    blacklist = [COL_TEAM, COL_PLAYED]
    max_values = {column: 0 for column in table_columns if column not in blacklist}
    min_values = {column: 2 ** 10 for column in table_columns if column not in blacklist}
    for row in league_table:
        for column in table_columns:
            if column not in blacklist:
                max_values[column] = max(max_values[column], row.get(column))
                min_values[column] = min(min_values[column], row.get(column))

    formatter = []
    for column in table_columns:
        if column not in blacklist:
            min_formatter = {
                'if': {'column_id': column.display, 'filter_query': f'{{{column.display}}} = {min_values[column]}'},
                'backgroundColor': '#CCE5FF',
                'color': '#004080'
            }

            max_formatter = {
                'if': {'column_id': column.display, 'filter_query': f'{{{column.display}}} = {max_values[column]}'},
                'backgroundColor': '#FFCDD2',
                'color': '#800000'
            }

            formatter.append(min_formatter)
            formatter.append(max_formatter)

    return formatter


def create_league_table(fixtures: List, half: Half):
    league_table: List[TableRow] = []
    fill_table_with_collected_values(league_table, fixtures, half)
    fill_table_with_computed_values(league_table)
    league_table.sort(key=lambda row: (row.PTS, row.GD, row.GF), reverse=True)
    table_data = [{column.display: row.get(column) for column in table_columns} for row in league_table]
    return table_data, create_cell_formatter(league_table)

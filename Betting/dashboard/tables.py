from abc import ABC
from dataclasses import dataclass, field
from datetime import datetime
from typing import Dict, List

from dashboard.colours import BAD_NEWS_COLOR, GOOD_NEWS_COLOR, HOT_COLOR, COOL_COLOR
from dashboard.fixtures import Fixture, Period, Venue, reverse_scoreline, UNKNOWN_SCORELINE


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
COL_DATE = StringTableColumn('Date', 'Date')
COL_VENUE = StringTableColumn('Venue', 'Venue')
COL_HOME = StringTableColumn('Home', 'Home Team')
COL_AWAY = StringTableColumn('Away', 'Away Team')
COL_FIRST_HALF = StringTableColumn(Period.FIRST.name, Period.FIRST.value)
COL_SECOND_HALF = StringTableColumn(Period.SECOND.name, Period.SECOND.value)
COL_FULL_TIME = StringTableColumn(Period.FULL.name, Period.FULL.value)

league_table_columns = [
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


class LeagueTableRow:
    def __init__(self):
        for column in league_table_columns:
            setattr(self, column.display, column.default)

    def set(self, column: TableColumn, value):
        setattr(self, column.display, value)

    def increase(self, column: IntegerTableColumn, value: int):
        setattr(self, column.display, getattr(self, column.display) + value)

    def get(self, column: TableColumn):
        return getattr(self, column.display)

    def __str__(self):
        return ' '.join(f'{column.display} {self.get(column)}' for column in league_table_columns)


@dataclass(slots=True)
class LeagueTable:
    rows: List = field(default_factory=list)

    def fill_with_collected_information(self, team_results: Dict[str, List[Fixture]], period: Period):
        for team_name, results in team_results.items():
            row = LeagueTableRow()
            row.set(COL_TEAM, team_name)
            self.rows.append(row)

            for result in results:
                if period == Period.FIRST:
                    scoreline = result.first_half
                elif period == Period.SECOND:
                    scoreline = result.second_half
                else:
                    scoreline = result.full_time

                if scoreline != UNKNOWN_SCORELINE:
                    if result.away_name == team_name:
                        scoreline = reverse_scoreline(scoreline)

                    if scoreline.home_goals > scoreline.away_goals:
                        row.increase(COL_WON, 1)
                    elif scoreline.away_goals > scoreline.home_goals:
                        row.increase(COL_LOST, 1)
                    else:
                        row.increase(COL_DRAWN, 1)

                    row.increase(COL_GOALS_FOR, scoreline.home_goals)
                    row.increase(COL_GOALS_AGAINST, scoreline.away_goals)

                    if scoreline.home_goals:
                        row.increase(COL_SCORED, 1)

                    if scoreline.away_goals:
                        row.increase(COL_CONCEDED, 1)

                    if scoreline.home_goals and scoreline.away_goals:
                        row.increase(COL_BTS, 1)

    def fill_with_computed_values(self):
        for row in self.rows:
            for column in league_table_columns:
                if column.display == COL_PLAYED.display:
                    setattr(row, column.display, row.W + row.D + row.L)
                elif column.display == COL_GOAL_DIFFERENCE.display:
                    setattr(row, column.display, row.GF - row.GA)
                elif column.display == COL_GOAL_RATE.display:
                    value = 0
                    if row.P:
                        value = round((row.GF + row.GA) / row.P, 1)
                    setattr(row, column.display, value)
                elif column.display in [COL_SCORED.display, COL_CONCEDED.display, COL_BTS.display]:
                    value = 0
                    if row.P:
                        value = getattr(row, column.display) / row.P
                    setattr(row, column.display, value)
                elif column.display == COL_POINTS.display:
                    setattr(row, column.display, 3 * row.W + row.D)

    def to_display_list(self):
        return [{column.display: row.get(column) for column in league_table_columns} for row in self.rows]


def create_league_table(team_results: Dict[str, List[Fixture]], period: Period) -> LeagueTable:
    league_table = LeagueTable()
    league_table.fill_with_collected_information(team_results, period)
    league_table.fill_with_computed_values()
    league_table.rows.sort(key=lambda row: (row.PTS, row.GD, row.GF), reverse=True)
    return league_table


@dataclass(slots=True)
class FilterConstraint:
    column: TableColumn
    value: str


def create_filter_text(constraints: List[FilterConstraint]):
    return ' && '.join(f'{{{c.column.display}}} = "{c.value}"' for c in constraints)


def create_league_table_formatter(league_table: LeagueTable, team_name: str):
    blacklist = [COL_TEAM, COL_PLAYED]
    max_values = {column: 0 for column in league_table_columns if column not in blacklist}
    min_values = {column: 2 ** 10 for column in league_table_columns if column not in blacklist}

    constraints = [FilterConstraint(COL_TEAM, team_name)]
    cell_formatter = {
        'if': {
            'filter_query': create_filter_text(constraints),
            'column_id': COL_TEAM.display,
        },
        'backgroundColor': GOOD_NEWS_COLOR
    }

    table_formatter = [cell_formatter]
    for row in league_table.rows:
        for column in league_table_columns:
            if column not in blacklist:
                max_values[column] = max(max_values[column], row.get(column))
                min_values[column] = min(min_values[column], row.get(column))

    for column in league_table_columns:
        if column not in blacklist:
            constraints = [FilterConstraint(column, min_values[column])]
            cell_formatter = {
                'if': {
                    'filter_query': create_filter_text(constraints),
                    'column_id': column.display
                },
                'backgroundColor': COOL_COLOR
            }
            table_formatter.append(cell_formatter)

            constraints = [FilterConstraint(column, max_values[column])]
            cell_formatter = {
                'if': {
                    'filter_query': create_filter_text(constraints),
                    'column_id': column.display
                },
                'backgroundColor': HOT_COLOR
            }
            table_formatter.append(cell_formatter)

    return table_formatter


def create_date_string(fixture_date: datetime):
    return fixture_date.strftime('%d-%m-%y')


def create_remaining_fixtures_table(fixtures: List[Fixture], team_name: str):
    table_rows = []
    for fixture in fixtures:
        today = datetime.today()
        if (fixture.date.year, fixture.date.month, fixture.date.day) < (today.year, today.month, today.day):
            row = {COL_DATE.display: '??-??-??'}
        else:
            row = {COL_DATE.display: create_date_string(fixture.date)}

        if team_name == fixture.home_name:
            row[COL_VENUE.display] = Venue.HOME.value
            row[COL_TEAM.display] = fixture.away_name
        else:
            row[COL_VENUE.display] = Venue.AWAY.value
            row[COL_TEAM.display] = fixture.home_name

        table_rows.append(row)

    return table_rows


def create_scoreline_formatter(fixture: Fixture, period: Period, team_name: str, constraints: List[FilterConstraint]):
    match period:
        case Period.FULL:
            scoreline = fixture.full_time
            highlighted_column = COL_FULL_TIME
        case Period.FIRST:
            scoreline = fixture.first_half
            highlighted_column = COL_FIRST_HALF
        case Period.SECOND:
            scoreline = fixture.second_half
            highlighted_column = COL_SECOND_HALF

    if team_name == fixture.away_name:
        scoreline = reverse_scoreline(scoreline)

    if scoreline != UNKNOWN_SCORELINE:
        if scoreline.home_goals > scoreline.away_goals:
            return {
                'if': {
                    'filter_query': create_filter_text(constraints),
                    'column_id': highlighted_column.display
                },
                'backgroundColor': GOOD_NEWS_COLOR
            }

        elif scoreline.home_goals < scoreline.away_goals:
            return {
                'if': {
                    'filter_query': create_filter_text(constraints),
                    'column_id': highlighted_column.display
                },
                'backgroundColor': BAD_NEWS_COLOR
            }


def create_finished_fixtures_table(fixtures: List[Fixture], team_name: str):
    table_rows = []
    table_formatter = []
    for fixture in fixtures:
        row = {
            COL_DATE.display: create_date_string(fixture.date),
            COL_HOME.display: fixture.home_name,
            COL_AWAY.display: fixture.away_name,
            COL_FIRST_HALF.display: str(fixture.first_half),
            COL_SECOND_HALF.display: str(fixture.second_half),
            COL_FULL_TIME.display: str(fixture.full_time)
        }
        table_rows.append(row)

        if team_name in [fixture.home_name, fixture.away_name]:
            constraints = [FilterConstraint(COL_DATE, create_date_string(fixture.date))]
            if team_name == fixture.home_name:
                constraints.append(FilterConstraint(COL_HOME, team_name))
            else:
                constraints.append(FilterConstraint(COL_AWAY, team_name))

            for period in Period:
                cell_formatter = create_scoreline_formatter(fixture, period, team_name, constraints)
                if cell_formatter is not None:
                    table_formatter.append(cell_formatter)

    for column in [COL_HOME, COL_AWAY]:
        cell_formatter = {
            'if': {
                'column_id': column.display,
                'filter_query': f'{{{column.display}}} = "{team_name}"'
            },
            'backgroundColor': GOOD_NEWS_COLOR
        }
        table_formatter.append(cell_formatter)

    return table_rows, table_formatter


def create_finished_fixtures_table_for_team(fixtures: List[Fixture], team_name: str):
    table_rows = []
    table_formatter = []
    for fixture in fixtures:
        row = {
            COL_DATE.display: create_date_string(fixture.date),
            COL_FIRST_HALF.display: str(fixture.first_half),
            COL_SECOND_HALF.display: str(fixture.second_half),
            COL_FULL_TIME.display: str(fixture.full_time)
        }

        if fixture.home_name == team_name:
            row = row | {COL_VENUE.display: Venue.HOME.value, COL_TEAM.display: fixture.away_name}
        else:
            row = row | {COL_VENUE.display: Venue.AWAY.value, COL_TEAM.display: fixture.home_name}

        table_rows.append(row)

        constraints = [FilterConstraint(COL_DATE, create_date_string(fixture.date))]
        for period in Period:
            cell_formatter = create_scoreline_formatter(fixture, period, team_name, constraints)
            if cell_formatter is not None:
                table_formatter.append(cell_formatter)

    return table_rows, table_formatter

import abc
import dataclasses
import datetime
import typing

import pandas as pd

import model.fixtures
import dashboard.data
from dashboard import colours
from dashboard.fixtures import Fixture, reverse_scoreline, UNKNOWN_SCORELINE


@dataclasses.dataclass
class TableColumn(abc.ABC):
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
COL_CLEAN_SHEET = IntegerTableColumn('C', 'Clean Sheets (%Games or %Halves)')
COL_BTS = IntegerTableColumn('BTS', 'Both Teams Scored (%Games or %Halves)')
COL_TOTAL_EQ_0 = IntegerTableColumn('T = 0', 'Total Goals = 0 (%Games or %Halves)')
COL_TOTAL_LE_1 = IntegerTableColumn('T ≤ 1', 'Total Goals ≤ 1 (%Games or %Halves)')
COL_POINTS = IntegerTableColumn('PTS', 'Points')
COL_DATE = StringTableColumn('Date', 'Date')
COL_VENUE = StringTableColumn('Venue', 'Venue')
COL_HOME = StringTableColumn('Home', 'Home Team')
COL_AWAY = StringTableColumn('Away', 'Away Team')
COL_FIRST_HALF = StringTableColumn('1st', '1st Half')
COL_SECOND_HALF = StringTableColumn('2nd', '2nd Half')
COL_FULL_TIME = StringTableColumn('FT', 'Full Time')

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
    COL_CLEAN_SHEET,
    COL_BTS,
    COL_TOTAL_EQ_0,
    COL_TOTAL_LE_1,
    COL_POINTS
]

fixtures_table_columns = [
    COL_DATE,
    COL_HOME,
    COL_AWAY
]

results_table_columns = [
    COL_DATE,
    COL_HOME,
    COL_AWAY,
    COL_FIRST_HALF,
    COL_SECOND_HALF,
    COL_FULL_TIME
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


@dataclasses.dataclass(slots=True)
class LeagueTable:
    rows: list = dataclasses.field(default_factory=list)

    def fill_with_collected_information(
            self,
            teams_df: pd.DataFrame,
            team_results: dict[int, list[Fixture]],
            period: model.fixtures.Period
    ):
        for team_id, results in team_results.items():
            team_name = dashboard.data.get_team_name(teams_df, team_id)
            row = LeagueTableRow()
            row.set(COL_TEAM, team_name)
            self.rows.append(row)

            for result in results:
                if period == model.fixtures.Period.FIRST:
                    scoreline = result.first_half
                elif period == model.fixtures.Period.SECOND:
                    scoreline = result.second_half
                else:
                    scoreline = result.full_time

                if scoreline != UNKNOWN_SCORELINE:
                    if result.away_id == team_id:
                        scoreline = reverse_scoreline(scoreline)

                    if scoreline.left > scoreline.right:
                        row.increase(COL_WON, 1)
                    elif scoreline.right > scoreline.left:
                        row.increase(COL_LOST, 1)
                    else:
                        row.increase(COL_DRAWN, 1)

                    row.increase(COL_GOALS_FOR, scoreline.left)
                    row.increase(COL_GOALS_AGAINST, scoreline.right)

                    if scoreline.left:
                        row.increase(COL_SCORED, 1)

                    if scoreline.right == 0:
                        row.increase(COL_CLEAN_SHEET, 1)

                    if scoreline.left and scoreline.right:
                        row.increase(COL_BTS, 1)

                    if scoreline.left + scoreline.right == 0:
                        row.increase(COL_TOTAL_EQ_0, 1)

                    if scoreline.left + scoreline.right <= 1:
                        row.increase(COL_TOTAL_LE_1, 1)

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
                elif column in [COL_SCORED, COL_CLEAN_SHEET, COL_BTS, COL_TOTAL_EQ_0, COL_TOTAL_LE_1]:
                    value = 0
                    if row.P:
                        value = getattr(row, column.display) / row.P
                    setattr(row, column.display, value)
                elif column.display == COL_POINTS.display:
                    setattr(row, column.display, 3 * row.W + row.D)

    def to_display_list(self):
        return [{column.display: row.get(column) for column in league_table_columns} for row in self.rows]

    def __str__(self):
        return '\n'.join(f'{position} {row.Team} {row.PTS}' for position, row in enumerate(self.rows, start=1))


def create_league_table(
        teams_df: pd.DataFrame,
        team_results: dict[int, list[Fixture]],
        period: model.fixtures.Period
) -> LeagueTable:
    league_table = LeagueTable()
    league_table.fill_with_collected_information(teams_df, team_results, period)
    league_table.fill_with_computed_values()
    league_table.rows.sort(key=lambda row: (row.PTS, row.GD, row.GF), reverse=True)
    return league_table


@dataclasses.dataclass(slots=True)
class FilterConstraint:
    column: TableColumn
    value: str


def create_filter_text(constraints: list[FilterConstraint]):
    return ' && '.join(f'{{{c.column.display}}} = "{c.value}"' for c in constraints)


def create_league_table_formatter(
        teams_df: pd.DataFrame,
        league_table: LeagueTable,
        team_id: int,
        fixture: typing.Optional[Fixture]
):
    blacklist = [COL_TEAM, COL_PLAYED]
    max_values = {column: 0 for column in league_table_columns if column not in blacklist}
    min_values = {column: 2 ** 10 for column in league_table_columns if column not in blacklist}

    team_name = dashboard.data.get_team_name(teams_df, team_id)
    constraints = [FilterConstraint(COL_TEAM, team_name)]
    team_cell_formatter = {
        'if': {
            'filter_query': create_filter_text(constraints),
            'column_id': COL_TEAM.display,
        },
        'backgroundColor': colours.GOOD_NEWS_BACKGROUND_COLOR
    }

    table_formatter = [team_cell_formatter]

    if fixture:
        if fixture.home_id == team_id:
            opponent_name = dashboard.data.get_team_name(teams_df, fixture.away_id)
        else:
            opponent_name = dashboard.data.get_team_name(teams_df, fixture.home_id)

        constraints = [FilterConstraint(COL_TEAM, opponent_name)]
        opponent_cell_formatter = {
            'if': {
                'filter_query': create_filter_text(constraints),
                'column_id': COL_TEAM.display,
            },
            'backgroundColor': colours.BAD_NEWS_BACKGROUND_COLOR,
            'color':  colours.BAD_NEWS_TEXT_COLOR
        }

        table_formatter.append(opponent_cell_formatter)

    games_played = 0
    for row in league_table.rows:
        for column in league_table_columns:
            if column == COL_PLAYED:
                games_played += row.get(column)

            if column not in blacklist:
                max_values[column] = max(max_values[column], row.get(column))
                min_values[column] = min(min_values[column], row.get(column))

    if games_played:
        for column in league_table_columns:
            if column not in blacklist:
                constraints = [FilterConstraint(column, min_values[column])]
                cell_formatter = {
                    'if': {
                        'filter_query': create_filter_text(constraints),
                        'column_id': column.display
                    },
                    'backgroundColor': colours.COOL_COLOR
                }
                table_formatter.append(cell_formatter)

                constraints = [FilterConstraint(column, max_values[column])]
                cell_formatter = {
                    'if': {
                        'filter_query': create_filter_text(constraints),
                        'column_id': column.display
                    },
                    'backgroundColor': colours.HOT_COLOR
                }
                table_formatter.append(cell_formatter)

    return table_formatter


def create_date_string(fixture_date: datetime.datetime):
    return fixture_date.strftime('%d-%m-%y')


def create_remaining_fixtures_table(teams_df: pd.DataFrame, fixtures: list[Fixture], team_id: int):
    table_rows = []
    for fixture in fixtures:
        today = datetime.datetime.today()
        if (fixture.date.year, fixture.date.month, fixture.date.day) < (today.year, today.month, today.day):
            row = {COL_DATE.display: '??-??-??'}
        else:
            row = {COL_DATE.display: create_date_string(fixture.date)}

        if team_id == fixture.home_id:
            row[COL_VENUE.display] = model.fixtures.Venue.HOME.value
            row[COL_TEAM.display] = dashboard.data.get_team_name(teams_df, fixture.away_id)
        else:
            row[COL_VENUE.display] = model.fixtures.Venue.AWAY.value
            row[COL_TEAM.display] = dashboard.data.get_team_name(teams_df, fixture.home_id)

        table_rows.append(row)

    return table_rows


def create_scoreline_formatter(
        fixture: Fixture,
        period: model.fixtures.Period,
        team_id: int,
        constraints: list[FilterConstraint]
):
    match period:
        case model.fixtures.Period.FULL:
            scoreline = fixture.full_time
            highlighted_column = COL_FULL_TIME
        case model.fixtures.Period.FIRST:
            scoreline = fixture.first_half
            highlighted_column = COL_FIRST_HALF
        case model.fixtures.Period.SECOND:
            scoreline = fixture.second_half
            highlighted_column = COL_SECOND_HALF

    if team_id == fixture.away_id:
        scoreline = reverse_scoreline(scoreline)

    if scoreline != UNKNOWN_SCORELINE:
        if scoreline.left > scoreline.right:
            return {
                'if': {
                    'filter_query': create_filter_text(constraints),
                    'column_id': highlighted_column.display
                },
                'backgroundColor': colours.GOOD_NEWS_BACKGROUND_COLOR
            }

        elif scoreline.left < scoreline.right:
            return {
                'if': {
                    'filter_query': create_filter_text(constraints),
                    'column_id': highlighted_column.display
                },
                'backgroundColor': colours.BAD_NEWS_BACKGROUND_COLOR,
                'color': colours.BAD_NEWS_TEXT_COLOR
            }


def create_scheduled_fixtures_table(teams_df: pd.DataFrame, fixtures: list[Fixture], team_id: int):
    table_rows = []
    table_formatter = []
    unknown_scheduling_rows = []
    for fixture in fixtures:
        today = datetime.datetime.today()
        if (fixture.date.year, fixture.date.month, fixture.date.day) < (today.year, today.month, today.day):
            row = {COL_DATE.display: '??-??-??'}
            unknown_scheduling_rows.append(row)
        else:
            row = {COL_DATE.display: create_date_string(fixture.date)}
            table_rows.append(row)

        row[COL_HOME.display] = dashboard.data.get_team_name(teams_df, fixture.home_id)
        row[COL_AWAY.display] = dashboard.data.get_team_name(teams_df, fixture.away_id)

    table_rows.extend(unknown_scheduling_rows)

    team_name = dashboard.data.get_team_name(teams_df, team_id)
    for column in [COL_HOME, COL_AWAY]:
        cell_formatter = {
            'if': {
                'column_id': column.display,
                'filter_query': f'{{{column.display}}} = "{team_name}"'
            },
            'backgroundColor': colours.GOOD_NEWS_BACKGROUND_COLOR
        }
        table_formatter.append(cell_formatter)

    return table_rows, table_formatter


def create_finished_fixtures_table(teams_df: pd.DataFrame, fixtures: list[Fixture], team_id: int):
    team_name = dashboard.data.get_team_name(teams_df, team_id)
    table_rows = []
    table_formatter = []
    for fixture in fixtures:
        row = {
            COL_DATE.display: create_date_string(fixture.date),
            COL_HOME.display: dashboard.data.get_team_name(teams_df, fixture.home_id),
            COL_AWAY.display: dashboard.data.get_team_name(teams_df, fixture.away_id),
            COL_FIRST_HALF.display: str(fixture.first_half),
            COL_SECOND_HALF.display: str(fixture.second_half),
            COL_FULL_TIME.display: str(fixture.full_time)
        }
        table_rows.append(row)

        if team_id in [fixture.home_id, fixture.away_id]:
            constraints = [FilterConstraint(COL_DATE, create_date_string(fixture.date))]
            if team_id == fixture.home_id:
                constraints.append(FilterConstraint(COL_HOME, team_name))
            else:
                constraints.append(FilterConstraint(COL_AWAY, team_name))

            for period in [model.fixtures.Period.FIRST, model.fixtures.Period.SECOND, model.fixtures.Period.FULL]:
                cell_formatter = create_scoreline_formatter(fixture, period, team_id, constraints)
                if cell_formatter is not None:
                    table_formatter.append(cell_formatter)

    for column in [COL_HOME, COL_AWAY]:
        cell_formatter = {
            'if': {
                'column_id': column.display,
                'filter_query': f'{{{column.display}}} = "{team_name}"'
            },
            'backgroundColor': colours.GOOD_NEWS_BACKGROUND_COLOR
        }
        table_formatter.append(cell_formatter)

    return table_rows, table_formatter


def create_finished_fixtures_table_for_team(teams_df: pd.DataFrame, fixtures: list[Fixture], team_id: int):
    table_rows = []
    table_formatter = []
    for fixture in fixtures:
        row = {
            COL_DATE.display: create_date_string(fixture.date),
            COL_FIRST_HALF.display: str(fixture.first_half),
            COL_SECOND_HALF.display: str(fixture.second_half),
            COL_FULL_TIME.display: str(fixture.full_time)
        }

        if fixture.home_id == team_id:
            away_name = dashboard.data.get_team_name(teams_df, fixture.away_id)
            row = row | {COL_VENUE.display: model.fixtures.Venue.HOME.value, COL_TEAM.display: away_name}
        else:
            home_name = dashboard.data.get_team_name(teams_df, fixture.home_id)
            row = row | {COL_VENUE.display: model.fixtures.Venue.AWAY.value, COL_TEAM.display: home_name}

        table_rows.append(row)

        constraints = [FilterConstraint(COL_DATE, create_date_string(fixture.date))]
        for period in [model.fixtures.Period.FIRST, model.fixtures.Period.SECOND, model.fixtures.Period.FULL]:
            cell_formatter = create_scoreline_formatter(fixture, period, team_id, constraints)
            if cell_formatter is not None:
                table_formatter.append(cell_formatter)

    return table_rows, table_formatter

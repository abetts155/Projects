import abc
import dataclasses
import datetime
import typing
import zoneinfo

import pandas as pd

import model.fixtures
import dashboard.data
from dashboard import colours
from dashboard.fixtures import Fixture, reverse_scoreline, UNKNOWN_SCORELINE


@dataclasses.dataclass
class TableColumn(abc.ABC):
    name: str
    display: str

    def default_factory(self):
        return None

    def __hash__(self):
        return hash(self.name)


class IntegerTableColumn(TableColumn):
    def default_factory(self):
        return 0


class StringTableColumn(TableColumn):
    def default_factory(self):
        return ''


class TrendTableColumn(TableColumn):
    def default_factory(self):
        return []


COL_TEAM = StringTableColumn('T', 'Team')
COL_PLAYED = IntegerTableColumn('P', 'P')
COL_WON = IntegerTableColumn('W', 'W')
COL_DRAWN = IntegerTableColumn('D', 'D')
COL_LOST = IntegerTableColumn('L', 'L')
COL_POINTS = IntegerTableColumn('PTS', 'PTS')
COL_GOALS_FOR = IntegerTableColumn('GF', 'For ⚽')
COL_GOALS_AGAINST = IntegerTableColumn('GA', 'Against ⚽')
COL_GOAL_DIFFERENCE = IntegerTableColumn('GD', '')
COL_GOAL_RATE = IntegerTableColumn('GR', 'For + Against ⚽')
COL_BTS = IntegerTableColumn('BTS', 'Both Teams Scored ⚽')
COL_TOTAL_GT_0 = IntegerTableColumn('T > 0', 'For + Against > 0 ⚽')
COL_TOTAL_GT_1 = IntegerTableColumn('T > 1', 'For + Against > 1 ⚽')
COL_TOTAL_GT_2 = IntegerTableColumn('T > 2', 'For + Against > 2 ⚽')
COL_SCORED_GT_0 = IntegerTableColumn('S > 0', 'For > 0 ⚽')
COL_SCORED_GT_1 = IntegerTableColumn('S > 1', 'For > 1 ⚽')
COL_CONCEDED_GT_0 = IntegerTableColumn('C > 0', 'Against > 0 ⚽')
COL_CONCEDED_GT_1 = IntegerTableColumn('C > 1', 'Against > 1 ⚽')
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
    COL_POINTS,
    COL_GOALS_FOR,
    COL_GOALS_AGAINST,
    COL_GOAL_RATE,
    COL_TOTAL_GT_0,
    COL_TOTAL_GT_1,
    COL_TOTAL_GT_2,
    COL_SCORED_GT_0,
    COL_SCORED_GT_1,
    COL_CONCEDED_GT_0,
    COL_CONCEDED_GT_1,
    COL_BTS
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
            setattr(self, column.name, column.default_factory())

    def set(self, column: TableColumn, value):
        setattr(self, column.name, value)

    def increase(self, column: IntegerTableColumn, value: int):
        setattr(self, column.name, getattr(self, column.name) + value)

    def append(self, column: TrendTableColumn, value: int):
        setattr(self, column.name, getattr(self, column.name) + [value])

    def get(self, column: TableColumn):
        return getattr(self, column.name)

    def __str__(self):
        return ' '.join(f'{column.name} {self.get(column)}' for column in league_table_columns)



def render_goals_sparkline(trend: list[int], goal_blocks: dict[int, str]) -> str:
    max_index = max(goal_blocks.keys())
    return ''.join(goal_blocks.get(g, goal_blocks[max_index]) for g in trend)


def render_truthy_sparkline(trend: list[bool]) -> str:
    return ''.join('✅' if x else '❌️' for x in trend)


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

                    if scoreline.left > 0:
                        row.increase(COL_SCORED_GT_0, 1)

                    if scoreline.left > 1:
                        row.increase(COL_SCORED_GT_1, 1)

                    if scoreline.right > 0:
                        row.increase(COL_CONCEDED_GT_0, 1)

                    if scoreline.right > 1:
                        row.increase(COL_CONCEDED_GT_1, 1)

                    if scoreline.left and scoreline.right:
                        row.increase(COL_BTS, 1)

                    if scoreline.left + scoreline.right > 0:
                        row.increase(COL_TOTAL_GT_0, 1)

                    if scoreline.left + scoreline.right > 1:
                        row.increase(COL_TOTAL_GT_1, 1)

                    if scoreline.left + scoreline.right > 2:
                        row.increase(COL_TOTAL_GT_2, 1)

    def fill_with_computed_values(self):
        for row in self.rows:
            setattr(row, COL_PLAYED.name, row.W + row.D + row.L)
            setattr(row, COL_POINTS.name, 3 * row.W + row.D)
            setattr(row, COL_GOAL_DIFFERENCE.name, row.GF - row.GA)

            if row.P:
                setattr(row, COL_GOAL_RATE.name, round((row.GF + row.GA) / row.P, 1))
                setattr(row, COL_GOALS_FOR.name, round(row.GF / row.P, 1))
                setattr(row, COL_GOALS_AGAINST.name, round(row.GA / row.P, 1))
                setattr(row, COL_SCORED_GT_0.name, getattr(row, COL_SCORED_GT_0.name) / row.P)
                setattr(row, COL_SCORED_GT_1.name, getattr(row, COL_SCORED_GT_1.name) / row.P)
                setattr(row, COL_CONCEDED_GT_0.name, getattr(row, COL_CONCEDED_GT_0.name) / row.P)
                setattr(row, COL_CONCEDED_GT_1.name, getattr(row, COL_CONCEDED_GT_1.name) / row.P)
                setattr(row, COL_BTS.name, getattr(row, COL_BTS.name) / row.P)
                setattr(row, COL_TOTAL_GT_0.name, getattr(row, COL_TOTAL_GT_0.name) / row.P)
                setattr(row, COL_TOTAL_GT_1.name, getattr(row, COL_TOTAL_GT_1.name) / row.P)
                setattr(row, COL_TOTAL_GT_2.name, getattr(row, COL_TOTAL_GT_2.name) / row.P)
            else:
                setattr(row, COL_GOAL_RATE.name, 0)
                setattr(row, COL_GOALS_FOR.name, 0)
                setattr(row, COL_GOALS_AGAINST.name, 0)
                setattr(row, COL_SCORED_GT_0.name, 0)
                setattr(row, COL_SCORED_GT_1.name, 0)
                setattr(row, COL_CONCEDED_GT_0.name, 0)
                setattr(row, COL_CONCEDED_GT_1.name, 0)
                setattr(row, COL_BTS.name, 0)
                setattr(row, COL_TOTAL_GT_0.name, 0)
                setattr(row, COL_TOTAL_GT_1.name, 0)
                setattr(row, COL_TOTAL_GT_2.name, 0)


    def to_display_list(self):
        return [{column.name: row.get(column) for column in league_table_columns} for row in self.rows]

    def get_row(self, index: int) -> LeagueTableRow:
        return self.rows[index]

    def get_row_index(self, team_name: str) -> typing.Optional[int]:
        for i, row in enumerate(self.rows):
            if getattr(row, COL_TEAM.name) == team_name:
                return i
        return None

    def __len__(self):
        return len(self.rows)

    def __str__(self):
        return '\n'.join(f'{position} {row.T} {row.PTS}' for position, row in enumerate(self.rows, start=1))


def create_league_table(
        teams_df: pd.DataFrame,
        team_results: dict[int, list[Fixture]],
        period: model.fixtures.Period
) -> LeagueTable:
    league_table = LeagueTable()
    league_table.fill_with_collected_information(teams_df, team_results, period)
    league_table.fill_with_computed_values()
    league_table.rows.sort(key=lambda row: (-row.PTS, -row.GD, -row.GF, row.T))
    return league_table


@dataclasses.dataclass(slots=True)
class FilterConstraint:
    column: TableColumn
    value: str


def create_filter_text(constraints: list[FilterConstraint]):
    return ' && '.join(f'{{{c.column.name}}} = "{c.value}"' for c in constraints)


def create_league_table_formatter(
        teams_df: pd.DataFrame,
        league_table: LeagueTable,
        fixtures: list[dashboard.fixtures.Fixture]
):
    blacklist = [COL_TEAM, COL_PLAYED]
    max_values = {column: 0 for column in league_table_columns if column not in blacklist}
    min_values = {column: 2 ** 10 for column in league_table_columns if column not in blacklist}

    table_formatter = []
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
                        'column_id': column.name
                    },
                    'backgroundColor': colours.COOL_COLOR
                }
                table_formatter.append(cell_formatter)

                constraints = [FilterConstraint(column, max_values[column])]
                cell_formatter = {
                    'if': {
                        'filter_query': create_filter_text(constraints),
                        'column_id': column.name
                    },
                    'backgroundColor': colours.HOT_COLOR
                }
                table_formatter.append(cell_formatter)


    tooltips = []
    for row in league_table.rows:
        team_name = row.get(COL_TEAM)
        tooltip_row = {}
        for fixture in fixtures:
            if fixture.date.date() >= datetime.date.today():
                home_team = dashboard.data.get_team_name(teams_df, fixture.home_id)
                away_team = dashboard.data.get_team_name(teams_df, fixture.away_id)
                localized = fixture.date.replace(tzinfo=zoneinfo.ZoneInfo("Europe/London"))
                time_str = f"{fixture.date.strftime('%d/%m/%y')} {localized.strftime('%H:%M %Z')}"

                if team_name == home_team:
                    tooltip_row[COL_TEAM.name] = {
                        'value': f'{time_str}: **(H) {away_team}**',
                        'type': 'markdown'
                    }

                elif team_name == away_team:
                    tooltip_row[COL_TEAM.name] = {
                        'value': f'{time_str}: **(A) {home_team}**',
                        'type': 'markdown'
                    }

                if tooltip_row:
                    break

        tooltips.append(tooltip_row)

    return table_formatter, tooltips


def create_date_string(fixture_date: datetime.datetime):
    return fixture_date.strftime('%d-%m-%y')


def create_remaining_fixtures_table(teams_df: pd.DataFrame, fixtures: list[Fixture], team_id: int):
    table_rows = []
    for fixture in fixtures:
        today = datetime.datetime.today()
        if (fixture.date.year, fixture.date.month, fixture.date.day) < (today.year, today.month, today.day):
            row = {COL_DATE.name: '??-??-??'}
        else:
            row = {COL_DATE.name: create_date_string(fixture.date)}

        if team_id == fixture.home_id:
            row[COL_VENUE.name] = model.fixtures.Venue.HOME.value
            row[COL_TEAM.name] = dashboard.data.get_team_name(teams_df, fixture.away_id)
        else:
            row[COL_VENUE.name] = model.fixtures.Venue.AWAY.value
            row[COL_TEAM.name] = dashboard.data.get_team_name(teams_df, fixture.home_id)

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
                    'column_id': highlighted_column.name
                },
                'backgroundColor': colours.GOOD_NEWS_BACKGROUND_COLOR
            }

        elif scoreline.left < scoreline.right:
            return {
                'if': {
                    'filter_query': create_filter_text(constraints),
                    'column_id': highlighted_column.name
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
            row = {COL_DATE.name: '??-??-??'}
            unknown_scheduling_rows.append(row)
        else:
            row = {COL_DATE.name: create_date_string(fixture.date)}
            table_rows.append(row)

        row[COL_HOME.name] = dashboard.data.get_team_name(teams_df, fixture.home_id)
        row[COL_AWAY.name] = dashboard.data.get_team_name(teams_df, fixture.away_id)

    table_rows.extend(unknown_scheduling_rows)

    team_name = dashboard.data.get_team_name(teams_df, team_id)
    for column in [COL_HOME, COL_AWAY]:
        cell_formatter = {
            'if': {
                'column_id': column.name,
                'filter_query': f'{{{column.name}}} = "{team_name}"'
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
            COL_DATE.name: create_date_string(fixture.date),
            COL_HOME.name: dashboard.data.get_team_name(teams_df, fixture.home_id),
            COL_AWAY.name: dashboard.data.get_team_name(teams_df, fixture.away_id),
            COL_FIRST_HALF.name: str(fixture.first_half),
            COL_SECOND_HALF.name: str(fixture.second_half),
            COL_FULL_TIME.name: str(fixture.full_time)
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
                'column_id': column.name,
                'filter_query': f'{{{column.name}}} = "{team_name}"'
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
            COL_DATE.name: create_date_string(fixture.date),
            COL_FIRST_HALF.name: str(fixture.first_half),
            COL_SECOND_HALF.name: str(fixture.second_half),
            COL_FULL_TIME.name: str(fixture.full_time)
        }

        if fixture.home_id == team_id:
            away_name = dashboard.data.get_team_name(teams_df, fixture.away_id)
            row = row | {COL_VENUE.name: model.fixtures.Venue.HOME.value, COL_TEAM.name: away_name}
        else:
            home_name = dashboard.data.get_team_name(teams_df, fixture.home_id)
            row = row | {COL_VENUE.name: model.fixtures.Venue.AWAY.value, COL_TEAM.name: home_name}

        table_rows.append(row)

        constraints = [FilterConstraint(COL_DATE, create_date_string(fixture.date))]
        for period in [model.fixtures.Period.FIRST, model.fixtures.Period.SECOND, model.fixtures.Period.FULL]:
            cell_formatter = create_scoreline_formatter(fixture, period, team_id, constraints)
            if cell_formatter is not None:
                table_formatter.append(cell_formatter)

    return table_rows, table_formatter

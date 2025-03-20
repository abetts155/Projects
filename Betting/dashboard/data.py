import collections
import dataclasses
import datetime
import pandas as pd

import football_api.structure
import model.competitions
import model.fixtures
import model.seasons
import model.teams
from dashboard.fixtures import (
    Fixture,
    Scoreline,
    Season,
    create_scoreline_from_string,
    reverse_scoreline,
    UNKNOWN_SCORELINE
)
from sql.sql import Database
from sql.sql_columns import ColumnNames
from sql.sql_language import Keywords


def create_fixture_from_row(fixture_row: pd.Series) -> Fixture:
    return Fixture(
        date=datetime.datetime.fromisoformat(fixture_row['Date']),
        home_id=fixture_row['Home_ID'],
        away_id=fixture_row['Away_ID'],
        finished=fixture_row['Finished'],
        first_half=create_scoreline_from_string(fixture_row['Half_Time']),
        full_time=create_scoreline_from_string(fixture_row['Full_Time'])
    )


def get_season_fixtures(fixtures_df: pd.DataFrame) -> list[Fixture]:
    fixtures = []
    for _, fixture_row in fixtures_df.iterrows():
        fixture = create_fixture_from_row(fixture_row)
        fixtures.append(fixture)
    return fixtures


def get_current_season_id(data_dict: dict) -> int:
    season_ids = [season_id for season_id in data_dict['Seasons'].keys() if data_dict['Seasons'][season_id]['Current']]
    (season_id,) = season_ids
    return season_id


def update_data(league: model.competitions.Competition) -> dict:
    sql_statement_find_seasons = f"""
    {Keywords.SELECT.name} {ColumnNames.Year.name}, {ColumnNames.Current.name} 
    {Keywords.FROM.name} {model.seasons.Season.__name__} 
    {Keywords.WHERE.name} {ColumnNames.Competition_ID.name}={league.id}
    """

    team_ids = set()
    data_dict = {'Country': league.country, 'League': league.name}
    with Database(football_api.structure.database) as db:
        data_dict['Seasons'] = {}
        season_df = pd.read_sql_query(sql_statement_find_seasons, db.connection)
        for _, season_row in season_df.iterrows():
            year = int(season_row[ColumnNames.Year.name])
            current = bool(season_row['Current'])

            sql_statement_find_fixtures = f"""
{Keywords.SELECT.name} 
{ColumnNames.Date.name}, 
{ColumnNames.Home_ID.name}, 
{ColumnNames.Away_ID.name}, 
{ColumnNames.Half_Time.name}, 
{ColumnNames.Full_Time.name}, 
{ColumnNames.Finished.name}
{Keywords.FROM.name} Fixture {Keywords.WHERE.name}
({ColumnNames.Season_ID.name}={year} {Keywords.AND.name} {ColumnNames.Competition_ID.name}={league.id})
"""

            fixtures_df = pd.read_sql_query(sql_statement_find_fixtures, db.connection)
            fixtures = get_season_fixtures(fixtures_df)
            fixtures.sort(key=lambda fixture: fixture.date)
            fixtures = [
                fixture for fixture in fixtures
                if (fixture.finished and fixture.full_time != UNKNOWN_SCORELINE) or season_row['Current']
            ]

            for fixture in fixtures:
                team_ids.add(fixture.home_id)
                team_ids.add(fixture.away_id)

            games_threshold_for_season = 50
            if len(fixtures) >= games_threshold_for_season:
                earliest_date: datetime.datetime = fixtures[0].date
                latest_date: datetime.datetime = fixtures[-1].date
                data_dict['Seasons'][year] = {
                    'Current': current,
                    'Fixtures': fixtures_df.to_dict(),
                    'Start_Year': earliest_date.year,
                    'End_Year': latest_date.year
                }

        sql_statement_find_teams = f"""
                    {Keywords.SELECT.name} {ColumnNames.ID.name}, {ColumnNames.Name.name}, {ColumnNames.Venue_Name.name} 
                    {Keywords.FROM.name} {model.teams.Team.__name__}
                    {Keywords.WHERE.name} {ColumnNames.ID.name} {Keywords.IN.name} ({', '.join(map(str, team_ids))})
        """
        teams_df = pd.read_sql_query(sql_statement_find_teams, db.connection)
        data_dict['Teams'] = teams_df.to_dict()
    return data_dict


def get_season_ids(data_dict: dict) -> list[int]:
    return list(data_dict['Seasons'].keys())


def get_fixtures_data_frame(data_dict: dict, season_id: int) -> pd.DataFrame:
    return pd.DataFrame.from_dict(data_dict['Seasons'][season_id]['Fixtures'])


def get_teams_data_frame(data_dict: dict) -> pd.DataFrame:
    teams_df = pd.DataFrame.from_dict(data_dict['Teams'])
    teams_df.reset_index(inplace=True)
    return teams_df


def get_season_id(data_dict: dict, start_year: int, end_year: int) -> int | None:
    for season_id in get_season_ids(data_dict):
        if (
                data_dict['Seasons'][season_id]['Start_Year'] == start_year and
                data_dict['Seasons'][season_id]['End_Year'] == end_year
        ):
            return season_id


def get_team_name(teams_df: pd.DataFrame, team_id: int) -> str:
    return teams_df.loc[teams_df['ID'] == team_id, 'Name'].values[0]


def get_team_id(teams_df: pd.DataFrame, team_name: str) -> int:
    return teams_df.loc[teams_df['Name'] == team_name, 'ID'].values[0]


def get_season_teams(data_dict: dict, season_id: int) -> set[int]:
    fixtures_df = get_fixtures_data_frame(data_dict, season_id)
    return set(fixtures_df['Home_ID'].tolist())


def filter_fixtures_for_a_particular_team(fixtures_df: pd.DataFrame, venue: model.fixtures.Venue, team_id: int) -> pd.DataFrame:
    if venue == model.fixtures.Venue.ANYWHERE:
        return fixtures_df[(fixtures_df['Home_ID'] == team_id) | (fixtures_df['Away_ID'] == team_id)]
    elif venue == model.fixtures.Venue.HOME:
        return fixtures_df[(fixtures_df['Home_ID'] == team_id)]
    else:
        return fixtures_df[(fixtures_df['Away_ID'] == team_id)]


def get_seasons(data_dict: dict, reverse: bool) -> list[Season]:
    seasons = []
    for season_id in get_season_ids(data_dict):
        start_year = data_dict['Seasons'][season_id]['Start_Year']
        end_year = data_dict['Seasons'][season_id]['End_Year']
        seasons.append(Season(season_id, start_year, end_year))
    seasons.sort(key=lambda season: (season.start_year, season.end_year), reverse=reverse)
    return seasons


def get_result_per_team(
        data_dict: dict,
        season_id: int,
        venue: model.fixtures.Venue,
        history: list[int]
) -> dict[int, list[Fixture]]:
    team_ids = get_season_teams(data_dict, season_id)
    fixtures_df = get_fixtures_data_frame(data_dict, season_id)
    fixtures = get_season_fixtures(fixtures_df)
    fixtures.sort(key=lambda fixture: fixture.date)

    team_results = {team_id: {} for team_id in team_ids}
    team_indices = {team_id: 0 for team_id in team_ids}
    for fixture in fixtures:
        if fixture.finished:
            if venue in [model.fixtures.Venue.ANYWHERE, model.fixtures.Venue.HOME]:
                i = team_indices[fixture.home_id] + 1
                team_results[fixture.home_id][i] = fixture
                team_indices[fixture.home_id] = i

            if venue in [model.fixtures.Venue.ANYWHERE, model.fixtures.Venue.AWAY]:
                i = team_indices[fixture.away_id] + 1
                team_results[fixture.away_id][i] = fixture
                team_indices[fixture.away_id] = i

    min_history, max_history = history
    if history == [0, 0]:
        min_history = 1
        max_history = max(len(fixtures) for fixtures in team_results.values())

    filtered_team_results = {team_id: [] for team_id in team_ids}
    for team_id in team_ids:
        for i in range(min_history, max_history + 1):
            if i in team_results[team_id]:
                fixture = team_results[team_id][i]
                filtered_team_results[team_id].append(fixture)

    return filtered_team_results


def get_scheduled_fixtures(data_dict: dict) -> list[Fixture]:
    season_id = get_current_season_id(data_dict)
    fixtures_df = get_fixtures_data_frame(data_dict, season_id)
    fixtures = []
    for _, fixture_row in fixtures_df.iterrows():
        fixture = create_fixture_from_row(fixture_row)
        if not fixture.finished:
            fixtures.append(fixture)

    fixtures.sort(key=lambda fixture: fixture.date)
    return fixtures


def get_completed_fixtures(data_dict: dict) -> list[Fixture]:
    fixtures = []
    for season_id in get_season_ids(data_dict):
        fixtures_df = get_fixtures_data_frame(data_dict, season_id)
        season_fixtures = get_season_fixtures(fixtures_df)
        for fixture in season_fixtures:
            if fixture.finished:
                fixtures.append(fixture)

    fixtures.sort(key=lambda fixture: fixture.date, reverse=True)
    return fixtures


def get_scheduled_fixtures_for_team(data_dict: dict, venue: model.fixtures.Venue, team_name: str) -> list[Fixture]:
    teams_df = get_teams_data_frame(data_dict)
    season_id = get_current_season_id(data_dict)
    fixtures_df = get_fixtures_data_frame(data_dict, season_id)
    team_id = get_team_id(teams_df, team_name)
    filtered_df = filter_fixtures_for_a_particular_team(fixtures_df, venue, team_id)

    fixtures = []
    for _, fixture_row in filtered_df.iterrows():
        fixture = create_fixture_from_row(fixture_row)
        if not fixture.finished:
            fixtures.append(fixture)

    fixtures.sort(key=lambda fixture: fixture.date)
    return fixtures


def get_next_fixture(data_dict: dict, team_name: str) -> Fixture | None:
    fixtures = get_scheduled_fixtures_for_team(data_dict, model.fixtures.Venue.ANYWHERE, team_name)
    if fixtures:
        return fixtures[0]


def get_completed_fixtures_for_team(data_dict: dict, venue: model.fixtures.Venue, team_name: str) -> list[Fixture]:
    teams_df = get_teams_data_frame(data_dict)
    fixtures = []
    for season_id in get_season_ids(data_dict):
        fixtures_df = get_fixtures_data_frame(data_dict, season_id)
        team_id = get_team_id(teams_df, team_name)
        filtered_df = filter_fixtures_for_a_particular_team(fixtures_df, venue, team_id)

        for _, fixture_row in filtered_df.iterrows():
            fixture = create_fixture_from_row(fixture_row)
            if fixture.finished:
                fixtures.append(fixture)

    fixtures.sort(key=lambda fixture: fixture.date, reverse=True)
    return fixtures


@dataclasses.dataclass(slots=True)
class DataBin:
    season_id: int
    dates: list[datetime.datetime] = dataclasses.field(default_factory=list)
    goal_counts: collections.Counter = dataclasses.field(default_factory=collections.Counter)
    results: collections.Counter = dataclasses.field(default_factory=collections.Counter)
    total_games: int = 0
    bts: int = 0

    def get_season_string(self) -> str:
        earliest_date = min(self.dates)
        latest_date = max(self.dates)
        start_year, end_year = earliest_date.year - 2000, latest_date.year - 2000
        if start_year == end_year:
            return f'{start_year}'
        else:
            return f'{start_year}/{end_year}'

    def update(self, scoreline: Scoreline, date: datetime.datetime):
        self.total_games += 1
        self.goal_counts[scoreline.left + scoreline.right] += 1
        self.results[(scoreline.left, scoreline.right)] += 1
        self.dates.append(date)

        if scoreline.left and scoreline.right:
            self.bts += 1


class SeasonBin(DataBin):
    home_wins: int = 0
    draws: int = 0
    away_wins: int = 0

    def update(self, scoreline: Scoreline, date: datetime.datetime):
        if scoreline.left > scoreline.right:
            self.home_wins += 1
        elif scoreline.left == scoreline.right:
            self.draws += 1
        else:
            self.away_wins += 1

        super().update(scoreline, date)


class TeamBin(DataBin):
    wins: int = 0
    draws: int = 0
    losses: int = 0
    scored: int = 0
    conceded: int = 0

    def update(self, scoreline: Scoreline, date: datetime.datetime):
        if scoreline.left > scoreline.right:
            self.wins += 1
        elif scoreline.left == scoreline.right:
            self.draws += 1
        else:
            self.losses += 1

        self.scored += scoreline.left
        self.conceded += scoreline.right

        super().update(scoreline, date)


def get_scoreline(fixture: Fixture, period: model.fixtures.Period) -> Scoreline:
    if period == model.fixtures.Period.FULL:
        return fixture.full_time
    elif period == model.fixtures.Period.FIRST:
        return fixture.first_half
    else:
        return fixture.second_half


def tidy_bins(bins: list[SeasonBin | TeamBin]) -> list[SeasonBin | TeamBin]:
    cleaned = []
    for data_bin in bins:
        if data_bin.total_games:
            cleaned.append(data_bin)

    return sorted(cleaned, key=lambda data_bin: min(data_bin.dates))


def aggregate_seasons(data_dict: dict, period: model.fixtures.Period) -> list[SeasonBin]:
    bins: list[SeasonBin] = []
    for season_id in get_season_ids(data_dict):
        fixtures_df = get_fixtures_data_frame(data_dict, season_id)
        data_bin = SeasonBin(season_id)
        bins.append(data_bin)

        for _, fixture_row in fixtures_df.iterrows():
            fixture = create_fixture_from_row(fixture_row)
            if fixture.finished:
                scoreline = get_scoreline(fixture, period)
                if scoreline != UNKNOWN_SCORELINE:
                    data_bin.update(scoreline, fixture.date)

    return tidy_bins(bins)


def aggregate_team(
        data_dict: dict,
        period: model.fixtures.Period,
        venue: model.fixtures.Venue,
        team_id: int
) -> list[TeamBin]:
    bins: list[TeamBin] = []
    for season_id in get_season_ids(data_dict):
        fixtures_df = get_fixtures_data_frame(data_dict, season_id)
        data_bin = TeamBin(season_id)
        bins.append(data_bin)
        filtered_df = filter_fixtures_for_a_particular_team(fixtures_df, venue, team_id)

        for _, fixture_row in filtered_df.iterrows():
            fixture = create_fixture_from_row(fixture_row)
            if fixture.finished:
                scoreline = get_scoreline(fixture, period)
                if team_id == fixture.away_id:
                    scoreline = reverse_scoreline(scoreline)

                if scoreline != UNKNOWN_SCORELINE:
                    data_bin.update(scoreline, fixture.date)

    return tidy_bins(bins)


def collect_fixture_sequences(
        data_dict: dict,
        period: model.fixtures.Period,
        venue: model.fixtures.Venue,
        team_id: int
):
    nil_nils = {}
    teams_scorelines = {}
    index = None
    seasons = get_seasons(data_dict, False)
    for season in seasons:
        fixtures_df = get_fixtures_data_frame(data_dict, season.the_id)

        for other_team_id in get_season_teams(data_dict, season.the_id):
            if other_team_id not in teams_scorelines:
                teams_scorelines[other_team_id] = []

            filtered_df = filter_fixtures_for_a_particular_team(fixtures_df, venue, other_team_id)
            fixtures = []
            for _, fixture_row in filtered_df.iterrows():
                fixture = create_fixture_from_row(fixture_row)
                if fixture.finished:
                    fixtures.append(fixture)

            fixtures.sort(key=lambda fixture: fixture.date)
            for fixture in fixtures:
                scoreline = get_scoreline(fixture, period)
                if scoreline != UNKNOWN_SCORELINE:
                    if fixture.home_id != other_team_id:
                        scoreline = reverse_scoreline(scoreline)

                    if season.the_id == get_current_season_id(data_dict):
                        if index is None and other_team_id == team_id:
                            index = len(teams_scorelines[other_team_id])

                    teams_scorelines[other_team_id].append(scoreline)

                    if scoreline.left + scoreline.right == 0:
                        nil_nils.setdefault(fixture.home_id, 0)
                        nil_nils.setdefault(fixture.away_id, 0)
                        nil_nils[fixture.home_id] += 1
                        nil_nils[fixture.away_id] += 1

    return teams_scorelines, index

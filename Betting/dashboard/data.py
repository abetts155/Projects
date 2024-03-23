from collections import Counter
from dataclasses import dataclass, field
from datetime import datetime
from typing import Dict, List

from pandas import read_sql_query, DataFrame

from dashboard.fixtures import (
    Period,
    Venue,
    Fixture,
    Scoreline,
    create_scoreline_from_string,
    reverse_scoreline,
    UNKNOWN_SCORELINE
)
from sql.sql import Database
from sql.sql_language import Keywords


def update_data(database: str, country_name: str, league_name: str) -> Dict:
    sql_statement_find_teams = f"""
            {Keywords.SELECT.name} ID, Name, Logo 
            {Keywords.FROM.name} Team
            """

    sql_statement_find_seasons = f"""
    {Keywords.SELECT.name} ID, Current 
    {Keywords.FROM.name} Season 
    {Keywords.WHERE.name} (Country='{country_name}') {Keywords.AND.name} (Code='{league_name}') 
    {Keywords.COLLATE.name} {Keywords.NOCASE.name}
    """

    data_dict = {}
    with Database(database) as db:
        teams_df = read_sql_query(sql_statement_find_teams, db.connection)
        data_dict['Teams'] = teams_df.to_dict()
        data_dict['Seasons'] = {}

        season_df = read_sql_query(sql_statement_find_seasons, db.connection)
        for _, season_row in season_df.iterrows():
            season_id = str(season_row['ID'])

            sql_statement_find_fixtures = f"""
            {Keywords.SELECT.name} Date, Home_ID, Away_ID, Half_Time, Full_Time, Finished 
            {Keywords.FROM.name} Fixture 
            {Keywords.WHERE.name} (Season_ID='{season_id}')
            """

            fixtures_df = read_sql_query(sql_statement_find_fixtures, db.connection)
            data_dict['Seasons'][season_id] = {
                'Current': season_row['Current'],
                'Fixtures': fixtures_df.to_dict()
            }

    return data_dict


def get_season_ids(data_dict: Dict) -> List[int]:
    return list(data_dict['Seasons'].keys())


def get_fixtures_data_frame(data_dict: Dict, season_id: int) -> DataFrame:
    return DataFrame.from_dict(data_dict['Seasons'][season_id]['Fixtures'])


def get_teams_data_frame(data_dict: Dict) -> DataFrame:
    teams_df = DataFrame.from_dict(data_dict['Teams'])
    teams_df.set_index('ID', inplace=True)
    return teams_df


def get_current_season_id(data_dict: Dict) -> int:
    season_ids = [season_id for season_id in data_dict['Seasons'].keys() if data_dict['Seasons'][season_id]['Current']]
    (season_id,) = season_ids
    return season_id


def get_team_name(teams_df: DataFrame, team_id: int) -> str | None:
    if team_id in teams_df.index:
        return teams_df.at[team_id, 'Name']
    else:
        return None


def get_team_id(teams_df: DataFrame, team_name: str) -> int | None:
    return teams_df.loc[teams_df['Name'] == team_name].index[0]


def get_current_season_teams(data_dict: Dict) -> List[str]:
    teams_df = get_teams_data_frame(data_dict)
    season_id = get_current_season_id(data_dict)
    fixtures_df = get_fixtures_data_frame(data_dict, season_id)
    teams_ids = fixtures_df['Home_ID'].tolist()
    team_names = set()
    for team_id in teams_ids:
        team_name = get_team_name(teams_df, team_id)
        team_names.add(team_name)
    team_names = list(sorted(team_names))
    return team_names


def filter_fixtures_for_a_particular_team(fixtures_df: DataFrame, venue: Venue, team_id: int) -> DataFrame:
    if venue == Venue.SOMEWHERE:
        return fixtures_df[(fixtures_df['Home_ID'] == team_id) | (fixtures_df['Away_ID'] == team_id)]
    elif venue == Venue.HOME:
        return fixtures_df[(fixtures_df['Home_ID'] == team_id)]
    else:
        return fixtures_df[(fixtures_df['Away_ID'] == team_id)]


def create_fixture_from_row(teams_df: DataFrame, fixture_row: Dict) -> Fixture:
    return Fixture(
        date=datetime.fromisoformat(fixture_row['Date']),
        home_name=get_team_name(teams_df, fixture_row['Home_ID']),
        away_name=get_team_name(teams_df, fixture_row['Away_ID']),
        finished=fixture_row['Finished'],
        first_half=create_scoreline_from_string(fixture_row['Half_Time']),
        full_time=create_scoreline_from_string(fixture_row['Full_Time'])
    )


def get_current_season_results(data_dict: Dict, team_name: str = '') -> List[Fixture]:
    teams_df = get_teams_data_frame(data_dict)
    season_id = get_current_season_id(data_dict)
    fixtures_df = get_fixtures_data_frame(data_dict, season_id)

    fixtures = []
    for _, fixture_row in fixtures_df.iterrows():
        fixture = create_fixture_from_row(teams_df, fixture_row)
        if fixture.finished:
            if not team_name:
                fixtures.append(fixture)
            elif team_name in [fixture.home_name, fixture.away_name]:
                fixtures.append(fixture)

    fixtures.sort(key=lambda fixture: fixture.date, reverse=True)
    return fixtures


def get_fixtures_per_team_within_venue_and_window(data_dict: Dict, venue: Venue, history: int) -> Dict[str, List[Fixture]]:
    team_names = get_current_season_teams(data_dict)
    fixtures = get_current_season_results(data_dict)

    team_fixtures = {team_name: [] for team_name in team_names}
    for fixture in fixtures:
        team_fixtures[fixture.home_name].append(fixture)
        team_fixtures[fixture.away_name].append(fixture)

    filtered_team_fixtures = {}
    for team_name, results in team_fixtures.items():
        if venue == Venue.HOME:
            completed = [result for result in results if result.home_name == team_name]
        elif venue == Venue.AWAY:
            completed = [result for result in results if result.away_name == team_name]
        else:
            completed = results

        filtered_team_fixtures[team_name] = completed
        if history:
            limit = min(history, len(completed))
            filtered_team_fixtures[team_name] = [completed[i] for i in range(limit)]

    return filtered_team_fixtures


def get_upcoming_fixtures(data_dict: Dict, venue: Venue, team_name: str) -> List[Fixture]:
    teams_df = get_teams_data_frame(data_dict)
    season_id = get_current_season_id(data_dict)
    fixtures_df = get_fixtures_data_frame(data_dict, season_id)
    team_id = get_team_id(teams_df, team_name)
    filtered_df = filter_fixtures_for_a_particular_team(fixtures_df, venue, team_id)

    fixtures = []
    for _, fixture_row in filtered_df.iterrows():
        fixture = create_fixture_from_row(teams_df, fixture_row)
        if not fixture.finished:
            fixtures.append(fixture)

    fixtures.sort(key=lambda fixture: fixture.date)
    return fixtures


def get_completed_fixtures(data_dict: Dict) -> List[Fixture]:
    teams_df = get_teams_data_frame(data_dict)
    fixtures = []
    for season_id in get_season_ids(data_dict):
        fixtures_df = get_fixtures_data_frame(data_dict, season_id)
        for _, fixture_row in fixtures_df.iterrows():
            fixture = create_fixture_from_row(teams_df, fixture_row)
            if fixture.finished:
                fixtures.append(fixture)

    fixtures.sort(key=lambda fixture: fixture.date, reverse=True)
    return fixtures


def get_completed_fixtures_for_team(data_dict: Dict, venue: Venue, team_name: str) -> List[Fixture]:
    teams_df = get_teams_data_frame(data_dict)
    fixtures = []
    for season_id in get_season_ids(data_dict):
        fixtures_df = get_fixtures_data_frame(data_dict, season_id)
        team_id = get_team_id(teams_df, team_name)
        filtered_df = filter_fixtures_for_a_particular_team(fixtures_df, venue, team_id)

        for _, fixture_row in filtered_df.iterrows():
            fixture = create_fixture_from_row(teams_df, fixture_row)
            if fixture.finished:
                fixtures.append(fixture)

    fixtures.sort(key=lambda fixture: fixture.date, reverse=True)
    return fixtures


@dataclass(slots=True)
class DataBin:
    season_id: int
    home_wins: int = 0
    away_wins: int = 0
    draws: int = 0
    bts: int = 0
    earliest_date: datetime = None
    latest_date: datetime = None
    goal_counts: Counter = field(default_factory=Counter)
    results: Counter = field(default_factory=Counter)

    def total_games(self) -> int:
        return self.home_wins + self.draws + self.away_wins

    def get_season_string(self) -> str:
        start_year, end_year = self.earliest_date.year - 2000, self.latest_date.year - 2000
        if start_year == end_year:
            return f'{start_year}'
        else:
            return f'{start_year}/{end_year}'

    def update(self, scoreline: Scoreline, date: datetime):
        self.goal_counts[scoreline.home_goals + scoreline.away_goals] += 1
        self.results[(scoreline.home_goals, scoreline.away_goals)] += 1

        if scoreline.home_goals > scoreline.away_goals:
            self.home_wins += 1
        elif scoreline.home_goals == scoreline.away_goals:
            self.draws += 1
        else:
            self.away_wins += 1

        if scoreline.home_goals and scoreline.away_goals:
            self.bts += 1

        if self.earliest_date is None:
            self.earliest_date = date
        else:
            self.earliest_date = min(self.earliest_date, date)

        if self.latest_date is None:
            self.latest_date = date
        else:
            self.latest_date = max(self.latest_date, date)


def get_scoreline(fixture: Fixture, period: Period) -> Scoreline:
    if period == Period.FULL:
        return fixture.full_time
    elif period == Period.FIRST:
        return fixture.first_half
    else:
        return fixture.second_half


def tidy_bins(data_dict: Dict, bins: List[DataBin]) -> List[DataBin]:
    threshold = 20
    cleaned = []
    for data_bin in bins:
        if data_bin.total_games() >= threshold or data_bin.season_id == get_current_season_id(data_dict):
            cleaned.append(data_bin)

    return sorted(cleaned, key=lambda data_bin: data_bin.earliest_date)


def aggregate_seasons(data_dict: Dict, period: Period) -> List[DataBin]:
    teams_df = get_teams_data_frame(data_dict)
    bins: List[DataBin] = []
    for season_id in get_season_ids(data_dict):
        fixtures_df = get_fixtures_data_frame(data_dict, season_id)
        data_bin = DataBin(season_id)
        bins.append(data_bin)

        for _, fixture_row in fixtures_df.iterrows():
            fixture = create_fixture_from_row(teams_df, fixture_row)
            if fixture.finished:
                scoreline = get_scoreline(fixture, period)
                if scoreline != UNKNOWN_SCORELINE:
                    data_bin.update(scoreline, fixture.date)

    return tidy_bins(data_dict, bins)


class TeamBin(DataBin):
    team_wins: int = 0
    team_losses: int = 0
    team_draws: int = 0
    scored: int = 0
    conceded: int = 0

    def update(self, scoreline: Scoreline, date: datetime):
        if scoreline.home_goals > scoreline.away_goals:
            self.team_wins += 1
        elif scoreline.home_goals == scoreline.away_goals:
            self.team_draws += 1
        else:
            self.team_losses += 1

        self.scored += scoreline.home_goals
        self.conceded += scoreline.away_goals

        super().update(scoreline, date)


def aggregate_team(data_dict: Dict, period: Period, venue: Venue, team_name: str):
    teams_df = get_teams_data_frame(data_dict)
    bins: List[TeamBin] = []
    for season_id in get_season_ids(data_dict):
        fixtures_df = get_fixtures_data_frame(data_dict, season_id)
        team_id = get_team_id(teams_df, team_name)
        data_bin = TeamBin(season_id)
        bins.append(data_bin)

        filtered_df = filter_fixtures_for_a_particular_team(fixtures_df, venue, team_id)
        for _, fixture_row in filtered_df.iterrows():
            fixture = create_fixture_from_row(teams_df, fixture_row)
            if fixture.finished:
                scoreline = get_scoreline(fixture, period)
                if fixture.away_name == team_name:
                    scoreline = reverse_scoreline(scoreline)

                if scoreline != UNKNOWN_SCORELINE:
                    data_bin.update(scoreline, fixture.date)

    return tidy_bins(data_dict, bins)


@dataclass
class SeasonBin:
    fresh: bool = True
    scored: List = field(default_factory=list)
    conceded: List = field(default_factory=list)
    team_wins: List = field(default_factory=list)
    team_draws: List = field(default_factory=list)
    team_losses: List = field(default_factory=list)

    def update(self, scoreline: Scoreline):
        if self.fresh:
            self.fresh = False
            if scoreline.home_goals > scoreline.away_goals:
                x, y, z = 1, 0, 0
            elif scoreline.home_goals == scoreline.away_goals:
                x, y, z = 0, 1, 0
            else:
                x, y, z = 0, 0, 1

            self.team_wins.append(x)
            self.team_draws.append(y)
            self.team_losses.append(z)
            self.scored.append(scoreline.home_goals)
            self.conceded.append(scoreline.away_goals)
        else:
            if scoreline.home_goals > scoreline.away_goals:
                x, y, z = self.team_wins[-1] + 1, self.team_draws[-1], self.team_losses[-1]
            elif scoreline.home_goals == scoreline.away_goals:
                x, y, z = self.team_wins[-1], self.team_draws[-1] + 1, self.team_losses[-1]
            else:
                x, y, z = self.team_wins[-1], self.team_draws[-1], self.team_losses[-1] + 1

            self.team_wins.append(x)
            self.team_draws.append(y)
            self.team_losses.append(z)
            self.scored.append(scoreline.home_goals + self.scored[-1])
            self.conceded.append(scoreline.away_goals + self.conceded[-1])


def collect_current_season_data(data_dict: Dict, period: Period, venue: Venue, team_name: str) -> SeasonBin:
    teams_df = get_teams_data_frame(data_dict)
    season_id = get_current_season_id(data_dict)
    fixtures_df = get_fixtures_data_frame(data_dict, season_id)
    team_id = get_team_id(teams_df, team_name)
    filtered_df = filter_fixtures_for_a_particular_team(fixtures_df, venue, team_id)

    fixtures = []
    for _, fixture_row in filtered_df.iterrows():
        fixture = create_fixture_from_row(teams_df, fixture_row)
        if fixture.finished:
            fixtures.append(fixture)

    fixtures.sort(key=lambda fixture: fixture.date)

    data_bin = SeasonBin()
    for fixture in fixtures:
        scoreline = get_scoreline(fixture, period)
        if fixture.away_name == team_name:
            scoreline = reverse_scoreline(scoreline)

        if scoreline != UNKNOWN_SCORELINE:
            data_bin.update(scoreline)

    return data_bin

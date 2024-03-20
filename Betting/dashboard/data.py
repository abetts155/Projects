import datetime
from collections import Counter
from dataclasses import dataclass, field
from datetime import date
from enum import Enum, auto
from typing import Dict

from model.fixtures import Half, Venue
from model.leagues import country_to_leagues
from model.seasons import Season
from model.teams import Team
from sql.sql import load_league, load_teams


TODAY = datetime.datetime.today()
UNKNOWN_SCORELINE = '?-?'
DATABASE = 'football.db'
load_teams(DATABASE)


class StoreKeys(Enum):
    SEASONS = auto()
    TEAMS = auto()
    FIXTURES = auto()
    START_YEAR = auto()
    END_YEAR = auto()


def update_data(country_name: str, league_name: str) -> Dict:
    (chosen_league,) = [league for league in country_to_leagues[country_name] if league.name == league_name]
    load_league(DATABASE, chosen_league)

    seasons = Season.seasons(chosen_league)
    data_dict = {StoreKeys.SEASONS.name: []}
    for season in seasons:
        season_id = str(season.id)
        teams = [team.name for team in season.teams()]
        fixtures = []

        start_date = min((fixture.date for fixture in season.fixtures()), default=None)
        end_date = max((fixture.date for fixture in season.fixtures()), default=None)

        for fixture in season.fixtures():
            full_time = fixture.result(Half.full)
            first_half = fixture.result(Half.first)
            second_half = fixture.result(Half.second)

            fixture_dict = {
                Venue.home.name: fixture.home_team.name,
                Venue.away.name: fixture.away_team.name,
                Half.full.name: f'{full_time.left}-{full_time.right}' if full_time else UNKNOWN_SCORELINE,
                Half.first.name: f'{first_half.left}-{first_half.right}' if first_half else UNKNOWN_SCORELINE,
                Half.second.name: f'{second_half.left}-{second_half.right}' if second_half else UNKNOWN_SCORELINE,
                date.__name__: fixture.date.strftime('%d-%m-%y')
            }
            fixtures.append(fixture_dict)

        data_dict[season_id] = {
            StoreKeys.TEAMS.name: teams,
            StoreKeys.FIXTURES.name: fixtures,
            StoreKeys.START_YEAR.name: start_date.year if start_date else None,
            StoreKeys.END_YEAR.name: end_date.year if end_date else None
        }
        data_dict[StoreKeys.SEASONS.name].append(season_id)

    return data_dict


def create_team_data(data_dict: Dict, venue: Venue, team: str) -> Dict:
    team_dict = {StoreKeys.SEASONS.name: []}
    for season in data_dict[StoreKeys.SEASONS.name]:
        teams = data_dict[season][StoreKeys.TEAMS.name]
        if team in teams:
            team_dict[StoreKeys.SEASONS.name].append(season)
            season_dict = {
                StoreKeys.FIXTURES.name: [],
                StoreKeys.START_YEAR.name: data_dict[season][StoreKeys.START_YEAR.name],
                StoreKeys.END_YEAR.name: data_dict[season][StoreKeys.END_YEAR.name]
            }
            team_dict[season] = season_dict

            for fixture_dict in data_dict[season][StoreKeys.FIXTURES.name]:
                create = ((venue == Venue.home and team == fixture_dict[Venue.home.name]) or
                          (venue == Venue.away and team == fixture_dict[Venue.away.name]) or
                          venue == Venue.anywhere and team in [fixture_dict[Venue.home.name], fixture_dict[Venue.away.name]])

                if create:
                    team_fixture_dict = {
                        Venue.home.name: fixture_dict[Venue.home.name],
                        Venue.away.name: fixture_dict[Venue.away.name],
                        date.__name__: fixture_dict[date.__name__],
                    }

                    for half in Half:
                        team_fixture_dict[half.name] = fixture_dict[half.name]

                    season_dict[StoreKeys.FIXTURES.name].append(team_fixture_dict)

    return team_dict


def get_latest_season(data_dict: Dict):
    last_season = data_dict[StoreKeys.SEASONS.name][-1]
    return data_dict[last_season]


def collect_fixtures(data_dict: Dict, venue: Venue, history: int):
    season_dict = get_latest_season(data_dict)
    completed_fixtures = {}
    for team in season_dict[StoreKeys.TEAMS.name]:
        completed_fixtures[team] = []

    for fixture_dict in season_dict[StoreKeys.FIXTURES.name]:
        home_team = fixture_dict[Venue.home.name]
        away_team = fixture_dict[Venue.away.name]

        if fixture_dict[Half.full.name] != UNKNOWN_SCORELINE:
            completed_fixtures[home_team].append(fixture_dict)
            completed_fixtures[away_team].append(fixture_dict)

    filtered_fixtures = {}
    for team, fixtures in completed_fixtures.items():
        completed = fixtures
        if venue == Venue.home:
            completed = [fixture_dict for fixture_dict in completed if fixture_dict[Venue.home.name] == team]
        elif venue == Venue.away:
            completed = [fixture_dict for fixture_dict in completed if fixture_dict[Venue.away.name] == team]

        if history:
            limit = min(history, len(completed))
            completed = list(reversed(completed))
            filtered_fixtures[team] = [completed[i] for i in range(limit)]
        else:
            filtered_fixtures[team] = completed

    return filtered_fixtures


def get_unplayed_fixtures(data_dict: Dict, venue: Venue, team: str):
    season_dict = get_latest_season(data_dict)
    rows = []
    for fixture_dict in season_dict[StoreKeys.FIXTURES.name]:
        home_team = fixture_dict[Venue.home.name]
        away_team = fixture_dict[Venue.away.name]

        if fixture_dict[Half.full.name] == UNKNOWN_SCORELINE:
            team_involved = (
                    (venue == Venue.home and team == fixture_dict[Venue.home.name]) or
                    (venue == Venue.away and team == fixture_dict[Venue.away.name]) or
                    venue == Venue.anywhere and team in [fixture_dict[Venue.home.name], fixture_dict[Venue.away.name]]
            )
            if team_involved:
                unplayed_fixture = {date.__name__: fixture_dict[date.__name__]}

                if team == fixture_dict[Venue.home.name]:
                    unplayed_fixture[Venue.__name__] = Venue.home.value
                    unplayed_fixture[Team.__name__] = fixture_dict[Venue.away.name]
                else:
                    unplayed_fixture[Venue.__name__] = Venue.away.value
                    unplayed_fixture[Team.__name__] = fixture_dict[Venue.home.name]

                rows.append(unplayed_fixture)
    return rows


def collect_results(data_dict: Dict):
    results = []
    for season in reversed(data_dict[StoreKeys.SEASONS.name]):
        season_dict = data_dict[season]
        for fixture_dict in reversed(season_dict[StoreKeys.FIXTURES.name]):
            if fixture_dict[Half.full.name] != UNKNOWN_SCORELINE:
                result = {
                    date.__name__: fixture_dict[date.__name__],
                    Venue.home.name: fixture_dict[Venue.home.name],
                    Venue.away.name: fixture_dict[Venue.away.name],
                    Half.full.name: fixture_dict[Half.full.name],
                    Half.first.name: fixture_dict[Half.first.name],
                    Half.second.name: fixture_dict[Half.second.name]
                }
                results.append(result)
    return results


def create_team_results_table(data_dict: Dict, venue: Venue, team: str):
    results = collect_results(data_dict)
    rows = []
    formatter = []
    for result in results:
        team_involved = (
                (venue == Venue.home and team == result[Venue.home.name]) or
                (venue == Venue.away and team == result[Venue.away.name]) or
                venue == Venue.anywhere and team in [result[Venue.home.name], result[Venue.away.name]]
        )
        if team_involved and result[Half.full.name] != UNKNOWN_SCORELINE:
            team_result = {
                date.__name__: result[date.__name__],
                Half.first.name: result[Half.first.name],
                Half.second.name: result[Half.second.name],
                Half.full.name: result[Half.full.name]
            }

            if team == result[Venue.home.name]:
                team_result[Venue.__name__] = Venue.home.value
                team_result[Team.__name__] = result[Venue.away.name]
            else:
                team_result[Venue.__name__] = Venue.away.value
                team_result[Team.__name__] = result[Venue.home.name]

            rows.append(team_result)

            for half in Half:
                if result[half.name] != '?-?':
                    x, y = result[half.name].split('-')
                    x, y = int(x), int(y)

                    if team == result[Venue.away.name]:
                        x, y = y, x

                    if x > y:
                        cell_formatter = {
                            'if': {'filter_query': f'{{{date.__name__}}} = {result[date.__name__]}', 'column_id': half.name},
                            'backgroundColor': '#ffa600',
                            'color': '#003f5c'
                        }
                        formatter.append(cell_formatter)

                    elif x < y:
                        cell_formatter = {
                            'if': {'filter_query': f'{{{date.__name__}}} = {result[date.__name__]}', 'column_id': half.name},
                            'backgroundColor': '#000000',
                            'color': '#FFFFFF'
                        }
                        formatter.append(cell_formatter)

    return rows, formatter


def get_teams(data_dict: Dict):
    last_season = data_dict[StoreKeys.SEASONS.name][-1]
    return sorted([team for team in data_dict[last_season][StoreKeys.TEAMS.name]])


@dataclass
class DataBin:
    goal_counts: Counter = field(default_factory=Counter)
    results: Counter = field(default_factory=Counter)
    home_wins: int = 0
    away_wins: int = 0
    draws: int = 0
    bts: int = 0
    start_year: int = 0
    end_year: int = 0

    def total_games(self) -> int:
        return self.home_wins + self.draws + self.away_wins

    def update(self, scoreline: str):
        if scoreline != '?-?':
            left, right = scoreline.split('-')
            left, right = int(left), int(right)

            self.goal_counts[left + right] += 1
            self.results[(left, right)] += 1

            if left > right:
                self.home_wins += 1
            elif left == right:
                self.draws += 1
            else:
                self.away_wins += 1

            if left and right:
                self.bts += 1


@dataclass
class TeamBin(DataBin):
    team_wins: int = 0
    team_losses: int = 0
    team_draws: int = 0
    scored: int = 0
    conceded: int = 0

    def update(self, scoreline: str):
        if scoreline != '?-?':
            left, right = scoreline.split('-')
            left, right = int(left), int(right)

            if left > right:
                self.team_wins += 1
            elif left == right:
                self.team_draws += 1
            else:
                self.team_losses += 1

            self.scored += left
            self.conceded += right

            super().update(scoreline)


def aggregate_seasons(data_dict: Dict, half: Half):
    bins = []
    for season_id in data_dict[StoreKeys.SEASONS.name]:
        season_dict = data_dict[season_id]
        data_bin = DataBin()
        data_bin.start_year = season_dict[StoreKeys.START_YEAR.name]
        data_bin.end_year = season_dict[StoreKeys.END_YEAR.name]

        for fixture_dict in season_dict[StoreKeys.FIXTURES.name]:
            if half == Half.full:
                result = fixture_dict[Half.full.name]
            elif half == Half.first:
                result = fixture_dict[Half.first.name]
            else:
                result = fixture_dict[Half.second.name]

            data_bin.update(result)

        if data_bin.total_games():
            bins.append(data_bin)

    return bins


def aggregate_team(data_dict: Dict, half: Half, team):
    bins = []
    for season_id in data_dict[StoreKeys.SEASONS.name]:
        season_dict = data_dict[season_id]
        data_bin = TeamBin()
        data_bin.start_year = season_dict[StoreKeys.START_YEAR.name]
        data_bin.end_year = season_dict[StoreKeys.END_YEAR.name]

        for fixture_dict in season_dict[StoreKeys.FIXTURES.name]:
            if half == Half.full:
                result = fixture_dict[Half.full.name]
            elif half == Half.first:
                result = fixture_dict[Half.first.name]
            else:
                result = fixture_dict[Half.second.name]

            if fixture_dict[Venue.away.name] == team:
                x, y = result.split('-')
                result = f'{y}-{x}'

            data_bin.update(result)

        if data_bin.total_games():
            bins.append(data_bin)

    return bins

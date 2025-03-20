import argparse
from colorama import Fore, Style
import dataclasses
from datetime import datetime, timedelta
import enum
import numpy as np
from scipy.stats import poisson
import tabulate

from cli.cli import (add_database_option,
                     add_league_option,
                     add_country_option,
                     add_logging_options,
                     set_logging_options)
from lib import messages
from model.fixtures import Fixture, Half, Scoreline
from model.competitions import League, league_register
from model.seasons import Season
from model.teams import Team
from sql.sql import load_league, load_teams


def parse_command_line():
    parser = argparse.ArgumentParser(description='Analyse games where goals are guaranteed!')
    add_database_option(parser)
    add_league_option(parser, False)
    add_country_option(parser, False)
    add_logging_options(parser)

    parser.add_argument('-l',
                        '--lower',
                        help='left-side of the time window to consider',
                        metavar='<INT>',
                        type=int,
                        default=0)

    parser.add_argument('-u',
                        '--upper',
                        help='right-side of the time window to consider',
                        metavar='<INT>',
                        type=int,
                        default=0)

    return parser.parse_args()


class Trend(enum.Enum):
    POSITIVE = '+'
    NEGATIVE = '-'
    NEUTRAL = '|'


def calculate_trend(data: list[int], window: int = 5) -> Trend:
    if len(data) < window:
        return Trend.NEUTRAL
    x = np.arange(len(data[-window:]))
    y = np.array(data[-window:])
    slope, _ = np.polyfit(x, y, 1, full=False)
    if slope > 0:
        return Trend.POSITIVE
    elif slope < 0:
        return Trend.NEGATIVE
    else:
        return Trend.NEUTRAL


def calculate_rolling_average(data: list[int], window: int = 5) -> float:
    return sum(data[-window:]) / min(len(data), window) if data else 0


def summarise_data(data: list[int], window: int = 5):
    return calculate_rolling_average(data, window), calculate_trend(data, window)


@dataclasses.dataclass
class TeamData:
    goals_for: list = dataclasses.field(default_factory=list)
    goals_against: list = dataclasses.field(default_factory=list)
    goals_total: list = dataclasses.field(default_factory=list)
    bts: list = dataclasses.field(default_factory=list)


def analyse_team(data: TeamData):
    average, trend = summarise_data(data.goals_for)
    print('F', average, trend.value)
    average, trend = summarise_data(data.goals_against)
    print('A', average, trend.value)


def calculate_average_goals(season: Season) -> float:
    total_goals = 0
    total_games = 0
    for fixture in season.fixtures():
        if fixture.finished:
            full_time: Scoreline = fixture.result(Half.full)
            total_goals += full_time.left + full_time.right
            total_games += 1

    return total_goals / total_games


def create_fixture_header(fixture: Fixture, home_color, away_color, league: League):
    return f'[{fixture.date.strftime('%H.%M: %d %b %Y')}] '\
           f'{home_color}{fixture.home_team.name}{Style.RESET_ALL} vs. '\
           f'{away_color}{fixture.away_team.name}{Style.RESET_ALL} '\
           f'[{str(league)}]'


def create_team_goals_header(team: Team, goals: int, goals_for_trend: Trend, goals_against_trend: Trend, color):
    return (f'Trends: {color}{goals_for_trend.value} {goals_against_trend.value}{Style.RESET_ALL}. '
            f'Expect {color}{goals}{Style.RESET_ALL} Goals For {color}{team.name}{Style.RESET_ALL}')


@dataclasses.dataclass
class Prediction:
    league: League
    fixture: Fixture
    home_expected: int
    away_expected: int
    home_trend_for: Trend
    away_trend_for: Trend
    home_trend_against: Trend
    away_trend_against: Trend
    probability_table: str


def gather_predictions(
        league: League,
        fixtures: list[Fixture],
        home_data: dict[Team, TeamData],
        away_data: dict[Team, TeamData]
) -> list[Prediction]:
    predictions = []
    for fixture in fixtures:
        home_avg_for = calculate_rolling_average(home_data[fixture.home_team].goals_for)
        home_avg_against = calculate_rolling_average(home_data[fixture.home_team].goals_against)
        away_avg_for = calculate_rolling_average(away_data[fixture.away_team].goals_for)
        away_avg_against = calculate_rolling_average(away_data[fixture.away_team].goals_against)
        home_expected = round((home_avg_for + away_avg_against) / 2)
        away_expected = round((away_avg_for + home_avg_against) / 2)
        total_expected = home_expected + away_expected

        table_data = []
        probabilities = []
        for goals in range(0, 5):
            outcome = f'{goals}'
            prob = round(poisson.pmf(goals, total_expected) * 100)
            probabilities.append(prob)

            if goals == 0:
                cum_outcome = f'== {goals}'
            else:
                cum_outcome = f'<= {goals}'
            cum_prob = sum(probabilities)

            table_data.append([outcome, prob, cum_outcome, cum_prob])

        probability_table = tabulate.tabulate(
            table_data,
            headers=["Goals", "Probability", "Event", "Cumulative"],
            tablefmt="grid"
        )

        prediction = Prediction(
            league,
            fixture,
            home_expected,
            away_expected,
            calculate_trend(home_data[fixture.home_team].goals_for),
            calculate_trend(away_data[fixture.away_team].goals_for),
            calculate_trend(home_data[fixture.home_team].goals_against),
            calculate_trend(away_data[fixture.away_team].goals_against),
            probability_table
        )

        predictions.append(prediction)

    return predictions


def output_predictions(predictions: list[Prediction]):
    predictions.sort(key=lambda prediction: (prediction.fixture.date, prediction.league.country, prediction.league.code))
    home_color = Fore.BLUE
    away_color = Fore.RED
    for prediction in predictions:
        print(create_fixture_header(prediction.fixture, home_color, away_color, prediction.league))
        print(create_team_goals_header(
            prediction.fixture.home_team,
            prediction.home_expected,
            prediction.home_trend_for,
            prediction.home_trend_against,
            home_color
        ))
        print(create_team_goals_header(
            prediction.fixture.away_team,
            prediction.away_expected,
            prediction.away_trend_for,
            prediction.away_trend_against,
            away_color
        ))
        print(prediction.probability_table)
        print()


def gather_fixtures_for_season(season: Season, left_window: datetime, right_window: datetime) -> list[Fixture]:
    fixtures = []
    for fixture in season.fixtures():
        fixture_datetime = datetime.fromisoformat(str(fixture.date)).replace(tzinfo=None)
        if left_window <= fixture_datetime and (right_window is None or fixture_datetime <= right_window):
            fixtures.append(fixture)
    return fixtures


def gather_data_for_season(season: Season) -> tuple[dict[Team, TeamData], dict[Team, TeamData]]:
    team: Team
    home_data = {}
    away_data = {}
    for team in season.teams():
        home_data[team] = TeamData()
        away_data[team] = TeamData()

    for team, fixtures in season.fixtures_per_team().items():
        for fixture in fixtures:
            if fixture.finished:
                full_time: Scoreline = fixture.result(Half.full)
                if full_time is not None:
                    if fixture.home_team == team:
                        goals_for = full_time.left
                        goals_against = full_time.right
                        team_data = home_data[team]
                    else:
                        goals_for = full_time.right
                        goals_against = full_time.left
                        team_data = away_data[team]

                    team_data.goals_for.append(goals_for)
                    team_data.goals_against.append(goals_against)
                    team_data.goals_total.append(full_time.left + full_time.right)
                    team_data.bts.append(int(full_time.left > 0 and full_time.right > 0))

    return home_data, away_data


def main(args: argparse.Namespace):
    load_teams(args.database)

    leagues = []
    if args.country:
        for country in args.country:
            leagues.extend([code for code, league in league_register.items() if league.country == country.capitalize()])

    if args.league:
        leagues.extend(list(args.league))

    if not args.country and not args.league:
        leagues.extend(list(league_register.keys()))

    left_window = datetime.now() + timedelta(hours=args.lower)
    if args.upper:
        right_window = datetime.now() + timedelta(hours=args.upper)
    else:
        right_window = None

    predictions = []
    for league_code in leagues:
        league = league_register[league_code]
        load_league(args.database, league)

        seasons = Season.seasons(league)
        if not seasons:
            messages.warning_message('No season data found for {}'.format(league))
        else:
            if seasons[-1].current:
                fixtures = gather_fixtures_for_season(seasons[-1], left_window, right_window)
                if fixtures:
                    home_data, away_data = gather_data_for_season(seasons[-1])
                    league_predictions = gather_predictions(league, fixtures, home_data, away_data)
                    predictions.extend(league_predictions)

    output_predictions(predictions)


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)


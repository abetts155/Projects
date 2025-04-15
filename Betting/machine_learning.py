import argparse
import dataclasses
import datetime

import pandas as pd
from sklearn.ensemble import RandomForestClassifier
from sklearn.multioutput import MultiOutputClassifier
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report


import cli.cli
import lib.messages
import lib.structure
import model.competitions
import model.fixtures
import model.seasons
import model.tables
import model.teams


def parse_command_line():
    parser = argparse.ArgumentParser(description='Machine learning predictions')
    cli.cli.add_logging_options(parser)

    parser.add_argument('-C',
                        '--countries',
                        nargs='+',
                        type=str,
                        help='pick out countries that start with this sequence')

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
                        default=7 * 24)

    return parser.parse_args()


def fixtures_within_the_window(
        fixtures: list[model.fixtures.Fixture],
        left_hand_side: datetime.datetime,
        right_hand_side: datetime.datetime
) -> list[model.fixtures.Fixture]:
    fixtures_to_analyse = []
    for fixture in fixtures:
        if not fixture.finished:
            fixture_datetime = datetime.datetime.fromisoformat(str(fixture.date)).replace(tzinfo=None)
            if left_hand_side <= fixture_datetime <= right_hand_side:
                fixtures_to_analyse.append(fixture)
    return fixtures_to_analyse


@dataclasses.dataclass(slots=True, frozen=True)
class VenueAndPeriod:
    venue: model.fixtures.Venue
    period: model.fixtures.Period


@dataclasses.dataclass(slots=True)
class Scorelines:
    scorelines: dict[VenueAndPeriod, list[model.fixtures.Scoreline]]

    @staticmethod
    def create() -> "Scorelines":
        return Scorelines(
            {
                VenueAndPeriod(venue, period): []
                for venue in model.fixtures.Venue for period in model.fixtures.Period.regular()
            }
        )


def collect_data(
        league: model.competitions.Competition,
        season: model.seasons.Season,
        data: dict[model.teams.Team, Scorelines]
):
    database = lib.structure.get_database(league.country)
    fixtures = model.seasons.load_fixtures(database, league, season)
    teams = model.fixtures.teams(fixtures)
    for team in teams:
        if team not in data:
            data[team] = Scorelines.create()

    for fixture in fixtures:
        if fixture.finished:
            home_data = data[fixture.home_team]
            away_data = data[fixture.away_team]

            for period in model.fixtures.Period.regular():
                scoreline = fixture.result(period)
                if scoreline is not None:
                    home_canonical = model.fixtures.canonicalise_scoreline(fixture, fixture.home_team, scoreline)
                    away_canonical = model.fixtures.canonicalise_scoreline(fixture, fixture.away_team, scoreline)

                    home_key = VenueAndPeriod(model.fixtures.Venue.HOME, period)
                    away_key = VenueAndPeriod(model.fixtures.Venue.AWAY, period)
                    anywhere_key = VenueAndPeriod(model.fixtures.Venue.ANYWHERE, period)

                    home_data.scorelines[home_key].append(home_canonical)
                    home_data.scorelines[anywhere_key].append(home_canonical)
                    away_data.scorelines[away_key].append(away_canonical)
                    away_data.scorelines[anywhere_key].append(away_canonical)


@dataclasses.dataclass(slots=True)
class TeamStats:
    games_played: int
    games_scored: float
    games_conceded: float
    bts: float
    over_1_5: float
    over_2_5: float
    avg_goals_for: float
    avg_goals_against: float
    avg_total_goals: float


def compute_team_stats(scorelines: list[model.fixtures.Scoreline]) -> TeamStats:
    games_played = len(scorelines)
    games_scored = sum(1 for s in scorelines if s.left > 0)
    games_conceded = sum(1 for s in scorelines if s.right > 0)
    bts = sum(1 for s in scorelines if s.left > 0 and s.right > 0)
    over_1_5 = sum(1 for s in scorelines if s.left + s.right > 1.5)
    over_2_5 = sum(1 for s in scorelines if s.left + s.right > 2.5)

    total_goals_for = sum(s.left for s in scorelines)
    total_goals_against = sum(s.right for s in scorelines)
    total_goals = total_goals_for + total_goals_against

    return TeamStats(
        games_played,
        games_scored / games_played,
        games_conceded / games_played,
        bts / games_played,
        over_1_5 / games_played,
        over_2_5 / games_played,
        total_goals_for / games_played,
        total_goals_against / games_played,
        total_goals / games_played
    )


def generate_training_examples(data: dict[model.teams.Team, Scorelines]) -> pd.DataFrame:
    rows = []

    for home_team, home_scores in data.items():
        for away_team, away_scores in data.items():
            if home_team == away_team:
                continue

            home_key = VenueAndPeriod(model.fixtures.Venue.HOME, model.fixtures.Period.FULL)
            away_key = VenueAndPeriod(model.fixtures.Venue.AWAY, model.fixtures.Period.FULL)

            home_recent = home_scores.scorelines[home_key][-10:]
            away_recent = away_scores.scorelines[away_key][-10:]

            home_stats = compute_team_stats(home_recent)
            away_stats = compute_team_stats(away_recent)

            avg_total_goals = home_stats.avg_total_goals + away_stats.avg_total_goals
            avg_bts = (home_stats.bts + away_stats.bts) / 2

            row = {
                "home_team": home_team.name,
                "away_team": away_team.name,

                "home_games_scored": home_stats.games_scored,
                "home_games_conceded": home_stats.games_conceded,
                "home_bts": home_stats.bts,
                "home_over_1_5": home_stats.over_1_5,
                "home_over_2_5": home_stats.over_2_5,
                "home_avg_goals_for": home_stats.avg_goals_for,
                "home_avg_goals_against": home_stats.avg_goals_against,
                "home_avg_total_goals": home_stats.avg_total_goals,

                "away_games_scored": away_stats.games_scored,
                "away_games_conceded": away_stats.games_conceded,
                "away_bts": away_stats.bts,
                "away_over_1_5": away_stats.over_1_5,
                "away_over_2_5": away_stats.over_2_5,
                "away_avg_goals_for": away_stats.avg_goals_for,
                "away_avg_goals_against": away_stats.avg_goals_against,
                "away_avg_total_goals": away_stats.avg_total_goals,

                "target_btts": int(avg_bts > 0.5),
                "target_over_1_5": int(avg_total_goals > 1.5),
                "target_over_2_5": int(avg_total_goals > 2.5),
                "target_under_1_5": int(avg_total_goals < 1.5),
                "target_under_2_5": int(avg_total_goals < 2.5),
            }

            rows.append(row)

    return pd.DataFrame(rows)


def train_multi_output_model(df: pd.DataFrame) -> MultiOutputClassifier:
    feature_cols = [col for col in df.columns if col.startswith("home_") or col.startswith("away_")]
    target_cols = [
        "target_btts",
        "target_over_1_5",
        "target_over_2_5",
        "target_under_1_5",
        "target_under_2_5"
    ]

    X = df[feature_cols]
    y = df[target_cols]

    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

    base_model = RandomForestClassifier(n_estimators=100, random_state=42)
    model = MultiOutputClassifier(base_model)
    model.fit(X_train, y_train)

    # Optional: evaluate performance
    y_pred = model.predict(X_test)
    print(classification_report(y_test, y_pred, target_names=target_cols))

    return model


def predict_fixture_multi(fixture, data, model) -> dict:
    home_key = VenueAndPeriod(model.fixtures.Venue.HOME, model.fixtures.Period.FULL)
    away_key = VenueAndPeriod(model.fixtures.Venue.AWAY, model.fixtures.Period.FULL)

    home_recent = data[fixture.home_team].scorelines[home_key][-10:]
    away_recent = data[fixture.away_team].scorelines[away_key][-10:]

    home_stats = compute_team_stats(home_recent)
    away_stats = compute_team_stats(away_recent)

    features = {
        "home_games_scored": home_stats.games_scored,
        "home_games_conceded": home_stats.games_conceded,
        "home_bts": home_stats.bts,
        "home_over_1_5": home_stats.over_1_5,
        "home_over_2_5": home_stats.over_2_5,
        "home_avg_goals_for": home_stats.avg_goals_for,
        "home_avg_goals_against": home_stats.avg_goals_against,
        "home_avg_total_goals": home_stats.avg_total_goals,

        "away_games_scored": away_stats.games_scored,
        "away_games_conceded": away_stats.games_conceded,
        "away_bts": away_stats.bts,
        "away_over_1_5": away_stats.over_1_5,
        "away_over_2_5": away_stats.over_2_5,
        "away_avg_goals_for": away_stats.avg_goals_for,
        "away_avg_goals_against": away_stats.avg_goals_against,
        "away_avg_total_goals": away_stats.avg_total_goals,
    }

    import pandas as pd
    input_df = pd.DataFrame([features])
    predictions = model.predict(input_df)[0]

    target_names = [
        "BTTS",
        "Over 1.5",
        "Over 2.5",
        "Under 1.5",
        "Under 2.5"
    ]
    return dict(zip(target_names, predictions))


def main(country_prefixes: list[str], hours_before: int, hours_after: int):
    lib.structure.purge_png_files(lib.structure.tweet_png_directory)

    left_window = datetime.datetime.now() + datetime.timedelta(hours=hours_before)
    right_window = datetime.datetime.now() + datetime.timedelta(hours=hours_after)

    whitelisted = model.competitions.get_whitelisted_competitions(model.competitions.CompetitionType.LEAGUE)
    if country_prefixes:
        whitelisted = [
            c for c in whitelisted for prefix in country_prefixes if c.country.casefold().startswith(prefix.casefold())
        ]

    for league in whitelisted:
        database = lib.structure.get_database(league.country)
        season = model.seasons.load_current_season(database, league)
        fixtures = model.seasons.load_fixtures(database, league, season)
        fixtures_to_analyse = fixtures_within_the_window(fixtures, left_window, right_window)

        if fixtures_to_analyse:
            lib.messages.vanilla_message(f"Analsying {league} (id={league.id})")

            seasons = model.seasons.load_seasons(database, league)
            historical_data: dict[model.teams.Team, Scorelines] = {}
            for season in seasons:
                collect_data(league, season, historical_data)

            df = generate_training_examples(historical_data)
            print(df)
            multi_model = train_multi_output_model(df)

            for fixture in fixtures_to_analyse:
                preds = predict_fixture_multi(fixture, historical_data, multi_model)
                print(f"{fixture.home_team.name} vs {fixture.away_team.name}: {preds}")


if __name__ == '__main__':
    args = parse_command_line()
    cli.cli.set_logging_options(args)
    main(args.countries, args.lower, args.upper)

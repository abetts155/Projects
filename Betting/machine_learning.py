import argparse
import dataclasses
import datetime

import pandas as pd
import shap
import matplotlib.pyplot as plt
from lightgbm import LGBMClassifier
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

    return parser.parse_args()


def main(country_prefixes: list[str]):
    whitelisted = model.competitions.get_whitelisted_competitions(model.competitions.CompetitionType.LEAGUE)
    if country_prefixes:
        whitelisted = [
            c for c in whitelisted for prefix in country_prefixes if
            c.country.casefold().startswith(prefix.casefold())
        ]

    for league in whitelisted:
        database = lib.structure.get_database(league.country)
        season = model.seasons.load_current_season(database, league)
        fixtures = model.seasons.load_fixtures(database, league, season)
        fixtures_to_analyse = model.fixtures.load_fixtures_within_window(database, league, 2, 24)

        if fixtures_to_analyse:
            lib.messages.vanilla_message(f"Analsying {league} (id={league.id})")

            data = {}
            home_data = {}
            away_data = {}
            seasons = model.seasons.load_seasons(database, league)
            for season in seasons:
                fixtures = model.seasons.load_fixtures(database, league, season)
                for fixture in fixtures:
                    if fixture.finished:
                        if fixture.home_team not in data:
                            data[fixture.home_team] = []
                        if fixture.home_team not in home_data:
                            home_data[fixture.home_team] = []

                        if fixture.away_team not in data:
                            data[fixture.away_team] = []
                        if fixture.away_team not in away_data:
                            away_data[fixture.away_team] = []

                        data[fixture.home_team].append(fixture)
                        data[fixture.away_team].append(fixture)
                        home_data[fixture.home_team].append(fixture)
                        away_data[fixture.away_team].append(fixture)

            window = 5
            for team, fixtures in data.items():
                if len(fixtures) >= window:
                    upper = len(fixtures) - window + 1
                    for i in range(upper):
                        scores = []
                        for fixture in fixtures[i:i + window]:
                            score = fixture.result(model.fixtures.Period.FULL)
                            if fixture.home_team == team:
                                scores.append(score)
                            else:
                                scores.append(score.reverse())

                        bts = round(100 * sum(1 for s in scores if s.left > 0 and s.right > 0) / window, 1)
                        scored = round(100 * sum(1 for s in scores if s.left > 0) / window, 1)
                        conceded = round(100 * sum(1 for s in scores if s.right > 0) / window, 1)
                        over_2_5 = round(100 * sum(1 for s in scores if s.left > 0 + s.right > 2) / window, 1)

                        if bts >= 80:
                            print('BTS', team, bts)

                        if over_2_5 >= 80:
                            print('Over 2.5', team, over_2_5)




if __name__ == '__main__':
    args = parse_command_line()
    cli.cli.set_logging_options(args)
    main(args.countries)

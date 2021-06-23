from argparse import ArgumentParser, Namespace
from cli.cli import (add_database_option,
                     add_history_option,
                     add_league_option,
                     add_half_option,
                     add_logging_options,
                     add_team_option,
                     add_events_option,
                     set_logging_options,
                     get_unique_event)
from model.fixtures import Event, Fixture, Half, Result, win, defeat, draw
from model.leagues import league_register
from model.seasons import Season
from model.tables import LeagueTable
from model.teams import Team
from sql.sql import load_league, load_teams
from typing import Callable, List

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
from sklearn.linear_model import LogisticRegression
from sklearn import tree
from sklearn.ensemble import RandomForestClassifier
from sklearn.svm import SVC
from sklearn.neighbors import KNeighborsClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn import metrics


def parse_command_line():
    parser = ArgumentParser(description='Machine learning trials')
    add_database_option(parser)
    add_history_option(parser)
    add_league_option(parser)
    add_team_option(parser)
    add_half_option(parser)
    add_events_option(parser, number=1)
    add_logging_options(parser)
    return parser.parse_args()


class HistorySummary:
    __slots__ = ['scored', 'conceded']

    def __init__(self):
        self.scored = 0
        self.conceded = 0

    def add(self, result: Result, func: Callable):
        self.scored += result.left
        self.conceded += result.right

        if func(result):
            pass

    def unpack(self):
        return [self.scored, self.conceded]

    def __str__(self):
        return 'F={:<2} A={:<2}'.format(self.scored, self.conceded)

    @classmethod
    def size(cls):
        return len(cls.__slots__)


def collapse(team: Team, form: List[Fixture], func: Callable) -> HistorySummary:
    summary = HistorySummary()
    for fixture in form:
        result = fixture.full_time()
        result = fixture.canonicalise_result(team, result)
        summary.add(result, func)
    summary.scored /= len(form)
    summary.conceded /= len(form)
    return summary


def collect_data(seasons: List[Season], func: Callable, data: List, history: List, cutoff: int = 5):
    for season in seasons:
        home_fixtures = {}
        away_fixtures = {}
        season.sort_fixtures()
        for fixture in season.fixtures():
            home_fixtures.setdefault(fixture.home_team, []).append(fixture)
            away_fixtures.setdefault(fixture.away_team, []).append(fixture)

        for fixture in season.fixtures():
            if fixture.first_half() and fixture.full_time():
                home_history = home_fixtures[fixture.home_team]
                home_index = home_history.index(fixture)
                away_history = away_fixtures[fixture.away_team]
                away_index = away_history.index(fixture)

                if home_index >= cutoff and away_index >= cutoff:
                    home_window = home_history[home_index - cutoff:home_index]
                    home_summary = collapse(fixture.home_team, home_window, func)

                    away_window = away_history[away_index - cutoff:away_index]
                    away_summary = collapse(fixture.away_team, away_window, func)

                    row = []
                    for x, y in zip(home_summary.unpack(), away_summary.unpack()):
                        row.extend([x, y])

                    if func(fixture.full_time()):
                        row.append(1)
                    else:
                        row.append(0)
                    data.append(row)
                    history.append(fixture)


def main(args: Namespace):
    load_teams(args.database)

    func = Event.get(get_unique_event(args))
    train_data = []
    train_history = []
    for code in args.league:
        league = league_register[code]
        load_league(args.database, league)

        seasons = Season.seasons(league)
        *seasons, _ = seasons
        collect_data(seasons, func, train_data, train_history)

    dependent_variable = Event.name(func, False)
    df = pd.DataFrame.from_records(train_data)
    cols = []
    for x in HistorySummary.__slots__:
        cols.extend(['H({})'.format(x), 'A({})'.format(x)])
    cols.append(dependent_variable)
    df.columns = cols

    plt.figure()

    y_train = df[dependent_variable]
    x_train = df.drop([dependent_variable], axis=1)

    rf = RandomForestClassifier()
    rf.fit(x_train, y_train)
    print ("Features sorted by their score:")
    print (sorted(zip(map(lambda x: round(x, 4), rf.feature_importances_), x_train), reverse=True))

    model = RandomForestClassifier()
    # Fit the model
    model.fit(x_train, y_train)
    # Accuracy
    score = model.score(x_train, y_train)
    print(score)

    test_data = []
    test_history = []
    for code in args.league:
        league = league_register[code]
        load_league(args.database, league)

        seasons = Season.seasons(league)
        *_, this_season = seasons
        collect_data([this_season], func, test_data, test_history)

    df = pd.DataFrame.from_records(test_data)
    cols = []
    for x in HistorySummary.__slots__:
        cols.extend(['H({})'.format(x), 'A({})'.format(x)])
    cols.append(dependent_variable)
    df.columns = cols

    y_test = df[dependent_variable]
    x_test = df.drop([dependent_variable], axis=1)

    predicted = list(model.predict(x_test))

    true_positives = []
    true_negatives = []
    false_positives = []
    false_negatives = []
    for i in range(len(y_test)):
        if y_test[i] and predicted[i]:
            true_positives.append(test_history[i])
        elif not y_test[i] and not predicted[i]:
            true_negatives.append(test_history[i])
        elif not y_test[i] and predicted[i]:
            false_positives.append(test_history[i])
        elif y_test[i] and not predicted[i]:
            false_negatives.append(test_history[i])

    print()

    print('>>>>>>>>>>>>>>>>>>> True positives')
    for fixture in true_positives:
        print(fixture)

    print()

    print('>>>>>>>>>>>>>>>>>>> True negatives')
    for fixture in true_negatives:
        print(fixture)

    print()

    print('>>>>>>>>>>>>>>>>>>> False positives')
    for fixture in false_positives:
        print(fixture)

    print()

    print('>>>>>>>>>>>>>>>>>>> False negatives')
    for fixture in false_negatives:
        print(fixture)

    print('TP={}  TN={}  FP={}  FN={}'.format(len(true_positives),
                                              len(true_negatives),
                                              len(false_positives),
                                              len(false_negatives)))


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)

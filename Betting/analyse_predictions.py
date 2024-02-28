from argparse import ArgumentParser, Namespace
from arrow import get as sanitise_date
from datetime import datetime
from itertools import accumulate
from matplotlib import pyplot as plt
from matplotlib.dates import DateFormatter
from matplotlib.ticker import MaxNLocator
from typing import Dict, List, KeysView

from cli.cli import (add_database_option,
                     add_logging_options,
                     set_logging_options)
from lib.helpful import DisplayGrid, set_matplotlib_defaults
from model.fixtures import Fixture, Half
from model.predictions import Prediction, create_prediction_from_row
from sql.sql import ColumnNames, Database, load_teams, get_fixtures


def parse_command_line():
    parser = ArgumentParser(description='Analyse predictions made by the Goal Gambling God')
    add_database_option(parser)
    add_logging_options(parser)

    parser.add_argument('-L',
                        '--left',
                        type=lambda s: datetime.strptime(s, '%d.%m.%Y'),
                        help='Only consider fixtures from this date (given in DD.MM.YYYY format)',
                        default=datetime.strptime('1.1.2000', '%d.%m.%Y'))

    parser.add_argument('-R',
                        '--right',
                        type=lambda s: datetime.strptime(s, '%d.%m.%Y'),
                        help='Only consider fixtures up to this date (given in DD.MM.YYYY format)',
                        default=datetime.strptime('1.1.3000', '%d.%m.%Y'))

    return parser.parse_args()


def check_prediction(prediction: Prediction, fixture: Fixture) -> bool:
    if prediction.half == Half.first:
        result = fixture.first_half()
    elif prediction.half == Half.second:
        result = fixture.second_half()
    else:
        result = fixture.full_time()
    return prediction.event(result)


def gather_fixtures_and_predictions(database, left: datetime, right: datetime) -> Dict[Fixture, Prediction]:
    load_teams(database)
    fixtures_to_predictions = {}
    with Database(database) as db:
        predictions_rows = db.fetch_all_rows(Prediction.sql_table())
        for row in predictions_rows:
            prediction = create_prediction_from_row(row)
            fixture_constraint = "{}={}".format(ColumnNames.ID.name, prediction.fixture_id)
            (fixture,) = get_fixtures(db, [fixture_constraint])
            if left.date() <= fixture.date.date() <= right.date():
                fixtures_to_predictions[fixture] = prediction
    return fixtures_to_predictions


def sort_fixtures_by_date(keys: KeysView) -> List[Fixture]:
    return sorted(keys, key=lambda x: x.date.date())


def calculate_rois(fixtures_to_predictions: Dict[Fixture, Prediction]):
    x_values = []
    stakes = []
    returns = []
    stake = 1
    fixtures = sort_fixtures_by_date(fixtures_to_predictions.keys())
    for fixture in fixtures:
        if not x_values or x_values[-1].date() < fixture.date.date():
            x_values.append(fixture.date)
            stakes.append(0)
            returns.append(0)

        stakes[-1] += stake
        prediction = fixtures_to_predictions[fixture]
        if check_prediction(prediction, fixture):
            returns[-1] += stake * prediction.odds

    cumulative_stakes = list(accumulate(stakes))
    cumulative_returns = list(accumulate(returns))
    y_values = []
    z_values = []
    for staked, returned in zip(cumulative_stakes, cumulative_returns):
        roi = 100 * (returned - staked) / staked
        y_values.append(roi)
        z_values.append(5)

    return x_values, y_values, z_values


def create_roi_axis(fixtures_to_predictions: Dict[Fixture, Prediction], ax):
    x_values, y_values, z_values = calculate_rois(fixtures_to_predictions)
    date_format = DateFormatter('%d-%m-%Y')
    ax.xaxis.set_major_formatter(date_format)
    ax.set_xticks([x_values[0], x_values[-1]])
    ax.plot(x_values, y_values, color='dodgerblue', zorder=1)
    ax.scatter(x_values, y_values, color='white', s=z_values, zorder=2)
    ax.yaxis.set_major_locator(MaxNLocator(nbins='auto', steps=[1, 2, 5, 10]))
    ax.spines['right'].set_visible(False)
    ax.spines['top'].set_visible(False)
    best_roi = max(y_values)
    ax.set_title('Tracking ROI over time with uniform staking (e.g. £10 per bet). Peak ROI: {:.1f}%.'.format(best_roi))
    ax.set_xlabel('Date')
    ax.set_ylabel('ROI (percentage)')


def add_to_bin(bins, odds):
    for key in bins:
        if key[0] <= odds < key[1]:
            bins[key] += 1


def create_odds_axis(fixtures_to_predictions: Dict[Fixture, Prediction], ax):
    bins = [(1, 1.2), (1.2, 1.5), (1.5, 1.75), (1.75, 2), (2, 1000)]
    winning_bins = {}
    losing_bins = {}
    for key in bins:
        winning_bins[key] = 0
        losing_bins[key] = 0

    for fixture, prediction in fixtures_to_predictions.items():
        if check_prediction(prediction, fixture):
            add_to_bin(winning_bins, prediction.odds)
        else:
            add_to_bin(losing_bins, prediction.odds)

    x_values = []
    y_values = []
    for i in range(2):
        for key in bins:
            if i == 0:
                x_values.append('Win\n[{}, {})'.format(*key))
                y_values.append(winning_bins[key])
            else:
                x_values.append('Loss\n[{}, {})'.format(*key))
                y_values.append(losing_bins[key])

    cmap = plt.get_cmap('Wistia')
    min_y = min(y_values)
    max_y = max(y_values)
    scaled = [(y - min_y) / (max_y - min_y) for y in y_values]
    ax.bar(x_values, y_values, width=0.3, align='center', color=cmap(scaled), zorder=1)
    for x, y in enumerate(y_values):
        ax.text(x, y, str(y), ha='center', zorder=2)

    ax.axvline(4.5, lw=1, ls='dashed')
    ax.set_yticks([])
    ax.set_frame_on(False)
    ax.set_title('Breakdown of winning and losing bets by decimal odds')


def display(fixtures_to_predictions: Dict[Fixture, Prediction]):
    fig, axs = plt.subplots(nrows=1, ncols=2, constrained_layout=True)
    create_roi_axis(fixtures_to_predictions, axs[0])
    create_odds_axis(fixtures_to_predictions, axs[1])

    fixtures = sort_fixtures_by_date(fixtures_to_predictions.keys())
    start_date = fixtures[0].date
    end_date = fixtures[-1].date
    title = 'Performance analysis:'
    if start_date.month == end_date.month and start_date.year == end_date.year:
        title += ' {}'.format(sanitise_date(start_date).format('Do MMMM'))
    elif start_date.year == end_date.year:
        title += ' {}'.format(sanitise_date(start_date).format('Do MMMM'))
    else:
        title += ' {}'.format(sanitise_date(start_date).format('Do MMMM YYYY'))
    title += ' to {}'.format(sanitise_date(end_date).format('Do MMMM YYYY'))

    fig.suptitle(title, fontweight='bold')
    plt.show()


def main(args: Namespace):
    set_matplotlib_defaults()
    fixtures_to_predictions = gather_fixtures_and_predictions(args.database, args.left, args.right)
    display(fixtures_to_predictions)


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)

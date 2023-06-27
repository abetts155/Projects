from argparse import ArgumentParser, Namespace
from arrow import get as sanitise_date
from collections import OrderedDict
from datetime import date
from matplotlib import pyplot as plt
from matplotlib.dates import DateFormatter
from matplotlib.ticker import MaxNLocator

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
    return parser.parse_args()


def check_prediction(prediction: Prediction, fixture: Fixture) -> bool:
    if prediction.half == Half.first:
        result = fixture.first_half()
    elif prediction.half == Half.second:
        result = fixture.second_half()
    else:
        result = fixture.full_time()

    return prediction.event(result)


def gather_fixtures_and_predictions(database):
    load_teams(database)
    fixtures_and_predictions = []
    with Database(database) as db:
        predictions_rows = db.fetch_all_rows(Prediction.sql_table())

        for row in predictions_rows:
            prediction = create_prediction_from_row(row)
            print(prediction.odds)
            fixture_constraint = "{}={}".format(ColumnNames.ID.name, prediction.fixture_id)
            (fixture,) = get_fixtures(db, [fixture_constraint])
            fixtures_and_predictions.append([fixture, prediction])

    return sorted(fixtures_and_predictions, key=lambda x: (x[0].date.date(), x[0].date.time()))


def calculate_rois(fixtures_and_predictions):
    start = fixtures_and_predictions[0]
    x_value = date(day=1, month=start[0].date.month, year=start[0].date.year)
    data = [(x_value, 0, 0)]

    stake = 10
    total_return = 0
    for total, (fixture, prediction) in enumerate(fixtures_and_predictions, start=1):
        if check_prediction(prediction, fixture):
            total_return += stake * prediction.odds
        total_spend = stake * total
        profit = total_return - total_spend
        x_value = date(day=fixture.date.day, month=fixture.date.month, year=fixture.date.year)
        y_value = 100 * profit / total_spend
        z_value = 1
        data.append((x_value, y_value, z_value))
        print('Staked: £{}  Profit: £{}  ROI: {:.2f}%'.format(total_spend, profit, y_value))

    data.append((date.today(), data[-1][1], 0))
    return data


def create_roi_axis(fixtures_and_predictions, ax):
    data = calculate_rois(fixtures_and_predictions)
    date_format = DateFormatter('%d-%m-%Y')
    ax.xaxis.set_major_formatter(date_format)
    x_values, y_values, z_values = zip(*data)
    ax.set_xticks([x_values[0], x_values[-1]])
    ax.plot(x_values, y_values, color='dodgerblue', zorder=1)
    ax.scatter(x_values, y_values, color='white', s=z_values, zorder=2)
    ax.set_ylim(0, max(y_values) * 1.1)
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


def create_odds_axis(fixtures_and_predictions, ax):
    bins = [(1, 1.2), (1.2, 1.5), (1.5, 1.75), (1.75, 2), (2, 1000)]
    winning_bins = {}
    losing_bins = {}
    for key in bins:
        winning_bins[key] = 0
        losing_bins[key] = 0

    for fixture, prediction in fixtures_and_predictions:
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


def display(fixtures_and_predictions):
    fig, axs = plt.subplots(nrows=1, ncols=2, constrained_layout=True)
    create_roi_axis(fixtures_and_predictions, axs[0])
    create_odds_axis(fixtures_and_predictions, axs[1])

    start_date = fixtures_and_predictions[0][0].date
    end_date = fixtures_and_predictions[-1][0].date
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
    fixtures_and_predictions = gather_fixtures_and_predictions(args.database)
    display(fixtures_and_predictions)


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)

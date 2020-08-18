import argparse
import json
import os

from miscellaneous import messages
from model import competitions, events, matches, teams
from sql import sql


def parse_command_line():
    parser = argparse.ArgumentParser(description='Do football analytics')

    parser.add_argument(dest='data',
                        help='the directory where the data reside',
                        metavar='<DIR>')

    countries = ['England', 'France', 'Germany', 'Italy', 'Spain']
    parser.add_argument('--country',
                        help='choose the country to analyse',
                        choices=countries,
                        metavar='<COUNTRY>',
                        type=str.capitalize,
                        required=True)

    parser.add_argument('--database',
                        help='read from this database',
                        metavar='<DATABASE>',
                        required=True)

    return parser.parse_args()


def main(arguments: argparse.Namespace):
    for match in matches.Match.inventory.values():
        if match.home_team.name == 'Manchester City' or match.away_team.name == 'Manchester City':
            print(match)
            match.compute_metrics()
            first_half_metrics = match.metrics(events.Period.FIRST_HALF)
            second_half_metrics = match.metrics(events.Period.SECOND_HALF)
            match_metrics = first_half_metrics + second_half_metrics
            print('HOME={:.2f}% shots={} corners={}'.format(match_metrics.possession(matches.Side.HOME),
                                                            match_metrics.query(matches.Side.HOME, matches.Metric.SHOTS),
                                                            match_metrics.query(matches.Side.HOME, matches.Metric.CORNERS)))
            print('AWAY={:.2f} shots={} corners={}'.format(match_metrics.possession(matches.Side.AWAY),
                                                           match_metrics.query(matches.Side.AWAY, matches.Metric.SHOTS),
                                                           match_metrics.query(matches.Side.AWAY, matches.Metric.CORNERS)))


if __name__ == '__main__':
    main(parse_command_line())

from argparse import ArgumentParser, Namespace
from typing import Callable, List

from cli.cli import (add_database_option,
                     add_country_option,
                     add_league_option,
                     add_logging_options,
                     set_logging_options)
from lib import messages
from model.fixtures import Event, bts
from model.leagues import league_register
from model.seasons import Season
from sql.sql import load_league, load_teams


def parse_command_line():
    parser = ArgumentParser(description='Identify high or low scoring teams')
    add_database_option(parser)
    add_league_option(parser, False)
    add_country_option(parser, False)
    add_logging_options(parser)
    return parser.parse_args()


def analyse_season(season: Season, event: Callable):
    data = {}
    games = {}
    for team in season.teams():
        data[team] = 0
        games[team] = 0

    for fixture in season.fixtures():
        if fixture.finished:
            games[fixture.home_team] += 1
            games[fixture.away_team] += 1

            if fixture.full_time() and event(fixture.full_time()):
                data[fixture.home_team] += 1
                data[fixture.away_team] += 1

    percentages = {}
    for team in season.teams():
        if games[team]:
            percentages[team] = round(100 * data[team] / games[team])

    return percentages


def find_bin(bins: List, n: int):
    i = 0
    while i < len(bins):
        lower, upper = bins[i]
        if lower <= n <= upper:
            return i
        i += 1
    assert False


def analyse_event(seasons: List[Season], event: Callable):
    data = {'Bins': [], 'Values': [], 'Teams': []}
    step = 10
    for n in range(0, 100, step):
        data['Bins'].append((n, n + step))
        data['Values'].append(0)
        data['Teams'].append([])

    for season in seasons[:-1]:
        percentages = analyse_season(season, event)
        for p in percentages.values():
            i = find_bin(data['Bins'], p)
            data['Values'][i] += 1

    percentages = analyse_season(seasons[-1], event)
    for team, p in percentages.items():
        i = find_bin(data['Bins'], p)
        data['Teams'][i].append(team.name)

    print(Event.name(event, False))
    for i, (lower, upper) in enumerate(data['Bins']):
        value = data['Values'][i]
        print('{} {:>2}-{:>3} | {:>3} | {}'.format('*' if value <= 10 else ' ',
                                                   lower,
                                                   upper,
                                                   value,
                                                   ' : '.join(data['Teams'][i])))
    print()


def main(args: Namespace):
    load_teams(args.database)

    leagues = []
    if args.country:
        for country in args.country:
            leagues.extend([code for code, league in league_register.items() if league.country == country.capitalize()])

    if args.league:
        leagues.extend(list(args.league))

    if not args.country and not args.league:
        leagues.extend(list(league_register.keys()))

    for league_code in leagues:
        league = league_register[league_code]
        load_league(args.database, league)

        seasons = Season.seasons(league)
        if not seasons:
            messages.warning_message('No season data found for {}'.format(league))
        else:
            print('-' * 80)
            print(league.name)
            print('-' * 80)
            if seasons[-1].current:
                for event in [Event.get('gfa_gt_2'), bts, Event.get('gfa_le_1')]:
                    analyse_event(seasons, event)
            else:
                messages.warning_message('The current season for {} has not yet started'.format(league.name))


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)

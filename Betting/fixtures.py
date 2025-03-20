from argparse import ArgumentParser, Namespace
from cli.cli import (add_database_option,
                     add_logging_options,
                     set_logging_options,
                     add_league_option,
                     add_country_option)
from colorama import Fore, Style
from datetime import datetime, timedelta
from model.fixtures import Half, Fixture, Venue
from model.competitions import league_register, League, prettify
from model.teams import create_team_from_row, Team
from sql.sql import Database, get_fixtures, get_current_season
from sql.sql_columns import ColumnNames
from sql.sql_language import Characters
from typing import List, Tuple


def parse_command_line():
    parser = ArgumentParser(description='Show fixtures for specified date')
    add_database_option(parser)
    add_logging_options(parser)
    add_league_option(parser, False)
    add_country_option(parser, False)

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
                        default=24)

    parser.add_argument('-T',
                        '--time',
                        action='store_true',
                        help='sort by time rather than by league',
                        default=False)

    return parser.parse_args()


def filter_fixtures(fixtures: List[Fixture], left_window: datetime, right_window: datetime):
    filtered = []
    for fixture in fixtures:
        match_date = datetime.fromisoformat(str(fixture.date)).replace(tzinfo=None)
        if left_window <= match_date <= right_window:
            filtered.append(fixture)
    filtered.sort(key=lambda fixture: (fixture.date.date(), fixture.date.time()))
    return filtered


def get_league_header(league: League) -> str:
    delimiter = '*' * (len(str(league)) + 2)
    return "{}\n {} \n{}".format(delimiter, league, delimiter)


def get_fixture_header(fixture: Fixture, home_color, away_color) -> str:
    return '[{}] {}{}{} vs. {}{}{}'.format(fixture.date.strftime('%d-%m-%Y %H.%M'),
                                           home_color, fixture.home_team.name, Style.RESET_ALL,
                                           away_color, fixture.away_team.name, Style.RESET_ALL)


def output_fixtures(league: League, fixtures: List[Fixture]):
    print(get_league_header(league))
    for fixture in fixtures:
        print(get_fixture_header(fixture, Fore.BLUE, Fore.RED))
    print()


def main(args: Namespace):
    leagues = []
    if args.country:
        for country in args.country:
            leagues.extend([code for code, league in league_register.items() if league.country == country.capitalize()])

    if args.league:
        leagues.extend(list(args.league))

    if not args.country and not args.league:
        leagues.extend(list(league_register.keys()))

    now = datetime.now()
    left_window = datetime.now() + timedelta(hours=args.lower)
    right_window = datetime.now() + timedelta(hours=args.upper)

    league_fixtures = {}
    with Database(args.database) as db:
        team_rows = db.fetch_all_rows(Team.sql_table())
        for row in team_rows:
            create_team_from_row(row)

        for league_code in leagues:
            league = league_register[league_code]
            season = get_current_season(db, league)

            if season is not None:
                season_id = season[0]
                season_constraint = "{}={}".format(ColumnNames.Season_ID.name, season_id)
                finished_constraint = "{}={}".format(ColumnNames.Finished.name, Characters.FALSE.value)
                constraints = [season_constraint, finished_constraint]
                fixtures = get_fixtures(db, constraints)
                fixtures = filter_fixtures(fixtures, left_window, right_window)
                if fixtures:
                    league_fixtures[league] = fixtures

    if args.time:
        flat = []
        for league, fixtures in league_fixtures.items():
            for fixture in fixtures:
                flat.append((league, fixture))
        flat.sort(key=lambda tup: (tup[1].date, tup[0].country, tup[0].code))

        last_league = None
        for league, fixture in flat:
            if last_league is None:
                print(get_league_header(league))
            elif last_league != league:
                print()
                print(get_league_header(league))

            print(get_fixture_header(fixture, Fore.BLUE, Fore.RED))
            last_league = league
        print()
    else:
        for league, fixtures in league_fixtures.items():
            output_fixtures(league, fixtures)


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)

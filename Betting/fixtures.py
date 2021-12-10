from argparse import ArgumentParser, Namespace
from cli.cli import (add_database_option,
                     add_venue_option,
                     add_events_option,
                     add_minimum_option,
                     add_logging_options,
                     set_logging_options,
                     add_league_option,
                     add_country_option,
                     add_half_option)
from datetime import datetime
from model.fixtures import Half, Fixture, Venue
from model.leagues import league_register, League, prettify
from model.teams import create_team_from_row, Team
from pathlib import Path
from sql.sql import Database, get_fixtures, get_current_season
from sql.sql_columns import ColumnNames
from sql.sql_language import Characters
from subprocess import run
from typing import List, Tuple


def parse_command_line():
    parser = ArgumentParser(description='Show fixtures for specified date')
    add_half_option(parser)
    add_venue_option(parser)
    add_database_option(parser)
    add_minimum_option(parser)
    add_logging_options(parser)
    add_league_option(parser, False)
    add_country_option(parser, False)
    add_events_option(parser, False)

    today = datetime.today()
    parser.add_argument('-D',
                        '--day',
                        help='day of fixtures',
                        metavar='<INT>',
                        type=int,
                        default=today.day)

    parser.add_argument('-M',
                        '--month',
                        help='month of fixtures',
                        metavar='<INT>',
                        type=int,
                        default=today.month)

    parser.add_argument('-Y',
                        '--year',
                        help='month of fixtures',
                        metavar='<INT>',
                        type=int,
                        default=today.year)

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
                        default=23)

    return parser.parse_args()


def filter_fixtures(fixtures: List[Fixture], left_datetime: datetime, right_datetime: datetime):
    filtered = []
    for fixture in fixtures:
        match_date = datetime.fromisoformat(str(fixture.date)).replace(tzinfo=None)
        if left_datetime <= match_date <= right_datetime:
            filtered.append(fixture)
    filtered.sort(key=lambda fixture: (fixture.date.date(), fixture.date.time()))
    return filtered


def output_fixtures(league: League, fixtures: List[Fixture]):
    print("{} Fixtures in {} {} {}".format('*' * 80 + '\n',
                                           prettify(league.country),
                                           league.name,
                                           '\n' + '*' * 80))
    for fixture in fixtures:
        next_match_message = 'Next match: {} {} vs. {}'.format(fixture.date.strftime('%Y-%m-%d %H.%M'),
                                                               fixture.home_team.name,
                                                               fixture.away_team.name)
        print(next_match_message)
    print()


def analyse_sequences(db: Database,
                      league_code:
                      str, teams: List[Team],
                      events: List[str],
                      negate: bool,
                      venue: Venue,
                      half: Half,
                      minimum: int):
    analyse_script = Path(__file__).parent.absolute().joinpath('analyse_sequences.py')
    args = ['python3', str(analyse_script),
            '--database', db.name,
            '--no-warnings',
            '-T', ':'.join([team.name for team in teams]),
            '-E', *events,
            '-L', league_code,
            '--minimum', str(minimum),
            '--half', half.name]

    if negate:
        args.append('--negate')

    if venue:
        args.extend(['--venue', venue.name])

    run(args)


def main(args: Namespace):
    leagues = []
    if args.country:
        for country in args.country:
            leagues.extend([code for code, league in league_register.items() if league.country == country.capitalize()])

    if args.league:
        leagues.extend(list(args.league))

    if not args.country and not args.league:
        leagues.extend(list(league_register.keys()))

    left_datetime = datetime(args.year, args.month, args.day, args.lower)
    right_datetime = datetime(args.year, args.month, args.day, args.upper)

    print(left_datetime, right_datetime)

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
                fixtures = filter_fixtures(fixtures, left_datetime, right_datetime)

                if fixtures:
                    if args.event:
                        teams = []
                        for fixture in fixtures:
                            teams.append(Team.inventory[fixture.home_team.id])
                            teams.append(Team.inventory[fixture.away_team.id])

                        analyse_sequences(db,
                                          league_code,
                                          teams,
                                          args.event,
                                          args.negate,
                                          args.venue,
                                          args.half,
                                          args.minimum)
                    else:
                        output_fixtures(league, fixtures)


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)

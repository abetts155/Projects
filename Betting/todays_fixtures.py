from argparse import ArgumentParser, Namespace
from cli.cli import (add_database_option,
                     add_venue_option,
                     add_events_option,
                     add_minimum_option,
                     add_logging_options,
                     set_logging_options,
                     add_league_option,
                     add_half_option)
from datetime import datetime, timedelta
from model.fixtures import Half, Fixture, Venue
from model.leagues import league_register, League, prettify
from model.seasons import Season
from model.teams import create_team_from_row, Team
from pathlib import Path
from sql.sql import Database, get_fixtures
from sql.sql_columns import ColumnNames
from sql.sql_language import Characters, Keywords
from subprocess import run
from typing import List


def parse_command_line():
    parser = ArgumentParser(description='Show fixtures for today')
    add_half_option(parser)
    add_venue_option(parser)
    add_database_option(parser)
    add_minimum_option(parser)
    add_logging_options(parser)
    add_league_option(parser, False)
    add_events_option(parser, False)

    parser.add_argument('-l',
                        '--left',
                        help='left-side of the time window to consider',
                        metavar='<INT>',
                        type=int,
                        default=-2)

    parser.add_argument('-r',
                        '--right',
                        help='right-side of the time window to consider',
                        metavar='<INT>',
                        type=int,
                        default=12)

    return parser.parse_args()


def get_current_season(db: Database, league: League):
    country_constraint = "{}='{}' {} {}".format(ColumnNames.Country.name,
                                                league.country,
                                                Keywords.COLLATE.name,
                                                Keywords.NOCASE.name)

    name_constraint = "{}='{}' {} {}".format(ColumnNames.Code.name,
                                             league.name,
                                             Keywords.COLLATE.name,
                                             Keywords.NOCASE.name)

    current_constraint = "{}={}".format(ColumnNames.Current.name,
                                        Characters.TRUE.value)
    constraints = [country_constraint, name_constraint, current_constraint]
    season_rows = db.fetch_all_rows(Season.sql_table(), constraints)
    if season_rows:
        (season,) = season_rows
        return season


def filter_fixtures(fixtures: List[Fixture], left: int, right: int):
    filtered = []
    lower_bound = datetime.today() + timedelta(hours=left)
    upper_bound = datetime.today() + timedelta(hours=right)
    for fixture in fixtures:
        match_date = datetime.fromisoformat(str(fixture.date)).replace(tzinfo=None)
        if lower_bound <= match_date <= upper_bound:
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
    if args.league:
        leagues = args.league
    else:
        leagues = league_register.keys()

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
                fixtures = filter_fixtures(fixtures, args.left, args.right)

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

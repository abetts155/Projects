from argparse import ArgumentParser, Namespace
from cli.cli import (add_database_option,
                     add_venue_option,
                     add_events_option,
                     add_minimum_option,
                     add_logging_options,
                     set_logging_options,
                     add_half_option)
from datetime import datetime, timedelta
from model.fixtures import Half, Fixture, Venue
from model.leagues import league_register, League, prettify
from model.seasons import Season
from model.teams import create_team_from_row, Team
from pathlib import Path
from sql.sql import Database
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
                        default=18)

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


def get_fixtures(db: Database, season_id: int, left: int, right: int):
    season_constraint = "{}={}".format(ColumnNames.Season_ID.name, season_id)
    finished_constraint = "{}={}".format(ColumnNames.Finished.name, Characters.FALSE.value)
    constraints = [season_constraint, finished_constraint]
    fixture_rows = db.fetch_all_rows(Fixture.sql_table(), constraints)

    filtered_rows = []
    lower_bound = datetime.today() + timedelta(hours=left)
    upper_bound = datetime.today() + timedelta(hours=right)
    for row in fixture_rows:
        match_date = datetime.fromisoformat(row[1]).replace(tzinfo=None)
        if lower_bound <= match_date <= upper_bound:
            filtered_rows.append(row)
    filtered_rows.sort(key=lambda row: (datetime.fromisoformat(row[1]).date(),
                                        datetime.fromisoformat(row[1]).time()))
    return filtered_rows


def output_fixtures(league: League, fixture_rows: List):
    print("{} Fixtures in {} {} {}".format('*' * 80 + '\n',
                                           prettify(league.country),
                                           league.name,
                                           '\n' + '*' * 80))
    for row in fixture_rows:
        home_team = Team.inventory[row[3]]
        away_team = Team.inventory[row[4]]
        match_date = datetime.fromisoformat(row[1])
        next_match_message = 'Next match: {} {} vs. {}'.format(match_date.strftime('%Y-%m-%d %H.%M'),
                                                               home_team.name,
                                                               away_team.name)
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
            '--minimum', str(minimum)]

    if negate:
        args.append('--negate')

    if venue:
        args.extend(['--venue', venue.name])

    if half:
        args.extend(['--half', half.name])

    run(args)


def main(args: Namespace):
    with Database(args.database) as db:
        team_rows = db.fetch_all_rows(Team.sql_table())
        for row in team_rows:
            create_team_from_row(row)

        for league_code, league in league_register.items():
            season = get_current_season(db, league)

            if season is not None:
                season_id = season[0]
                fixture_rows = get_fixtures(db, season_id, args.left, args.right)

                if fixture_rows:
                    if args.event:
                        teams = []
                        for row in fixture_rows:
                            teams.append(Team.inventory[row[3]])
                            teams.append(Team.inventory[row[4]])

                        analyse_sequences(db,
                                          league_code,
                                          teams,
                                          args.event,
                                          args.negate,
                                          args.venue,
                                          args.half,
                                          args.minimum)
                    else:
                        output_fixtures(league, fixture_rows)


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)

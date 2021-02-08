from argparse import ArgumentParser, Namespace
from cli.cli import (add_database_option,
                     add_venue_option,
                     add_events_option,
                     add_minimum_option,
                     add_logging_options,
                     set_logging_options,
                     add_half_option)
from datetime import date, datetime, timedelta
from model.fixtures import Half, Fixture, Venue
from model.leagues import league_register, League
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


def get_fixtures(db: Database, season_id: int):
    season_constraint = "{}={}".format(ColumnNames.Season_ID.name, season_id)
    finished_constraint = "{}={}".format(ColumnNames.Finished.name, Characters.FALSE.value)
    constraints = [season_constraint, finished_constraint]
    fixture_rows = db.fetch_all_rows(Fixture.sql_table(), constraints)
    todays_rows = []
    tomorrow = datetime.today() + timedelta(days=1)
    for row in fixture_rows:
        match_date = datetime.fromisoformat(row[1])
        if match_date.date() == date.today():
            cutoff = datetime.now() - timedelta(hours=1)
            if cutoff.time().hour <= match_date.time().hour:
                todays_rows.append(row)
        elif match_date.date() == tomorrow.date():
            cutoff = 10
            if match_date.time().hour <= cutoff:
                todays_rows.append(row)
    todays_rows.sort(key=lambda row: (datetime.fromisoformat(row[1]).date(), datetime.fromisoformat(row[1]).time()))
    return todays_rows


def output_fixtures(league: League, fixture_rows: List):
    matches = []
    print("{} Fixtures in {} {} {}".format('*' * 80 + '\n',
                                           league.country,
                                           league.name,
                                           '\n' + '*' * 80))
    for row in fixture_rows:
        home_team = Team.inventory[row[3]]
        away_team = Team.inventory[row[4]]
        match_date = datetime.fromisoformat(row[1])
        next_match_message = 'Next match: {} {} vs. {}'.format(match_date.strftime('%Y-%m-%d %H.%M'),
                                                               home_team.name,
                                                               away_team.name)
        matches.append((home_team, away_team))
        print(next_match_message)
    print()
    return matches


def analyse_sequences(db: Database,
                      league_code:
                      str, teams: List[Team],
                      events: List[str],
                      negate: bool,
                      venue: Venue,
                      half: Half,
                      minimum: int):
    analyse_script = Path(__file__).parent.absolute().joinpath('analyse_sequences.py')
    arguments = ['python3', str(analyse_script),
                 '--database', db.name,
                 '--no-header',
                 '--no-warnings',
                 '-T', ':'.join([team.name for team in teams]),
                 '-E', *events,
                 '-L', league_code,
                 '--minimum', str(minimum)]

    if negate:
        arguments.append('--negate')

    if venue:
        arguments.extend(['--venue', venue.name])

    if half:
        arguments.extend(['--half', half.name])
    run(arguments)


def main(arguments: Namespace):
    with Database(arguments.database) as db:
        team_rows = db.fetch_all_rows(Team.sql_table())
        for row in team_rows:
            create_team_from_row(row)

        for league_code, league in league_register.items():
            season = get_current_season(db, league)
            home_teams = []
            away_teams = []

            if season is not None:
                season_id = season[0]
                fixtures_rows = get_fixtures(db, season_id)
                if fixtures_rows:
                    team_pairs = output_fixtures(league, fixtures_rows)
                    for home_team, away_team in team_pairs:
                        home_teams.append(home_team)
                        away_teams.append(away_team)

            if arguments.event and home_teams and away_teams:
                analyse_sequences(db,
                                  league_code,
                                  home_teams + away_teams,
                                  arguments.event,
                                  arguments.negate,
                                  arguments.venue,
                                  arguments.half,
                                  arguments.minimum)


if __name__ == '__main__':
    arguments = parse_command_line()
    set_logging_options(arguments)
    main(arguments)

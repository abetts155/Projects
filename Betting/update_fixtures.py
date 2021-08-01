from argparse import ArgumentParser, Namespace
from cli.cli import (add_database_option,
                     add_logging_options,
                     add_league_option,
                     set_logging_options,
                     add_past_option,
                     add_force_option)
from datetime import datetime, timedelta
from football_api.football_api import get_fixtures
from football_api.structure import get_fixtures_json, store
from json import load
from lib import messages
from model.fixtures import (Fixture,
                            create_fixture_from_json,
                            create_fixture_from_row,
                            get_home_team_data,
                            get_away_team_data)
from model.leagues import League, league_register
from model.seasons import Season, create_season_from_row
from model.teams import Team, create_team_from_row
from os import EX_OK
from sql.sql_columns import ColumnNames
from sql.sql_language import Characters, Keywords
from sql.sql import Database, check_database_exists
from sys import exit
from typing import List


def parse_command_line():
    parser = ArgumentParser(description='Update football results database with fixture information')
    add_database_option(parser)
    add_logging_options(parser)
    add_league_option(parser, False)
    add_past_option(parser)
    add_force_option(parser)
    return parser.parse_args()


def create_fixtures_json(season: int, force: bool):
    fixtures_json = get_fixtures_json(season)
    if not fixtures_json.exists() or force:
        messages.vanilla_message("Extracting fixtures JSON for '{}'".format(season))
        store(fixtures_json, get_fixtures(season))


def create_team_row(league: League, id_: int, name: str):
    url = 'https://media.api-sports.io/football/teams/{}.png'.format(id_)
    return [id_, name, league.country, url]


def load_fixture_data(league: League, season: Season):
    fixtures_json = get_fixtures_json(season.id)
    if not fixtures_json.exists():
        messages.warning_message("No fixtures available for season {}".format(season.id))
    else:
        messages.verbose_message('Season {}'.format(season.year))
        with fixtures_json.open() as in_file:
            json_text = load(in_file)
            for data in json_text['api']['fixtures']:
                home_id, home_name = get_home_team_data(data)
                if not Team.has_team(home_id):
                    team_row = create_team_row(league, home_id, home_name)
                    create_team_from_row(team_row)

                away_id, away_name = get_away_team_data(data)
                if not Team.has_team(away_id):
                    team_row = create_team_row(league, away_id, away_name)
                    create_team_from_row(team_row)

                create_fixture_from_json(data)


def update_leagues(database: str, leagues: List[str], past: bool, force: bool):
    with Database(database) as db:
        team_rows = db.fetch_all_rows(Team.sql_table())
        for row in team_rows:
            create_team_from_row(row)

        for league_code in leagues:
            messages.vanilla_message('Updating {}...'.format(league_code))
            league = league_register[league_code]

            name_constraint = "{}='{}' {} {}".format(ColumnNames.Code.name,
                                                     league.name,
                                                     Keywords.COLLATE.name,
                                                     Keywords.NOCASE.name)

            country_constraint = "{}='{}' {} {}".format(ColumnNames.Country.name,
                                                        league.country,
                                                        Keywords.COLLATE.name,
                                                        Keywords.NOCASE.name)

            current_constraint = "{}={}".format(ColumnNames.Current.name,
                                                Characters.FALSE.value if past else Characters.TRUE.value)
            constraints = [name_constraint, country_constraint, current_constraint]
            season_rows = db.fetch_all_rows(Season.sql_table(), constraints)
            for row in season_rows:
                season = create_season_from_row(row)
                create_fixtures_json(season.id, force)
                load_fixture_data(league, season)
                db.create_table(Fixture)
                db.create_rows(Fixture)
                db.create_table(Team)
                db.create_rows(Team)


def fixtures_played(database: str, season: Season) -> bool:
    played = False
    with Database(database) as db:
        constraints = ["{}='{}'".format(ColumnNames.Season_ID.name, season.id)]
        fixture_rows = db.fetch_all_rows(Fixture.sql_table(), constraints)
        for row in fixture_rows:
            fixture = create_fixture_from_row(row)
            if fixture.home_team is not None and fixture.away_team is not None and not fixture.finished:
                lower_bound = datetime.today() + timedelta(days=-3)
                upper_bound = datetime.today()
                match_date = fixture.date.replace(tzinfo=None)
                if lower_bound <= match_date <= upper_bound:
                    played = True
    return played


def update_all(database: str, past: bool, force: bool):
    codes = []
    with Database(database) as db:
        team_rows = db.fetch_all_rows(Team.sql_table())
        for row in team_rows:
            create_team_from_row(row)

        for code, league in league_register.items():
            constraints = ["{}='{}'".format(ColumnNames.Country.name, league.country),
                           "{}='{}'".format(ColumnNames.Code.name, league.name)]
            season_rows = db.fetch_all_rows(Season.sql_table(), constraints)
            for season_row in season_rows:
                season = create_season_from_row(season_row)
                if season.current:
                    if force or fixtures_played(database, season):
                        codes.append(code)

    update_leagues(database, codes, past, force or codes)


def main(args: Namespace):
    if args.league:
        update_leagues(args.database, args.league, args.past, args.force)
    else:
        update_all(args.database, args.past, args.force)


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    check_database_exists(args.database)
    main(args)
    exit(EX_OK)

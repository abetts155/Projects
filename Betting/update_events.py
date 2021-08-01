from argparse import ArgumentParser, Namespace
from cli.cli import (add_database_option,
                     add_logging_options,
                     add_league_option,
                     set_logging_options,
                     add_past_option,
                     add_force_option)
from concurrent.futures import ThreadPoolExecutor
from football_api.football_api import get_events
from football_api.structure import get_events_json, store
from json import load
from lib import messages
from model.events import Event, create_event_from_json
from model.fixtures import Fixture
from model.leagues import league_register, League
from model.seasons import create_season_from_row, Season
from model.teams import create_team_from_row, Team
from os import EX_OK
from sql.sql_columns import ColumnNames
from sql.sql_language import Characters, Keywords
from sql.sql import Database, get_fixtures
from sys import exit


def parse_command_line():
    parser = ArgumentParser(description='Update football results database with events information')
    add_database_option(parser)
    add_logging_options(parser)
    add_league_option(parser, True)
    add_past_option(parser)
    add_force_option(parser)
    return parser.parse_args()


def create_events_json(fixture_id: int):
    events_json = get_events_json(fixture_id)
    if not events_json.exists():
        messages.verbose_message("Extracting events JSON for '{}'".format(fixture_id))
        store(events_json, get_events(fixture_id))


def create_events_for_fixture(fixture: Fixture):
    create_events_json(fixture.id)
    events_json = get_events_json(fixture.id)
    with events_json.open() as in_file:
        json_text = load(in_file)
        for data in json_text['api']['events']:
            create_event_from_json(data, fixture)


def update_leagues(database: str, league: League, past: bool):
    with Database(database) as db:
        messages.vanilla_message('Updating {} {}...'.format(league.country, league.name))

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
            messages.vanilla_message("Extracting events for season {}".format(season.year))
            season_constraint = "{}={}".format(ColumnNames.Season_ID.name, season.id)
            finished_constraint = "{}={}".format(ColumnNames.Finished.name, Characters.TRUE.value)
            constraints = [season_constraint, finished_constraint]
            fixtures = get_fixtures(db, constraints)

            with ThreadPoolExecutor(max_workers=32) as executor:
                for fixture in fixtures:
                    executor.submit(create_events_for_fixture, fixture)


def main(args: Namespace):
    with Database(args.database) as db:
        team_rows = db.fetch_all_rows(Team.sql_table())
        for row in team_rows:
            create_team_from_row(row)

    for league_code in args.league:
        league = league_register[league_code]
        update_leagues(args.database, league, args.past)

    with Database(args.database) as db:
        db.create_table(Event)
        db.create_rows(Event)


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)
    exit(EX_OK)

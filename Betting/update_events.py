import argparse
import json
import os
import sys
import time

import cli.cli
import cli.user_input
import football_api.football_api
import football_api.helpers
import football_api.structure
import lib.messages
import model.fixtures
import model.competitions
import model.events
import model.seasons
import model.teams
import sql.sql


def parse_command_line():
    parser = argparse.ArgumentParser(description='Update database with fixtures events')
    cli.cli.add_logging_options(parser)
    return parser.parse_args()


def create_events_json(fixture: model.fixtures.Fixture) -> bool:
    events_json = football_api.structure.get_events_json(fixture.id)
    created = False
    if not events_json.exists():
        created = True
        lib.messages.vanilla_message(f"Extracting events JSON for fixture '{fixture.id}'")
        response = football_api.football_api.get_events(fixture.id)
        football_api.structure.store(events_json, response)
    return created


def load_events_data(
        fixture: model.fixtures.Fixture
) -> list[model.events.Event]:
    events = []
    events_json = football_api.structure.get_events_json(fixture.id)
    with events_json.open() as in_file:
        json_text = json.load(in_file)
        for event_id, event_json in enumerate(json_text['response'], start=1):
            event = model.events.create_event_from_json(event_json, event_id, fixture)
            events.append(event)
    return events


def main(competition: model.competitions.Competition, season: model.seasons.Season):
    if season.events:
        fixtures = model.seasons.load_fixtures(competition, season)
        with sql.sql.Database(football_api.structure.database) as db:
            season_events = []
            for fixture in fixtures:
                if fixture.finished:
                    created = create_events_json(fixture)
                    events = load_events_data(fixture)
                    season_events.extend(events)
                    for event in events:
                        season_events.append(event)

                    if created:
                        time.sleep(1)

            table = model.events.Event.sql_table()
            db.create_rows(table, season_events)
    else:
        lib.messages.warning_message(f"There are no events available for {competition} in {season.year}")


if __name__ == '__main__':
    args = parse_command_line()
    cli.cli.set_logging_options(args)
    country = cli.user_input.pick_country()
    competition = cli.user_input.pick_competition(country)
    season = cli.user_input.pick_season(competition)
    main(competition, season)
    sys.exit(os.EX_OK)

import datetime
import pathlib
import time

import lib.football_api
import lib.messages
import lib.structure
import model.competitions
import model.fixtures
import model.seasons
import model.statistics


THRESHOLD = 6000


def get_api_calls_file() -> pathlib.Path:
    date_str = datetime.datetime.today().strftime("%Y-%m-%d")
    return lib.structure.api_calls_directory / date_str


def read_api_call_count() -> int:
    file = get_api_calls_file()
    if not file.exists():
        return 0
    else:
        return int(file.read_text().strip())


def increment_api_call_count():
    calls = read_api_call_count() + 1
    file = get_api_calls_file()
    file.write_text(f"{calls}\n")


def capacity_exists() -> bool:
    return read_api_call_count() <= THRESHOLD


def create_fixtures_json(competition: model.competitions.Competition, season: model.seasons.Season):
    fixtures_json = lib.structure.get_fixtures_json(competition.id, season.year)
    if capacity_exists():
        created = False
        if not fixtures_json.exists() or season.current:
            created = not season.current
            lib.messages.vanilla_message(f"Updating '{fixtures_json}'")
            response = lib.football_api.get_fixtures(competition.id, season.year)
            lib.structure.store(fixtures_json, response)

        if created:
            increment_api_call_count()
            if season.current:
                time.sleep(0.5)
            else:
                time.sleep(1)


def create_events_json(fixture: model.fixtures.Fixture):
    events_json = lib.structure.get_events_json(fixture.id)
    if capacity_exists():
        created = False
        if not events_json.exists():
            created = True
            lib.messages.vanilla_message(f"Extracting events JSON for fixture '{fixture.id}'")
            response = lib.football_api.get_events(fixture.id)
            lib.structure.store(events_json, response)

        if created:
            increment_api_call_count()
            time.sleep(5)


def create_lineups_json(fixture: model.fixtures.Fixture):
    lineups_json = lib.structure.get_lineups_json(fixture.id)
    if capacity_exists():
        created = False
        if not lineups_json.exists():
            created = True
            lib.messages.vanilla_message(f"Extracting lineups JSON for '{fixture.id}'")
            response = lib.football_api.get_lineups(fixture.id)
            lib.structure.store(lineups_json, response)

        if created:
            increment_api_call_count()
            time.sleep(5)


def create_fixture_stats_json(fixture: model.fixtures.Fixture):
    stats_json = lib.structure.get_stats_json(fixture.id)
    if capacity_exists():
        created = False
        if not stats_json.exists():
            created = True
            lib.messages.vanilla_message(f"Extracting fixture stats JSON for '{fixture.id}'")
            response = lib.football_api.get_stats(fixture.id)
            lib.structure.store(stats_json, response)

        if created:
            increment_api_call_count()
            time.sleep(5)


def create_player_stats_json(fixture: model.fixtures.Fixture):
    player_stats_json = lib.structure.get_player_stats_json(fixture.id)
    if capacity_exists():
        created = False
        if not player_stats_json.exists():
            created = True
            lib.messages.vanilla_message(f"Extracting player stats JSON for '{fixture.id}'")
            response = lib.football_api.get_player_stats(fixture.id)
            lib.structure.store(player_stats_json, response)

        if created:
            increment_api_call_count()
            time.sleep(5)


def create_players_json(page_id: int):
    players_json = lib.structure.get_players_json(page_id)
    if capacity_exists():
        created = False
        if not players_json.exists():
            created = True
            lib.messages.vanilla_message(f"Extracting players JSON for page {page_id}")
            response = lib.football_api.get_players(page_id)
            lib.structure.store(players_json, response)

        if created:
            increment_api_call_count()
            time.sleep(1)


def create_leagues_json(force: bool = False):
    leagues_json = lib.structure.get_leagues_json()
    if capacity_exists():
        created = False
        if not leagues_json.exists() or force:
            created = True
            lib.messages.vanilla_message(f"Updating leagues JSON")
            response = lib.football_api.get_leagues()
            lib.structure.store(leagues_json, response)

        if created:
            increment_api_call_count()
            time.sleep(1)

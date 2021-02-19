from pathlib import Path

hidden_directory = Path.home().joinpath('.football')
json_directory = hidden_directory.joinpath('json')
json_directory.mkdir(parents=True, exist_ok=True)
json_teams_directory = json_directory.joinpath('teams')
json_teams_directory.mkdir(parents=True, exist_ok=True)
json_fixtures_directory = json_directory.joinpath('fixtures')
json_fixtures_directory.mkdir(parents=True, exist_ok=True)
json_events_directory = json_directory.joinpath('events')
json_events_directory.mkdir(parents=True, exist_ok=True)


def get_seasons_json():
    return json_directory.joinpath('seasons.json')


def get_teams_json(country: str):
    return json_teams_directory.joinpath('{}.json'.format(country))


def get_fixtures_json(season: int):
    return json_fixtures_directory.joinpath('{}.json'.format(season))


def get_events_json(fixture: int):
    return json_events_directory.joinpath('{}.json'.format(fixture))


def store(path: Path, response):
    with path.open('w') as out_file:
        out_file.write(response.text)

from pathlib import Path

hidden_directory = Path.home().joinpath('.football')
json_directory = hidden_directory.joinpath('json')
json_directory.mkdir(parents=True, exist_ok=True)
json_teams_directory = json_directory.joinpath('teams')
json_teams_directory.mkdir(parents=True, exist_ok=True)
json_fixtures_directory = json_directory.joinpath('fixtures')
json_fixtures_directory.mkdir(parents=True, exist_ok=True)


headers = {'x-rapidapi-host': 'api-football-v1.p.rapidapi.com'}
api_key = hidden_directory.joinpath('api_key.txt')
with api_key.open() as file:
    (line,) = file.readlines()
    headers['x-rapidapi-key'] = line.strip()


base_url = 'https://api-football-v1.p.rapidapi.com/v2'


def get_seasons_json():
    return json_directory.joinpath('seasons.json')


def get_teams_json(country: str):
    return json_teams_directory.joinpath('{}.json'.format(country))


def get_fixtures_json(season: int):
    return json_fixtures_directory.joinpath('{}.json'.format(season))

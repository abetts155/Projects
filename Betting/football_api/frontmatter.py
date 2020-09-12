from pathlib import Path

base_url = 'https://api-football-v1.p.rapidapi.com/v2'
headers = {'x-rapidapi-host': 'api-football-v1.p.rapidapi.com',
           'x-rapidapi-key': 'cb8a771471msh9a21ebd34047a24p1a2e2fjsn402f20fae83c'}

hidden_directory = '.football'
json_directory = Path.home().joinpath(hidden_directory, 'json')
json_directory.mkdir(parents=True, exist_ok=True)
json_teams_directory = json_directory.joinpath('teams')
json_teams_directory.mkdir(parents=True, exist_ok=True)
json_fixtures_directory = json_directory.joinpath('fixtures')
json_fixtures_directory.mkdir(parents=True, exist_ok=True)


def get_seasons_json():
    return json_directory.joinpath('seasons.json')


def get_teams_json(country: str):
    return json_teams_directory.joinpath('{}.json'.format(country))


def get_fixtures_json(season: int):
    return json_fixtures_directory.joinpath('{}.json'.format(season))

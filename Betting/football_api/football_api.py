from football_api import structure
from requests import request


headers = {'x-rapidapi-host': 'api-football-v1.p.rapidapi.com'}
api_key = structure.hidden_directory.joinpath('api_key.txt')
with api_key.open() as file:
    (line,) = file.readlines()
    headers['x-rapidapi-key'] = line.strip()


base_url = 'https://api-football-v1.p.rapidapi.com/v2'


def get_seasons():
    response = request('GET',
                       '{}/{}'.format(base_url, 'leagues'),
                       headers=headers)
    return response


def get_teams(country: str):
    response = request('GET',
                       '{}/teams/search/{}'.format(base_url, country),
                       headers=headers)
    return response


def get_fixtures(season: int):
    response = request('GET',
                       '{}/fixtures/league/{}'.format(base_url, season),
                       headers=headers)
    return response

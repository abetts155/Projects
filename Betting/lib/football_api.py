import requests

import lib.structure


headers = {'x-rapidapi-host': 'api-football-v1.p.rapidapi.com'}
api_key = lib.structure.hidden_directory.joinpath('api_key.txt')
with api_key.open() as file:
    (line,) = file.readlines()
    headers['x-rapidapi-key'] = line.strip()


base_url = 'https://api-football-v1.p.rapidapi.com'


def get_leagues():
    url = f'{base_url}/v3/leagues'
    response = requests.request('GET', url, headers=headers)
    return response


def get_fixtures(competition_id: int, season_year: int):
    url = f'{base_url}/v3/fixtures?league={competition_id}&season={season_year}'
    response = requests.request('GET', url, headers=headers)
    return response


def get_teams(country: str):
    url = f'{base_url}/v3/teams?country={country}'
    response = requests.request('GET', url, headers=headers)
    return response


def get_players(page_id: int):
    url = f'{base_url}/v3/players/profiles?page={page_id}'
    response = requests.request('GET', url, headers=headers)
    return response


def get_stats(fixture_id: int):
    url = f'{base_url}/v3/fixtures/statistics?fixture={fixture_id}'
    response = requests.request('GET', url, headers=headers)
    return response


def get_lineups(fixture_id: int):
    url = f'{base_url}/v3/fixtures/lineups?fixture={fixture_id}'
    response = requests.request('GET', url, headers=headers)
    return response


def get_player_stats(fixture_id: int):
    url = f'{base_url}/v3/fixtures/players?fixture={fixture_id}'
    response = requests.request('GET', url, headers=headers)
    return response


def get_events(fixture_id: int):
    url = f'{base_url}/v3/fixtures/events?fixture={fixture_id}'
    response = requests.request('GET', url, headers=headers)
    return response


def get_timezone():
    response = requests.request('GET', '{}/timezone'.format(base_url), headers=headers)
    return response

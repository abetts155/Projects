import requests

import football_api.structure


headers = {'x-rapidapi-host': 'api-football-v1.p.rapidapi.com'}
api_key = football_api.structure.hidden_directory.joinpath('api_key.txt')
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


def get_logo(team_id: int):
    url = f'https://media.api-sports.io/football/teams/{team_id}.png'
    response = requests.request('GET', url, headers=headers)
    return response


def get_events(fixture_id: int):
    url = f'{base_url}/v3/fixtures/events?fixture={fixture_id}'
    response = requests.request('GET', url, headers=headers)
    return response


def get_timezone():
    response = requests.request('GET', '{}/timezone'.format(base_url), headers=headers)
    return response

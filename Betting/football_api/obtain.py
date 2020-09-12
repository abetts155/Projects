from . import frontmatter
from requests import request


def get_seasons():
    response = request('GET',
                       '{}/{}'.format(frontmatter.base_url, 'leagues'),
                       headers=frontmatter.headers)
    return response


def get_teams(country: str):
    response = request('GET',
                       '{}/teams/search/{}'.format(frontmatter.base_url, country),
                       headers=frontmatter.headers)
    return response


def get_fixtures(season: int):
    response = request('GET',
                       '{}/fixtures/league/{}'.format(frontmatter.base_url, season),
                       headers=frontmatter.headers)
    return response

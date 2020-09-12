from argparse import ArgumentParser, Namespace
from football_api.frontmatter import get_fixtures_json, get_seasons_json, get_teams_json
from football_api.obtain import get_fixtures, get_seasons, get_teams
from lib import messages


def parse_command_line():
    parser = ArgumentParser(description='Download JSON files from API-Football')

    parser.add_argument('--seasons',
                        action='store_true',
                        help='extract JSON file containing season information',
                        default=False)

    parser.add_argument('--teams',
                        nargs='+',
                        help='extract JSON files containing team information for these countries',
                        metavar='<COUNTRY>')

    parser.add_argument('--fixtures',
                        nargs='+',
                        help='extract JSON files containing fixture information for these seasons',
                        metavar='<SEASON>')

    parser.add_argument('-d',
                        '--debug',
                        action='store_true',
                        help='print debug messages',
                        default=False)

    parser.add_argument('-v',
                        '--verbose',
                        action='store_true',
                        help='print verbose messages',
                        default=False)

    return parser.parse_args()


def store(file, response):
    with file.open('w') as out_file:
        out_file.write(response.text)


def create_seasons():
    if not get_seasons_json().exists():
        messages.verbose_message("Extracting season JSON")
        store(get_seasons_json(), get_seasons())


def create_teams(country: str):
    teams_json = get_teams_json(country)
    if not teams_json.exists():
        messages.verbose_message("Extracting teams JSON for '{}'".format(country))
        store(teams_json, get_teams(country))


def create_fixtures(season: int):
    fixtures_json = get_fixtures_json(season)
    if not fixtures_json.exists():
        messages.verbose_message("Extracting fixtures JSON for '{}'".format(season))
        store(fixtures_json, get_fixtures(season))


def main(arguments: Namespace):
    messages.verbose = arguments.verbose
    messages.debug = arguments.debug

    if arguments.seasons:
        create_seasons()

    if arguments.teams:
        for country in arguments.teams:
            country = country.lower()
            country = country.capitalize()
            create_teams(country)

    if arguments.fixtures:
        for season in arguments.fixtures:
            create_fixtures(season)


if __name__ == '__main__':
    main(parse_command_line())

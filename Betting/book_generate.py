from argparse import ArgumentParser, Namespace

from cli.cli import (add_database_option,
                     add_league_option,
                     add_logging_options,
                     set_logging_options,
                     get_unique_league)
from lib import messages
from lib.helpful import set_matplotlib_defaults
from model.fixtures import Venue
from model.leagues import league_register
from model.seasons import Season
from sql.sql import load_league, load_teams
from show_team import main as show_team


def parse_command_line():
    parser = ArgumentParser(description='Generate boo')
    add_database_option(parser)
    add_league_option(parser, True)
    add_logging_options(parser)

    return parser.parse_args()


def main(args: Namespace):
    set_matplotlib_defaults()
    load_teams(args.database)
    league = league_register[get_unique_league(args)]
    load_league(args.database, league)

    seasons = Season.seasons(league)
    if not seasons:
        messages.error_message("No season data found")

    season = seasons[-1]
    teams = sorted(season.teams(), key=lambda t: t.name)
    for team in teams:
        print('Generating information for {}'.format(team.name))
        for venue in Venue:
            other_args = Namespace()
            other_args.database = args.database
            other_args.league = args.league
            other_args.team = team.name
            other_args.history = None
            other_args.venue = venue
            other_args.game_states = []
            other_args.averages = None
            other_args.save = '{}_{}.png'.format(team.name.replace(' ', '_'), venue.name)
            show_team(other_args)


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)

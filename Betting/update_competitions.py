import argparse
import json

import cli.cli
from lib import football_api
import lib.structure
import lib.messages
import model.competitions
import model.seasons
import sql.sql


def parse_command_line():
    parser = argparse.ArgumentParser(description='Update database with leagues/cups and season information')
    cli.cli.add_logging_options(parser)
    return parser.parse_args()


def create_leagues_json():
    leagues_json = lib.structure.get_leagues_json()
    lib.messages.verbose_message("Extracting leagues JSON")
    response = lib.football_api.get_leagues()
    lib.structure.store(leagues_json, response)


def main():
    create_leagues_json()
    leagues_json = lib.structure.get_leagues_json()

    competitions = []
    seasons = []
    with leagues_json.open() as in_file:
        json_text = json.load(in_file)
        for league_json in json_text['response']:
            competition = model.competitions.create_competition_from_json(league_json)
            competitions.append(competition)
            for season_json in league_json['seasons']:
                season = model.seasons.create_season_from_json(competition, season_json)
                seasons.append(season)

    with sql.sql.Database(lib.structure.database) as db:
        db.drop_table(model.competitions.Competition.sql_table())
        db.drop_table(model.seasons.Season.sql_table())
        db.create_table(model.competitions.Competition.sql_table())
        db.create_table(model.seasons.Season.sql_table())
        db.create_rows(model.competitions.Competition.sql_table(), competitions)
        db.create_rows(model.seasons.Season.sql_table(), seasons)

if __name__ == '__main__':
    args = parse_command_line()
    cli.cli.set_logging_options(args)
    main()

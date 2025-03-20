import argparse
import json
import os
import sys

import cli.cli
import football_api.football_api
import football_api.structure
import lib.messages
import model.teams
import sql.sql


def parse_command_line():
    parser = argparse.ArgumentParser(description='Update database with team information')
    cli.cli.add_logging_options(parser)

    parser.add_argument('-f',
                        '--force',
                        action='store_true',
                        help='force an update',
                        default=False)

    return parser.parse_args()


def create_teams_json(country: str, force: bool):
    teams_json = football_api.structure.get_teams_json(country)
    if not teams_json.exists() or force:
        lib.messages.verbose_message(f"Extracting teams JSON for '{country}'")
        response = football_api.football_api.get_teams(country)
        football_api.structure.store(teams_json, response)


def load_team_data(country: str) -> list[model.teams.Team]:
    teams_json = football_api.structure.get_teams_json(country)
    teams = []
    with teams_json.open() as in_file:
        json_text = json.load(in_file)
        for team_json in json_text['api']['teams']:
            team = model.teams.create_team_from_json(team_json)
            teams.append(team)
    return teams


def main(force: bool):
    teams = []
    with open(football_api.structure.get_countries_whitelist(), 'r') as in_file:
        for line in in_file:
            country = line.strip()
            create_teams_json(country, force)
            country_teams = load_team_data(country)
            teams.extend(country_teams)

    with sql.sql.Database(football_api.structure.database) as db:
        table = model.teams.Team.sql_table()
        db.create_rows(table, teams)


if __name__ == '__main__':
    args = parse_command_line()
    cli.cli.set_logging_options(args)
    main(args.force)
    sys.exit(os.EX_OK)

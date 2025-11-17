import argparse
import json
import os
import sys

import cli.cli
import lib.football_api
import lib.json_manager
import lib.messages
import lib.structure
import model.competitions
import model.players
import model.seasons
import model.teams
import sql.sql


def parse_command_line():
    parser = argparse.ArgumentParser(description="Create the base database")
    cli.cli.add_logging_options(parser)

    parser.add_argument('-C',
                        '--update-competitions',
                        action='store_true',
                        help='update competitions',
                        default=False)

    parser.add_argument('-P',
                        '--update-players',
                        action='store_true',
                        help='update players',
                        default=False)

    parser.add_argument('-S',
                       '--update-seasons',
                       action='store_true',
                       help='update seasons',
                       default=False)

    parser.add_argument('-T',
                        '--update-teams',
                        action='store_true',
                        help='update teams',
                        default=False)

    return parser.parse_args()


def create_teams_json(country: str):
    teams_json = lib.structure.get_teams_json(country)
    if not teams_json.exists():
        lib.messages.debug_message(f"Extracting teams JSON for '{country}'")
        response = lib.football_api.get_teams(country)
        lib.structure.store(teams_json, response)


def create_teams(country: str) -> list[model.teams.Team]:
    teams_json = lib.structure.get_teams_json(country)
    teams = []
    if teams_json.exists():
        with teams_json.open() as in_file:
            json_text = json.load(in_file)
            if 'api' in json_text:
                for team_json in json_text['api']['teams']:
                    team = model.teams.create_team_from_json(team_json)
                    teams.append(team)
    else:
        lib.messages.debug_message(f"No team information for {country}")
    return teams


def create_players() -> list[model.players.Player]:
    page_id = 2472
    pages = True
    players = []

    with sql.sql.Database(lib.structure.get_base_database()) as db:
        rows = db.count_rows(model.players.Player.sql_table())

    while pages:
        lib.messages.verbose_message(f"Loading players from page {page_id}")
        lib.json_manager.create_players_json(page_id)
        players_json = lib.structure.get_players_json(page_id)
        old_size = len(players)
        with players_json.open() as in_file:
            json_text = json.load(in_file)
            for player_json in json_text['response']:
                player = model.players.create_player_from_json(player_json)
                players.append(player)

        if old_size == len(players):
            players_json.unlink()
            pages = False
            lib.messages.vanilla_message(f"The next player page ID is {page_id}")

        page_id += 1

    return players


def gather_competitions_and_seasons_and_teams() -> tuple[
    dict[str, list[model.competitions.Competition]],
    dict[model.competitions.Competition, list[model.seasons.Season]],
    dict[str, list[model.teams.Team]]
]:
    whitelist = (
            {c.id for c in model.competitions.get_whitelisted_competitions(model.competitions.CompetitionType.LEAGUE)} or
            {c.id for c in model.competitions.get_whitelisted_competitions(model.competitions.CompetitionType.CUP)}
    )

    competitions = {}
    seasons = {}
    teams = {}

    lib.json_manager.create_leagues_json(True)
    leagues_json = lib.structure.get_leagues_json()
    with leagues_json.open() as in_file:
        json_text = json.load(in_file)
        for league_json in json_text['response']:
            c = model.competitions.create_competition_from_json(league_json)
            c.whitelisted = c.id in whitelist

            if c.country not in competitions:
                competitions[c.country] = [c]
                teams[c.country] = create_teams(c.country)
            else:
                competitions[c.country].append(c)

            for season_json in league_json['seasons']:
                season = model.seasons.create_season_from_json(c, season_json)
                if c not in seasons:
                    seasons[c] = [season]
                else:
                    seasons[c].append(season)

    return competitions, seasons, teams


def main(update_competitions: bool, update_seasons: bool, update_players: bool, update_teams: bool):
    competitions, seasons, teams = gather_competitions_and_seasons_and_teams()

    if update_competitions:
        with sql.sql.Database(lib.structure.get_base_database()) as db:
            for country in sorted(competitions.keys()):
                db.create_rows(model.competitions.Competition.sql_table(), competitions[country])

    if update_seasons:
        for country in sorted(competitions.keys()):
            with sql.sql.Database(lib.structure.get_database(country)) as db:
                for c in competitions[country]:
                    if c.whitelisted:
                        db.create_rows(model.seasons.Season.sql_table(), seasons[c])

    if update_players:
        with sql.sql.Database(lib.structure.get_base_database()) as db:
            db.create_rows(model.players.Player.sql_table(), create_players())

    if update_teams:
        with sql.sql.Database(lib.structure.get_base_database()) as db:
            for country in sorted(competitions.keys()):
                db.create_rows(model.teams.Team.sql_table(), teams[country])


if __name__ == '__main__':
    args = parse_command_line()
    cli.cli.set_logging_options(args)
    main(args.update_competitions, args.update_seasons, args.update_players, args.update_teams)
    sys.exit(os.EX_OK)

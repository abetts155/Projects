import argparse
import json
import os
import sys
import time

import cli.cli
import football_api.football_api
import football_api.structure
import lib.messages
import model.players
import sql.sql


def parse_command_line():
    parser = argparse.ArgumentParser(description='Update database with player information')
    cli.cli.add_logging_options(parser)
    return parser.parse_args()


def create_players_json(page_id: int) -> bool:
    players_json = football_api.structure.get_players_json(page_id)
    created = False
    if not players_json.exists():
        created = True
        lib.messages.vanilla_message(f"Extracting players JSON for page '{page_id}'")
        response = football_api.football_api.get_players(page_id)
        football_api.structure.store(players_json, response)
    return created


def load_player_data(page_id: int) -> list[model.players.Player]:
    players_json = football_api.structure.get_players_json(page_id)
    players = []
    with players_json.open() as in_file:
        json_text = json.load(in_file)
        for player_json in json_text['response']:
            player = model.players.create_player_from_json(player_json)
            players.append(player)
    return players


def main():
    page_id = 2041
    pages = True
    while pages:
        lib.messages.vanilla_message(f"Loading players from page '{page_id}'")
        created = create_players_json(page_id)
        if created:
            time.sleep(10)
            players = load_player_data(page_id)
            if players:
                with sql.sql.Database(football_api.structure.database) as db:
                    table = model.players.Player.sql_table()
                    db.create_rows(table, players)
            else:
                players_json = football_api.structure.get_players_json(page_id)
                players_json.unlink()
                pages = False
        page_id += 1


if __name__ == '__main__':
    args = parse_command_line()
    cli.cli.set_logging_options(args)
    main()
    sys.exit(os.EX_OK)

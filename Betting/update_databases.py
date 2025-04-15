import argparse
import json
import os
import pathlib
import sys
import typing

import cli.cli
import update_base_database
import lib.football_api
import lib.json_manager
import lib.messages
import lib.structure
import model.fixtures
import model.competitions
import model.events
import model.lineups
import model.players
import model.seasons
import model.statistics
import model.teams
import sql.sql
import sql.sql_columns


def parse_command_line():
    parser = argparse.ArgumentParser(description="Update each country's database")
    cli.cli.add_logging_options(parser)

    parser.add_argument('-C',
                        '--countries',
                        nargs='+',
                        type=str,
                        help='pick out countries that start with this sequence')

    parser.add_argument('-E',
                        '--update-events',
                        action='store_true',
                        help='update events',
                        default=False)

    parser.add_argument('-F',
                        '--update-fixtures',
                        action='store_true',
                        help='update fixtures',
                        default=False)

    parser.add_argument('-L',
                        '--update-lineups',
                        action='store_true',
                        help='update lineups',
                        default=False)

    parser.add_argument('-P',
                        '--update-player-stats',
                        action='store_true',
                        help='update player stats',
                        default=False)

    parser.add_argument('-T',
                        '--update-team-stats',
                        action='store_true',
                        help='update team stats',
                        default=False)

    return parser.parse_args()


def create_country_databases(
        competitions: dict[str, list[model.competitions.Competition]],
        seasons: dict[model.competitions.Competition, list[model.seasons.Season]],
        teams: dict[str, list[model.teams.Team]]
):
    lib.messages.vanilla_message("Creating country databases")
    tables = [
        model.competitions.Competition.sql_table(),
        model.events.Event.sql_table(),
        model.fixtures.CupFixture.sql_table(),
        model.fixtures.Fixture.sql_table(),
        model.lineups.Lineup.sql_table(),
        model.seasons.Season.sql_table(),
        model.teams.Team.sql_table(),
        model.statistics.TeamStats.sql_table(),
        model.statistics.PlayerStats.sql_table()
    ]

    for country in competitions.keys():
        database = lib.structure.get_database(country)
        if not database.exists():
            with sql.sql.Database(database) as db:
                for table in tables:
                    db.create_table(table)

                db.create_rows(model.competitions.Competition.sql_table(), competitions[country])
                db.create_rows(model.teams.Team.sql_table(), teams[country])
                for competition in competitions[country]:
                    db.create_rows(model.seasons.Season.sql_table(), seasons[competition])


def read_competition_ids(path: pathlib.Path) -> set[int]:
    competition_ids = set()
    with open(path, 'r') as in_file:
        for line in in_file:
            line = line.strip()
            competition_ids.add(int(line))
    return competition_ids


def check_if_team_exists_in_base_database(team_id: int) -> typing.Optional[list[str]]:
    with sql.sql.Database(lib.structure.get_base_database()) as db:
        table = model.teams.Team.sql_table()
        team_constraint = f"{sql.sql_columns.ColumnNames.ID.name}='{team_id}'"
        team_rows = db.fetch_all_rows(table, [team_constraint])
        if team_rows:
            (row,) = team_rows
            return row


def add_unknown_team(competition: model.competitions.Competition, row: list) -> model.teams.Team:
    team = model.teams.create_team_from_row(row)
    with sql.sql.Database(lib.structure.get_database(competition.country)) as db:
        table = model.teams.Team.sql_table()
        db.create_rows(table, [team])
    return team


def read_fixtures_from_json_files(
        competition: model.competitions.Competition,
        season: model.seasons.Season,
        update_fixtures: bool
) -> list[model.fixtures.Fixture]:
    if update_fixtures:
        lib.json_manager.create_fixtures_json(competition, season)

    fixtures_json = lib.structure.get_fixtures_json(competition.id, season.year)
    fixtures = []
    if fixtures_json.exists():
        team_ids = set()
        with fixtures_json.open() as in_file:
            json_text = json.load(in_file)

            for fixture_json in json_text['response']:
                home_id = int(fixture_json['teams']['home']['id'])
                away_id = int(fixture_json['teams']['away']['id'])
                team_ids.add(home_id)
                team_ids.add(away_id)

        database = lib.structure.get_database(competition.country)
        teams = model.teams.load_teams(database, team_ids)
        with fixtures_json.open() as in_file:
            json_text = json.load(in_file)
            for fixture_json in json_text['response']:
                home_id = int(fixture_json['teams']['home']['id'])
                away_id = int(fixture_json['teams']['away']['id'])

                if home_id not in teams:
                    row = check_if_team_exists_in_base_database(home_id)
                    if row is None:
                        name = fixture_json['teams']['home']['name']
                        row = [home_id, name, competition.country, None]

                    home_team = add_unknown_team(competition, row)
                    teams[home_id] = home_team
                    lib.messages.vanilla_message(f"Did not know the team {home_team.name}")
                else:
                    home_team = teams[home_id]

                if away_id not in teams:
                    row = check_if_team_exists_in_base_database(away_id)
                    if row is None:
                        name = fixture_json['teams']['away']['name']
                        row = [away_id, name, competition.country, None]

                    away_team = add_unknown_team(competition, row)
                    teams[away_id] = away_team
                    lib.messages.vanilla_message(f"Did not know the team {away_team.name}")
                else:
                    away_team = teams[away_id]

                if competition.type == model.competitions.CompetitionType.LEAGUE:
                    fixture = model.fixtures.create_fixture_from_json(
                        competition.id,
                        season.year,
                        home_team,
                        away_team,
                        fixture_json
                    )
                else:
                    fixture = model.fixtures.create_cup_fixture_from_json(
                        competition.id,
                        season.year,
                        home_team,
                        away_team,
                        fixture_json
                    )

                if fixture is not None:
                    fixtures.append(fixture)

    return fixtures


def create_events(
        fixture: model.fixtures.Fixture
) -> list[model.events.Event]:
    lib.json_manager.create_events_json(fixture)
    events_json = lib.structure.get_events_json(fixture.id)
    events = []
    if events_json.exists():
        with events_json.open() as in_file:
            json_text = json.load(in_file)
            if json_text['response']:
                for event_id, event_json in enumerate(json_text['response'], start=1):
                    event = model.events.create_event_from_json(event_json, event_id, fixture)
                    events.append(event)
    return events


def create_lineups(fixture: model.fixtures.Fixture) -> list[model.lineups.Lineup]:
    lib.json_manager.create_lineups_json(fixture)
    lineups_json = lib.structure.get_lineups_json(fixture.id)
    lineups = []
    if lineups_json.exists():
        with lineups_json.open() as in_file:
            json_text = json.load(in_file)
            if json_text['response']:
                for lineup_json in json_text['response']:
                    team_id = int(lineup_json['team']['id'])
                    if team_id == fixture.home_team.id:
                        home_lineup = model.lineups.create_lineup_from_json(fixture, fixture.home_team, lineup_json)
                        lineups.append(home_lineup)
                    else:
                        away_lineup = model.lineups.create_lineup_from_json(fixture, fixture.away_team, lineup_json)
                        lineups.append(away_lineup)
    return lineups


def create_fixture_stats(fixture: model.fixtures.Fixture) ->  list[model.statistics.TeamStats]:
    lib.json_manager.create_fixture_stats_json(fixture)
    stats_json = lib.structure.get_stats_json(fixture.id)
    stats = []
    if stats_json.exists():
        with stats_json.open() as in_file:
            json_text = json.load(in_file)
            if json_text['response']:
                for team_json in json_text['response']:
                    team_id = int(team_json['team']['id'])
                    if team_id == fixture.home_team.id:
                        home_stats = model.statistics.create_team_stats_from_json(fixture, fixture.home_team, team_json)
                        stats.append(home_stats)
                    else:
                        away_stats = model.statistics.create_team_stats_from_json(fixture, fixture.away_team, team_json)
                        stats.append(away_stats)
    return stats


def create_player_stats(
        fixture: model.fixtures.Fixture
) -> list[model.statistics.PlayerStats]:
    lib.json_manager.create_player_stats_json(fixture)
    player_stats_json = lib.structure.get_player_stats_json(fixture.id)
    players_stats = []
    if player_stats_json.exists():
        with player_stats_json.open() as in_file:
            json_text = json.load(in_file)
            if json_text['response']:
                for i in [0, 1]:
                    for player_json in json_text['response'][i]['players']:
                        player_id = int(player_json['player']['id'])
                        player = model.players.load_player(player_id)
                        if player is not None:
                            stats = model.statistics.create_players_stats_from_json(
                                fixture,
                                player,
                                player_json['statistics'][0]
                            )
                            players_stats.append(stats)
                        else:
                            player_name = player_json['player']['name']
                            lib.messages.vanilla_message(f"Do not know the player {player_name} with id={player_id}")
    return players_stats


def update_country_databases(
        countries_prefixes: str,
        update_fixtures: bool,
        update_events: bool,
        update_lineups: bool,
        update_player_stats: bool,
        update_team_stats: bool
):
    lib.messages.vanilla_message("Updating country databases")

    whitelisted_competitions = (
            model.competitions.get_whitelisted_competitions(model.competitions.CompetitionType.LEAGUE) +
            model.competitions.get_whitelisted_competitions(model.competitions.CompetitionType.CUP)
    )

    countries = set(competition.country for competition in whitelisted_competitions)
    if countries_prefixes:
        countries = [
            c for c in countries for prefix in countries_prefixes if c.casefold().startswith(prefix.casefold())
        ]

    for country in sorted(countries):
        lib.messages.vanilla_message(f"{'>' * 10} {country}")
        database = lib.structure.get_database(country)
        competitions = model.competitions.load_competitions(database, country)
        competitions = [competition for competition in competitions if competition in whitelisted_competitions]

        for competition in competitions:
            lib.messages.vanilla_message(f"{'>' * 10} Analysing {competition.name} (id={competition.id})")
            seasons = model.seasons.load_seasons(database, competition)
            fixtures_per_season = {}
            for season in seasons:
                if season.current:
                    fixtures_per_season[season] = read_fixtures_from_json_files(competition, season, update_fixtures)
                else:
                    fixtures = model.seasons.load_fixtures(database, competition, season)
                    if not fixtures:
                        fixtures_per_season[season] = read_fixtures_from_json_files(competition, season, True)

            with sql.sql.Database(database) as db:
                for fixtures in fixtures_per_season.values():
                    if competition.type == model.competitions.CompetitionType.LEAGUE:
                        db.create_rows(model.fixtures.Fixture.sql_table(), fixtures)
                    else:
                        db.create_rows(model.fixtures.CupFixture.sql_table(), fixtures)

            update_others = update_events or update_lineups or update_team_stats or update_player_stats
            if update_others:
                season = seasons[-1]
                assert season.current
                print(f"{'>' * 10} In {competition}, {season.year}...")
                events = []
                lineups = []
                team_stats = []
                player_stats = []
                for fixture in fixtures_per_season[season]:
                    if fixture.finished:
                        if update_events and season.events:
                            if not model.events.load_events(database, fixture):
                                events.extend(create_events(fixture))

                        if update_lineups and season.lineups:
                            if not model.lineups.load_lineup(database, fixture, fixture.home_team):
                                lineups.extend(create_lineups(fixture))

                        if update_team_stats and season.statistics_fixtures:
                            if not model.statistics.load_team_stats(database, fixture, fixture.home_team):
                                team_stats.extend(create_fixture_stats(fixture))

                        if update_player_stats and season.statistics_players:
                            player_stats.extend(create_player_stats(fixture))

                with sql.sql.Database(database) as db:
                    db.create_rows(model.events.Event.sql_table(), events)
                    db.create_rows(model.lineups.Lineup.sql_table(), lineups)
                    db.create_rows(model.statistics.TeamStats.sql_table(), team_stats)
                    db.create_rows(model.statistics.PlayerStats.sql_table(), player_stats)


def main():
    competitions, seasons, teams = update_base_database.gather_competitions_and_seasons_and_teams()
    create_country_databases(competitions, seasons, teams)
    if lib.json_manager.capacity_exists():
        update_country_databases(
            args.countries,
            args.update_fixtures,
            args.update_events,
            args.update_lineups,
            args.update_player_stats,
            args.update_team_stats
        )
    else:
        lib.messages.vanilla_message(f"Out of capacity in requesting {lib.football_api.base_url}")


if __name__ == '__main__':
    args = parse_command_line()
    cli.cli.set_logging_options(args)
    main()
    sys.exit(os.EX_OK)

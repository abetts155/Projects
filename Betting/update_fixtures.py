import argparse
import json
import os
import typing

import cli.cli
import cli.user_input
import football_api.football_api
import football_api.structure
import lib.messages
import model.competitions
import model.fixtures
import model.seasons
import model.teams
import sql.sql
import sql.sql_columns
import sql.sql_language
from sql.sql_columns import ColumnNames


def parse_command_line():
    parser = argparse.ArgumentParser(description='Update database with fixture information')
    cli.cli.add_logging_options(parser)
    cli.cli.add_country_option(parser, False)
    cli.cli.add_past_option(parser)
    cli.cli.add_force_option(parser)
    cli.cli.add_competition_option(parser, True)

    parser.add_argument('-M',
                        '--missing-teams',
                        action='store_true',
                        help='add missing teams to the database',
                        default=False)

    return parser.parse_args()


def create_fixtures_json(competition: model.competitions.Competition, season: model.seasons.Season, force: bool):
    fixtures_json = football_api.structure.get_fixtures_json(competition.id, season.year)
    if not fixtures_json.exists() or force:
        lib.messages.vanilla_message(f"Updating '{fixtures_json}'")
        response = football_api.football_api.get_fixtures(competition.id, season.year)
        football_api.structure.store(fixtures_json, response)


def add_unknown_teams(unknown_teams: dict[int, str], teams: dict[int, model.teams.Team]):
    print("In the following, 'Y' or 'y' means Yes and anything else means No")
    new_teams = []
    for team_id, team_name in unknown_teams.items():
        answer = input(f"Add '{team_name}' to the database? ")
        if answer in ['y', 'Y']:
            country = cli.user_input.pick_country(False)
            row = [team_id, team_name, country, None]
            team = model.teams.create_team_from_row(row)
            new_teams.append(team)
            teams[team_id] = team

    with sql.sql.Database(football_api.structure.database) as db:
        table = model.teams.Team.sql_table()
        db.create_rows(table, new_teams)


def check_team_exists_in_database(team_id: int) -> typing.Optional[model.teams.Team]:
    with sql.sql.Database(football_api.structure.database) as db:
        table = model.teams.Team.sql_table()
        team_constraint = f"{ColumnNames.ID.name}='{team_id}'"
        team_rows = db.fetch_all_rows(table, [team_constraint])
        if team_rows:
            (row,) = team_rows
            return model.teams.create_team_from_row(row)


def load_fixture_data(
        competition: model.competitions.Competition,
        season: model.seasons.Season,
        add_missing_teams: bool
) -> list[model.fixtures.Fixture]:
    fixtures = []
    unknown_teams = {}

    fixtures_json = football_api.structure.get_fixtures_json(competition.id, season.year)
    if not fixtures_json.exists():
        lib.messages.warning_message(f"No fixtures available for competition {competition} in season {season.year}")
    else:
        team_ids = set()
        with fixtures_json.open() as in_file:
            json_text = json.load(in_file)

            for fixture_json in json_text['response']:
                home_id = int(fixture_json['teams']['home']['id'])
                away_id = int(fixture_json['teams']['away']['id'])
                team_ids.add(home_id)
                team_ids.add(away_id)

        teams = model.teams.load_teams(team_ids)
        with fixtures_json.open() as in_file:
            json_text = json.load(in_file)
            for fixture_json in json_text['response']:
                home_id = int(fixture_json['teams']['home']['id'])
                away_id = int(fixture_json['teams']['away']['id'])

                if home_id not in teams:
                    home_team = check_team_exists_in_database(home_id)
                    if home_team is None:
                        name = fixture_json['teams']['home']['name']
                        lib.messages.warning_message(f"Do not know the team whose ID is {home_id} called '{name}'")
                        unknown_teams[home_id] = name
                else:
                    home_team = teams[home_id]

                if away_id not in teams:
                    away_team = check_team_exists_in_database(away_id)
                    if away_team is None:
                        name = fixture_json['teams']['away']['name']
                        lib.messages.warning_message(f"Do not know the team whose ID is {away_id} called '{name}'")
                        unknown_teams[away_id] = name
                else:
                    away_team = teams[away_id]

                if home_team is not None and away_team is not None:
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

    if unknown_teams and add_missing_teams:
        add_unknown_teams(unknown_teams, teams)

    return fixtures


def update(past: bool, force: bool, competitions: list[model.competitions.Competition], add_missing_teams: bool):
    from sql.sql_columns import ColumnNames
    from sql.sql_language import Characters

    with sql.sql.Database(football_api.structure.database) as db:
        fixtures_table = model.fixtures.Fixture.sql_table()
        cup_fixtures_table = model.fixtures.CupFixture.sql_table()

        competitions.sort(key=lambda league: league.country)
        for competition in competitions:
            lib.messages.vanilla_message(f"{'>' * 10}  Updating {competition}  {'<' * 10}")

            competition_constraint = f"{ColumnNames.Competition_ID.name}={competition.id}"
            if past:
                current_constraint = f"{ColumnNames.Current.name}={Characters.FALSE.value}"
            else:
                current_constraint = f"{ColumnNames.Current.name}={Characters.TRUE.value}"

            season_rows = db.fetch_all_rows(
                model.seasons.Season.sql_table(),
                [competition_constraint, current_constraint]
            )
            assert season_rows

            for row in season_rows:
                season = model.seasons.create_season_from_row(row)
                lib.messages.vanilla_message(f'Updating season {season.year}')
                create_fixtures_json(competition, season, force)
                season_fixtures = load_fixture_data(competition, season, add_missing_teams)

                if competition.type == model.competitions.CompetitionType.LEAGUE:
                    db.create_rows(fixtures_table, season_fixtures)
                else:
                    db.create_rows(cup_fixtures_table, season_fixtures)


def main(past: bool, force: bool, competitions: list[int], countries: list[str], add_missing_teams: bool):
    league_competitions = model.competitions.get_competition_whitelist(model.competitions.CompetitionType.LEAGUE)
    cup_competitions = model.competitions.get_competition_whitelist(model.competitions.CompetitionType.CUP)

    if competitions:
        competitions = [
            competition for competition in league_competitions + cup_competitions if competition.id in competitions
        ]
    else:
        competitions = league_competitions
        if countries:
            competitions = [
                competition for competition in league_competitions if competition.country.casefold() in countries
            ]

    update(past, force, competitions, add_missing_teams)


if __name__ == '__main__':
    args = parse_command_line()
    cli.cli.set_logging_options(args)
    main(args.past, args.force, args.competition, args.country, args.missing_teams)
    exit(os.EX_OK)

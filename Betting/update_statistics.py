import argparse
import json
import os
import sys
import time
import typing

import cli.cli
import cli.user_input
from lib import football_api
import football_api.helpers
import lib.structure
import lib.messages
import model.fixtures
import model.competitions
import model.seasons
import model.statistics
import model.teams
import sql.sql


def parse_command_line():
    parser = argparse.ArgumentParser(description='Update database with fixture statistics')
    cli.cli.add_logging_options(parser)
    cli.cli.add_competition_option(parser, True)
    cli.cli.add_season_option(parser)
    return parser.parse_args()


def create_statistics_json(fixture: model.fixtures.Fixture) -> bool:
    stats_json = lib.structure.get_stats_json(fixture.id)
    created = False
    if not stats_json.exists():
        created = True
        lib.messages.vanilla_message(f"Extracting stats JSON for '{fixture.id}'")
        response = lib.football_api.get_stats(fixture.id)
        lib.structure.store(stats_json, response)
    return created


def load_stats_data(
        fixture: model.fixtures.Fixture,
        team: model.teams.Team
) -> typing.Optional[model.statistics.TeamStats]:
    stats_json = lib.structure.get_stats_json(fixture.id)
    with stats_json.open() as in_file:
        json_text = json.load(in_file)
        if json_text['response']:
            for team_json in json_text['response']:
                if team_json['team']['id'] == team.id:
                    return model.statistics.create_team_stats_from_json(fixture, team, team_json)


def main(
        competition: model.competitions.Competition,
        season: model.seasons.Season,
        teams: set[model.teams.Team]
):
    if season.statistics_fixtures:
        fixtures = model.seasons.load_fixtures(competition, season)
        with sql.sql.Database(lib.structure.database) as db:
            fixture_statistics = []
            for fixture in fixtures:
                if fixture.finished:
                    if fixture.home_team in teams or fixture.away_team in teams:
                        created = create_statistics_json(fixture)

                        home_stats = load_stats_data(fixture, fixture.home_team)
                        if home_stats is not None:
                            fixture_statistics.append(home_stats)

                        away_stats = load_stats_data(fixture, fixture.away_team)
                        if away_stats is not None:
                            fixture_statistics.append(away_stats)

                        if created:
                            time.sleep(1)

            table = model.statistics.TeamStats.sql_table()
            db.create_rows(table, fixture_statistics)
    else:
        lib.messages.warning_message(f"There are no stats for the games in {season.year}")


if __name__ == '__main__':
    args = parse_command_line()
    cli.cli.set_logging_options(args)

    competitions = []
    if args.competition:
        for competition_id in args.competition:
            competition = model.competitions.load_competition(competition_id)
            competitions.append(competition)
    else:
        country = cli.user_input.pick_country()
        competition = cli.user_input.pick_competition(country)
        competitions.append(competition)

    for competition in competitions:
        if args.season:
            season = model.seasons.load_season(competition, args.season)
        else:
            season = cli.user_input.pick_season(competition)

        fixtures = model.seasons.load_fixtures(competition, season)
        teams = model.fixtures.teams(fixtures)
        main(competition, season, teams)

    sys.exit(os.EX_OK)

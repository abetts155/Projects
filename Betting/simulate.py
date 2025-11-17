import argparse
import collections
import datetime
import numpy as np
import os
import plotly.graph_objects as go
import plotly.subplots
import sys
import typing

import cli.cli
import cli.user_input
import lib.messages
import lib.structure
import model.fixtures
import model.competitions
import model.events
import model.seasons
import model.statistics
import model.tables
import model.teams


def parse_command_line():
    parser = argparse.ArgumentParser(description='Simulate Football Games using a Monte Carlo Model')
    cli.cli.add_logging_options(parser)
    cli.cli.add_competition_option(parser)
    return parser.parse_args()


def set_competition(selected_competition: typing.Optional[int]) -> model.competitions.Competition:
    if selected_competition is not None:
        return model.competitions.load_competition(selected_competition)
    else:
        country = cli.user_input.pick_country()
        return cli.user_input.pick_competition(country)



def compute_averages(stats_by_team: dict[model.teams.Team, list[model.statistics.TeamStats]]):
    averages = collections.defaultdict(dict)
    for team, stats in stats_by_team.items():
        averages[team] = {
            "expected_goals": np.mean([s.expected_goals if s.expected_goals else 0 for s in stats]),
            "shots_on_goal": np.mean([s.shots_on_goal if s.shots_on_goal else 0 for s in stats]),
            "total_shots": np.mean([s.total_shots if s.total_shots else 0 for s in stats]),
            "corner_kicks": np.mean([s.corner_kicks if s.corner_kicks else 0 for s in stats]),
            "shots_inside_box": np.mean([s.shots_inside_box if s.shots_inside_box else 0 for s in stats]),
        }

    return averages

def estimate_expected_goals(stats: dict):
    return (

    )


def set_expected_goals(stats_by_team: dict[model.teams.Team, list[model.statistics.TeamStats]]):
    xG = {}
    averages = compute_averages(stats_by_team)
    for team, data in averages.items():
        if not data["expected_goals"]:
            xG[team] = (
                    0.09 * data["shots_on_goal"] +
                    0.03 * data["total_shots"] +
                    0.02 * data["corner_kicks"] +
                    0.04 * data["shots_inside_box"]
            )
        else:
            xG[team] = data["expected_goals"]
    return xG


def simulate_match(home_xG: float, away_xG: float, n=10000):
    goals_home = np.random.poisson(home_xG, n)
    goals_away = np.random.poisson(away_xG, n)
    total = goals_home + goals_away
    return {
        "xG_home": home_xG,
        "xG_away": away_xG,
        "BTTS": np.mean((goals_home > 0) & (goals_away > 0)),
        "Over 0.5": np.mean(total > 0.5),
        "Over 1.5": np.mean(total > 1.5),
        "Over 2.5": np.mean(total > 2.5),
    }


def main(selected_competition_id: typing.Optional[int]):
    competition = set_competition(selected_competition_id)
    database = lib.structure.get_database(competition.country)
    season = model.seasons.load_current_season(database, competition)

    lib.messages.vanilla_message(f"Analysing {competition} in {season.year} (id={competition.id})")

    fixtures = model.seasons.load_fixtures(database, competition, season)

    home_stats_by_team = collections.defaultdict(list)
    away_stats_by_team = collections.defaultdict(list)

    for fixture in fixtures:
        if fixture.finished:
            home_stats = model.statistics.load_team_stats(database, fixture, fixture.home_team)
            home_stats_by_team[fixture.home_team].append(home_stats)
            away_stats = model.statistics.load_team_stats(database, fixture, fixture.away_team)
            away_stats_by_team[fixture.away_team].append(away_stats)

    home_xg = set_expected_goals(home_stats_by_team)
    away_xg = set_expected_goals(away_stats_by_team)

    for fixture in fixtures:
        if not fixture.finished:
            print(fixture)
            print(simulate_match(home_xg[fixture.home_team], away_xg[fixture.away_team]))
            print()


if __name__ == '__main__':
    args = parse_command_line()
    cli.cli.set_logging_options(args)
    main(args.competition)
    sys.exit(os.EX_OK)

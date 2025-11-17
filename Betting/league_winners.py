import argparse
import collections
import datetime
import os
import pathlib

import matplotlib.pyplot as plt
import sys
import typing

import cli.cli
import cli.user_input
import lib.messages
import lib.structure
import model.fixtures
import model.competitions
import model.events
import model.players
import model.seasons
import model.tables
import model.teams


def parse_command_line():
    parser = argparse.ArgumentParser(description='Predict league winners')
    cli.cli.add_logging_options(parser)
    cli.cli.add_venue_option(parser)
    cli.cli.add_competition_option(parser)
    cli.cli.add_season_option(parser)
    cli.cli.add_team_option(parser, multiple=True)

    parser.add_argument('-G',
                        '--game-week',
                        help='only consider up to this game week',
                        metavar='<INT>',
                        type=int)

    return parser.parse_args()


def set_competition(selected_competition: typing.Optional[int]) -> model.competitions.Competition:
    if selected_competition is not None:
        return model.competitions.load_competition(selected_competition)
    else:
        country = cli.user_input.pick_country()
        return cli.user_input.pick_competition(country)


def collect_averages(
        competition: model.competitions.Competition,
        database:
        pathlib.Path,
        cols: list[str],
        winners: dict[int, model.teams.Team]
):
    this_season = model.seasons.load_current_season(database, competition)
    lib.messages.vanilla_message(f"Analysing {competition} in season {this_season.year} (id={competition.id})")
    seasons = model.seasons.load_seasons(database, competition)

    data = {}
    counts = {}
    for season in seasons:
        if season != this_season:
            winner = winners[season.year]
            fixtures = model.seasons.load_fixtures(database, competition, season)
            week = 0
            for fixture in fixtures:
                if winner in [fixture.home_team, fixture.away_team]:
                    result = fixture.result(model.fixtures.Period.FULL)
                    if winner == fixture.away_team:
                        result = result.reverse()

                    week += 1
                    if week not in data:
                        data[week] = {
                            model.tables.PTS_COL: 0,
                            model.tables.F_COL: 0,
                            model.tables.A_COL: 0,
                            model.tables.S_COL: 0,
                            model.tables.C_COL: 0
                        }
                        counts[week] = 0

                    if model.fixtures.win(result):
                        data[week][model.tables.PTS_COL] += 3
                    elif model.fixtures.draw(result):
                        data[week][model.tables.PTS_COL] += 1

                    data[week][model.tables.F_COL] += result.left
                    data[week][model.tables.A_COL] += result.right

                    if result.left > 0:
                        data[week][model.tables.S_COL] += 1

                    if result.right > 0:
                        data[week][model.tables.C_COL] += 1

                    counts[week] += 1

    cum_running = {col: 0 for col in cols}
    cum_data = {}
    avg_cum = {}
    for w in sorted(data.keys()):
        for col in cols:
            cum_running[col] += data[w].get(col, 0)
        cum_data[w] = {col: cum_running[col] for col in cols}
        n = counts.get(w, 0) or 1
        avg_cum[w] = {col: (cum_running[col] / n) for col in cols}

    return avg_cum


def collect_season_data(
        team: model.teams.Team,
        fixtures: list[model.fixtures.Fixture],
        cols: list[str],
        selected_game: typing.Optional[int]
):
    data = {}
    week = 0
    for fixture in fixtures:
        if fixture.finished:
            result = fixture.result(model.fixtures.Period.FULL)
            if team == fixture.away_team:
                result = result.reverse()

            week += 1
            data[week] = {
                model.tables.PTS_COL: 0,
                model.tables.F_COL: 0,
                model.tables.A_COL: 0,
                model.tables.S_COL: 0,
                model.tables.C_COL: 0
            }

            if model.fixtures.win(result):
                data[week][model.tables.PTS_COL] += 3
            elif model.fixtures.draw(result):
                data[week][model.tables.PTS_COL] += 1

            data[week][model.tables.F_COL] += result.left
            data[week][model.tables.A_COL] += result.right

            if result.left > 0:
                data[week][model.tables.S_COL] += 1

            if result.right > 0:
                data[week][model.tables.C_COL] += 1

            if selected_game is not None and selected_game == week:
                break

    cum_running = {col: 0 for col in cols}
    cum_data = {}
    for w in sorted(data.keys()):
        for col in cols:
            cum_running[col] += data[w].get(col, 0)
        cum_data[w] = {col: cum_running[col] for col in cols}

    return cum_data

import math
import plotly.graph_objects as go
from plotly.subplots import make_subplots

def plot_winners_subplots(
        cols,
        avg_cum,
        seasons_list: list[model.seasons.Season],
        teams_list: list[model.teams.Team],
        teams_data_list,
        cols_per_row: int = 5
):
    nrows = math.ceil(len(seasons_list) / cols_per_row)

    # --- Labels & Colors ---
    labels = {
        model.tables.PTS_COL: "Points",
        model.tables.F_COL:   "Goals For",
        model.tables.A_COL:   "Goals Against",
        model.tables.S_COL:   "Scoring Games",
        model.tables.C_COL:   "Conceding Games",
    }

    colors = {
        model.tables.PTS_COL: "#000000",
        model.tables.F_COL:   "royalblue",
        model.tables.A_COL:   "firebrick",
        model.tables.S_COL:   "darkorange",
        model.tables.C_COL:   "purple",
    }

    avg_weeks = sorted(avg_cum.keys())

    # --- Subplot grid ---
    fig = make_subplots(
        rows=nrows,
        cols=cols_per_row,
        shared_xaxes=False,
        shared_yaxes=False,
        vertical_spacing=0.07,
        horizontal_spacing=0.05,
        subplot_titles=[f"{season.year}: {team.name}" for season, team in zip(seasons_list, teams_list)]
    )

    # Only show legends in the first subplot
    def show_legend_for(idx): return idx == 0

    # --- Add data ---
    for i, (season, team, team_data) in enumerate(zip(seasons_list, teams_list, teams_data_list)):
        row = (i // cols_per_row) + 1
        col = (i % cols_per_row) + 1
        team_weeks = sorted(team_data.keys())
        last_week = team_weeks[-1] if team_weeks else None

        # Plot each metric
        for metric in cols:
            # Winner line (solid)
            y_team = [team_data[w].get(metric, 0) for w in team_weeks]
            fig.add_trace(
                go.Scatter(
                    x=team_weeks,
                    y=y_team,
                    mode="lines+markers",
                    line=dict(color=colors.get(metric, "#333333"), width=2),
                    name=f"Winner {labels.get(metric, metric)}",
                    legendgroup=metric,
                    showlegend=show_legend_for(i),
                    hovertemplate=f"Week %{{x}}<br>{labels.get(metric, metric)}: %{{y}}<extra></extra>"
                ),
                row=row, col=col
            )

            # Average champion line (dashed)
            if last_week is not None:
                x_avg = [w for w in avg_weeks if w <= last_week]
                y_avg = [avg_cum[w].get(metric, 0) for w in x_avg]
                fig.add_trace(
                    go.Scatter(
                        x=x_avg,
                        y=y_avg,
                        mode="lines",
                        line=dict(color=colors.get(metric, "#999999"), width=1.5, dash="dot"),
                        name=f"Avg Winner {labels.get(metric, metric)}",
                        legendgroup=metric,
                        showlegend=show_legend_for(i),
                        hovertemplate=f"Week %{{x}}<br>Avg {labels.get(metric, metric)}: %{{y:.2f}}<extra></extra>"
                    ),
                    row=row, col=col
                )

        fig.update_xaxes(title_text="Games played", row=row, col=col)
        fig.update_yaxes(title_text="Cumulative", row=row, col=col)

    # --- Layout ---
    fig.update_layout(
        template="plotly_white",
        hovermode="x unified",
        legend=dict(
            orientation="h",
            yanchor="bottom",
            y=-0.15,
            xanchor="center",
            x=0.5
        ),
        margin=dict(l=60, r=30, t=80, b=60)
    )

    y_max = float("-inf")
    for traces in fig.data:
        if traces.y is not None and len(traces.y) > 0:
            y_max = max(y_max, max(traces.y))
    fig.update_yaxes(range=[0, y_max+1])

    fig.show()



def main(
        selected_competition_id: typing.Optional[int],
        selected_season: typing.Optional[int],
        selected_game: typing.Optional[int]
):
    cols = [
        model.tables.PTS_COL,
        model.tables.F_COL,
        model.tables.A_COL
    ]
    competition = set_competition(selected_competition_id)
    database = lib.structure.get_database(competition.country)
    this_season = model.seasons.load_current_season(database, competition)

    seasons = model.seasons.load_seasons(database, competition)
    season_to_winners = {}
    winners_list = []
    seasons_list = []
    winners_data_list = []
    for season in seasons:
        if season != this_season:
            table = model.tables.LeagueTable(competition, season)
            winner = table[0][model.tables.TEAM_COL]
            season_to_winners[season.year] = winner
            seasons_list.append(season)
            winners_list.append(winner)
            fixtures = model.seasons.load_fixtures(database, competition, season)
            team_fixtures = model.fixtures.fixtures_per_team(fixtures)
            cum_data = collect_season_data(winner, team_fixtures[winner], cols, selected_game)
            winners_data_list.append(cum_data)

    avg_cum = collect_averages(competition, database, cols, season_to_winners)
    plot_winners_subplots(cols, avg_cum, seasons_list, winners_list, winners_data_list)

    this_season_fixtures = model.seasons.load_fixtures(database, competition, this_season)
    this_season_teams = model.fixtures.teams(this_season_fixtures)
    this_season_fixtures_per_team = model.fixtures.fixtures_per_team(this_season_fixtures)
    named = {'Liverpool', 'Manchester City', 'Arsenal'}
    named_teams = {team for team in this_season_teams if team.name in named}
    seasons_list = []
    teams_list = []
    teams_data_list = []
    for team in named_teams:
        seasons_list.append(this_season)
        teams_list.append(team)
        cum_data = collect_season_data(team, this_season_fixtures_per_team[team], cols, selected_game)
        teams_data_list.append(cum_data)

    plot_winners_subplots(cols, avg_cum, seasons_list, teams_list, teams_data_list, 3)


if __name__ == '__main__':
    args = parse_command_line()
    cli.cli.set_logging_options(args)
    main(args.competition, args.season, args.game_week)
    sys.exit(os.EX_OK)

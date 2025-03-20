import argparse
import itertools
import os
import plotly.graph_objects as go
import sys

import cli.cli
import cli.user_input
import model.fixtures
import model.competitions
import model.seasons
import model.statistics
import model.teams
import sql.sql
import update_statistics


def parse_command_line():
    parser = argparse.ArgumentParser(description='View statistics')
    cli.cli.add_database_option(parser)
    cli.cli.add_logging_options(parser)
    cli.cli.add_venue_option(parser)
    return parser.parse_args()


def show_data(team: model.teams.Team, venue: model.fixtures.Venue, g_for: list[float], g_against: list[float], xg_for: list[float], xg_against: list[float]):
    x_values = list(range(1, len(xg_for) + 1))

    fig = go.Figure()

    fig.add_trace(go.Scatter(
        x=x_values,
        y=g_for,
        mode='lines',
        name="G For",
        line=dict(color='#1F77B4', width=4)
    ))

    fig.add_trace(go.Scatter(
        x=x_values,
        y=xg_for,
        mode='lines',
        name="XG For",
        line=dict(color='#76B7EB', width=2, dash='dash')
    ))

    fig.add_trace(go.Scatter(
        x=x_values,
        y=g_against,
        mode='lines',
        name="G Against",
        line=dict(color='#D62728', width=4)
    ))

    fig.add_trace(go.Scatter(
        x=x_values,
        y=xg_against,
        mode='lines',
        name="XG Against",
        line=dict(color='#FF9896', width=2, dash='dash')
    ))

    fig.update_layout(
        xaxis=dict(title="Game", showgrid=True, tickmode="linear", tick0=1, dtick=1),
        yaxis=dict(title="Number of Goals", showgrid=True, zeroline=True),
        template="plotly_white",
        legend=dict(orientation="h", yanchor="bottom", y=-0.2, xanchor="center", x=0.5),
        hovermode="x unified"
    )

    fig.update_layout(
        title={
            'text': f"{team.name} {model.fixtures.Venue.to_string([venue])}: Goals Analysis",
            'x': 0.5,
            'xanchor': 'center',
            'yanchor': 'top',
            'font': dict(size=18, family="Arial", color="black")
        }
    )

    fig.show()


def gather_data(database: str, season: model.seasons.Season, team: model.teams.Team, venue: model.fixtures.Venue):
    g_for = []
    g_against = []
    xg_for = []
    xg_against = []
    with sql.sql.Database(database) as db:
        for fixture in season.fixtures():
            if fixture.finished:
                if team == fixture.home_team and venue in [model.fixtures.Venue.home, model.fixtures.Venue.anywhere]:
                    opponent_stats = model.statistics.load_stats_from_database(
                        database,
                        fixture,
                        fixture.away_team
                    )
                    team_stats = model.statistics.load_stats_from_database(
                        database,
                        fixture,
                        fixture.home_team
                    )

                    result: model.fixtures.Scoreline = fixture.result(model.fixtures.Half.full)
                    g_for.append(result.left)
                    g_against.append(result.right)
                    xg_for.append(team_stats.expected_goals)
                    xg_against.append(opponent_stats.expected_goals)

                elif team == fixture.away_team and venue in [model.fixtures.Venue.away, model.fixtures.Venue.anywhere]:
                    opponent_stats = model.statistics.load_stats_from_database(
                        database,
                        fixture,
                        fixture.home_team
                    )
                    team_stats = model.statistics.load_stats_from_database(
                        database,
                        fixture,
                        fixture.away_team
                    )

                    result: model.fixtures.Scoreline = fixture.result(model.fixtures.Half.full)
                    g_for.append(result.right)
                    g_against.append(result.left)
                    xg_for.append(team_stats.expected_goals)
                    xg_against.append(opponent_stats.expected_goals)

    return list(itertools.accumulate(g_for)), list(itertools.accumulate(g_against)), list(itertools.accumulate(xg_for)), list(itertools.accumulate(xg_against))


def main(database: str, venue: model.fixtures.Venue):
    country = cli.user_input.pick_country()
    league = cli.user_input.pick_league(database, country)
    season = model.seasons.load_current_season(database, league)
    team = cli.user_input.pick_team(season)
    update_statistics.main(database, season, {team})

    g_for, g_against, xg_for, xg_against = gather_data(database, season, team, venue)
    show_data(team, venue, g_for, g_against, xg_for, xg_against)


if __name__ == '__main__':
    args = parse_command_line()
    cli.cli.set_logging_options(args)
    main(args.database, args.venue)
    sys.exit(os.EX_OK)

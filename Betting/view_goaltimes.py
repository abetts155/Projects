import argparse
import collections
import datetime
import os
import plotly.graph_objects as go
import plotly.subplots
import sys
import typing

import cli.cli
import cli.user_input
import lib.messages
import model.fixtures
import model.competitions
import model.events
import model.seasons
import model.tables
import model.teams
import update_events


def parse_command_line():
    parser = argparse.ArgumentParser(description='View goal times for a particular team')
    cli.cli.add_logging_options(parser)
    cli.cli.add_venue_option(parser)
    cli.cli.add_competition_option(parser)
    cli.cli.add_season_option(parser)
    return parser.parse_args()


def place(n: int) -> str:
    suffix = ['th', 'st', 'nd', 'rd', 'th'][min(n % 10, 4)] if not 10 <= n % 100 <= 20 else 'th'
    return f"{n}{suffix}"


def construct_title(
        competition: model.competitions.Competition,
        venue: model.fixtures.Venue,
        for_colour: str,
        against_colour: str
):
    for_str = f"<span style='color:{for_colour};'>FOR</span>"
    against_str = f"<span style='color:{against_colour};'>AGAINST</span>"
    if venue == model.fixtures.Venue.ANYWHERE:
        venue_description = f"<span style='color:#d97706;'>{venue.value}</span> Games"
    else:
        venue_description = f"<span style='color:#d97706;'>{venue.value} ONLY</span>"
    league_description = f"<span style='color:#7f8c8d; font-weight:bold;'>{competition.name}</span>"
    today = datetime.datetime.today()
    title = (f"Goals {for_str}/{against_str} Times in {league_description} ({venue_description}) "
             f"as of {today.strftime('%d-%m-%y')}")
    return title


def get_intervals() -> list[model.events.Interval]:
    time_partition = [
        (1, 15),
        (16, 30),
        (31, 45),
        (46, 60),
        (61, 75),
        (76, 90)
    ]
    return [model.events.Interval(left, right) for left, right in time_partition]


def show_data(
        league_table: model.tables.LeagueTable,
        venue: model.fixtures.Venue,
        highlighted: set[model.teams.Team],
        goals_for_intervals: dict[model.teams.Team, list[model.events.Interval]],
        goals_against_intervals: dict[model.teams.Team, list[model.events.Interval]]
):
    max_y = 0
    subplot_titles = []
    for i, table_row in enumerate(league_table, start=1):
        for interval in goals_for_intervals[table_row.TEAM]:
            max_y = max(max_y, interval.total)

        if table_row.TEAM in highlighted:
            subplot_titles.append(f"<b>{table_row.TEAM.name} ({place(i)})</b>")
        else:
            subplot_titles.append(f"{table_row.TEAM.name} ({place(i)})")

        for interval in goals_against_intervals[table_row.TEAM]:
            max_y = max(max_y, interval.total)

    rows = 4
    cols = len(league_table) // rows
    if len(league_table) % rows != 0:
        cols += 1

    fig = plotly.subplots.make_subplots(
        rows=rows,
        cols=cols,
        shared_xaxes=False,
        subplot_titles=subplot_titles
    )

    interval_labels = [str(interval) for interval in get_intervals()]
    row = 1
    col = 1
    for_colour = "#3498db"
    against_colour = "#c0392b"
    for table_row in league_table:
        goals_for = [interval.total for interval in goals_for_intervals[table_row.TEAM]]
        fig.add_trace(
            go.Bar(
                x=interval_labels,
                y=goals_for,
                name=f'{table_row.TEAM.name} Goals For',
                marker_color=for_colour,
                opacity=0.9,
                hoverinfo='x+y'
            ),
            row=row, col=col
        )

        goals_against = [interval.total for interval in goals_against_intervals[table_row.TEAM]]
        fig.add_trace(
            go.Bar(
                x=interval_labels,
                y=goals_against,
                name=f'{table_row.TEAM.name} Goals Against',
                marker_color=against_colour,
                opacity=0.9,
                hoverinfo='x+y'
            ),
            row=row, col=col
        )

        fig.add_layout_image(
            dict(
                source=table_row.TEAM.get_team_logo(),
                x=0.05, y=1.1,
                xref="x domain", yref="y domain",
                sizex=0.15, sizey=0.15,
                xanchor="center", yanchor="middle",
                layer="above"
            ),
            row=row, col=col
        )

        fig.update_yaxes(
            range=[0, max_y],
            row=row, col=col,
            showgrid=True,
            gridcolor='lightgray'
        )

        fig.update_xaxes(
            showgrid=True,
            gridcolor='lightgray'
        )

        col += 1
        if col > cols:
            col = 1
            row += 1

    fig.update_layout(
        title=construct_title(competition, venue, for_colour, against_colour),
        title_x=0.5,
        title_yanchor="top",
        title_y=0.98,
        title_font=dict(size=18, family="Arial, sans-serif", color="black"),
        showlegend=False,
        margin=dict(t=90, b=40, l=40, r=40),
        plot_bgcolor='#f9f9f9',
        paper_bgcolor='#ffffff'
    )

    fig.show()


def categorise_goal_times(goals: collections.Counter[model.events.TimeKey, int]) -> list[model.events.Interval]:
    goal_intervals: list[model.events.Interval] = get_intervals()
    for time_key, count in goals.items():
        found = False
        index = 0
        while not found:
            interval = goal_intervals[index]
            index += 1
            if interval.belongs(time_key.time, count):
                found = True

    return goal_intervals


def gather_data(
        fixtures: list[model.fixtures.Fixture],
        teams: set[model.teams.Team],
        venue: model.fixtures.Venue
) -> tuple[dict[model.teams.Team, collections.Counter], dict[model.teams.Team, collections.Counter]]:
    goals_for = {}
    goals_against = {}
    for team in teams:
        goals_for[team] = collections.Counter()
        goals_against[team] = collections.Counter()

    for fixture in fixtures:
        if fixture.finished:
            events = model.events.load_events(fixture)
            for event in events:
                if model.events.is_goal(event.detail):
                    time_key = model.events.TimeKey(event.time, event.extra_time)
                    scoring_team = event.team

                    if scoring_team == fixture.home_team:
                        conceding_team = fixture.away_team
                    else:
                        conceding_team = fixture.home_team

                    if venue == model.fixtures.Venue.HOME:
                        if scoring_team == fixture.home_team:
                            goals_for[scoring_team][time_key] += 1
                        else:
                            goals_against[conceding_team][time_key] += 1
                    elif venue == model.fixtures.Venue.AWAY:
                        if scoring_team == fixture.away_team:
                            goals_for[scoring_team][time_key] += 1
                        else:
                            goals_against[conceding_team][time_key] += 1
                    else:
                        goals_for[scoring_team][time_key] += 1
                        goals_against[conceding_team][time_key] += 1

    return goals_for, goals_against


def main(
        competition: model.competitions.Competition,
        season: model.seasons.Season,
        venue: model.fixtures.Venue
):
    lib.messages.vanilla_message(f"Analysing {competition} in {season.year} (id={competition.id})")
    fixtures = model.seasons.load_fixtures(competition, season)
    teams = model.fixtures.teams(fixtures)
    update_events.main(competition, season)
    goals_for, goals_against = gather_data(fixtures, teams, venue)

    goals_for_intervals = {}
    for team, goals in goals_for.items():
        goals_for_intervals[team] = categorise_goal_times(goals)

    goals_against_intervals = {}
    for team, goals in goals_against.items():
        goals_against_intervals[team] = categorise_goal_times(goals)

    league_table = model.tables.LeagueTable(competition, season, model.fixtures.Period.FULL)

    highlighted = set()
    todays_date = datetime.datetime.today()
    for fixture in fixtures:
        if todays_date.date() == fixture.date.date():
            if venue in [model.fixtures.Venue.ANYWHERE, model.fixtures.Venue.HOME]:
                highlighted.add(fixture.home_team)

            if venue in [model.fixtures.Venue.ANYWHERE, model.fixtures.Venue.AWAY]:
                highlighted.add(fixture.away_team)

    show_data(league_table, venue, highlighted, goals_for_intervals, goals_against_intervals)


def set_season(
        competition: model.competitions.Competition,
        selected_season: typing.Optional[int]
) -> model.seasons.Season:
    if selected_season is not None:
        return model.seasons.load_season(competition, selected_season)
    else:
        return model.seasons.load_current_season(competition)


def set_competition(selected_competition: typing.Optional[int]) -> model.competitions.Competition:
    if selected_competition is not None:
        return model.competitions.load_competition(selected_competition)
    else:
        country = cli.user_input.pick_country()
        return cli.user_input.pick_competition(country)


if __name__ == '__main__':
    args = parse_command_line()
    cli.cli.set_logging_options(args)
    competition = set_competition(args.competition)
    season = set_season(competition, args.season)
    main(competition, season, args.venue)
    sys.exit(os.EX_OK)

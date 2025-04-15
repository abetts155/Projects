import argparse
import dataclasses
import datetime
import os
import plotly.graph_objects as go
import sys
import typing

import cli.cli
import cli.user_input
import lib.messages
import lib.structure
import model.fixtures
import model.competitions
import model.seasons
import model.statistics
import model.teams


def parse_command_line():
    parser = argparse.ArgumentParser(description='View scatter plot of a chosen statistic')
    cli.cli.add_logging_options(parser)
    cli.cli.add_venue_option(parser)
    cli.cli.add_competition_option(parser)
    cli.cli.add_season_option(parser)

    parser.add_argument('-T',
                        '--today',
                        action='store_true',
                        help='only consider teams playing today',
                        default=False)

    stat_options = []
    for field in dataclasses.fields(model.statistics.TeamStats):
        if field.type in {typing.Optional[int], typing.Optional[float]}:
            stat_options.append(field.name)
    parser.add_argument("-S",
                        "--stat",
                        type=str,
                        choices=stat_options,
                        help="The statistic to analyse (e.g., expected goals, shots on goal, corner kicks, etc.)",
                        default="goals")

    return parser.parse_args()


@dataclasses.dataclass(slots=True)
class TeamData:
    team: model.teams.Team
    positive: float = 0
    negative: float = 0
    games: int = 0

    def get_x(self) -> float:
        return self.positive / self.games

    def get_y(self) -> float:
        return self.negative / self.games


def construct_title(competition: model.competitions.Competition, venue: model.fixtures.Venue, stat_description: str):
    venue_colour = "#c0392b"
    if venue == model.fixtures.Venue.ANYWHERE:
        venue_description = f"<span style='color:{venue_colour};'>{venue.value}</span> Games"
    else:
        venue_description = f"<span style='color:{venue_colour};'>{venue.value} ONLY</span>"
    league_description = f"<span style='color:#7f8c8d; font-weight:bold;'>{competition.name}</span>"
    today = datetime.datetime.today()
    title = (f"{stat_description} Performance Matrix in {league_description} ({venue_description}) "
             f"as of {today.strftime('%d-%m-%y')}")
    return title


def show_data(
        stat: str,
        competition: model.competitions.Competition,
        highlighted: set[model.teams.Team],
        data: dict[model.teams.Team, TeamData],
        venue: model.fixtures.Venue
):
    x_values = []
    y_values = []
    team_names = []
    for team, team_data in data.items():
        x_values.append(team_data.get_x())
        y_values.append(team_data.get_y())
        team_names.append(team.name)

    x_mid = (max(x_values) + min(x_values)) / 2
    y_mid = (max(y_values) + min(y_values)) / 2
    x_min, x_max = min(x_values) - 0.1, max(x_values) + 0.1
    y_min, y_max = min(y_values) - 0.1, max(y_values) + 0.1
    logo_size_x = (x_max - x_min) * 0.05
    logo_size_y = (y_max - y_min) * 0.05

    fig = go.Figure()

    fig.add_trace(go.Scatter(
        x=x_values,
        y=y_values,
        mode='markers',
        marker=dict(size=0, color="black"),
        text=team_names,
        hoverinfo="x+y+text"
    ))

    for team, team_data in data.items():
        fig.add_layout_image(
            dict(
                source=team.get_logo() if team in highlighted else '',
                x=team_data.get_x(), y=team_data.get_y(),
                xref="x", yref="y",
                sizex=logo_size_x, sizey=logo_size_y,
                xanchor="center", yanchor="middle",
                opacity=0.9,
                layer="above"
            )
        )

    fig.add_shape(type="line", x0=x_mid, x1=x_mid, y0=y_min, y1=y_max, line=dict(color="black", width=2, dash="dash"))
    fig.add_shape(type="line", x0=x_min, x1=x_max, y0=y_mid, y1=y_mid, line=dict(color="black", width=2, dash="dash"))

    stat_descriptions = {
        "goals":
            ("Strong Attack", "Weak Attack", "Strong Defence", "Weak Defence"),
        "expected_goals":
            ("Strong at Creating Chances", "Weak at Creating Chances", "Strong at Preventing Chances", "Weak at Preventing Chances"),
        "shots_on_goal":
            ("High Shot Accuracy", "Low Shot Accuracy", "Defensively Resilient", "Defensively Susceptible"),
        "shots_off_goal":
            ("Frequently Off Target", "Few Off Target", "Good Shot Blocking", "Weak Shot Blocking"),
        "total_shots":
            ("High Shot Volume", "Low Shot Volume", "Strong Shot Prevention", "Weak Shot Prevention"),
        "blocked_shots":
            ("Defensively Solid", "Struggles to Block Shots", "Few Shots Blocked Against", "Frequent Shots Blocked Against"),
        "shots_inside_box":
            ("Threat Inside Box", "No Threat Inside Box", "Strong Defence in Box", "Weak Defence in Box"),
        "shots_outside_box":
            ("Long-Range Threat", "Rarely Shoots from Distance", "Strong Perimeter Defence", "Weak Perimeter Defence"),
        "fouls":
            ("Overly Aggressive", "Disciplined Play", "Few Fouls Suffered", "Many Fouls Suffered"),
        "corner_kicks":
            ("Win Many Corners", "Win Few Corners", "Concede Few Corners", "Concede Many Corners"),
        "offsides":
            ("Often Offside", "Rarely Offside", "Strong Offside Trap", "Weak Offside Trap"),
        "yellow_cards":
            ("Ill-Disciplined", "Well Disciplined", "Hardly Challenged", "Hardly Challenged"),
        "red_cards":
            ("Ill-Disciplined", "Rarely Sees Red", "Few Red Cards Against", "Frequent Red Cards Against"),
        "saves":
            ("Busy Goalkeeper", "Idle Goalkeeper", "Idle Opponent Goalkeeper", "Busy Opponent Goalkeeper"),
        "save_percentage":
            ("Great Shot Stopping", "Poor Shot Stopping", "Inspired Opponent Goalkeeping", "Weak Opponent Goalkeeping"),
        "passes":
            ("Possession Dominance", "Possession Struggles", "Passing Stability", "Erratic Passing"),
        "accurate_passes":
            ("High Passing Accuracy", "Low Passing Accuracy", "Strong Pressing Defence", "Weak Pressing Defence")
    }

    positive_strong, positive_weak, negative_strong, negative_weak = stat_descriptions[stat]

    color = "#3498db"
    size = 24
    fig.add_annotation(
        text=f"{positive_strong} / {negative_weak}",
        x=(x_max + x_mid) / 2, y=y_max,
        showarrow=False, font=dict(size=size, color=color)
    )

    fig.add_annotation(
        text=f"{positive_weak} / {negative_weak}",
        x=(x_min + x_mid) / 2, y=y_max,
        showarrow=False, font=dict(size=size, color=color)
    )

    fig.add_annotation(
        text=f"{positive_strong} / {negative_strong}",
        x=(x_max + x_mid) / 2, y=y_min,
        showarrow=False, font=dict(size=size, color=color)
    )

    fig.add_annotation(
        text=f"{positive_weak} / {negative_strong}",
        x=(x_min + x_mid) / 2, y=y_min,
        showarrow=False, font=dict(size=size, color=color)
    )


    stat_description = stat.replace("_", " ").title()

    fig.update_layout(
        title=dict(
            text=construct_title(competition, venue, stat_description),
            font=dict(size=30, color="black"),
            x=0.5,
            xanchor="center"
        ),
        xaxis=dict(
            title=f"{stat_description} For per Game",
            range=[x_min, x_max],
            showgrid=False
        ),
        yaxis=dict(
            title=f"{stat_description} Against per Game",
            range=[y_min, y_max],
            showgrid=False,
            autorange="reversed",
        ),
        template="plotly_white",
        showlegend=False,
        paper_bgcolor="#f9f9f9",
        plot_bgcolor="#ffffff"
    )

    fig.show()


def gather_data(
        stat: str,
        competition: model.competitions.Competition,
        fixtures: list[model.fixtures.Fixture],
        teams: set[model.teams.Team],
        venue: model.fixtures.Venue
) -> dict[model.teams.Team, TeamData]:
    database = lib.structure.get_database(competition.country)
    data = {team: TeamData(team) for team in teams}
    for fixture in fixtures:
        if fixture.finished:
            home_stats = model.statistics.load_team_stats(database, fixture, fixture.home_team)
            away_stats = model.statistics.load_team_stats(database, fixture, fixture.away_team)

            if home_stats is not None and away_stats is not None:
                home_value = getattr(home_stats, stat)
                away_value = getattr(away_stats, stat)

                if venue in [model.fixtures.Venue.ANYWHERE, model.fixtures.Venue.HOME]:
                    if home_value is not None and away_value is not None:
                        data[fixture.home_team].games += 1
                        data[fixture.home_team].positive += home_value
                        data[fixture.home_team].negative += away_value

                if venue in [model.fixtures.Venue.ANYWHERE, model.fixtures.Venue.AWAY]:
                    if home_value is not None and away_value is not None:
                        data[fixture.away_team].games += 1
                        data[fixture.away_team].positive += away_value
                        data[fixture.away_team].negative += home_value

    return data


def select_rounds(
        rounds: set[str]
) -> list[str]:
    selected = []
    print("In the following, 'N' or 'n' means No and anything else means Yes")
    for fixture_round in sorted(rounds):
        answer = input(f"Include '{fixture_round}'? ")
        if answer not in ['n', 'N']:
            selected.append(fixture_round)
    return selected


def set_season(
        competition: model.competitions.Competition,
        selected_season: typing.Optional[int]
) -> model.seasons.Season:
    database = lib.structure.get_database(competition.country)
    if selected_season is not None:
        return model.seasons.load_season(database, competition, selected_season)
    else:
        return model.seasons.load_current_season(database, competition)


def set_competition(selected_competition: typing.Optional[int]) -> model.competitions.Competition:
    if selected_competition is not None:
        return model.competitions.load_competition(selected_competition)
    else:
        country = cli.user_input.pick_country()
        return cli.user_input.pick_competition(country)


def main(
        competition_id: typing.Optional[int],
        season_id: typing.Optional[int],
        venue: model.fixtures.Venue,
        stat: str,
        highlight_today: bool
):
    competition = set_competition(competition_id)
    season = set_season(competition, season_id)
    lib.messages.vanilla_message(f"Analysing {competition} (id={competition.id})")
    database = lib.structure.get_database(competition.country)
    if season.statistics_fixtures:
        fixtures = model.seasons.load_fixtures(database, competition, season)
        if competition.type == model.competitions.CompetitionType.CUP:
            fixture: model.fixtures.CupFixture
            rounds = set()
            for fixture in fixtures:
                rounds.add(fixture.round)
            filtered_rounds = select_rounds(rounds)
            fixtures = [fixture for fixture in fixtures if fixture.round in filtered_rounds]

        teams = model.fixtures.teams(fixtures)
        data = gather_data(stat, competition, fixtures, teams, venue)

        if highlight_today:
            highlighted = set()
            today = datetime.datetime.today()
            for fixture in fixtures:
                if today.date() == fixture.date.date():
                    if venue in [model.fixtures.Venue.ANYWHERE, model.fixtures.Venue.HOME]:
                        highlighted.add(fixture.home_team)

                    if venue in [model.fixtures.Venue.ANYWHERE, model.fixtures.Venue.AWAY]:
                        highlighted.add(fixture.away_team)
        else:
            highlighted = teams

        show_data(stat, competition, highlighted, data, venue)


if __name__ == '__main__':
    args = parse_command_line()
    cli.cli.set_logging_options(args)
    main(args.competition, args.season, args.venue, args.stat, args.today)
    sys.exit(os.EX_OK)

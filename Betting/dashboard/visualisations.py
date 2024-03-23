from dataclasses import dataclass, field
from itertools import accumulate
from typing import List

from numpy import argmax
from plotly import graph_objects

from dashboard.data import DataBin, SeasonBin, TeamBin
from dashboard.colours import BAD_NEWS_COLOR, GOOD_NEWS_COLOR, HOME_COLOR, AWAY_COLOR, NEUTRAL_COLOR
from dashboard.fixtures import Period, Venue


HOVER_PREFIX = '<b>Season</b>: %{x}<br>'
HOVER_POSTFIX = '<extra></extra>'


def convert_seasons_into_labels(bins: List[DataBin]):
    return [data_bin.get_season_string() for data_bin in bins]


@dataclass
class Line:
    name: str
    mode: str
    hover_template: str = None
    marker: str = None
    shape: str = None
    dash: str = None
    x_values: List = field(default_factory=list)
    y_values: List = field(default_factory=list)
    colour: str = 'black'


def create_scatter(lines: List[Line], title_text: str, y_axis_label: str = ''):
    fig = graph_objects.Figure()

    for line in lines:
        fig.add_trace(
            graph_objects.Scatter(
                x=line.x_values,
                y=line.y_values,
                name=line.name,
                mode=line.mode,
                hovertemplate=line.hover_template if line.hover_template else None,
                marker=dict(size=10, symbol=line.marker),
                line=dict(color=line.colour, width=0.5, shape=line.shape, dash=line.dash)
            )
        )

    title = dict(
        text=title_text,
        x=0.5,
        y=0.95,
        xanchor='center',
        yanchor='top'
    )

    fig.update_layout(title=title, yaxis_title=y_axis_label)
    fig.update_xaxes(tickangle=45)
    return fig


def create_average_y_values(line: Line):
    return [sum(line.y_values) / len(line.y_values)] * len(line.y_values)


def create_results_scatter_graph(bins: List[DataBin], period: Period):
    x_values = convert_seasons_into_labels(bins)

    home_line = Line(
        name='Home Wins: Trend',
        mode='lines+markers',
        marker='triangle-up',
        shape='spline',
        x_values=x_values,
        colour=HOME_COLOR,
        hover_template=HOVER_PREFIX + '<i>Home Wins</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    away_line = Line(
        name='Away Wins: Trend',
        mode='lines+markers',
        marker='triangle-down',
        shape='spline',
        x_values=x_values,
        colour=AWAY_COLOR,
        hover_template=HOVER_PREFIX + '<i>Away Wins</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    draw_line = Line(
        name='Draws: Trend',
        mode='lines+markers',
        marker='circle',
        shape='spline',
        x_values=x_values,
        colour=NEUTRAL_COLOR,
        hover_template=HOVER_PREFIX + '<i>Draws</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    for data_bin in bins:
        home_line.y_values.append(100 * data_bin.home_wins / data_bin.total_games())
        draw_line.y_values.append(100 * data_bin.draws / data_bin.total_games())
        away_line.y_values.append(100 * data_bin.away_wins / data_bin.total_games())

    home_average_line = Line(
        name='Home Wins: Average',
        mode='lines',
        dash='dot',
        x_values=x_values,
        y_values=create_average_y_values(home_line),
        colour=HOME_COLOR,
        hover_template='<i>Average Home Wins</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    away_average_line = Line(
        name='Away Wins: Average',
        mode='lines',
        dash='dash',
        x_values=x_values,
        y_values=create_average_y_values(away_line),
        colour=AWAY_COLOR,
        hover_template='<i>Average Away Wins</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    draw_average_line = Line(
        name='Draws: Average',
        mode='lines',
        dash='longdash',
        x_values=x_values,
        y_values=create_average_y_values(draw_line),
        colour=NEUTRAL_COLOR,
        hover_template='<i>Average Draws</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    lines = [home_line, away_line, draw_line, home_average_line, away_average_line , draw_average_line]
    title_text = f'Results per Season ({period})'
    scatter = create_scatter(lines=lines, title_text=title_text, y_axis_label='%')
    return scatter


def create_goals_scatter_graph(bins: List[DataBin], period: Period, venue: Venue = None):
    x_values = convert_seasons_into_labels(bins)

    line = Line(
        name='Trend',
        mode='lines+markers',
        marker='diamond',
        shape='spline',
        x_values=x_values,
        hover_template=HOVER_PREFIX + '<i>Average Goals</i>: %{y:.1f}' + HOVER_POSTFIX
    )

    for data_bin in bins:
        total_goals = sum([goals * frequency for goals, frequency in data_bin.goal_counts.items()])
        line.y_values.append(total_goals / data_bin.total_games())

    average_line = Line(
        name='Average',
        mode='lines',
        dash='longdash',
        x_values=x_values,
        y_values=create_average_y_values(line),
        hover_template='<i>Average</i>: %{y:.1f}' + HOVER_POSTFIX
    )

    prefix = 'Average Goals per Season'
    if venue:
        title_text = f'{prefix} ({period}, {venue})'
    else:
        title_text = f'{prefix} ({period})'

    if period == Period.FULL:
        y_axis_label = 'Per Game'
    else:
        y_axis_label = 'Per Half'

    scatter = create_scatter(lines=[line, average_line], title_text=title_text, y_axis_label=y_axis_label)
    return scatter


def create_bts_scatter_graph(bins: List[DataBin], period: Period):
    x_values = convert_seasons_into_labels(bins)

    line = Line(
        name='Trend',
        mode='lines+markers',
        marker='star',
        shape='spline',
        x_values=x_values,
        colour=NEUTRAL_COLOR,
        hover_template=HOVER_PREFIX + '<i>BTS </i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    for data_bin in bins:
        line.y_values.append(100 * data_bin.bts / data_bin.total_games())

    average_line = Line(
        name='Average',
        mode='lines',
        dash='longdash',
        x_values=x_values,
        y_values=create_average_y_values(line),
        colour=NEUTRAL_COLOR,
        hover_template='<i>Average</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    title_text = f'Both Teams Scored per Season ({period})'
    scatter = create_scatter(lines=[line, average_line], title_text=title_text, y_axis_label='%')
    return scatter


def create_scored_and_conceded_scatter_graph(bins: List[TeamBin], period: Period, venue: Venue):
    x_values = convert_seasons_into_labels(bins)

    scored_line = Line(
        name='Scored: Trend',
        mode='lines+markers',
        marker='circle',
        shape='spline',
        x_values=x_values,
        colour=GOOD_NEWS_COLOR,
        hover_template=HOVER_PREFIX + '<i>Scoring Rate</i>: %{y:.1f} goals' + HOVER_POSTFIX
    )

    conceded_line = Line(
        name='Conceded: Trend',
        mode='lines+markers',
        marker='x',
        shape='spline',
        x_values=x_values,
        colour=BAD_NEWS_COLOR,
        hover_template=HOVER_PREFIX + '<i>Conceding Rate</i>: %{y:.1f} goals' + HOVER_POSTFIX
    )

    for data_bin in bins:
        scored_line.y_values.append(data_bin.scored / data_bin.total_games())
        conceded_line.y_values.append(data_bin.conceded / data_bin.total_games())

    scored_average_line = Line(
        name='Scored: Average',
        mode='lines',
        dash='dot',
        x_values=x_values,
        y_values=create_average_y_values(scored_line),
        colour=GOOD_NEWS_COLOR,
        hover_template='<i>Average Scoring Rate</i>: %{y:.1f} goals' + HOVER_POSTFIX
    )

    conceded_average_line = Line(
        name='Conceded: Average',
        mode='lines',
        dash='dash',
        x_values=x_values,
        y_values=create_average_y_values(conceded_line),
        colour=BAD_NEWS_COLOR,
        hover_template='<i>Average Conceding Rate</i>: %{y:.1f} goals' + HOVER_POSTFIX
    )

    lines = [scored_line, conceded_line, scored_average_line, conceded_average_line]
    title_text = f'Goals Scored/Conceded Rates per Season ({period}, {venue})'
    if period == Period.FULL:
        y_axis_label = 'Per Game'
    else:
        y_axis_label = 'Per Half'
    scatter = create_scatter(lines=lines, title_text=title_text, y_axis_label=y_axis_label)
    return scatter


def create_wins_draw_losses_scatter_graph(bins: List[TeamBin], period: Period, venue: Venue):
    x_values = convert_seasons_into_labels(bins)

    wins_line = Line(
        name='Wins: Trend',
        mode='lines+markers',
        marker='circle',
        shape='spline',
        x_values=x_values,
        colour=GOOD_NEWS_COLOR,
        hover_template=HOVER_PREFIX + '<i>Win Rate</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    draws_line = Line(
        name='Draws: Trend',
        mode='lines+markers',
        marker='star-square',
        shape='spline',
        x_values=x_values,
        colour=NEUTRAL_COLOR,
        hover_template=HOVER_PREFIX + '<i>Draw Rate</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    losses_line = Line(
        name='Losses: Trend',
        mode='lines+markers',
        marker='x',
        shape='spline',
        x_values=x_values,
        colour=BAD_NEWS_COLOR,
        hover_template=HOVER_PREFIX + '<i>Loss Rate</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    for data_bin in bins:
        wins_line.y_values.append(100 * data_bin.team_wins / data_bin.total_games())
        draws_line.y_values.append(100 * data_bin.team_draws / data_bin.total_games())
        losses_line.y_values.append(100 * data_bin.team_losses / data_bin.total_games())

    wins_average_line = Line(
        name='Wins: Average',
        mode='lines',
        dash='dot',
        x_values=x_values,
        y_values=create_average_y_values(wins_line),
        colour=GOOD_NEWS_COLOR,
        hover_template='<i>Average Win Rate</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    draws_average_line = Line(
        name='Draws: Average',
        mode='lines',
        dash='dash',
        x_values=x_values,
        y_values=create_average_y_values(draws_line),
        colour=NEUTRAL_COLOR,
        hover_template='<i>Average Draw Rate</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    losses_average_line = Line(
        name='Losses: Average',
        mode='lines',
        dash='longdash',
        x_values=x_values,
        y_values=create_average_y_values(losses_line),
        colour=BAD_NEWS_COLOR,
        hover_template='<i>Average Loss Rate</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    lines = [wins_line, draws_line, losses_line, wins_average_line, draws_average_line, losses_average_line]
    title_text = f'Win/Draw/Loss Rates per Season ({period}, {venue})'
    scatter = create_scatter(lines=lines, title_text=title_text, y_axis_label='%')
    return scatter


def create_team_goals_trend(data_bin: SeasonBin, period: Period, venue: Venue, team_name: str):
    x_values = []
    scored_y_values = []
    conceded_y_values = []
    for game in range(len(data_bin.scored)):
        x_values.append(game + 1)
        scored_y_values.append(data_bin.scored[game] / (game + 1))
        conceded_y_values.append(data_bin.conceded[game] / (game + 1))

    scored_line = Line(
        name='Goals For: Trend',
        mode='lines+markers',
        marker='circle',
        shape='spline',
        x_values=x_values,
        y_values=scored_y_values,
        colour=GOOD_NEWS_COLOR,
        hover_template=HOVER_PREFIX + '<i>Goals For Rate</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    conceded_line = Line(
        name='Goals Against: Trend',
        mode='lines+markers',
        marker='x',
        shape='spline',
        x_values=x_values,
        y_values=conceded_y_values,
        colour=BAD_NEWS_COLOR,
        hover_template=HOVER_PREFIX + '<i>Goals Against Rate</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    title_text = f'Goals Scored/Goals Against Rates over the Season ({period}, {venue})'
    scatter = create_scatter(lines=[scored_line, conceded_line], title_text=title_text, y_axis_label='%')
    return scatter


def create_team_results_trend(data_bin: SeasonBin, period: Period, venue: Venue, team_name: str):
    x_values = []
    win_y_values = []
    draw_y_values = []
    loss_y_values = []
    for game in range(len(data_bin.scored)):
        x_values.append(game + 1)
        win_y_values.append(100 * data_bin.team_wins[game] / (game + 1))
        draw_y_values.append(100 * data_bin.team_draws[game] / (game + 1))
        loss_y_values.append(100 * data_bin.team_losses[game] / (game + 1))

    win_line = Line(
        name='Wins: Trend',
        mode='lines+markers',
        marker='circle',
        shape='spline',
        x_values=x_values,
        y_values=win_y_values,
        colour=GOOD_NEWS_COLOR,
        hover_template=HOVER_PREFIX + '<i>Win Rate</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    draw_line = Line(
        name='Draws: Trend',
        mode='lines+markers',
        marker='star-square',
        shape='spline',
        x_values=x_values,
        y_values=draw_y_values,
        colour=NEUTRAL_COLOR,
        hover_template=HOVER_PREFIX + '<i>Draw Rate</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    loss_line = Line(
        name='Losses: Trend',
        mode='lines+markers',
        marker='x',
        shape='spline',
        x_values=x_values,
        y_values=loss_y_values,
        colour=BAD_NEWS_COLOR,
        hover_template=HOVER_PREFIX + '<i>Loss Rate</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    title_text = f'Win/Draw/Loss Rates over the Season ({period}, {venue})'
    scatter = create_scatter(lines=[win_line, draw_line, loss_line], title_text=title_text, y_axis_label='%')
    return scatter


def create_scorelines_heatmap(bins: List[DataBin], period: Period, venue: Venue = None):
    if period == Period.FIRST:
        scorelines_of_interest = ['0-0', '1-1',
                                  '1-0', '2-0',
                                  '0-1', '0-2',
                                  'other']
        split = [1.5, 3.5, 5.5]
    elif period == Period.SECOND:
        scorelines_of_interest = ['0-0', '1-1',
                                  '1-0', '2-0', '2-1',
                                  '0-1', '0-2', '1-2',
                                  'other']
        split = [1.5, 4.5, 7.5]
    else:
        scorelines_of_interest = ['0-0', '1-1', '2-2',
                                  '1-0', '2-0', '2-1', '3-0', '3-1', '3-2', '4-0',
                                  '0-1', '0-2', '1-2', '0-3', '1-3', '2-3', '0-4',
                                  'other']
        split = [2.5, 9.45, 16.45]

    frequencies = []
    for data_bin in bins:
        frequencies.append([0] * len(scorelines_of_interest))
        for (a, b), frequency in data_bin.results.items():
            key = '{}-{}'.format(a, b)
            if key in scorelines_of_interest:
                index = scorelines_of_interest.index(key)
                frequencies[-1][index] += frequency
            else:
                frequencies[-1][-1] += frequency

    z_values = []
    for per_season in frequencies:
        total = sum(per_season)
        z_values.append([round(100 * frequency / total) for frequency in per_season])

    fig = graph_objects.Figure()
    marker_size = 10
    line_width = 0.5

    y_values = convert_seasons_into_labels(bins)
    heatmap = graph_objects.Heatmap(
        x=scorelines_of_interest,
        y=y_values,
        z=z_values,
        hovertemplate='Score: %{x}<br>' + 'Season: %{y}<br>' + 'Frequency: %{z}%<extra></extra>',
        colorscale='reds',
        colorbar_title='%',
        xgap=5,
        ygap=2,
        texttemplate='%{z}'
    )

    fig.add_trace(heatmap)
    for x in split:
        fig.add_vline(x, line=dict(color='black', width=3, dash='dot'))

    prefix = 'Scoreline Trends by Season'
    if venue:
        title_text = f'{prefix} ({period}, {venue})'
    else:
        title_text = f'{prefix} ({period})'

    title = {
        'text': title_text,
        'x': 0.5,
        'y': 0.95,
        'xanchor': 'center',
        'yanchor': 'top'
    }

    fig.update_layout(
        title=title,
        yaxis=dict(tickvals=y_values, ticktext=[str(label) for label in y_values], type='category')
    )
    return fig


@dataclass
class PieChunk:
    label: str
    colour: str
    value: int = 0


def create_pie(chunks: List[PieChunk], title_text: str):
    max_index = argmax([chunk.value for chunk in chunks])
    pull = []
    for i, _ in enumerate(chunks):
        if i == max_index:
            pull.append(0.1)
        else:
            pull.append(0)

    fig = graph_objects.Figure()

    fig.add_trace(
        graph_objects.Pie(
            values=[chunk.value for chunk in chunks],
            labels=[chunk.label for chunk in chunks],
            pull=pull,
            sort=False,
            direction='clockwise',
            showlegend=False,
            hole=.35,
            hoverinfo='value',
            textinfo='percent+label',
            marker=dict(colors=[chunk.colour for chunk in chunks], line=dict(color='#000000', width=1))
        )
    )

    title = {
        'text': title_text,
        'x': 0.5,
        'y': 0.95,
        'xanchor': 'center',
        'yanchor': 'top'
    }
    fig.update_layout(title=title)
    return fig


def create_goals_pie(bins: List[DataBin], period: Period, venue: Venue = None):
    if period == Period.FULL:
        chunks = [
            PieChunk(label='0 goals', colour='#003f5c'),
            PieChunk(label='1 goal', colour='#444e86'),
            PieChunk(label='2 goals', colour='#955196'),
            PieChunk(label='3 goals', colour='#dd5182'),
            PieChunk(label='4 goals', colour='#ff6e54'),
            PieChunk(label='5+ goals', colour='#ffa600')
        ]
    else:
        chunks = [
            PieChunk(label='0 goals', colour='#003f5c'),
            PieChunk(label='1 goal', colour='#7a5195'),
            PieChunk(label='2 goals', colour='#ef5675'),
            PieChunk(label='3+ goals', colour='#ffa600')
        ]

    sizes = [0] * len(chunks)
    for data_bin in bins:
        for goals, frequency in data_bin.goal_counts.items():
            if goals <= len(sizes) - 1:
                sizes[goals] += frequency
            else:
                sizes[-1] += frequency

    for size, chunk in zip(sizes, chunks):
        chunk.value = size

    prefix = 'Goal Rate over All Seasons'
    if venue:
        title_text = f'{prefix} ({period}, {venue})'
    else:
        title_text = f'{prefix} ({period})'

    pie = create_pie(chunks, title_text=title_text)
    return pie


def create_results_pie_for_league(bins: List[DataBin], period: Period):
    chunks = [
        PieChunk(
            label='Home Wins',
            colour=HOME_COLOR,
            value=sum([data_bin.home_wins for data_bin in bins])
        ),

        PieChunk(
            label='Draws',
            colour=NEUTRAL_COLOR,
            value=sum([data_bin.draws for data_bin in bins])
        ),

        PieChunk(
            label='Away Wins',
            colour=AWAY_COLOR,
            value=sum([data_bin.away_wins for data_bin in bins])
        )
    ]

    pie = create_pie(chunks, title_text=f'Results Breakdown over All Seasons ({period})')
    return pie


def create_results_pie_for_team(bins: List[TeamBin], period: Period, venue: Venue):
    win_chunk = PieChunk(
        label='Wins',
        colour=GOOD_NEWS_COLOR,
        value=sum([data_bin.team_wins for data_bin in bins])
    )

    draw_chunk = PieChunk(
        label='Draws',
        colour=NEUTRAL_COLOR,
        value=sum([data_bin.team_draws for data_bin in bins])
    )

    loss_chunk = PieChunk(
        label='Losses',
        colour=BAD_NEWS_COLOR,
        value=sum([data_bin.team_losses for data_bin in bins])
    )

    title_text = f'Results Breakdown over All Seasons ({period}, {venue})'
    pie = create_pie([win_chunk, draw_chunk, loss_chunk], title_text=title_text)
    return pie

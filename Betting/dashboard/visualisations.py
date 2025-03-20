import dataclasses
import numpy
import plotly.graph_objects

import model.fixtures

from dashboard import colours, data, analysis


SEASON_HOVER_PREFIX = '<b>Season</b>: %{x}<br>'
GAME_HOVER_PREFIX = '<b>Game</b>: %{x}<br>'
HOVER_POSTFIX = '<extra></extra>'


def blank_graph():
    fig = plotly.graph_objects.Figure(plotly.graph_objects.Scatter(x=[], y=[]))
    fig.update_layout(template=None)
    fig.update_xaxes(showgrid=False, showticklabels=False, zeroline=False)
    fig.update_yaxes(showgrid=False, showticklabels=False, zeroline=False)
    return fig


def convert_seasons_into_labels(bins: list[data.DataBin]):
    return [data_bin.get_season_string() for data_bin in bins]


@dataclasses.dataclass
class Line:
    name: str
    mode: str
    hover_template: str = None
    marker: str = None
    shape: str = None
    dash: str = None
    x_values: list = dataclasses.field(default_factory=list)
    y_values: list = dataclasses.field(default_factory=list)
    colour: str = colours.NEUTRAL_COLOR
    line_width: float = 0.5


def create_scatter(lines: list[Line], title_text: str, x_axis_label: str = '', y_axis_label: str = ''):
    fig = plotly.graph_objects.Figure()

    for line in lines:
        fig.add_trace(
            plotly.graph_objects.Scatter(
                x=line.x_values,
                y=line.y_values,
                name=line.name,
                mode=line.mode,
                hovertemplate=line.hover_template if line.hover_template else None,
                marker=dict(size=8, symbol=line.marker),
                line=dict(color=line.colour, width=line.line_width, shape=line.shape, dash=line.dash)
            )
        )

    title = dict(
        text=title_text,
        x=0.5,
        y=0.95,
        xanchor='center',
        yanchor='top'
    )

    fig.update_layout(title=title, xaxis_title=x_axis_label, yaxis_title=y_axis_label)
    fig.update_xaxes(tickangle=45)
    return fig


def create_average_y_values(line: Line):
    return [sum(line.y_values) / len(line.y_values)] * len(line.y_values)


def create_results_scatter_graph(bins: list[data.SeasonBin], period: model.fixtures.Period):
    x_values = convert_seasons_into_labels(bins)

    home_line = Line(
        name='Home Wins: Trend',
        mode='lines+markers',
        marker='triangle-up',
        shape='linear',
        x_values=x_values,
        colour=colours.HOME_COLOR,
        hover_template=SEASON_HOVER_PREFIX + '<i>Home Wins</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    away_line = Line(
        name='Away Wins: Trend',
        mode='lines+markers',
        marker='triangle-down',
        shape='linear',
        x_values=x_values,
        colour=colours.AWAY_COLOR,
        hover_template=SEASON_HOVER_PREFIX + '<i>Away Wins</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    draw_line = Line(
        name='Draws: Trend',
        mode='lines+markers',
        marker='circle',
        shape='linear',
        x_values=x_values,
        colour=colours.NEUTRAL_COLOR,
        hover_template=SEASON_HOVER_PREFIX + '<i>Draws</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    for data_bin in bins:
        home_line.y_values.append(100 * data_bin.home_wins / data_bin.total_games)
        draw_line.y_values.append(100 * data_bin.draws / data_bin.total_games)
        away_line.y_values.append(100 * data_bin.away_wins / data_bin.total_games)

    home_average_line = Line(
        name='Home Wins: Average',
        mode='lines',
        dash='dot',
        x_values=x_values,
        y_values=create_average_y_values(home_line),
        colour=colours.HOME_COLOR,
        hover_template='<i>Average Home Wins</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    away_average_line = Line(
        name='Away Wins: Average',
        mode='lines',
        dash='dash',
        x_values=x_values,
        y_values=create_average_y_values(away_line),
        colour=colours.AWAY_COLOR,
        hover_template='<i>Average Away Wins</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    draw_average_line = Line(
        name='Draws: Average',
        mode='lines',
        dash='longdash',
        x_values=x_values,
        y_values=create_average_y_values(draw_line),
        colour=colours.NEUTRAL_COLOR,
        hover_template='<i>Average Draws</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    lines = [home_line, away_line, draw_line, home_average_line, away_average_line, draw_average_line]
    title_text = f'Results per Season ({period})'
    scatter = create_scatter(lines=lines, title_text=title_text, y_axis_label='%')
    return scatter


def create_goals_scatter_graph(bins: list[data.DataBin], period: model.fixtures.Period, venue: model.fixtures.Venue = None):
    x_values = convert_seasons_into_labels(bins)

    line = Line(
        name='Trend',
        mode='lines+markers',
        marker='diamond',
        shape='linear',
        x_values=x_values,
        hover_template=SEASON_HOVER_PREFIX + '<i>Average Goals</i>: %{y:.1f}' + HOVER_POSTFIX
    )

    for data_bin in bins:
        total_goals = sum([goals * frequency for goals, frequency in data_bin.goal_counts.items()])
        line.y_values.append(total_goals / data_bin.total_games)

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

    if period == model.fixtures.Period.FULL:
        y_axis_label = 'Per Game'
    else:
        y_axis_label = 'Per Half'

    scatter = create_scatter(lines=[line, average_line], title_text=title_text, y_axis_label=y_axis_label)
    return scatter


def create_bts_scatter_graph(bins: list[data.DataBin], period: model.fixtures.Period):
    x_values = convert_seasons_into_labels(bins)

    line = Line(
        name='Trend',
        mode='lines+markers',
        marker='star',
        shape='linear',
        x_values=x_values,
        colour=colours.NEUTRAL_COLOR,
        hover_template=SEASON_HOVER_PREFIX + '<i>BTS </i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    for data_bin in bins:
        line.y_values.append(100 * data_bin.bts / data_bin.total_games)

    average_line = Line(
        name='Average',
        mode='lines',
        dash='longdash',
        x_values=x_values,
        y_values=create_average_y_values(line),
        colour=colours.NEUTRAL_COLOR,
        hover_template='<i>Average</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    title_text = f'Both Teams Scored per Season ({period})'
    scatter = create_scatter(lines=[line, average_line], title_text=title_text, y_axis_label='%')
    return scatter


def create_scored_and_conceded_scatter_graph(bins: list[data.TeamBin], period: model.fixtures.Period, venue: model.fixtures.Venue):
    x_values = convert_seasons_into_labels(bins)

    scored_line = Line(
        name='Scored: Trend',
        mode='lines+markers',
        marker='circle',
        shape='linear',
        x_values=x_values,
        colour=colours.GOOD_NEWS_BACKGROUND_COLOR,
        hover_template=SEASON_HOVER_PREFIX + '<i>Scoring Rate</i>: %{y:.1f} goals' + HOVER_POSTFIX
    )

    conceded_line = Line(
        name='Conceded: Trend',
        mode='lines+markers',
        marker='x',
        shape='linear',
        x_values=x_values,
        colour=colours.BAD_NEWS_BACKGROUND_COLOR,
        hover_template=SEASON_HOVER_PREFIX + '<i>Conceding Rate</i>: %{y:.1f} goals' + HOVER_POSTFIX
    )

    for data_bin in bins:
        scored_line.y_values.append(data_bin.scored / data_bin.total_games)
        conceded_line.y_values.append(data_bin.conceded / data_bin.total_games)

    scored_average_line = Line(
        name='Scored: Average',
        mode='lines',
        dash='dot',
        x_values=x_values,
        y_values=create_average_y_values(scored_line),
        colour=colours.GOOD_NEWS_BACKGROUND_COLOR,
        hover_template='<i>Average Scoring Rate</i>: %{y:.1f} goals' + HOVER_POSTFIX
    )

    conceded_average_line = Line(
        name='Conceded: Average',
        mode='lines',
        dash='dash',
        x_values=x_values,
        y_values=create_average_y_values(conceded_line),
        colour=colours.BAD_NEWS_BACKGROUND_COLOR,
        hover_template='<i>Average Conceding Rate</i>: %{y:.1f} goals' + HOVER_POSTFIX
    )

    lines = [scored_line, conceded_line, scored_average_line, conceded_average_line]
    title_text = f'Goals Scored/Conceded Rates per Season ({period}, {venue})'
    if period == model.fixtures.Period.FULL:
        y_axis_label = 'Per Game'
    else:
        y_axis_label = 'Per Half'
    scatter = create_scatter(lines=lines, title_text=title_text, y_axis_label=y_axis_label)
    return scatter


def create_wins_draw_losses_scatter_graph(bins: list[data.TeamBin], period: model.fixtures.Period, venue: model.fixtures.Venue):
    x_values = convert_seasons_into_labels(bins)

    wins_line = Line(
        name='Wins: Trend',
        mode='lines+markers',
        marker='circle',
        shape='linear',
        x_values=x_values,
        colour=colours.GOOD_NEWS_BACKGROUND_COLOR,
        hover_template=SEASON_HOVER_PREFIX + '<i>Win Rate</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    draws_line = Line(
        name='Draws: Trend',
        mode='lines+markers',
        marker='star-square',
        shape='linear',
        x_values=x_values,
        colour=colours.NEUTRAL_COLOR,
        hover_template=SEASON_HOVER_PREFIX + '<i>Draw Rate</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    losses_line = Line(
        name='Losses: Trend',
        mode='lines+markers',
        marker='x',
        shape='linear',
        x_values=x_values,
        colour=colours.BAD_NEWS_BACKGROUND_COLOR,
        hover_template=SEASON_HOVER_PREFIX + '<i>Loss Rate</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    for data_bin in bins:
        wins_line.y_values.append(100 * data_bin.wins / data_bin.total_games)
        draws_line.y_values.append(100 * data_bin.draws / data_bin.total_games)
        losses_line.y_values.append(100 * data_bin.losses / data_bin.total_games)

    wins_average_line = Line(
        name='Wins: Average',
        mode='lines',
        dash='dot',
        x_values=x_values,
        y_values=create_average_y_values(wins_line),
        colour=colours.GOOD_NEWS_BACKGROUND_COLOR,
        hover_template='<i>Average Win Rate</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    draws_average_line = Line(
        name='Draws: Average',
        mode='lines',
        dash='dash',
        x_values=x_values,
        y_values=create_average_y_values(draws_line),
        colour=colours.NEUTRAL_COLOR,
        hover_template='<i>Average Draw Rate</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    losses_average_line = Line(
        name='Losses: Average',
        mode='lines',
        dash='longdash',
        x_values=x_values,
        y_values=create_average_y_values(losses_line),
        colour=colours.BAD_NEWS_BACKGROUND_COLOR,
        hover_template='<i>Average Loss Rate</i>: %{y:.1f}%' + HOVER_POSTFIX
    )

    lines = [wins_line, draws_line, losses_line, wins_average_line, draws_average_line, losses_average_line]
    title_text = f'Win/Draw/Loss Rates per Season ({period}, {venue})'
    scatter = create_scatter(lines=lines, title_text=title_text, y_axis_label='%')
    return scatter


def create_scorelines_heatmap(bins: list[data.DataBin], period: model.fixtures.Period, venue: model.fixtures.Venue = None):
    if period == model.fixtures.Period.FIRST:
        scorelines_of_interest = ['0-0', '1-1',
                                  '1-0', '2-0',
                                  '0-1', '0-2',
                                  'other']
        split = [1.5, 3.5, 5.5]
    elif period == model.fixtures.Period.SECOND:
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

    fig = plotly.graph_objects.Figure()

    y_values = convert_seasons_into_labels(bins)
    heatmap = plotly.graph_objects.Heatmap(
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


@dataclasses.dataclass(slots=True)
class PieChunk:
    label: str
    colour: str
    value: int = 0


def create_pie(chunks: list[PieChunk], title_text: str):
    max_index = numpy.argmax([chunk.value for chunk in chunks])
    pull = []
    for i, _ in enumerate(chunks):
        if i == max_index:
            pull.append(0.1)
        else:
            pull.append(0)

    fig = plotly.graph_objects.Figure()

    fig.add_trace(
        plotly.graph_objects.Pie(
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


def create_goals_pie(bins: list[data.DataBin], period: model.fixtures.Period, venue: model.fixtures.Venue = None):
    if period == model.fixtures.Period.FULL:
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

    prefix = 'Goal Distribution over All Seasons'
    if venue:
        title_text = f'{prefix} ({period}, {venue})'
    else:
        title_text = f'{prefix} ({period})'

    pie = create_pie(chunks, title_text=title_text)
    return pie


def create_results_pie_for_league(bins: list[data.SeasonBin], period: model.fixtures.Period):
    chunks = [
        PieChunk(
            label='Home Wins',
            colour=colours.HOME_COLOR,
            value=sum([data_bin.home_wins for data_bin in bins])
        ),

        PieChunk(
            label='Draws',
            colour=colours.NEUTRAL_COLOR,
            value=sum([data_bin.draws for data_bin in bins])
        ),

        PieChunk(
            label='Away Wins',
            colour=colours.AWAY_COLOR,
            value=sum([data_bin.away_wins for data_bin in bins])
        )
    ]

    pie = create_pie(chunks, title_text=f'Results Breakdown over All Seasons ({period})')
    return pie


def create_results_pie_for_team(bins: list[data.TeamBin], period: model.fixtures.Period, venue: model.fixtures.Venue):
    win_chunk = PieChunk(
        label='Wins',
        colour=colours.GOOD_NEWS_BACKGROUND_COLOR,
        value=sum([data_bin.wins for data_bin in bins])
    )

    draw_chunk = PieChunk(
        label='Draws',
        colour=colours.NEUTRAL_COLOR,
        value=sum([data_bin.draws for data_bin in bins])
    )

    loss_chunk = PieChunk(
        label='Losses',
        colour=colours.BAD_NEWS_BACKGROUND_COLOR,
        value=sum([data_bin.losses for data_bin in bins])
    )

    title_text = f'Results Breakdown over All Seasons ({period}, {venue})'
    pie = create_pie([win_chunk, draw_chunk, loss_chunk], title_text=title_text)
    return pie


def create_bar(event: analysis.Event, team_name: str):
    fig = plotly.graph_objects.Figure()

    x_values = []
    y_values = []
    text = []
    colors = []
    denominator = sum(event.counter.values())
    for x in range(max(event.counter.keys()) + 1):
        y = 100 * event.counter[x] / denominator
        if y > 0:
            x_values.append(f'{x}')
            y_values.append(y)
            if x == event.current_team_value:
                text.append(team_name)
                colors.append(colours.GOOD_NEWS_BACKGROUND_COLOR)
            else:
                if y < 0.5:
                    text.append(f'{y:.2f}%')
                else:
                    text.append('')
                colors.append(colours.BAD_NEWS_BACKGROUND_COLOR)

    fig.add_trace(
        plotly.graph_objects.Bar(
            x=x_values,
            y=y_values,
            textposition='auto',
            text=text,
            hovertemplate='%{x}: %{y:.1f}%' + HOVER_POSTFIX
        )
    )

    fig.update_traces(
        marker_line_color='black',
        marker_line_width=2,
        opacity=0.75,
        marker_color=colors,
        marker=dict(cornerradius='20%')
    )

    fig.update_layout(
        xaxis_title='Sequence length',
        yaxis_title='%'
    )

    title = {
        'text': str(event),
        'x': 0.5,
        'y': 0.95,
        'xanchor': 'center',
        'yanchor': 'top'
    }
    fig.update_layout(title=title)
    return fig


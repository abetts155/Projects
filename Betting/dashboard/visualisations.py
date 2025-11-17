import dataclasses
import numpy
import plotly.graph_objects
import plotly.subplots

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


def create_goals_scatter_graph(
        bins: list[data.DataBin],
        period: model.fixtures.Period,
        venue: model.fixtures.Venue = None,
        team_name: str = None
):
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

    if team_name:
        title_text = f'{team_name}: {title_text}'

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


def create_scored_and_conceded_scatter_graph(
        bins: list[data.TeamBin],
        period: model.fixtures.Period,
        venue: model.fixtures.Venue,
        team_name: str = None):
    x_values = convert_seasons_into_labels(bins)

    total_line = Line(
        name='Total: Trend',
        mode='lines+markers',
        marker='x',
        shape='linear',
        x_values=x_values,
        colour=colours.NEUTRAL_COLOR,
        hover_template=SEASON_HOVER_PREFIX + '<i>Goal Rate</i>: %{y:.1f} goals' + HOVER_POSTFIX
    )

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
        total_line.y_values.append(sum(data_bin.goals_total()) / data_bin.total_games)
        scored_line.y_values.append(sum(data_bin.goals_for()) / data_bin.total_games)
        conceded_line.y_values.append(sum(data_bin.goals_against()) / data_bin.total_games)

    total_average_line = Line(
        name='Total: Average',
        mode='lines',
        dash='dash',
        x_values=x_values,
        y_values=create_average_y_values(total_line),
        colour=colours.BAD_NEWS_BACKGROUND_COLOR,
        hover_template='<i>Average Total Goals Rate</i>: %{y:.1f} goals' + HOVER_POSTFIX
    )

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

    lines = [total_line, scored_line, conceded_line, total_average_line, scored_average_line, conceded_average_line]
    title_text = f'Goal rates per season ({period}, {venue})'
    if period == model.fixtures.Period.FULL:
        y_axis_label = 'Per Game'
    else:
        y_axis_label = 'Per Half'

    if team_name:
        title_text = f'{team_name}: {title_text}'

    scatter = create_scatter(lines=lines, title_text=title_text, y_axis_label=y_axis_label)
    return scatter


def create_wins_draw_losses_scatter_graph(
        bins: list[data.TeamBin],
        period: model.fixtures.Period,
        venue: model.fixtures.Venue,
        team_name: str = None
):
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
        wins_line.y_values.append(100 * sum(data_bin.wins()) / data_bin.total_games)
        draws_line.y_values.append(100 * sum(data_bin.draws()) / data_bin.total_games)
        losses_line.y_values.append(100 * sum(data_bin.losses()) / data_bin.total_games)

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
    title_text = f'Result rates per season ({period}, {venue})'

    if team_name:
        title_text = f'{team_name}: {title_text}'

    scatter = create_scatter(lines=lines, title_text=title_text, y_axis_label='%')
    return scatter


def create_scorelines_heatmap(
        bins: list[data.DataBin],
        period: model.fixtures.Period,
        venue: model.fixtures.Venue = None,
        team_name: str = None
):
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

    if team_name:
        title_text = f'{team_name}: {title_text}'

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


def create_goals_pie(
        bins: list[data.DataBin],
        period: model.fixtures.Period,
        venue: model.fixtures.Venue = None,
        team_name: str = None
):
    if period == model.fixtures.Period.FULL:
        aggregated = {
            (0, 0): 0,
            (1, 2): 0,
            (3, 90): 0
        }
    else:
        aggregated = {
            (0, 0): 0,
            (1, 1): 0,
            (2, 45): 0
        }

    for data_bin in bins:
        for goals, frequency in data_bin.goal_counts.items():
            for min_goals, max_goals in aggregated.keys():
                if min_goals <= goals <= max_goals:
                    aggregated[(min_goals, max_goals)] += frequency

    chunks = []
    for (min_goals, max_goals), value in aggregated.items():
        if min_goals == 0:
            assert max_goals == 0
            chunk = PieChunk(label='0 goals', colour='#003f5c', value=value)
        elif min_goals == 1:
            if max_goals == 1:
                chunk = PieChunk(label='1 goal', colour='#7a5195', value=value)
            else:
                chunk = PieChunk(label=f'1-{max_goals} goals', colour='#7a5195', value=value)
        else:
            chunk = PieChunk(label=f'≥ {min_goals} goals', colour='#ffa600', value=value)

        chunks.append(chunk)

    prefix = 'Goal distribution over all seasons'
    if venue:
        title_text = f'{prefix} ({period}, {venue})'
    else:
        title_text = f'{prefix} ({period})'

    if team_name:
        title_text = f'{team_name}: {title_text}'

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


def create_trends(
        trends: list[analysis.Trend],
        period: model.fixtures.Period,
        venue: model.fixtures.Venue,
        team_name: str
):
    events = [t.event for t in trends]
    current_vals = [t.current for t in trends]
    max_team_vals = [t.max_team for t in trends]
    max_history_vals = [t.max_history for t in trends]

    fig = plotly.graph_objects.Figure()

    fig.add_trace(plotly.graph_objects.Bar(
        y=events,
        x=max_history_vals,
        name='League Max',
        orientation='h',
        marker=dict(color='#dcdcdc'),
        hovertemplate='<i>Max</i>: %{x}' + HOVER_POSTFIX
    ))

    fig.add_trace(plotly.graph_objects.Bar(
        y=events,
        x=max_team_vals,
        name='Team Max',
        orientation='h',
        marker=dict(color='#a3c4dc'),
        hovertemplate='<i>Team Max</i>: %{x}' + HOVER_POSTFIX
    ))

    fig.add_trace(plotly.graph_objects.Bar(
        y=events,
        x=current_vals,
        name='Current',
        orientation='h',
        marker=dict(color='#1f77b4'),
        hovertemplate='<i>Current</i>: %{x}' + HOVER_POSTFIX
    ))

    title = {
        'text': f"{team_name}: Trends ({period}, {venue})",
        'x': 0.5,
        'y': 0.95,
        'xanchor': 'center',
        'yanchor': 'top'
    }

    fig.update_layout(
        title=title,
        barmode='overlay'
    )

    return fig


def create_moving_average(data: list[int]) -> list[float]:
    if len(data) < 3:
        return data

    moving_avg = numpy.convolve(data, numpy.ones(3) / 3, mode='valid')
    prefix = [data[0], (data[0] + data[1]) / 2]
    return prefix + list(moving_avg)


def normalise_bars(values: list[int], max_val: int):
    return [min(v / max_val, 1) for v in values]


def create_team_scoring_patterns(
        bins: list[data.TeamBin],
        period: model.fixtures.Period,
        venue: model.fixtures.Venue,
        team_name: str
):
    bin = bins[-1]
    rows = 5
    fig = plotly.subplots.make_subplots(rows=rows, cols=1, shared_xaxes=True, vertical_spacing=0.1)

    matchdays = list(range(1, len(bin) + 1))

    result_colors = {
        'W': 'blue',
        'D': 'black',
        'L': 'red'
    }

    results = {
        'W': [],
        'D': [],
        'L': []
    }

    bts = []
    for gf, ga in zip(bin.goals_for(), bin.goals_against()):
        bts.append('✔' if gf > 0 and ga > 0 else '✘')

        if gf > ga:
            results['W'].append('W')
            results['D'].append(None)
            results['L'].append(None)
        elif gf == ga:
            results['W'].append(None)
            results['D'].append('D')
            results['L'].append(None)
        else:
            results['W'].append(None)
            results['D'].append(None)
            results['L'].append('L')

    for result, color in result_colors.items():
        fig.add_trace(
            plotly.graph_objects.Scatter(
                x=matchdays,
                y=[1] * len(results[result]),
                mode='text',
                text=results[result],
                textfont=dict(size=10, color=result_colors[result]),
                textposition='middle center',
                hoverinfo='skip',
                showlegend=False
            ),
            row=1,
            col=1
        )

    fig.add_trace(
        plotly.graph_objects.Scatter(
            x=matchdays,
            y=[1] * len(results[result]),
            mode='text',
            text=bts,
            textfont=dict(size=10, color=result_colors[result]),
            textposition='middle center',
            hoverinfo='skip',
            showlegend=False
        ),
        row=2,
        col=1
    )

    fig.add_trace(
        plotly.graph_objects.Bar(
            x=matchdays,
            y=bin.goals_total(),
            marker_color=plotly.colors.sample_colorscale('Blues', normalise_bars(bin.goals_total(), 6)),
            name='Total',
            text=bin.goals_total(),
            textfont=dict(size=8),
            textposition='outside',
            cliponaxis=False,
            hoverinfo='skip',
            showlegend=False
        ),
        row=3,
        col=1
    )

    fig.add_trace(
        plotly.graph_objects.Scatter(
            x=matchdays,
            y=create_moving_average(bin.goals_total()),
            mode='lines',
            line=dict(color='black', width=1, shape='spline', smoothing=1),
            showlegend=False,
            hovertemplate='<i>Total Rate</i>: %{y:.1f} goals' + HOVER_POSTFIX
        ),
        row=3,
        col=1
    )

    fig.add_trace(
        plotly.graph_objects.Bar(
            x=matchdays,
            y=bin.goals_for(),
            marker_color=plotly.colors.sample_colorscale('Darkmint', normalise_bars(bin.goals_for(), 4)),
            name='For',
            text=bin.goals_for(),
            textfont=dict(size=8),
            textposition='outside',
            cliponaxis=False,
            hoverinfo='skip',
            showlegend=False
        ),
        row=4,
        col=1
    )

    fig.add_trace(
        plotly.graph_objects.Scatter(
            x=matchdays,
            y=create_moving_average(bin.goals_for()),
            mode='lines',
            line=dict(color='black', width=1, shape='spline', smoothing=1),
            showlegend=False,
            hovertemplate='<i>For Rate</i>: %{y:.1f} goals' + HOVER_POSTFIX
        ),
        row=4,
        col=1
    )

    fig.add_trace(
        plotly.graph_objects.Bar(
            x=matchdays,
            y=bin.goals_against(),
            marker_color=plotly.colors.sample_colorscale('Reds', normalise_bars(bin.goals_against(), 4)),
            name='Against',
            text=bin.goals_against(),
            textfont=dict(size=8),
            textposition='outside',
            cliponaxis=False,
            hoverinfo='skip',
            showlegend=False
        ),
        row=5,
        col=1
    )

    fig.add_trace(
        plotly.graph_objects.Scatter(
            x=matchdays,
            y=create_moving_average(bin.goals_against()),
            mode='lines',
            line=dict(color='black', width=1, shape='spline', smoothing=1),
            showlegend=False,
            hovertemplate='<i>Against Rate</i>: %{y:.1f} goals' + HOVER_POSTFIX
        ),
        row=5,
        col=1
    )

    for i in range(1, rows + 1):
        fig.update_yaxes(
            row=i,
            col=1,
            showgrid=False,
            showticklabels=False,
            zeroline=False
        )

    fig.update_xaxes(showgrid=False)

    title = {
        'text': f"{team_name}: {period}, {venue}",
        'x': 0.5,
        'y': 0.85,
        'xanchor': 'center',
        'yanchor': 'top'
    }

    fig.update_layout(
        title=title,
        xaxis5=dict(title='Match'),
        yaxis2=dict(title='BTS ⚽️'),
        yaxis3=dict(title='F+A ⚽️'),
        yaxis4=dict(title='F ⚽️'),
        yaxis5=dict(title='A ⚽️')
    )

    return fig

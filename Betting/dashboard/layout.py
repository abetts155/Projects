import dash_bootstrap_components as dbc
import dash_mantine_components as dmc
from dash import html, dcc, dash_table

import model.competitions
import model.fixtures
from dashboard import ids, tables


BOXED_STYLE = {'border': '2px solid #000000', 'padding': '3px', 'border-radius': '5px'}


def create_fixtures_layout() -> html.Div:
    div = html.Div(
        [
            html.Hr(),

            dcc.Interval(
                id=ids.Miscellaneous.FIXTURE_INTERVAL,
                interval=3600000,
                n_intervals=0
            ),

            html.Div(id=ids.Miscellaneous.FIXTURE_LIST)
        ]
    )

    return div


def create_team_overview_layout() -> html.Div:
    upcoming_columns = [
        dict(name=column, id=column) for column in [tables.COL_DATE.name, tables.COL_VENUE.name, tables.COL_TEAM.name]
    ]
    fixtures_table_card = dbc.Card(
        [
            dbc.CardHeader(
                id=ids.Miscellaneous.TEAM_FIXTURES_CARD.value,
                style={'text-align': 'center', 'margin-bottom': '20px'}
            ),

            html.Div(
                [
                    dash_table.DataTable(
                        id=ids.Table.TEAM_FIXTURES_TABLE.value,
                        columns=upcoming_columns,
                        sort_action='native',
                        filter_action='native',
                        style_header={'border': '3px solid black'},
                        style_cell={'textAlign': 'left'},
                        page_size=20,
                        page_current=0
                    )
                ],
                className="dbc"
            )
        ],
        body=True
    )

    results_columns = [
        dict(name=column, id=column) for column in [tables.COL_DATE.name,
                                                    tables.COL_VENUE.name,
                                                    tables.COL_TEAM.name,
                                                    tables.COL_FIRST_HALF.name,
                                                    tables.COL_SECOND_HALF.name,
                                                    tables.COL_FULL_TIME.name]
    ]
    results_table_card = dbc.Card(
        [
            dbc.CardHeader(
                id=ids.Miscellaneous.TEAM_RESULTS_CARD.value,
                style={'text-align': 'center', 'margin-bottom': '20px'}
            ),

            html.Div(
                [
                    dash_table.DataTable(
                        id=ids.Table.TEAM_RESULTS_TABLE.value,
                        columns=results_columns,
                        sort_action='native',
                        filter_action='native',
                        style_header={'border': '3px solid black'},
                        style_cell={'textAlign': 'left'},
                        style_cell_conditional=[
                            {'if': {'column_id': tables.COL_FIRST_HALF.name}, 'textAlign': 'center'},
                            {'if': {'column_id': tables.COL_SECOND_HALF.name}, 'textAlign': 'center'},
                            {'if': {'column_id': tables.COL_FULL_TIME.name}, 'textAlign': 'center'}
                        ],
                        page_size=20,
                        page_current=0
                    )
                ],
                className="dbc"
            )
        ],
        body=True
    )

    row_1_col_1 = dbc.Card(
        [
            dcc.Graph(id=ids.Team.ROW_1_COL_1.value, config={'displayModeBar': False})
        ],
        body=True
    )

    row_1_col_2 = dbc.Card(
        [
            dcc.Graph(id=ids.Team.ROW_1_COL_2.value, config={'displayModeBar': False})
        ],
        body=True
    )

    row_1_col_3 = dbc.Card(
        [
            dcc.Graph(id=ids.Team.ROW_1_COL_3.value, config={'displayModeBar': False})
        ],
        body=True
    )

    row_2_col_1 = dbc.Card(
        [
            dcc.Graph(id=ids.Team.ROW_2_COL_1.value, config={'displayModeBar': False})
        ],
        body=True
    )

    row_2_col_2 = dbc.Card(
        [
            dcc.Graph(id=ids.Team.ROW_2_COL_2.value, config={'displayModeBar': False}),
        ],
        body=True
    )

    row_2_col_3 = dbc.Card(
        [
            dcc.Graph(id=ids.Team.ROW_2_COL_3.value, config={'displayModeBar': False})
        ],
        body=True,
    )

    div = html.Div(
        [
            html.Hr(),

            dbc.Row(
                [
                    dbc.Col(
                        [
                            row_1_col_1,
                            row_2_col_1
                        ],
                        width={'size': 4}
                    ),

                    dbc.Col(
                        [
                            row_1_col_2,
                            row_2_col_2
                        ],
                        width={'size': 4}
                    ),

                    dbc.Col(
                        [
                            row_1_col_3,
                            row_2_col_3
                        ],
                        width={'size': 4}
                    )
                ],
                justify='center',
                align='start'
            )
        ]
    )
    return div


def create_league_overview_layout() -> html.Div:
    league_columns = []
    for column in tables.league_table_columns:
        json = dict(name=column.display, id=column.name)

        if column == tables.COL_TEAM:
            pass
        else:
            json['type'] = 'numeric'

        if column in [
            tables.COL_SCORED_GT_0,
            tables.COL_SCORED_GT_1,
            tables.COL_CONCEDED_GT_0,
            tables.COL_CONCEDED_GT_1,
            tables.COL_BTS,
            tables.COL_TOTAL_GT_0,
            tables.COL_TOTAL_GT_1,
            tables.COL_TOTAL_GT_2
        ]:
            json['format'] = dash_table.Format.Format(precision=2, scheme=dash_table.Format.Scheme.percentage_rounded)

        league_columns.append(json)

    history_div = html.Div(
        [
            html.Label('History'),
            dcc.RangeSlider(
                id=ids.Slider.HISTORY_SLIDER.value,
                min=0,
                max=0,
                value=[0, 0],
                step=1,
                allowCross=False
            )
        ],
        style={
            'border': '2px solid #000000',
            'padding': '3px',
            'border-radius': '5px',
            'margin-bottom': '20px'
        }
    )

    league_table_div = html.Div(
        [
            dash_table.DataTable(
                id=ids.Table.LEAGUE_TABLE,
                columns=league_columns,
                tooltip_duration=None,
                tooltip_delay=0,
                sort_action='native',
                style_header={'border': '3px solid black'},
                style_cell={'textAlign': 'center'},
                style_cell_conditional=[
                    {'if': {'column_id': tables.COL_TEAM.name}, 'textAlign': 'left', 'width': '100px'},
                    {'if': {'column_id': tables.COL_PLAYED.name}, 'width': '30px'},
                    {'if': {'column_id': tables.COL_WON.name}, 'width': '30px'},
                    {'if': {'column_id': tables.COL_DRAWN.name}, 'width': '30px'},
                    {'if': {'column_id': tables.COL_LOST.name}, 'width': '30px'},
                    {'if': {'column_id': tables.COL_POINTS.name}, 'width': '30px'},
                    {'if': {'column_id': tables.COL_GOALS_FOR.name}, 'width': '40px'},
                    {'if': {'column_id': tables.COL_GOALS_AGAINST.name}, 'width': '40px'},
                    {'if': {'column_id': tables.COL_GOAL_RATE.name}, 'width': '40px'}
                ],
            )
        ],
        className="dbc"
    )

    goals_pie_card = dbc.Card(
        [
            dcc.Graph(id=ids.Pie.GOALS_PIE.value, config={'displayModeBar': False})
        ],
        body=True
    )

    goals_graph_card = dbc.Card(
        [
            dcc.Graph(id=ids.Graph.GOALS_GRAPH.value, config={'displayModeBar': False})
        ],
        body=True
    )

    scores_heatmap_card = dbc.Card(
        [
            dcc.Graph(id=ids.Graph.SCORES_HEATMAP.value, config={'displayModeBar': False})
        ],
        body=True
    )

    bts_graph_card = dbc.Card(
        [
            dcc.Graph(id=ids.Graph.BTS_GRAPH.value, config={'displayModeBar': False})
        ],
        body=True
    )

    results_pie_card = dbc.Card(
        [
            dcc.Graph(id=ids.Pie.RESULTS_PIE.value, config={'displayModeBar': False}),
        ],
        body=True
    )

    results_graph_card = dbc.Card(
        [
            dcc.Graph(id=ids.Graph.RESULTS_GRAPH.value, config={'displayModeBar': False})
        ],
        body=True,
    )

    league_table_card = dbc.Card(
        [
            dbc.CardBody(
                [
                    history_div,
                    league_table_div
                ]
            )
        ]
    )

    div = html.Div(
        [
            dbc.Row(
                [
                    league_table_card
                ],
                justify='center',
                align='start'
            ),

            dbc.Row(
                [
                    dbc.Col(
                        [
                            goals_pie_card,
                            goals_graph_card
                        ],
                        width={'size': 4}
                    ),

                    dbc.Col(
                        [
                            scores_heatmap_card,
                            bts_graph_card
                        ],
                        width={'size': 4}
                    ),

                    dbc.Col(
                        [
                            results_graph_card,
                            results_pie_card
                        ],
                        width={'size': 4}
                    )
                ],
                justify='center',
                align='start'
            )
        ]
    )

    return div


def create_layout() -> dbc.Container:
    tabs = dbc.Tabs(
        [
            dbc.Tab(
                create_fixtures_layout(),
                label="Upcoming Fixtures",
                id=ids.Tab.FIXTURES_TAB.value
            ),

            dbc.Tab(
                create_league_overview_layout(),
                id=ids.Tab.LEAGUE_OVERVIEW_TAB.value
            ),

            dbc.Tab(
                create_team_overview_layout(),
                id=ids.Tab.TEAM_OVERVIEW_TAB.value
            )
        ],
        id=ids.Tab.TAB_CONTAINER.value
    )

    loader = dbc.Spinner(
        html.Div(tabs),
        id=ids.Miscellaneous.LOADER.value,
        fullscreen=True,
        type='grow',
        size='md',
        color='success'
    )

    country_options = []
    competitions = model.competitions.get_whitelisted_competitions(model.competitions.CompetitionType.LEAGUE)
    countries = set(competition.country for competition in competitions)
    for country in sorted(countries):
        country_options.append({'label': model.competitions.prettify(country), 'value': country})

    country_dropdown = dcc.Dropdown(
        id=ids.Dropdown.COUNTRY_DROPDOWN.value,
        options=country_options,
        value='Sweden',
        clearable=False,
        style={'text-align': 'left'}
    )

    league_dropdown = dcc.Dropdown(
        id=ids.Dropdown.LEAGUE_DROPDOWN.value,
        clearable=False,
        style={'text-align': 'left'}
    )

    season_dropdown = dcc.Dropdown(
        id=ids.Dropdown.SEASON_DROPDOWN.value,
        clearable=False,
        style={'text-align': 'left'}
    )

    venue_radio = dbc.RadioItems(
        id=ids.Radio.VENUE_RADIO.value,
        options=[{'label': venue.value, 'value': venue} for venue in model.fixtures.Venue],
        value=model.fixtures.Venue.ANYWHERE,
        inline=True,
        style=BOXED_STYLE
    )

    period_radio = dbc.RadioItems(
        id=ids.Radio.HALF_RADIO.value,
        options=[
            {'label': period.value, 'value': period}
            for period in [model.fixtures.Period.FULL, model.fixtures.Period.FIRST, model.fixtures.Period.SECOND]
        ],
        value=model.fixtures.Period.FULL,
        inline=True,
        style=BOXED_STYLE
    )

    container = dbc.Container(
        [
            dcc.Store(id=ids.Miscellaneous.DATA_STORE),
            dcc.Store(id=ids.Miscellaneous.HISTORY),
            dbc.Row(
                [
                    dbc.Col(html.Label('Country'), width=2),
                    dbc.Col(html.Label('League'), width=2),
                    dbc.Col(html.Label('Season'), width=2),
                    dbc.Col(html.Label('Venue'), width=2),
                    dbc.Col(html.Label('Period'), width=2)
                ],
                justify='center',
                align='start'
            ),

            dbc.Row(
                [
                    dbc.Col(country_dropdown, width=2),
                    dbc.Col(league_dropdown, width=2),
                    dbc.Col(season_dropdown, width=2),
                    dbc.Col(venue_radio, width=2),
                    dbc.Col(period_radio, width=2)
                ],
                justify='center',
                align='start'
            ),

            html.Hr(),

            loader
        ],
        id=ids.Miscellaneous.CONTAINER.value,
        fluid=True
    )

    layout = dmc.MantineProvider(children=container)
    return layout

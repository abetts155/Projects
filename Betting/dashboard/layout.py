import datetime

from dash import html, dcc, dash_table
from dash_bootstrap_components import Row, Col, RadioItems, Card, Container, CardHeader

from dashboard import ids, tables
from model.fixtures import Half, Venue
from model.leagues import country_whitelist, prettify
from model.teams import Team


def create_team_analysis_layout() -> html.Div:
    upcoming_columns = [
        dict(name=column, id=column) for column in [datetime.date.__name__, Venue.__name__, Team.__name__]
    ]
    upcoming_card = Card(
        [
            CardHeader(
                id=ids.Components.TEAM_RESULTS_CARD.name,
                style={'text-align': 'center', 'margin-bottom': '20px'}
            ),

            dash_table.DataTable(
                id=ids.Components.TEAM_FIXTURES_TABLE.name,
                columns=upcoming_columns,
                sort_action='native',
                filter_action='native',
                style_header={'border': '3px solid black'},
                style_cell={'textAlign': 'center'},
                style_cell_conditional=[{'if': {'column_id': Team.__name__}, 'textAlign': 'left'}],
                page_size=5,
                page_current=0,
            )
        ],
        body=True
    )

    results_columns = [
        dict(name=column, id=column) for column in [datetime.date.__name__,
                                                    Venue.__name__,
                                                    Team.__name__,
                                                    Half.first.name,
                                                    Half.second.name,
                                                    Half.full.name]
    ]
    results_card = Card(
        [
            CardHeader(
                id=ids.Components.TEAM_FIXTURES_CARD.name,
                style={'text-align': 'center', 'margin-bottom': '20px'}
            ),

            dash_table.DataTable(
                id=ids.Components.TEAM_RESULTS_TABLE.name,
                columns=results_columns,
                sort_action='native',
                filter_action='native',
                style_header={'border': '3px solid black'},
                style_cell={'textAlign': 'center'},
                style_cell_conditional=[{'if': {'column_id': Team.__name__}, 'textAlign': 'left'}],
                page_size=20,
                page_current=0,
            )
        ],
        body=True
    )

    goals_pie_card = Card(
        [
            dcc.Graph(id=ids.Components.TEAM_GOALS_PIE.name, config={'displayModeBar': False}, figure={})
        ],
        body=True
    )

    goals_graph_card = Card(
        [
            dcc.Graph(id=ids.Components.TEAM_GOALS_GRAPH.name, config={'displayModeBar': False}, figure={})
        ],
        body=True
    )

    scores_heatmap_card = Card(
        [
            dcc.Graph(id=ids.Components.TEAM_SCORES_HEATMAP.name, config={'displayModeBar': False}, figure={})
        ],
        body=True
    )

    bts_graph_card = Card(
        [
            dcc.Graph(id=ids.Components.TEAM_SCORED_AND_CONCEDED_GRAPH.name, config={'displayModeBar': False}, figure={})
        ],
        body=True
    )

    results_pie_card = Card(
        [
            dcc.Graph(id=ids.Components.TEAM_RESULTS_PIE.name, config={'displayModeBar': False}, figure={}),
        ],
        body=True
    )

    results_graph_card = Card(
        [
            dcc.Graph(id=ids.Components.TEAM_RESULTS_GRAPH.name, config={'displayModeBar': False}, figure={})
        ],
        body=True,
    )

    div = html.Div(
        [
            html.Hr(),

            Row(
                [
                    Col(
                        [
                            upcoming_card,
                            results_card
                        ],
                        width={'size': 4},
                        style={'font-weight': 'bold', 'text-align': 'left'}
                    ),

                    Col(
                        [
                            goals_pie_card,
                            bts_graph_card,
                            results_pie_card
                        ],
                        width={'size': 4}
                    ),

                    Col(
                        [
                            goals_graph_card,
                            scores_heatmap_card,
                            results_graph_card
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


def create_league_analysis_layout() -> html.Div:
    league_columns = []
    league_tooltips = {}
    for column in tables.table_columns:
        league_tooltips[column.display] = column.tooltip
        json = dict(name=column.display, id=column.display)

        if column != tables.COL_TEAM:
            json['type'] = 'numeric'

        if column in [tables.COL_BTS, tables.COL_SCORED, tables.COL_CONCEDED]:
            json['format'] = dash_table.Format.Format(precision=2, scheme=dash_table.Format.Scheme.percentage_rounded)

        league_columns.append(json)

    radio_style = {'border': '2px solid #000000', 'padding': '3px', 'border-radius': '5px', 'margin-bottom': '20px'}
    history_radio = RadioItems(
        id=ids.Components.HISTORY_RADIO.name,
        options=[{'label': 'Last {}'.format(i) if i > 0 else 'No Filter', 'value': i} for i in [0, 5, 6, 7, 8, 9, 10]],
        value=0,
        inline=True,
        style=radio_style
    )

    league_card = Card(
        [
            CardHeader('League Table', style={'text-align': 'center', 'margin-bottom': '20px'}),

            html.Label('History'),
            history_radio,

            dash_table.DataTable(
                id=ids.Components.LEAGUE_TABLE.name,
                columns=league_columns,
                tooltip_header=league_tooltips,
                tooltip_duration=None,
                tooltip_delay=0,
                sort_action='native',
                style_header={'border': '3px solid black'},
                style_cell={'textAlign': 'right'},
                style_cell_conditional=[{'if': {'column_id': 'Team'}, 'textAlign': 'left'}],
            )
        ],
        body=True
    )

    results_columns = [
        dict(name=column, id=column) for column in [datetime.date.__name__,
                                                    Venue.home.name,
                                                    Venue.away.name,
                                                    Half.first.name,
                                                    Half.second.name,
                                                    Half.full.name]
    ]
    results_card = Card(
        [
            CardHeader('Results', style={'text-align': 'center', 'margin-bottom': '20px'}),

            dash_table.DataTable(
                id=ids.Components.RESULTS_TABLE.name,
                columns=results_columns,
                sort_action='native',
                filter_action='native',
                style_header={'border': '3px solid black'},
                style_cell={'textAlign': 'center'},
                style_cell_conditional=[{'if': {'column_id': Venue.home.name}, 'textAlign': 'left'},
                                        {'if': {'column_id': Venue.away.name}, 'textAlign': 'left'}],
                page_size=20,
                page_current=0,
            )
        ],
        body=True
    )

    goals_pie_card = Card(
        [
            dcc.Graph(id=ids.Components.GOALS_PIE.name, config={'displayModeBar': False}, figure={})
        ],
        body=True
    )

    goals_graph_card = Card(
        [
            dcc.Graph(id=ids.Components.GOALS_GRAPH.name, config={'displayModeBar': False}, figure={})
        ],
        body=True
    )

    scores_heatmap_card = Card(
        [
            dcc.Graph(id=ids.Components.SCORES_HEATMAP.name, config={'displayModeBar': False}, figure={})
        ],
        body=True
    )

    bts_graph_card = Card(
        [
            dcc.Graph(id=ids.Components.BTS_GRAPH.name, config={'displayModeBar': False}, figure={})
        ],
        body=True
    )

    results_pie_card = Card(
        [
            dcc.Graph(id=ids.Components.RESULTS_PIE.name, config={'displayModeBar': False}, figure={}),
        ],
        body=True
    )

    results_graph_card = Card(
        [
            dcc.Graph(id=ids.Components.RESULTS_GRAPH.name, config={'displayModeBar': False}, figure={})
        ],
        body=True,
    )

    div = html.Div(
        [
            Row(
                [
                    Col(
                        [
                            league_card,
                            results_card
                        ],
                        width={'size': 4},
                        style={'font-weight': 'bold', 'text-align': 'left'}
                    ),

                    Col(
                        [
                            goals_pie_card,
                            bts_graph_card,
                            results_pie_card
                        ],
                        width={'size': 4}
                    ),

                    Col(
                        [
                            goals_graph_card,
                            scores_heatmap_card,
                            results_graph_card
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


def create_layout() -> Container:
    tab_style_when_selected = {
        'backgroundColor': '#007bff',
        'color': 'white',
        'border': '5px solid black',
        'padding': '10px'
    }

    tab_style_when_silent = {
        'backgroundColor': '#f8f9fa',
        'color': '#6c757d',
        'borderTop': '2px solid #dee2e6',
        'borderBottom': '2px solid #dee2e6',
        'padding': '10px'
    }

    tabs = dcc.Tabs(
        [
            dcc.Tab(
                create_league_analysis_layout(),
                id=ids.Components.LEAGUE_TAB.name,
                label='League Analysis',
                style=tab_style_when_silent,
                selected_style=tab_style_when_selected
            ),

            dcc.Tab(
                create_team_analysis_layout(),
                id=ids.Components.TEAM_TAB.name,
                label='Team Analysis',
                style=tab_style_when_silent,
                selected_style=tab_style_when_selected
            )
        ]
    )

    loader = dcc.Loading(
        html.Div(tabs),
        id=ids.Components.LOADER.name,
        fullscreen=True,
        type='cube'
    )

    country_options = [{'label': prettify(country), 'value': country} for country in country_whitelist]
    default_country = 'England'
    country_dropdown = dcc.Dropdown(id=ids.Components.COUNTRY_DROPDOWN.name,
                                    options=country_options,
                                    value=default_country,
                                    clearable=False,
                                    style={'text-align': 'left'})

    league_dropdown = dcc.Dropdown(id=ids.Components.LEAGUE_DROPDOWN.name,
                                   clearable=False,
                                   style={'text-align': 'left'})

    team_dropdown = dcc.Dropdown(id=ids.Components.TEAM_DROPDOWN.name,
                                 clearable=False,
                                 style={'text-align': 'left'})

    radio_style = {'border': '2px solid #000000', 'padding': '3px', 'border-radius': '5px'}

    venue_radio = RadioItems(
        id=ids.Components.VENUE_RADIO.name,
        options=[{'label': Venue.to_string([venue]), 'value': venue.name} for venue in Venue],
        value=Venue.anywhere.name,
        inline=True,
        style=radio_style
    )

    period_radio = RadioItems(
        id=ids.Components.HALF_RADIO.name,
        options=[{'label': Half.to_string([half]), 'value': half.name} for half in Half],
        value=Half.full.name,
        inline=True,
        style=radio_style
    )

    container = Container(
        [
            dcc.Store(id=ids.Components.DATA_STORE.name),
            Row(
                [
                    Col(html.Label('Country'), width=2),
                    Col(html.Label('League'), width=2),
                    Col(html.Label('Team'), width=2),
                    Col(html.Label('Venue'), width=2),
                    Col(html.Label('Period'), width=2)
                ],
                justify='center',
                align='start'
            ),

            Row(
                [
                    Col(country_dropdown, width=2),
                    Col(league_dropdown, width=2),
                    Col(team_dropdown, width=2),
                    Col(venue_radio, width=2),
                    Col(period_radio, width=2)
                ],
                justify='center',
                align='start'
            ),

            html.Hr(),

            loader
        ],
        id=ids.Components.CONTAINER.name,
        fluid=True
    )

    return container

import random
import dash_bootstrap_components as dbc
from dash import html, dcc, dash_table

import football_api.helpers
import model.competitions
import model.fixtures
from dashboard import analysis, ids, tables


BOXED_STYLE = {'border': '2px solid #000000', 'padding': '3px', 'border-radius': '5px'}


def create_team_now_layout() -> html.Div:
    results_radio = dbc.RadioItems(
        id=ids.Radio.RESULTS_RADIO.value,
        options=[{'value': i, 'label': analysis.prettify(opt)} for i, opt in enumerate(analysis.result_predicates)],
        value=random.randint(0, len(analysis.result_predicates) - 1),
        inline=True,
        style=BOXED_STYLE
    )

    win_draw_loss_card = dbc.Card(
        [
            dbc.CardHeader(
                'Results Sequence Analysis',
                style={'text-align': 'center', 'margin-bottom': '20px'}
            ),

            dbc.Row(
                [
                    dbc.Col(
                        [
                            results_radio
                        ]
                    ),

                    dbc.Col(
                        [
                            dbc.Checklist(
                                id=ids.Switch.RESULTS_SWITCH.value,
                                options=['Negate'],
                                style=BOXED_STYLE
                            )
                        ]
                    )
                ],
                justify='center',
                align='start'
            ),

            dcc.Graph(id=ids.Bar.RESULTS_BAR.value, config={'displayModeBar': False}),
        ],
        body=True
    )

    goals_radio = dbc.RadioItems(
        id=ids.Radio.GOALS_RADIO.value,
        options=[{'value': i, 'label': analysis.prettify(opt)} for i, opt in enumerate(analysis.goals_predicates)],
        value=random.randint(0, len(analysis.goals_predicates) - 1),
        inline=True,
        style=BOXED_STYLE
    )

    goals_card = dbc.Card(
        [
            dbc.CardHeader(
                'Goals Sequence Analysis',
                style={'text-align': 'center', 'margin-bottom': '20px'}
            ),

            dbc.Row(
                [
                    dbc.Col(
                        [
                            goals_radio
                        ]
                    ),

                    dbc.Col(
                        [
                            dbc.Checklist(
                                id=ids.Switch.GOALS_SWITCH.value,
                                options=['Negate'],
                                style=BOXED_STYLE
                            )
                        ]
                    )
                ],
                justify='center',
                align='start'
            ),

            dcc.Graph(id=ids.Bar.GOALS_BAR.value, config={'displayModeBar': False}),
        ],
        body=True
    )

    relations_radio = dbc.RadioItems(
        id=ids.Radio.TOTAL_GOALS_RELATIONS_RADIO.value,
        options=[{'value': key, 'label': value} for key, value in analysis.goal_relations.items()],
        value='eq',
        inline=True,
        style=BOXED_STYLE
    )

    total_goals = [0, 1, 2, 3]
    total_goals_radio = dbc.RadioItems(
        id=ids.Radio.TOTAL_GOALS_RADIO.value,
        options=[{'value': i, 'label': opt} for i, opt in enumerate(total_goals)],
        value=random.randint(0, len(total_goals) - 1),
        inline=True,
        style=BOXED_STYLE
    )

    total_goals_card = dbc.Card(
        [
            dbc.CardHeader(
                'Total Goals Sequence Analysis',
                style={'text-align': 'center', 'margin-bottom': '20px'}
            ),

            dbc.Row(
                [
                    dbc.Col(
                        [
                            relations_radio
                        ]
                    ),

                    dbc.Col(
                        [
                            total_goals_radio
                        ]
                    )
                ],
                justify='center',
                align='start'
            ),

            dcc.Graph(id=ids.Bar.TOTAL_GOALS_BAR.value, config={'displayModeBar': False}),
        ],
        body=True
    )

    div = html.Div(
        [
            dbc.Row(
                [
                    dbc.Col(
                        [
                            win_draw_loss_card
                        ],
                        width={'size': 4}
                    ),

                    dbc.Col(
                        [
                            goals_card
                        ],
                        width={'size': 4}
                    ),

                    dbc.Col(
                        [
                            total_goals_card
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


def create_team_overview_layout() -> html.Div:
    upcoming_columns = [
        dict(name=column, id=column) for column in [tables.COL_DATE.display,
                                                    tables.COL_VENUE.display,
                                                    tables.COL_TEAM.display]
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
        dict(name=column, id=column) for column in [tables.COL_DATE.display,
                                                    tables.COL_VENUE.display,
                                                    tables.COL_TEAM.display,
                                                    tables.COL_FIRST_HALF.display,
                                                    tables.COL_SECOND_HALF.display,
                                                    tables.COL_FULL_TIME.display]
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
                            {'if': {'column_id': tables.COL_FIRST_HALF.display}, 'textAlign': 'center'},
                            {'if': {'column_id': tables.COL_SECOND_HALF.display}, 'textAlign': 'center'},
                            {'if': {'column_id': tables.COL_FULL_TIME.display}, 'textAlign': 'center'}
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

    goals_pie_card = dbc.Card(
        [
            dcc.Graph(id=ids.Pie.TEAM_GOALS_PIE.value, config={'displayModeBar': False})
        ],
        body=True
    )

    goals_graph_card = dbc.Card(
        [
            dcc.Graph(id=ids.Graph.TEAM_GOALS_GRAPH.value, config={'displayModeBar': False})
        ],
        body=True
    )

    scores_heatmap_card = dbc.Card(
        [
            dcc.Graph(id=ids.Graph.TEAM_SCORES_HEATMAP.value, config={'displayModeBar': False})
        ],
        body=True
    )

    bts_graph_card = dbc.Card(
        [
            dcc.Graph(id=ids.Graph.TEAM_SCORED_AND_CONCEDED_GRAPH.value, config={'displayModeBar': False})
        ],
        body=True
    )

    results_pie_card = dbc.Card(
        [
            dcc.Graph(id=ids.Pie.TEAM_RESULTS_PIE.value, config={'displayModeBar': False}),
        ],
        body=True
    )

    results_graph_card = dbc.Card(
        [
            dcc.Graph(id=ids.Graph.TEAM_RESULTS_GRAPH.value, config={'displayModeBar': False})
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
                            goals_pie_card,
                            goals_graph_card,
                            fixtures_table_card
                        ],
                        width={'size': 4}
                    ),

                    dbc.Col(
                        [
                            scores_heatmap_card,
                            bts_graph_card,
                            results_table_card
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


def create_league_overview_layout() -> html.Div:
    league_columns = []
    league_tooltips = {}
    for column in tables.league_table_columns:
        league_tooltips[column.display] = column.tooltip
        json = dict(name=column.display, id=column.display)

        if column != tables.COL_TEAM:
            json['type'] = 'numeric'

        if column in [tables.COL_BTS,
                      tables.COL_SCORED,
                      tables.COL_CLEAN_SHEET,
                      tables.COL_TOTAL_EQ_0,
                      tables.COL_TOTAL_LE_1]:
            json['format'] = dash_table.Format.Format(precision=2, scheme=dash_table.Format.Scheme.percentage_rounded)

        league_columns.append(json)

    season_div = html.Div(
        [
            html.Label('Season'),
            dcc.Dropdown(
                id=ids.Dropdown.SEASON_DROPDOWN.value,
                clearable=False,
            )
        ],
        style={
            'padding': '3px',
            'border-radius': '5px',
            'margin-bottom': '10px'
        }
    )

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
                tooltip_header=league_tooltips,
                tooltip_duration=None,
                tooltip_delay=0,
                sort_action='native',
                style_header={'border': '3px solid black'},
                style_cell={'textAlign': 'right'},
                style_cell_conditional=[{'if': {'column_id': 'Team'}, 'textAlign': 'left'}],
            )
        ],
        className="dbc"
    )

    fixtures_table = html.Div(
        [
            dash_table.DataTable(
                id=ids.Table.LEAGUE_FIXTURES_TABLE.value,
                columns=[dict(name=column.display, id=column.display) for column in tables.fixtures_table_columns],
                sort_action='native',
                filter_action='native',
                style_header={'border': '3px solid black'},
                style_cell={'textAlign': 'left'},
                page_size=30,
                page_current=0
            )
        ],
        className="dbc"
    )

    results_table = html.Div(
        [
            dash_table.DataTable(
                id=ids.Table.LEAGUE_RESULTS_TABLE.value,
                columns=[dict(name=column.display, id=column.display) for column in tables.results_table_columns],
                sort_action='native',
                filter_action='native',
                style_header={'border': '3px solid black'},
                style_cell={'textAlign': 'left'},
                style_cell_conditional=[
                    {'if': {'column_id': tables.COL_FIRST_HALF.display}, 'textAlign': 'center'},
                    {'if': {'column_id': tables.COL_SECOND_HALF.display}, 'textAlign': 'center'},
                    {'if': {'column_id': tables.COL_FULL_TIME.display}, 'textAlign': 'center'}
                ],
                page_size=30,
                page_current=0
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
            dbc.CardHeader(
                html.H3('Table', className="card-title"),
                style={'text-align': 'center', 'margin-bottom': '20px'}
            ),

            dbc.CardBody(
                [
                    season_div,
                    history_div,
                    league_table_div
                ]
            )
        ]
    )

    fixtures_table_card = dbc.Card(
        [
            dbc.CardHeader(
                html.H3('Fixtures', className="card-title"),
                style={'text-align': 'center', 'margin-bottom': '20px'}
            ),

            dbc.CardBody(fixtures_table)
        ]
    )

    results_table_card = dbc.Card(
        [
            dbc.CardHeader(
                html.H3('Results', className="card-title"),
                style={'text-align': 'center', 'margin-bottom': '20px'}
            ),

            dbc.CardBody(results_table)
        ]
    )

    #
    # league_table_accordion = dbc.Accordion(
    #     [
    #         dbc.AccordionItem(
    #             [
    #
    #             ],
    #             title='League Table',
    #             item_id='1'
    #         ),
    #         dbc.AccordionItem(
    #             [
    #                 fixtures_table
    #             ],
    #             title='Fixtures',
    #             item_id='2'
    #         ),
    #         dbc.AccordionItem(
    #             [
    #                 results_table
    #             ],
    #
    #             item_id='3'
    #         )
    #     ],
    #     flush=True,
    #     always_open=True,
    #     active_item=['1', '2', '3'],
    #     id=ids.Miscellaneous.LEAGUE_ACCORDION.value
    # )

    div = html.Div(
        [
            dbc.Row(
                [
                    dbc.Col(
                        [
                            goals_pie_card,
                            goals_graph_card,
                            league_table_card
                        ],
                        width={'size': 4}
                    ),

                    dbc.Col(
                        [
                            scores_heatmap_card,
                            bts_graph_card,
                            fixtures_table_card,
                        ],
                        width={'size': 4}
                    ),

                    dbc.Col(
                        [
                            results_graph_card,
                            results_pie_card,
                            results_table_card
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
                create_league_overview_layout(),
                id=ids.Tab.LEAGUE_OVERVIEW_TAB.value
            ),

            dbc.Tab(
                create_team_overview_layout(),
                id=ids.Tab.TEAM_OVERVIEW_TAB.value
            ),

            dbc.Tab(
                create_team_now_layout(),
                id=ids.Tab.TEAM_NOW_TAB.value
            )
        ]
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
    for country in football_api.helpers.get_countries():
        country_options.append({'label': model.competitions.prettify(country), 'value': country})

    default_country = 'England'
    country_dropdown = dcc.Dropdown(
        id=ids.Dropdown.COUNTRY_DROPDOWN.value,
        options=country_options,
        value=default_country,
        clearable=False,
        style={'text-align': 'left'}
    )

    league_dropdown = dcc.Dropdown(
        id=ids.Dropdown.LEAGUE_DROPDOWN.value,
        clearable=False,
        style={'text-align': 'left'}
    )

    team_dropdown = dcc.Dropdown(
        id=ids.Dropdown.TEAM_DROPDOWN.value,
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
            dbc.Row(
                [
                    dbc.Col(html.Label('Country'), width=2),
                    dbc.Col(html.Label('League'), width=2),
                    dbc.Col(html.Label('Team'), width=2),
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
                    dbc.Col(team_dropdown, width=2),
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

    return container

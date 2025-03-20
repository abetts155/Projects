import concurrent.futures
import dataclasses
import dash
import dash.dependencies
import dash_bootstrap_components
import dash_bootstrap_templates
import functools
import logging
import random

import dashboard.analysis
import dashboard.data
import dashboard.fixtures
import dashboard.ids
import dashboard.layout
import dashboard.tables
import dashboard.visualisations
import model.fixtures
import model.competitions


def update_data(country: str, league_name: str):
    logging.debug("Updating data")

    league = model.competitions.get_league(country, league_name)
    data_dict = dashboard.data.update_data(league)
    seasons = dashboard.data.get_seasons(data_dict, True)
    tab_name = f"{country} {league_name}: {len(seasons)} seasons of data"
    options = [str(season) for season in seasons]
    default = options[0]

    outputs = [
        tab_name,
        options,
        default,
        data_dict
    ]
    return outputs


def update_league_dropdown(country: str):
    logging.debug("Updating league dropdown")

    options = []
    default = None
    if country:
        leagues = model.competitions.get_competition_whitelist(model.competitions.CompetitionType.LEAGUE)
        leagues.sort(key=lambda league: league.id)
        options = [league.name for league in leagues if league.country == country]
        if options:
            default = options[0]
    return options, default


def update_league(data_dict: dict, period_name: str):
    logging.debug("Updating league")

    functions = [
        dashboard.visualisations.create_goals_pie,
        dashboard.visualisations.create_goals_scatter_graph,
        dashboard.visualisations.create_scorelines_heatmap,
        dashboard.visualisations.create_bts_scatter_graph,
        dashboard.visualisations.create_results_pie_for_league,
        dashboard.visualisations.create_results_scatter_graph
    ]

    period = model.fixtures.Period(period_name)
    bins = dashboard.data.aggregate_seasons(data_dict, period)
    partial_function = functools.partial(
        lambda func, bins, period: func(bins, period),
        bins=bins,
        period=period
    )

    with concurrent.futures.ThreadPoolExecutor() as executor:
        futures = [executor.submit(partial_function, func) for func in functions]
        concurrent.futures.wait(futures)

    figures = [future.result() for future in futures]

    return figures


@dataclasses.dataclass(slots=True, frozen=True)
class SelectionHistory:
    country_name: str = None
    league_name: str = None
    season_id: int = None
    period: model.fixtures.Period = None
    venue: model.fixtures.Venue = None


last_selection: SelectionHistory = SelectionHistory()


def update_league_table(
        data_dict: dict,
        period_name: str,
        venue_name: str,
        team_name: str,
        season_years: str,
        max_input: int,
        history_input: list[int]
):
    global last_selection
    logging.debug("Updating league table")

    venue = model.fixtures.Venue(venue_name)
    period = model.fixtures.Period(period_name)
    start_year, end_year = dashboard.fixtures.get_start_and_end_years(season_years)
    season_id = dashboard.data.get_season_id(data_dict, start_year, end_year)

    this_selection = SelectionHistory(data_dict['Country'], data_dict['League'], season_id, period, venue)
    if last_selection != this_selection:
        filtered_team_fixtures = dashboard.data.get_result_per_team(data_dict, season_id, venue, [0, 0])
    else:
        filtered_team_fixtures = dashboard.data.get_result_per_team(data_dict, season_id, venue, history_input)

    teams_df = dashboard.data.get_teams_data_frame(data_dict)
    league_table = dashboard.tables.create_league_table(teams_df, filtered_team_fixtures, period)
    fixture = dashboard.data.get_next_fixture(data_dict, team_name)
    team_id = dashboard.data.get_team_id(teams_df, team_name)
    formatter = dashboard.tables.create_league_table_formatter(teams_df, league_table, team_id, fixture)

    if last_selection != this_selection:
        max_output = 0
        for row in league_table.rows:
            max_output = max(max_output, getattr(row, dashboard.tables.COL_PLAYED.display))
        history_output = [1, max_output]
    else:
        history_output = history_input
        max_output = max_input

    if max_output:
        min_output = 1
    else:
        min_output = 0

    last_selection = this_selection

    return league_table.to_display_list(), formatter, min_output, max_output, history_output


def update_league_scheduled_and_completed_tables(data_dict: dict, team_name: str):
    logging.debug("Updating league scheduled and completed tables")

    fixtures = dashboard.data.get_scheduled_fixtures(data_dict)
    teams_df = dashboard.data.get_teams_data_frame(data_dict)
    team_id = dashboard.data.get_team_id(teams_df, team_name)
    upcoming_table_rows, upcoming_formatter = dashboard.tables.create_scheduled_fixtures_table(teams_df, fixtures, team_id)
    fixtures = dashboard.data.get_completed_fixtures(data_dict)
    completed_table_rows, completed_formatter = dashboard.tables.create_finished_fixtures_table(teams_df, fixtures, team_id)
    return upcoming_table_rows, upcoming_formatter, completed_table_rows, completed_formatter


def update_team_dropdown(data_dict: dict):
    logging.debug("Updating team dropdown")

    teams_df = dashboard.data.get_teams_data_frame(data_dict)
    season_id = dashboard.data.get_current_season_id(data_dict)
    team_ids = dashboard.data.get_season_teams(data_dict, season_id)
    team_names = []
    for team_id in team_ids:
        team_name = dashboard.data.get_team_name(teams_df, team_id)
        team_names.append(team_name)
    team_names.sort()

    if team_names:
        (default,) = random.sample(team_names, 1)
    else:
        default = None
    return team_names, default


def update_team_tabs_names(period_name: str, venue_name: str, team_name: str):
    period = model.fixtures.Period(period_name)
    venue = model.fixtures.Venue(venue_name)

    outputs = [
        f'Overview: {team_name} ({period}, {venue})',
        f'This Season: {team_name} ({period}, {venue})'
    ]

    return outputs


def update_team_overview(data_dict: dict, period_name: str, venue_name: str, team_name: str):
    logging.debug("Updating team overview")

    period = model.fixtures.Period(period_name)
    venue = model.fixtures.Venue(venue_name)
    outputs = [
        dash.html.H3(f'Fixtures ({venue})', className="card-title"),
        dash.html.H3(f'Results ({venue})', className="card-title"),
    ]

    fixtures = dashboard.data.get_scheduled_fixtures_for_team(data_dict, venue, team_name)
    teams_df = dashboard.data.get_teams_data_frame(data_dict)
    team_id = dashboard.data.get_team_id(teams_df, team_name)
    upcoming_fixtures_rows = dashboard.tables.create_remaining_fixtures_table(teams_df, fixtures, team_id)
    fixtures = dashboard.data.get_completed_fixtures_for_team(data_dict, venue, team_name)
    results_table_rows, results_table_formatter = dashboard.tables.create_finished_fixtures_table_for_team(teams_df, fixtures, team_id)
    outputs.extend([
        upcoming_fixtures_rows,
        results_table_rows,
        results_table_formatter
    ])

    functions = [
        dashboard.visualisations.create_goals_pie,
        dashboard.visualisations.create_goals_scatter_graph,
        dashboard.visualisations.create_scorelines_heatmap,
        dashboard.visualisations.create_scored_and_conceded_scatter_graph,
        dashboard.visualisations.create_results_pie_for_team,
        dashboard.visualisations.create_wins_draw_losses_scatter_graph
    ]

    bins = dashboard.data.aggregate_team(data_dict, period, venue, team_id)
    partial_function = functools.partial(
        lambda func, bins, period, venue: func(bins, period, venue),
        bins=bins,
        period=period,
        venue=venue
    )

    with concurrent.futures.ThreadPoolExecutor() as executor:
        futures = [executor.submit(partial_function, func) for func in functions]
        concurrent.futures.wait(futures)

    figures = [future.result() for future in futures]
    outputs.extend(figures)
    return outputs


def update_team_sequences(data_dict: dict,
                          period_name: str,
                          venue_name: str,
                          team_name: str,
                          result_index: int,
                          result_negate: list,
                          goal_index: int,
                          goal_negate: list,
                          total_goals_relation: str,
                          total_goals: int):
    logging.debug("Updating team this season")

    period = model.fixtures.Period(period_name)
    venue = model.fixtures.Venue(venue_name)
    teams_df = dashboard.data.get_teams_data_frame(data_dict)
    team_id = dashboard.data.get_team_id(teams_df, team_name)
    teams_scorelines, index = dashboard.data.collect_fixture_sequences(data_dict, period, venue, team_id)

    total_goals_predicate = dashboard.analysis.create_total_goals_predicate(total_goals_relation, total_goals)

    events = [
        dashboard.analysis.Event(dashboard.analysis.result_predicates[result_index], True if result_negate else False),
        dashboard.analysis.Event(dashboard.analysis.goals_predicates[goal_index], True if goal_negate else False),
        dashboard.analysis.Event(total_goals_predicate, False)
    ]

    dashboard.analysis.analyse(teams_df, teams_scorelines, index, team_name, events)
    outputs = []
    for event in events:
        bar = dashboard.visualisations.create_bar(event, team_name)
        outputs.append(bar)

    return outputs


def register_callbacks(app: dash.Dash):
    logging.info("Registering callbacks")

    app.callback(
        [
            dash.dependencies.Output(dashboard.ids.Tab.LEAGUE_OVERVIEW_TAB, 'label'),
            dash.dependencies.Output(dashboard.ids.Dropdown.SEASON_DROPDOWN, 'options'),
            dash.dependencies.Output(dashboard.ids.Dropdown.SEASON_DROPDOWN, 'value'),
            dash.dependencies.Output(dashboard.ids.Miscellaneous.DATA_STORE, 'data')
        ],
        [
            dash.dependencies.Input(dashboard.ids.Dropdown.COUNTRY_DROPDOWN, 'value'),
            dash.dependencies.Input(dashboard.ids.Dropdown.LEAGUE_DROPDOWN, 'value')
        ]
    )(update_data)

    app.callback(
        [
            dash.dependencies.Output(dashboard.ids.Dropdown.LEAGUE_DROPDOWN, 'options'),
            dash.dependencies.Output(dashboard.ids.Dropdown.LEAGUE_DROPDOWN, 'value')
        ],
        [
            dash.dependencies.Input(dashboard.ids.Dropdown.COUNTRY_DROPDOWN, 'value')
        ]
    )(update_league_dropdown)

    app.callback(
        [
            dash.dependencies.Output(dashboard.ids.Pie.GOALS_PIE, 'figure'),
            dash.dependencies.Output(dashboard.ids.Graph.GOALS_GRAPH, 'figure'),
            dash.dependencies.Output(dashboard.ids.Graph.SCORES_HEATMAP, 'figure'),
            dash.dependencies.Output(dashboard.ids.Graph.BTS_GRAPH, 'figure'),
            dash.dependencies.Output(dashboard.ids.Pie.RESULTS_PIE, 'figure'),
            dash.dependencies.Output(dashboard.ids.Graph.RESULTS_GRAPH, 'figure')
        ],
        [
            dash.dependencies.Input(dashboard.ids.Miscellaneous.DATA_STORE, 'data'),
            dash.dependencies.Input(dashboard.ids.Radio.HALF_RADIO, 'value')
        ]
    )(update_league)

    app.callback(
        [
            dash.dependencies.Output(dashboard.ids.Table.LEAGUE_TABLE, 'data'),
            dash.dependencies.Output(dashboard.ids.Table.LEAGUE_TABLE, 'style_data_conditional'),
            dash.dependencies.Output(dashboard.ids.Slider.HISTORY_SLIDER, 'min'),
            dash.dependencies.Output(dashboard.ids.Slider.HISTORY_SLIDER, 'max'),
            dash.dependencies.Output(dashboard.ids.Slider.HISTORY_SLIDER, 'value')
        ],
        [
            dash.dependencies.Input(dashboard.ids.Miscellaneous.DATA_STORE, 'data'),
            dash.dependencies.Input(dashboard.ids.Radio.HALF_RADIO, 'value'),
            dash.dependencies.Input(dashboard.ids.Radio.VENUE_RADIO, 'value'),
            dash.dependencies.Input(dashboard.ids.Dropdown.TEAM_DROPDOWN, 'value'),
            dash.dependencies.Input(dashboard.ids.Dropdown.SEASON_DROPDOWN, 'value'),
            dash.dependencies.Input(dashboard.ids.Slider.HISTORY_SLIDER, 'max'),
            dash.dependencies.Input(dashboard.ids.Slider.HISTORY_SLIDER, 'value')
        ]
    )(update_league_table)

    app.callback(
        [
            dash.dependencies.Output(dashboard.ids.Table.LEAGUE_FIXTURES_TABLE, 'data'),
            dash.dependencies.Output(dashboard.ids.Table.LEAGUE_FIXTURES_TABLE, 'style_data_conditional'),
            dash.dependencies.Output(dashboard.ids.Table.LEAGUE_RESULTS_TABLE, 'data'),
            dash.dependencies.Output(dashboard.ids.Table.LEAGUE_RESULTS_TABLE, 'style_data_conditional')
        ],
        [
            dash.dependencies.Input(dashboard.ids.Miscellaneous.DATA_STORE, 'data'),
            dash.dependencies.Input(dashboard.ids.Dropdown.TEAM_DROPDOWN, 'value')
        ]
    )(update_league_scheduled_and_completed_tables)

    app.callback(
        [
            dash.dependencies.Output(dashboard.ids.Dropdown.TEAM_DROPDOWN, 'options'),
            dash.dependencies.Output(dashboard.ids.Dropdown.TEAM_DROPDOWN, 'value')
        ],
        dash.dependencies.Input(dashboard.ids.Miscellaneous.DATA_STORE, 'data')
    )(update_team_dropdown)

    app.callback(
        [
            dash.dependencies.Output(dashboard.ids.Miscellaneous.TEAM_FIXTURES_CARD, 'children'),
            dash.dependencies.Output(dashboard.ids.Miscellaneous.TEAM_RESULTS_CARD, 'children'),
            dash.dependencies.Output(dashboard.ids.Table.TEAM_FIXTURES_TABLE, 'data'),
            dash.dependencies.Output(dashboard.ids.Table.TEAM_RESULTS_TABLE, 'data'),
            dash.dependencies.Output(dashboard.ids.Table.TEAM_RESULTS_TABLE, 'style_data_conditional'),
            dash.dependencies.Output(dashboard.ids.Pie.TEAM_GOALS_PIE, 'figure'),
            dash.dependencies.Output(dashboard.ids.Graph.TEAM_GOALS_GRAPH, 'figure'),
            dash.dependencies.Output(dashboard.ids.Graph.TEAM_SCORES_HEATMAP, 'figure'),
            dash.dependencies.Output(dashboard.ids.Graph.TEAM_SCORED_AND_CONCEDED_GRAPH, 'figure'),
            dash.dependencies.Output(dashboard.ids.Pie.TEAM_RESULTS_PIE, 'figure'),
            dash.dependencies.Output(dashboard.ids.Graph.TEAM_RESULTS_GRAPH, 'figure')
        ],
        [
            dash.dependencies.Input(dashboard.ids.Miscellaneous.DATA_STORE, 'data'),
            dash.dependencies.Input(dashboard.ids.Radio.HALF_RADIO, 'value'),
            dash.dependencies.Input(dashboard.ids.Radio.VENUE_RADIO, 'value'),
            dash.dependencies.Input(dashboard.ids.Dropdown.TEAM_DROPDOWN, 'value')
        ]
    )(update_team_overview)

    app.callback(
        [
            dash.dependencies.Output(dashboard.ids.Tab.TEAM_OVERVIEW_TAB, 'label'),
            dash.dependencies.Output(dashboard.ids.Tab.TEAM_NOW_TAB, 'label'),
        ],
        [
            dash.dependencies.Input(dashboard.ids.Radio.HALF_RADIO, 'value'),
            dash.dependencies.Input(dashboard.ids.Radio.VENUE_RADIO, 'value'),
            dash.dependencies.Input(dashboard.ids.Dropdown.TEAM_DROPDOWN, 'value')
        ]
    )(update_team_tabs_names)

    app.callback(
        [
            dash.dependencies.Output(dashboard.ids.Bar.RESULTS_BAR, 'figure'),
            dash.dependencies.Output(dashboard.ids.Bar.GOALS_BAR, 'figure'),
            dash.dependencies.Output(dashboard.ids.Bar.TOTAL_GOALS_BAR, 'figure')
        ],
        [
            dash.dependencies.Input(dashboard.ids.Miscellaneous.DATA_STORE, 'data'),
            dash.dependencies.Input(dashboard.ids.Radio.HALF_RADIO, 'value'),
            dash.dependencies.Input(dashboard.ids.Radio.VENUE_RADIO, 'value'),
            dash.dependencies.Input(dashboard.ids.Dropdown.TEAM_DROPDOWN, 'value'),
            dash.dependencies.Input(dashboard.ids.Radio.RESULTS_RADIO, 'value'),
            dash.dependencies.Input(dashboard.ids.Switch.RESULTS_SWITCH, 'value'),
            dash.dependencies.Input(dashboard.ids.Radio.GOALS_RADIO, 'value'),
            dash.dependencies.Input(dashboard.ids.Switch.GOALS_SWITCH, 'value'),
            dash.dependencies.Input(dashboard.ids.Radio.TOTAL_GOALS_RELATIONS_RADIO, 'value'),
            dash.dependencies.Input(dashboard.ids.Radio.TOTAL_GOALS_RADIO, 'value'),
        ]
    )(update_team_sequences)


def create_app() -> dash.Dash:
    dbc_css = "https://cdn.jsdelivr.net/gh/AnnMarieW/dash-bootstrap-templates/dbc.min.css"
    app = dash.Dash(external_stylesheets=[dash_bootstrap_components.themes.YETI, dbc_css])
    dash_bootstrap_templates.load_figure_template('YETI')
    app.title = 'Goals Goals Goals!'
    app.layout = dashboard.layout.create_layout()
    return app


def main():
    logging.basicConfig(level=logging.DEBUG)
    logging.basicConfig(format="%(levelname)s | %(asctime)s | %(message)s")
    app = create_app()
    register_callbacks(app)
    logging.info("App created")
    app.run(debug=True)


if __name__ == '__main__':
    main()

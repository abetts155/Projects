import collections
import concurrent.futures
import dash
import dash.dependencies
import dash_bootstrap_components
import dash_bootstrap_templates
import functools
import logging
import random
import zoneinfo

import dashboard.analysis
import dashboard.data
import dashboard.fixtures
import dashboard.ids
import dashboard.layout
import dashboard.tables
import dashboard.visualisations
import lib.structure
import model.competitions
import model.fixtures
import model.seasons
import model.teams


def update_league_dropdown(country: str):
    logging.debug("Updating league dropdown")
    leagues = model.competitions.get_whitelisted_competitions(model.competitions.CompetitionType.LEAGUE)
    leagues.sort(key=lambda league: league.id)
    options = [league.name for league in leagues if league.country == country]
    return options, options[0]


def update_data(country: str, league_name: str):
    logging.debug("Updating data")

    league = model.competitions.get_league(country, league_name)
    data_dict = dashboard.data.update_data(league)
    seasons = dashboard.data.get_seasons(data_dict, True)
    tab_name = f"{country} {league_name}: {len(seasons)} seasons of data"
    season_options = [str(season) for season in seasons]

    outputs = [
        tab_name,
        season_options,
        season_options[0],
        data_dict,

    ]
    return outputs


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


def update_league_table(
        data_dict: dict,
        period_name: str,
        venue_name: str,
        season_years: str,
        max_input: int,
        history_input: list[int]
):
    logging.debug("Updating league table")

    start_year, end_year = dashboard.fixtures.get_start_and_end_years(season_years)
    season_id = dashboard.data.get_season_id(data_dict, start_year, end_year)
    venue = model.fixtures.Venue(venue_name)

    if history_input == [0, 0]:
       filtered_team_fixtures = dashboard.data.get_result_per_team(data_dict, season_id, venue, None)
    else:
        filtered_team_fixtures = dashboard.data.get_result_per_team(data_dict, season_id, venue, history_input)

    teams_df = dashboard.data.get_teams_data_frame(data_dict)
    period = model.fixtures.Period(period_name)
    league_table = dashboard.tables.create_league_table(teams_df, filtered_team_fixtures, period)
    fixtures = dashboard.data.get_scheduled_fixtures(data_dict)
    formatter, tooltips = dashboard.tables.create_league_table_formatter(teams_df, league_table, fixtures)

    min_output = max_output = 0
    for row in league_table.rows:
        played = getattr(row, dashboard.tables.COL_PLAYED.name)
        max_output = max(max_output, played)
        if min_output == 0 and played > 0:
            min_output = 1

    history_output = [0, 0]

    active_cell = {
        'row': random.randint(0, len(league_table) - 1),
        'column': 0,
        'column_id': dashboard.tables.COL_TEAM.name
    }

    return league_table.to_display_list(), formatter, tooltips, active_cell, min_output, max_output, history_output


def update_league_scheduled_and_completed_tables(data_dict: dict, team_name: str):
    logging.debug("Updating league scheduled and completed tables")

    fixtures = dashboard.data.get_scheduled_fixtures(data_dict)
    teams_df = dashboard.data.get_teams_data_frame(data_dict)
    team_id = dashboard.data.get_team_id(teams_df, team_name)
    upcoming_table_rows, upcoming_formatter = dashboard.tables.create_scheduled_fixtures_table(teams_df, fixtures, team_id)
    fixtures = dashboard.data.get_completed_fixtures(data_dict)
    completed_table_rows, completed_formatter = dashboard.tables.create_finished_fixtures_table(teams_df, fixtures, team_id)
    return upcoming_table_rows, upcoming_formatter, completed_table_rows, completed_formatter


def update_team_tab(data_dict: dict, period_name: str, venue_name: str, active_cell: dict, table_data: list):
    if not active_cell or active_cell['column_id'] != dashboard.tables.COL_TEAM.name:
        raise dash.exceptions.PreventUpdate

    logging.debug("Updating team tab")
    team_name = table_data[active_cell['row']][dashboard.tables.COL_TEAM.name]
    period = model.fixtures.Period(period_name)
    venue = model.fixtures.Venue(venue_name)

    tab_title = f'{team_name} ({period}, {venue})'
    outputs = [
        tab_title,
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

    outputs = [tab_title]

    functions = [
        dashboard.visualisations.create_goals_scatter_graph,
        dashboard.visualisations.create_scored_and_conceded_scatter_graph,
        dashboard.visualisations.create_wins_draw_losses_scatter_graph,
        dashboard.visualisations.create_scorelines_heatmap,
        dashboard.visualisations.create_team_scoring_patterns,
    ]

    bins = dashboard.data.aggregate_team(data_dict, period, venue, team_id)
    partial_function = functools.partial(
        lambda func, bins, period, venue, team_name: func(bins, period, venue, team_name),
        bins=bins,
        period=period,
        venue=venue,
        team_name=team_name
    )

    with concurrent.futures.ThreadPoolExecutor() as executor:
        futures = [executor.submit(partial_function, func) for func in functions]
        concurrent.futures.wait(futures)

    figs = [future.result() for future in futures]
    outputs.extend(figs)

    teams_scorelines, index = dashboard.data.collect_fixture_sequences(data_dict, period, venue, team_id)
    trends = dashboard.analysis.analyse(period, teams_scorelines, index, team_id)
    fig = dashboard.visualisations.create_trends(trends, period, venue, team_name)
    outputs.append(fig)

    return outputs


def update_fixture_list(n_intervals: int):
    print(f"Refresh triggered: {n_intervals}")
    LOWER = 1
    UPPER = 12

    whitelisted = model.competitions.get_whitelisted_competitions(model.competitions.CompetitionType.LEAGUE)
    grouped = collections.defaultdict(lambda: collections.defaultdict(list))  # hour -> league -> list of fixtures

    league_data = {}
    for league in whitelisted:
        database = lib.structure.get_database(league.country)
        season = model.seasons.load_current_season(database, league)
        fixtures = model.seasons.load_fixtures(database, league, season)
        fixtures = [f for f in fixtures if f.finished]

        if len(fixtures) >= 30:
            upcoming = model.fixtures.load_fixtures_within_window(database, league, LOWER, UPPER)

            for f in upcoming:
                minute_block = (f.date.minute // 15) * 15
                quarter_block = f.date.replace(minute=minute_block, second=0, microsecond=0)
                grouped[quarter_block][league].append(f)

            if upcoming:
                league_data[league] = dashboard.data.update_data(league)

    interval_items = []
    for interval in sorted(grouped):
        league_items = []

        for league, upcoming in sorted(grouped[interval].items(), key=lambda x: (x[0].country, x[0].id)):
            logging.info(f"Analysing {league} at {interval}")
            fixture_list = []
            upcoming.sort(key=lambda f: f.date)
            for f in upcoming:
                data_dict = league_data[league]
                if dashboard.data.get_current_season_id(data_dict):
                    fixture_str = f"{f.home_team.name} vs {f.away_team.name}"
                    fixture_list.append(dash.html.H6(fixture_str))

            league_content = dash.html.Ul(fixture_list)
            flag = model.competitions.country_flag(league.get_2_letter_iso_code())
            league_title = f"{flag} {str(league)}"

            league_items.append(
                dash_bootstrap_components.AccordionItem(
                    children=league_content,
                    title=league_title,
                )
            )

        interval_title = f"{interval.strftime('%H:%M')}"
        interval_items.append(
            dash_bootstrap_components.AccordionItem(
                title=interval_title,
                children=dash_bootstrap_components.Accordion(
                    league_items,
                    start_collapsed=False,
                    always_open=True,
                    flush=True
                )
            )
        )

    accordion = dash_bootstrap_components.Accordion(
        interval_items,
        start_collapsed=False,
        always_open=True,
        flush=True
    )

    return [accordion]


def register_callbacks(app: dash.Dash):
    logging.info("Registering callbacks")

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
            dash.dependencies.Output(dashboard.ids.Table.LEAGUE_TABLE, 'tooltip_data'),
            dash.dependencies.Output(dashboard.ids.Table.LEAGUE_TABLE, 'active_cell'),
            dash.dependencies.Output(dashboard.ids.Slider.HISTORY_SLIDER, 'min'),
            dash.dependencies.Output(dashboard.ids.Slider.HISTORY_SLIDER, 'max'),
            dash.dependencies.Output(dashboard.ids.Slider.HISTORY_SLIDER, 'value')
        ],
        [
            dash.dependencies.Input(dashboard.ids.Miscellaneous.DATA_STORE, 'data'),
            dash.dependencies.Input(dashboard.ids.Radio.HALF_RADIO, 'value'),
            dash.dependencies.Input(dashboard.ids.Radio.VENUE_RADIO, 'value'),
            dash.dependencies.Input(dashboard.ids.Dropdown.SEASON_DROPDOWN, 'value'),
            dash.dependencies.Input(dashboard.ids.Slider.HISTORY_SLIDER, 'max'),
            dash.dependencies.Input(dashboard.ids.Slider.HISTORY_SLIDER, 'value')
        ]
    )(update_league_table)

    app.callback(
        [
            dash.dependencies.Output(dashboard.ids.Tab.TEAM_OVERVIEW_TAB, 'label'),
            #dash.dependencies.Output(dashboard.ids.Miscellaneous.TEAM_FIXTURES_CARD, 'children'),
            #dash.dependencies.Output(dashboard.ids.Miscellaneous.TEAM_RESULTS_CARD, 'children'),
            #dash.dependencies.Output(dashboard.ids.Table.TEAM_FIXTURES_TABLE, 'data'),
            #dash.dependencies.Output(dashboard.ids.Table.TEAM_RESULTS_TABLE, 'data'),
            #dash.dependencies.Output(dashboard.ids.Table.TEAM_RESULTS_TABLE, 'style_data_conditional'),
            dash.dependencies.Output(dashboard.ids.Team.ROW_1_COL_1, 'figure'),
            dash.dependencies.Output(dashboard.ids.Team.ROW_1_COL_2, 'figure'),
            dash.dependencies.Output(dashboard.ids.Team.ROW_1_COL_3, 'figure'),
            dash.dependencies.Output(dashboard.ids.Team.ROW_2_COL_1, 'figure'),
            dash.dependencies.Output(dashboard.ids.Team.ROW_2_COL_2, 'figure'),
            dash.dependencies.Output(dashboard.ids.Team.ROW_2_COL_3, 'figure')
        ],
        [
            dash.dependencies.Input(dashboard.ids.Miscellaneous.DATA_STORE, 'data'),
            dash.dependencies.Input(dashboard.ids.Radio.HALF_RADIO, 'value'),
            dash.dependencies.Input(dashboard.ids.Radio.VENUE_RADIO, 'value'),
            dash.dependencies.Input(dashboard.ids.Table.LEAGUE_TABLE, 'active_cell')
        ],
        [
            dash.dependencies.State(dashboard.ids.Table.LEAGUE_TABLE, 'data')
        ]
    )(update_team_tab)

    app.callback(
        [
            dash.dependencies.Output(dashboard.ids.Miscellaneous.FIXTURE_LIST, 'children')
        ],
        [
            dash.dependencies.Input(dashboard.ids.Miscellaneous.FIXTURE_INTERVAL, 'n_intervals')
        ]
    )(update_fixture_list)


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

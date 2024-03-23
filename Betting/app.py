import concurrent.futures
import logging

from argparse import ArgumentParser
from functools import partial
from random import sample
from typing import Dict

from dash import Dash
from dash.dependencies import Input, Output
from dash_bootstrap_components.icons import FONT_AWESOME
from dash_bootstrap_components.themes import LUX
from dash_bootstrap_templates import load_figure_template

from cli.cli import add_database_option
from dashboard import data, layout, ids, visualisations, tables
from dashboard.fixtures import Period, Venue
from model.leagues import country_to_leagues, uglify


def update_league_tab(database: str, country_name: str, league_name: str):
    logging.debug("Updating data")

    outputs = [
        f'Overview: {country_name} {league_name}',
        data.update_data(database, country_name, league_name)
    ]
    return outputs


def update_league_dropdown(country: str):
    logging.debug("Updating league dropdown")

    options = []
    default = None
    if country:
        country = uglify(country)
        if country_to_leagues[country]:
            options = [league.name for league in country_to_leagues[country]]
            default = options[0]
    return options, default


def update_league_visualisations(data_dict: Dict, period_name: str):
    logging.debug("Updating league visualisations")

    functions = [
        visualisations.create_goals_pie,
        visualisations.create_goals_scatter_graph,
        visualisations.create_scorelines_heatmap,
        visualisations.create_bts_scatter_graph,
        visualisations.create_results_pie_for_league,
        visualisations.create_results_scatter_graph
    ]

    period = Period(period_name)
    bins = data.aggregate_seasons(data_dict, period)
    partial_function = partial(
        lambda func, bins, period: func(bins, period),
        bins=bins,
        period=period
    )

    with concurrent.futures.ThreadPoolExecutor() as executor:
        futures = [executor.submit(partial_function, func) for func in functions]
        concurrent.futures.wait(futures)

    figures = [future.result() for future in futures]

    return figures


def update_league_table(data_dict: Dict, period_name: str, venue_name: str, history: int, team_name: str):
    logging.debug("Updating league table")

    venue = Venue(venue_name)
    filtered_team_fixtures = data.get_fixtures_per_team_within_venue_and_window(data_dict, venue, history)
    period = Period(period_name)
    league_table = tables.create_league_table(filtered_team_fixtures, period)
    formatter = tables.create_league_table_formatter(league_table, team_name)
    return league_table.to_display_list(), formatter


def update_results_table(data_dict: Dict, team_name: str):
    logging.debug("Updating results table")

    fixtures = data.get_completed_fixtures(data_dict)
    table_rows, formatter = tables.create_finished_fixtures_table(fixtures, team_name)
    return table_rows, formatter


def update_team_dropdown(data_dict: Dict):
    logging.debug("Updating team dropdown")

    team_names = data.get_current_season_teams(data_dict)
    if team_names:
        (default,) = sample(team_names, 1)
    else:
        default = None
    return team_names, default


def update_team_overview_visualisations(data_dict: Dict, period_name: str, venue_name: str, team_name: str):
    logging.debug("Updating team overview visualisations")

    period = Period(period_name)
    venue = Venue(venue_name)
    outputs = [
        f'Overview: {team_name} ({period}, {venue})',
        f'Remaining ({venue})',
        f'Results ({venue})'
    ]

    fixtures = data.get_upcoming_fixtures(data_dict, venue, team_name)
    upcoming_fixtures_rows = tables.create_remaining_fixtures_table(fixtures, team_name)
    fixtures = data.get_completed_fixtures_for_team(data_dict, venue, team_name)
    results_table_rows, results_table_formatter = tables.create_finished_fixtures_table_for_team(fixtures, team_name)
    outputs.extend([
        upcoming_fixtures_rows,
        results_table_rows,
        results_table_formatter
    ])

    functions = [
        visualisations.create_goals_pie,
        visualisations.create_goals_scatter_graph,
        visualisations.create_scorelines_heatmap,
        visualisations.create_scored_and_conceded_scatter_graph,
        visualisations.create_results_pie_for_team,
        visualisations.create_wins_draw_losses_scatter_graph
    ]

    bins = data.aggregate_team(data_dict, period, venue, team_name)
    partial_function = partial(
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


def update_team_now_visualisations(data_dict: Dict, period_name: str, venue_name: str, team_name: str):
    logging.debug("Updating team now visualisations")

    period = Period(period_name)
    venue = Venue(venue_name)
    outputs = [
        f'Detailed Analysis: {team_name} ({period}, {venue})'
    ]

    data_bin = data.collect_current_season_data(data_dict, period, venue, team_name)

    outputs.extend(
        [
            visualisations.create_team_goals_trend(data_bin, period, venue, team_name),
            visualisations.create_team_results_trend(data_bin, period, venue, team_name)
        ]
    )

    return outputs


def register_callbacks(app: Dash, database: str):
    logging.info("Registering callbacks")

    app.callback(
        [
            Output(ids.Components.LEAGUE_OVERVIEW_TAB.name, 'label'),
            Output(ids.Components.DATA_STORE.name, 'data')
        ],
        [
            Input(ids.Components.COUNTRY_DROPDOWN.name, 'value'),
            Input(ids.Components.LEAGUE_DROPDOWN.name, 'value')
        ]
    )(partial(update_league_tab, database))

    app.callback(
        [
            Output(ids.Components.LEAGUE_DROPDOWN.name, 'options'),
            Output(ids.Components.LEAGUE_DROPDOWN.name, 'value')
        ],
        [
            Input(ids.Components.COUNTRY_DROPDOWN.name, 'value')
        ]
    )(update_league_dropdown)

    app.callback(
        [
            Output(ids.Components.GOALS_PIE.name, 'figure'),
            Output(ids.Components.GOALS_GRAPH.name, 'figure'),
            Output(ids.Components.SCORES_HEATMAP.name, 'figure'),
            Output(ids.Components.BTS_GRAPH.name, 'figure'),
            Output(ids.Components.RESULTS_PIE.name, 'figure'),
            Output(ids.Components.RESULTS_GRAPH.name, 'figure')
        ],
        [
            Input(ids.Components.DATA_STORE.name, 'data'),
            Input(ids.Components.HALF_RADIO.name, 'value')
        ]
    )(update_league_visualisations)

    app.callback(
        [
            Output(ids.Components.LEAGUE_TABLE.name, 'data'),
            Output(ids.Components.LEAGUE_TABLE.name, 'style_data_conditional')
        ],
        [
            Input(ids.Components.DATA_STORE.name, 'data'),
            Input(ids.Components.HALF_RADIO.name, 'value'),
            Input(ids.Components.VENUE_RADIO.name, 'value'),
            Input(ids.Components.HISTORY_RADIO.name, 'value'),
            Input(ids.Components.TEAM_DROPDOWN.name, 'value')
        ]
    )(update_league_table)

    app.callback(
        [
            Output(ids.Components.RESULTS_TABLE.name, 'data'),
            Output(ids.Components.RESULTS_TABLE.name, 'style_data_conditional')
        ],
        [
            Input(ids.Components.DATA_STORE.name, 'data'),
            Input(ids.Components.TEAM_DROPDOWN.name, 'value')
        ]
    )(update_results_table)

    app.callback(
        [
            Output(ids.Components.TEAM_DROPDOWN.name, 'options'),
            Output(ids.Components.TEAM_DROPDOWN.name, 'value')
        ],
        Input(ids.Components.DATA_STORE.name, 'data')
    )(update_team_dropdown)

    app.callback(
        [
            Output(ids.Components.TEAM_OVERVIEW_TAB.name, 'label'),
            Output(ids.Components.TEAM_FIXTURES_CARD.name, 'children'),
            Output(ids.Components.TEAM_RESULTS_CARD.name, 'children'),
            Output(ids.Components.TEAM_FIXTURES_TABLE.name, 'data'),
            Output(ids.Components.TEAM_RESULTS_TABLE.name, 'data'),
            Output(ids.Components.TEAM_RESULTS_TABLE.name, 'style_data_conditional'),
            Output(ids.Components.TEAM_GOALS_PIE.name, 'figure'),
            Output(ids.Components.TEAM_GOALS_GRAPH.name, 'figure'),
            Output(ids.Components.TEAM_SCORES_HEATMAP.name, 'figure'),
            Output(ids.Components.TEAM_SCORED_AND_CONCEDED_GRAPH.name, 'figure'),
            Output(ids.Components.TEAM_RESULTS_PIE.name, 'figure'),
            Output(ids.Components.TEAM_RESULTS_GRAPH.name, 'figure')
        ],
        [
            Input(ids.Components.DATA_STORE.name, 'data'),
            Input(ids.Components.HALF_RADIO.name, 'value'),
            Input(ids.Components.VENUE_RADIO.name, 'value'),
            Input(ids.Components.TEAM_DROPDOWN.name, 'value')
        ]
    )(update_team_overview_visualisations)

    app.callback(
        [
            Output(ids.Components.TEAM_NOW_TAB.name, 'label'),
            Output(ids.Components.TEAM_GOALS_TREND.name, 'figure'),
            Output(ids.Components.TEAM_RESULTS_TREND.name, 'figure')
        ],
        [
            Input(ids.Components.DATA_STORE.name, 'data'),
            Input(ids.Components.HALF_RADIO.name, 'value'),
            Input(ids.Components.VENUE_RADIO.name, 'value'),
            Input(ids.Components.TEAM_DROPDOWN.name, 'value')
        ]
    )(update_team_now_visualisations)


def parse_command_line():
    parser = ArgumentParser(description='Show summary of seasons by sequences')
    add_database_option(parser)
    return parser.parse_args()


def create_app() -> Dash:
    app = Dash(external_stylesheets=[LUX, FONT_AWESOME])
    load_figure_template('LUX')
    app.title = 'Goals Goals Goals!'
    app.layout = layout.create_layout()
    return app


def main():
    logging.basicConfig(level=logging.DEBUG)
    logging.basicConfig(format="%(levelname)s | %(asctime)s | %(message)s")
    args = parse_command_line()
    logging.debug(f"Extracting data from '{args.database}'")
    app = create_app()
    register_callbacks(app, args.database)
    logging.info("App created")
    app.run(debug=True)


if __name__ == '__main__':
    main()

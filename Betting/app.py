from datetime import date
from random import sample
from typing import Dict

from dash import Dash, html
from dash.dependencies import Input, Output
from dash_bootstrap_components.icons import FONT_AWESOME
from dash_bootstrap_components.themes import LUX
from dash_bootstrap_templates import load_figure_template

from dashboard import data, layout, ids, visualisations, tables
from model.fixtures import Half, Venue
from model.leagues import country_to_leagues, uglify


def update_league_dropdown(country: str):
    options = []
    default = None
    if country:
        country = uglify(country)
        if country_to_leagues[country]:
            options = [league.name for league in country_to_leagues[country]]
            default = options[0]
    return options, default


def update_league_tab(country_name: str, league_name: str):
    outputs = [
        f'League Analysis: {country_name} {league_name}',
        data.update_data(country_name, league_name)
    ]
    return outputs


def update_league_visualisations(data_dict: Dict, half_name: str):
    half = Half.from_string(half_name)
    bins = data.aggregate_seasons(data_dict, half)

    outputs = [
        visualisations.create_goals_pie(bins, half),
        visualisations.create_goals_scatter_graph(bins, half),
        visualisations.create_scorelines_heatmap(bins, half),
        visualisations.create_bts_scatter_graph(bins, half),
        visualisations.create_results_pie_for_league(bins, half),
        visualisations.create_results_scatter_graph(bins, half)
    ]
    return outputs


def update_league_table(data_dict: Dict, half_name: str, venue_name: str, history: int):
    venue = Venue.from_string(venue_name)
    fixtures = data.collect_fixtures(data_dict, venue, history)
    half = Half.from_string(half_name)
    return tables.create_league_table(fixtures, half)


def update_results_table(data_dict: Dict):
    return data.collect_results(data_dict)


def update_team_dropdown(data_dict: Dict):
    teams = data.get_teams(data_dict)
    (default,) = sample(teams, 1)
    outputs = [teams, default]
    return outputs


def update_team_visualisations(data_dict: Dict, half_name: str, venue_name: str, team: str):
    half = Half.from_string(half_name)
    venue = Venue.from_string(venue_name)
    team_dict = data.create_team_data(data_dict, venue, team)
    bins = data.aggregate_team(team_dict, half, team)
    unplayed_table_rows = data.get_unplayed_fixtures(data_dict, venue, team)
    results_table_rows, results_table_formatter = data.create_team_results_table(data_dict, venue, team)

    outputs = [
        f'Team Analysis: {team} ({half.value}, {venue.value})',
        visualisations.create_goals_pie(bins, half, venue),
        visualisations.create_goals_scatter_graph(bins, half, venue),
        visualisations.create_scorelines_heatmap(bins, half, venue),
        visualisations.create_scored_and_conceded_scatter_graph(bins, half, venue),
        visualisations.create_results_pie_for_team(bins, half, venue),
        visualisations.create_wins_draw_losses_scatter_graph(bins, half, venue),
        f'Remaining ({venue.value})',
        unplayed_table_rows,
        f'Results ({venue.value})',
        results_table_rows,
        results_table_formatter
    ]
    return outputs


def register_callbacks(app: Dash):
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
            Output(ids.Components.LEAGUE_TAB.name, 'label'),
            Output(ids.Components.DATA_STORE.name, 'data')
        ],
        [
            Input(ids.Components.COUNTRY_DROPDOWN.name, 'value'),
            Input(ids.Components.LEAGUE_DROPDOWN.name, 'value')
        ]
    )(update_league_tab)

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
            Input(ids.Components.HISTORY_RADIO.name, 'value')
        ]
    )(update_league_table)

    app.callback(
        Output(ids.Components.RESULTS_TABLE.name, 'data'),
        [
            Input(ids.Components.DATA_STORE.name, 'data')
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
            Output(ids.Components.TEAM_TAB.name, 'label'),
            Output(ids.Components.TEAM_GOALS_PIE.name, 'figure'),
            Output(ids.Components.TEAM_GOALS_GRAPH.name, 'figure'),
            Output(ids.Components.TEAM_SCORES_HEATMAP.name, 'figure'),
            Output(ids.Components.TEAM_SCORED_AND_CONCEDED_GRAPH.name, 'figure'),
            Output(ids.Components.TEAM_RESULTS_PIE.name, 'figure'),
            Output(ids.Components.TEAM_RESULTS_GRAPH.name, 'figure'),
            Output(ids.Components.TEAM_FIXTURES_CARD.name, 'children'),
            Output(ids.Components.TEAM_FIXTURES_TABLE.name, 'data'),
            Output(ids.Components.TEAM_RESULTS_CARD.name, 'children'),
            Output(ids.Components.TEAM_RESULTS_TABLE.name, 'data'),
            Output(ids.Components.TEAM_RESULTS_TABLE.name, 'style_data_conditional')
        ],
        [
            Input(ids.Components.DATA_STORE.name, 'data'),
            Input(ids.Components.HALF_RADIO.name, 'value'),
            Input(ids.Components.VENUE_RADIO.name, 'value'),
            Input(ids.Components.TEAM_DROPDOWN.name, 'value')
        ]
    )(update_team_visualisations)


def main():
    app = Dash(external_stylesheets=[LUX, FONT_AWESOME])
    load_figure_template('LUX')
    app.title = 'Goals Goals Goals!'
    app.layout = layout.create_layout()
    register_callbacks(app)
    app.run(debug=True)


if __name__ == '__main__':
    main()

from argparse import ArgumentParser, Namespace
from cli.cli import (add_database_option,
                     add_events_option,
                     add_history_option,
                     add_league_option,
                     add_team_option,
                     add_logging_options,
                     add_venue_option,
                     set_logging_options,
                     get_unique_league,
                     get_unique_event)
from collections import Counter
from lib import messages
from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
from model.fixtures import Result, Venue
from model.leagues import League, league_register
from model.seasons import Season
from model.sequences import count_events, BarChart
from sql.sql import load_database, extract_picked_team
from typing import Callable, List


def parse_command_line():
    parser = ArgumentParser(description='Show sequence data in bar charts')
    add_database_option(parser)
    add_events_option(parser)
    add_history_option(parser)
    add_league_option(parser)
    add_team_option(parser)
    add_venue_option(parser)
    add_logging_options(parser)
    return parser.parse_args()


def display_bar_graphs(event_function: Callable,
                       venue: Venue,
                       league: League,
                       charts: List[BarChart]):
    x_limit = 0
    for chart in charts:
        if chart.counter:
            max_key = max(chart.counter)
            x_limit = max(x_limit, max_key)

    if len(charts) == 4:
        fig, axes = plt.subplots(2, 2, figsize=(20, 10))
    else:
        fig, axes = plt.subplots(2, figsize=(20, 10))

    if venue == Venue.any:
        prologue = '{} ({} or {})'.format(Result.event_name(event_function), Venue.home.name, Venue.away.name)
    else:
        prologue = '{} ({} only)'.format(Result.event_name(event_function), venue.name)

    fig.suptitle('{} in {} {}'.format(prologue, league.country, league.name), fontweight='bold', fontsize=14)

    for chart_number, chart in enumerate(charts):
        sorted(chart.counter)
        x_values = []
        y_values = []
        for key in range(0, x_limit + 1):
            x_values.append(key)
            if key in chart.counter:
                y_values.append(chart.counter[key])
            else:
                y_values.append(0)

        if len(charts) == 4:
            binary = '{:02b}'.format(chart_number)
            x = int(binary[0])
            y = int(binary[1])
            ax = axes[x, y]
        else:
            ax = axes[chart_number]

        ax.xaxis.set_major_locator(MaxNLocator(integer=True))
        ax.yaxis.set_major_locator(MaxNLocator(integer=True))
        ax.set_xticks(range(0, x_limit + 1))
        ax.set_yticks([])

        if sum(y_values) == 0:
            ax.set_yticks([0])
        for k, v in chart.counter.items():
            ax.text(k, v, str(v), ha='center', fontsize=8)

        if len(chart.years) > 1:
            year_string = '{}-{}'.format(chart.years[0], chart.years[-1])
        else:
            year_string = str(chart.years[0])

        if chart.team is not None:
            summary_string = chart.team.name
        else:
            summary_string = 'Aggregated'

        ax.set_title('{} over {} with {} data points.'.format(summary_string,
                                                              year_string,
                                                              sum(chart.counter.values())))
        bar = ax.bar(x_values, y_values)

        if chart.team is not None and len(chart.years) == 1 and chart.last is not None:
            bar[chart.last].set_color('red')
    plt.subplots_adjust(hspace=0.5)
    plt.tight_layout()
    plt.show()


def main(arguments: Namespace):
    league = league_register[get_unique_league(arguments)]
    load_database(arguments.database, league)

    seasons = Season.seasons()
    if not seasons:
        messages.error_message("No season data found")

    if seasons[-1].current:
        this_season = seasons.pop()
    else:
        this_season = None

    if arguments.history:
        seasons = seasons[-arguments.history:]

    if arguments.team:
        selected_team = extract_picked_team(arguments.database, arguments.team, league)
    else:
        selected_team = None

    event_function = getattr(Result, get_unique_event(arguments))

    charts = []
    aggregated_history = BarChart(Counter(), [seasons[0].year, seasons[-1].year])
    charts.append(aggregated_history)
    for season in seasons:
        for team in season.teams():
            count_events(season,
                         team,
                         arguments.venue,
                         event_function,
                         aggregated_history)

    if selected_team is not None:
        team_history = BarChart(Counter(), [seasons[0].year, seasons[-1].year], selected_team)
        charts.append(team_history)
        for season in seasons:
            count_events(season,
                         team_history.team,
                         arguments.venue,
                         event_function,
                         team_history)

    if this_season is not None:
        aggregated_now = BarChart(Counter(), [this_season.year])
        charts.append(aggregated_now)
        for team in this_season.teams():
            count_events(this_season,
                         team,
                         arguments.venue,
                         event_function,
                         aggregated_now)

        if selected_team is not None:
            team_now = BarChart(Counter(), [this_season.year], selected_team)
            charts.append(team_now)
            count_events(this_season,
                         team_now.team,
                         arguments.venue,
                         event_function,
                         team_now)

    display_bar_graphs(event_function, arguments.venue, league, charts)


if __name__ == '__main__':
    arguments = parse_command_line()
    set_logging_options(arguments)
    main(arguments)

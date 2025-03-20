from argparse import ArgumentParser, Namespace
from cli.cli import (add_database_option,
                     add_league_option,
                     add_logging_options,
                     set_logging_options,
                     add_block_option,
                     add_team_option,
                     get_unique_league,
                     get_unique_event)
from datetime import datetime
from lib import messages
from lib.helpful import DisplayGrid, set_matplotlib_defaults
from matplotlib import pyplot as plt
from model.fixtures import Event, Fixture, Half, Scoreline, Venue, win, loss
from model.competitions import League, league_register, prettify
from model.seasons import Season
from model.teams import Team
from pandas import DataFrame
from sql.sql import load_league, load_teams, extract_picked_team
from typing import Callable, Dict, List, Tuple


def parse_command_line():
    parser = ArgumentParser(description='Show season-by-season results of team')
    add_database_option(parser)
    add_league_option(parser)
    add_logging_options(parser)
    add_team_option(parser, True)
    add_block_option(parser)
    return parser.parse_args()


def decide_cell_color(score: Scoreline,
                      left_color: Tuple[float, float, float],
                      right_color: Tuple[float, float, float],
                      neutral_color: Tuple[float, float, float]):
    if score:
        if win(score):
            return left_color
        elif loss(score):
            return right_color
        else:
            return neutral_color
    else:
        return neutral_color


def create_results_table(ax,
                         league: League,
                         season: Season,
                         fixtures: List[Fixture],
                         team: Team,
                         team_color: Tuple[float, float, float],
                         other_color: Tuple[float, float, float],
                         neutral_color: Tuple[float, float, float]):
    colors = []
    table = []
    fixtures.sort(key=lambda row: row.date)
    for i, fixture in enumerate(fixtures):
        first_half = fixture.first_half()
        second_half = fixture.second_half()
        full_time = fixture.full_time()

        if team == fixture.away_team:
            if first_half:
                first_half = first_half.reverse()

            if second_half:
                second_half = second_half.reverse()

            if full_time:
                full_time = full_time.reverse()

        fixture_colors = [neutral_color,
                          neutral_color if fixture.home_team != team else team_color,
                          neutral_color if fixture.away_team != team else team_color,
                          decide_cell_color(first_half, team_color, other_color, neutral_color),
                          decide_cell_color(second_half, team_color, other_color, neutral_color),
                          decide_cell_color(full_time, team_color, other_color, neutral_color)]

        if team == fixture.away_team:
            if first_half:
                first_half = first_half.reverse()

            if second_half:
                second_half = second_half.reverse()

            if full_time:
                full_time = full_time.reverse()

        now = datetime.now()
        if (fixture.date.day != now.day and
                fixture.date.month != now.month and
                fixture.date.year != now.year and
                not fixture.finished):
            row = ['TBD',
                   fixture.home_team.name,
                   fixture.away_team.name,
                   '',
                   '',
                   '']
        else:
            row = [fixture.date.strftime('%Y %b'),
                   fixture.home_team.name,
                   fixture.away_team.name,
                   str(first_half) if first_half else '',
                   str(second_half) if second_half else '',
                   str(full_time) if full_time else '']

        table.append(row)
        colors.append(fixture_colors)

    df = DataFrame(table)
    df.columns = ['Date', 'Home', 'Away', '1st', '2nd', 'FT']

    the_table = ax.table(cellText=df.values,
                         colLabels=df.columns,
                         colColours=[neutral_color] * len(df.columns),
                         colLoc='left',
                         cellColours=colors,
                         cellLoc='left',
                         colWidths=[0.15, 0.3, 0.3, 0.05, 0.05, 0.05],
                         loc='upper center')
    the_table.auto_set_font_size(False)
    the_table.set_fontsize(7.5)
    ax.axis('off')
    ax.set_title('{}: {}'.format(season.year, league))


def main(args: Namespace):
    set_matplotlib_defaults()
    load_teams(args.database)
    league = league_register[get_unique_league(args)]
    load_league(args.database, league)

    seasons = Season.seasons(league)
    if not seasons:
        messages.error_message("No season data found")
    history = 4
    seasons = seasons[-history:]

    (row,) = extract_picked_team(args.database, args.team, league)
    selected_team = Team.inventory[row[0]]

    fig, axs = plt.subplots(nrows=1, ncols=history, constrained_layout=True)
    team_color = '#0086c3'
    other_color = '#ef5350'
    neutral_color = '#000000'

    for i, season in enumerate(seasons):
        found = False
        for team, fixtures in season.fixtures_per_team().items():
            if selected_team:
                if team == selected_team:
                    found = True
                    create_results_table(axs[i],
                                         league,
                                         season,
                                         fixtures,
                                         selected_team,
                                         team_color,
                                         other_color,
                                         neutral_color)

        if not found:
            fig.delaxes(axs[i])

    title = '{} in the last {} seasons'.format(selected_team.name, history)
    fig.suptitle(title, fontweight='bold')
    plt.show(block=args.block)


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)

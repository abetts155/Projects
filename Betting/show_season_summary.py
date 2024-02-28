import operator
from argparse import ArgumentParser, Namespace
from cli.cli import (add_database_option,
                     add_league_option,
                     add_logging_options,
                     set_logging_options,
                     add_block_option,
                     add_half_option,
                     get_unique_league,
                     add_team_option,
                     add_venue_option)
from collections import Counter, OrderedDict
from lib import messages
from lib.helpful import DisplayGrid, set_matplotlib_defaults
from matplotlib import pyplot as plt
from model.fixtures import Half, Venue
from model.leagues import league_register
from model.seasons import Season
from model.teams import Team
from sql.sql import extract_picked_team, load_league, load_teams
from typing import List


def parse_command_line():
    parser = ArgumentParser(description='Show summary of each season by scorelines')
    add_database_option(parser)
    add_league_option(parser)
    add_logging_options(parser)
    add_half_option(parser)
    add_block_option(parser)
    add_team_option(parser)
    add_venue_option(parser)
    return parser.parse_args()


def compute_scorelines(season: Season, halves: List[Half], venue: Venue, team: Team) -> Counter:
    scorelines = Counter()
    for fixture in season.fixtures():
        results = []

        analyse = False
        if team is None:
            analyse = True
        else:
            if venue:
                if venue == Venue.home and fixture.home_team == team:
                    analyse = True
                elif venue == Venue.away and fixture.away_team == team:
                    analyse = True
                elif venue == Venue.anywhere and team in [fixture.home_team, fixture.away_team]:
                    analyse = True

        if analyse:
            if Half.first in halves:
                results.append(fixture.first_half())

            if Half.second in halves:
                results.append(fixture.second_half())

            if Half.full in halves:
                results.append(fixture.full_time())

        for result in results:
            if result is not None:
                if team is not None and fixture.away_team == team:
                    result = result.reverse()

                scorelines[(result.left, result.right)] += 1
    return scorelines


def compute_slack(x_values, scorelines, key: str):
    if '>' in key:
        op = operator.gt
    elif '<' in key:
        op = operator.lt
    else:
        op = operator.eq

    slack = 0
    for k, v in scorelines.items():
        left, right = k
        if op(left, right) and '{}-{}'.format(left, right) not in x_values:
            slack += v
    return slack


def show(title: str, half: Half, team: Team, season_scorelines, block: bool):
    display = DisplayGrid(len(season_scorelines), 2)
    fig, axs = plt.subplots(nrows=display.nrows,
                            ncols=display.ncols,
                            squeeze=False,
                            constrained_layout=True)

    for i, (season, scorelines) in enumerate(season_scorelines.items()):
        x_values = ['1-0', '2-0', '2-1', '3-0', '3-1', '3-2', '4-0', '4-1', 'X>Y',
                    '0-0', '1-1', '2-2', '3-3', 'X==Y',
                    '0-1', '0-2', '1-2', '0-3', '1-3', '2-3', '0-4', '1-4', 'X<Y']
        y_values = []
        slack = 0
        for score in x_values:
            if score in ['X>Y', 'X==Y', 'X<Y']:
                slack = compute_slack(x_values, scorelines, score)
                y_values.append(slack)
            else:
                left, right = score.split('-')
                key = (int(left), int(right))
                y_values.append(scorelines[key])

        cmap = plt.get_cmap('Reds')
        min_y = min(y_values)
        max_y = max(y_values)
        scaled = [(y - min_y) / (max_y - min_y) for y in y_values]

        cell_x, cell_y = display.index(i)
        ax = axs[cell_x, cell_y]
        bar = ax.bar(x_values, y_values, color=cmap(scaled))
        ax.set_ylabel(season.year)
        ax.set_yticks([])
        ax.set_frame_on(False)

        total_games = sum(scorelines.values())
        ax.set_title('{} results'.format(total_games))

        if total_games:
            for k, v in zip(x_values, y_values):
                ax.text(k, v, v, ha='center', fontsize=8)

            ax.axvline(8.5, ls='-', lw=1)
            ax.axvline(13.5, ls='-', lw=1)

        total_homes = 0
        total_draws = 0
        total_aways = 0
        for scoreline, count in scorelines.items():
            left, right = scoreline
            if left > right:
                total_homes += count
            elif left < right:
                total_aways += count
            else:
                total_draws += count

        home_percentage = round(100 * total_homes / total_games)
        draw_percentage = round(100 * total_draws / total_games)
        away_percentage = round(100 * total_aways / total_games)

        colours = ['#4169E1', '#FFD700', '#DA70D6']
        percentages = [home_percentage, draw_percentage, away_percentage]
        percentages.sort(reverse=True)

        ax.annotate('{}%'.format(home_percentage),
                    xy=(0.2, 0.8),
                    xycoords='axes fraction',
                    color='black',
                    bbox=dict(facecolor=colours[percentages.index(home_percentage)], edgecolor='black'))

        ax.annotate('{}%'.format(draw_percentage),
                    xy=(0.5, 0.8),
                    xycoords='axes fraction',
                    color='black',
                    bbox=dict(facecolor=colours[percentages.index(draw_percentage)], edgecolor='black'))

        ax.annotate('{}%'.format(away_percentage),
                    xy=(0.75, 0.8),
                    xycoords='axes fraction',
                    color='black',
                    bbox=dict(facecolor=colours[percentages.index(away_percentage)], edgecolor='black'))

    for i in range(len(season_scorelines), display.nrows * display.ncols):
        cell_x, cell_y = display.index(i)
        ax = axs[cell_x][cell_y]
        fig.delaxes(ax)

    #fig.suptitle(title, fontweight='bold')
    plt.show(block=block)


def main(args: Namespace):
    set_matplotlib_defaults()
    load_teams(args.database)
    league = league_register[get_unique_league(args)]
    load_league(args.database, league)

    seasons = Season.seasons(league)
    if not seasons:
        messages.error_message("No season data found")

    if args.team:
        (row,) = extract_picked_team(args.database, args.team, league)
        selected_team = Team.inventory[row[0]]
    else:
        selected_team = None

    season_scorelines = OrderedDict()
    for season in seasons:
        scorelines = compute_scorelines(season, args.half, args.venue, selected_team)
        if scorelines:
            season_scorelines[season] = scorelines

    if selected_team:
        title = '{} in {} {} ({})'.format(selected_team.name, league.country, league.name, args.venue.name)
    else:
        title = '{} {}'.format(league.country, league.name)

    title += ' ({})'.format(Half.to_string(args.half))

    show(title, args.half,  selected_team, season_scorelines, args.block)


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)

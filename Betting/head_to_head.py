from argparse import ArgumentParser, Namespace
from cli.cli import (add_database_option,
                     add_logging_options,
                     set_logging_options,
                     add_league_option,
                     add_team_option,
                     add_block_option,
                     get_multiple_teams,
                     get_unique_league)
from collections import Counter
from lib.messages import warning_message
from matplotlib import pyplot as plt
from model.fixtures import Half, Fixture, Result, create_fixture_from_row, win, draw, defeat
from model.leagues import league_register
from model.teams import Team
from sql.sql import Database
from sql.sql import ColumnNames, Characters, Keywords, extract_picked_team, load_league, load_teams
from typing import Callable, List, Tuple

import pandas as pd


def parse_command_line():
    parser = ArgumentParser(description='Show head-to-head record')
    add_database_option(parser)
    add_logging_options(parser)
    add_league_option(parser, False)
    add_team_option(parser, True)
    add_block_option(parser)
    return parser.parse_args()


def get_fixtures(database: str, left_team: Team, right_team: Team):
    fixtures = []
    with Database(database) as db:
        teams_constraint = "{}={} {} {}={}".format(ColumnNames.Home_ID.name,
                                                   left_team.id,
                                                   Keywords.AND.name,
                                                   ColumnNames.Away_ID.name,
                                                   right_team.id)
        finished_constraint = "{}={}".format(ColumnNames.Finished.name, Characters.TRUE.value)
        constraints = [teams_constraint, finished_constraint]
        fixtures_rows = db.fetch_all_rows(Fixture.sql_table(), constraints)

        for row in fixtures_rows:
            fixture = create_fixture_from_row(row)
            fixtures.append(fixture)
    return fixtures


class Statistics:
    __slots__ = ['wins', 'draws', 'losses', 'goals_for', 'goals_against', 'scores']

    def __init__(self):
        self.wins = 0
        self.draws = 0
        self.losses = 0
        self.goals_for = 0
        self.goals_against = 0
        self.scores = Counter()


def populate(stats: Statistics, result: Result):
    if result:
        if win(result):
            stats.wins += 1
        elif defeat(result):
            stats.losses += 1
        else:
            assert draw(result)
            stats.draws += 1

        stats.goals_for += result.left
        stats.goals_against += result.right
        stats.scores[(result.left, result.right)] += 1


def compute_records(fixtures: List[Fixture]):
    first_half_stats = Statistics()
    second_half_stats = Statistics()
    full_time_stats = Statistics()
    for fixture in fixtures:
        populate(first_half_stats, fixture.first_half())
        populate(second_half_stats, fixture.second_half())
        populate(full_time_stats, fixture.full_time())
    return first_half_stats, second_half_stats, full_time_stats


def attach_scorelines(ax,
                      x_values: List,
                      y_values: List,
                      colors: List,
                      color: Tuple[float, float, float],
                      stats: Statistics,
                      predicate: Callable):
    index = 0
    offset = len(y_values)
    for (left, right), count in sorted(stats.scores.items(), key=lambda x: x[1], reverse=True):
        if predicate(left, right):
            x_values.append('{}-{}'.format(left, right))
            y_values.append(count)
            colors.append(color)
            effective_index = index + offset
            ax.text(effective_index, count, str(count), ha='center', fontsize=8)
            index += 1


def create_title(team: Team):
    title = 'At {}'.format(team.name)
    return title


def create_single_bar(ax,
                      home_team: Team,
                      left_color: Tuple[float, float, float],
                      right_color: Tuple[float, float, float],
                      neutral_color: Tuple[float, float, float],
                      stats: Statistics):
    x_values = ['H', 'D', 'A', 'HG', 'AG']
    y_values = [stats.wins, stats.draws, stats.losses, stats.goals_for, stats.goals_against]
    colors = [left_color, neutral_color, right_color, left_color, right_color]

    for x, y in enumerate(y_values):
        ax.text(x, y, str(y), ha='center', fontsize=8)

    attach_scorelines(ax, x_values, y_values, colors, left_color, stats, int.__gt__)
    attach_scorelines(ax, x_values, y_values, colors, neutral_color, stats, int.__eq__)
    attach_scorelines(ax, x_values, y_values, colors, right_color, stats, int.__lt__)

    ax.bar(x_values, y_values, align='center', color=colors, edgecolor='black')
    indices = range(len(x_values))
    ax.set_xticks(indices)
    ax.set_xticklabels(x_values, rotation=30)
    ax.set_yticks([])
    ax.set_title(create_title(home_team), fontweight='bold', fontsize=14, color=left_color)


def decide_cell_color(result: Result,
                      left_color: Tuple[float, float, float],
                      right_color: Tuple[float, float, float],
                      neutral_color: Tuple[float, float, float],
                      unknown_color: Tuple[float, float, float]):
    if result:
        if win(result):
            return left_color
        elif defeat(result):
            return right_color
        else:
            return neutral_color
    else:
        return unknown_color


def create_results_table(ax,
                         fixtures: List[Fixture],
                         team: Team,
                         left_color: Tuple[float, float, float],
                         right_color: Tuple[float, float, float],
                         neutral_color: Tuple[float, float, float],
                         unknown_color: Tuple[float, float, float]):
    colors = []
    table = []
    fixtures.sort(key=lambda row: row.date)
    for i, fixture in enumerate(fixtures):
        first_half = fixture.first_half()
        second_half = fixture.second_half()
        full_time = fixture.full_time()
        colors.append([neutral_color,
                       decide_cell_color(first_half, left_color, right_color, neutral_color, unknown_color),
                       decide_cell_color(second_half, left_color, right_color, neutral_color, unknown_color),
                       decide_cell_color(full_time, left_color, right_color, neutral_color, unknown_color)])
        row = ['{}'.format(fixture.date.strftime('%Y-%m-%d')), str(first_half), str(second_half), str(full_time)]
        table.append(row)

    df = pd.DataFrame(table)
    df.columns = ['Date', '1st', '2nd', 'FT']

    ax.table(cellText=df.values,
             colLabels=df.columns,
             colLoc='left',
             cellColours=colors,
             cellLoc='left',
             loc='upper center')
    ax.set_title(create_title(team), fontweight='bold', fontsize=14, color=left_color)
    ax.axis('off')


def main(args: Namespace):
    load_teams(args.database)

    if args.league:
        league = league_register[get_unique_league(args)]
        load_league(args.database, league)

    nrows = 2
    ncols = 4
    fig, axs = plt.subplots(nrows=nrows, ncols=ncols, figsize=(20, 13.5), squeeze=False, constrained_layout=True)

    left_name, right_name = get_multiple_teams(args)
    (row,) = extract_picked_team(args.database, left_name, league)
    left_team = Team.inventory[row[0]]
    (row,) = extract_picked_team(args.database, right_name, league)
    right_team = Team.inventory[row[0]]

    left_team_color = (54 / 255, 104 / 255, 141 / 255)
    right_team_color = (240 / 255, 88 / 255, 55 / 255)
    neutral_color = (1, 1, 1)
    unknown_color = (0, 0, 0)
    halves = [Half.first, Half.second, Half.both]

    left_fixtures = get_fixtures(args.database, left_team, right_team)
    right_fixtures = get_fixtures(args.database, right_team, left_team)

    if not left_fixtures and not right_fixtures:
        warning_message("No head-to-head between {} and {}".format(left_team.name, right_team.name))
    else:
        if left_fixtures:
            first_half_stats, second_half_stats, full_time_stats = compute_records(left_fixtures)
            data = [first_half_stats, second_half_stats, full_time_stats]
            for i, (half, stats) in enumerate(zip(halves, data)):
                create_single_bar(axs[0, i],
                                  left_team,
                                  left_team_color,
                                  right_team_color,
                                  neutral_color,
                                  stats)

            create_results_table(axs[0, ncols - 1],
                                 left_fixtures,
                                 left_team,
                                 left_team_color,
                                 right_team_color,
                                 neutral_color,
                                 unknown_color)
        else:
            for i in range(ncols):
                fig.delaxes(axs[0, i])

        if right_fixtures:
            first_half_stats, second_half_stats, full_time_stats = compute_records(right_fixtures)
            data = [first_half_stats, second_half_stats, full_time_stats]
            for i, (half, stats) in enumerate(zip(halves, data)):
                create_single_bar(axs[1, i],
                                  right_team,
                                  right_team_color,
                                  left_team_color,
                                  neutral_color,
                                  stats)
            create_results_table(axs[1, ncols - 1],
                                 right_fixtures,
                                 right_team,
                                 right_team_color,
                                 left_team_color,
                                 neutral_color,
                                 unknown_color)
        else:
            for i in range(ncols):
                fig.delaxes(axs[1, i])

        fig.suptitle('Head-to-head: {} vs {}'.format(left_team.name, right_team.name), fontweight='bold', fontsize=14)
        plt.show(block=args.block)


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)

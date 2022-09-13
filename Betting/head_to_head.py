from argparse import ArgumentParser, Namespace
from matplotlib import pyplot as plt
from matplotlib import gridspec as gridspec
from pandas import DataFrame
from typing import Dict, List, Tuple
from urllib.error import URLError

from cli.cli import (add_database_option,
                     add_logging_options,
                     set_logging_options,
                     add_league_option,
                     add_team_option,
                     add_block_option,
                     get_multiple_teams,
                     get_unique_league)
from lib.helpful import set_matplotlib_defaults
from lib.messages import warning_message
from model.fixtures import Half, Fixture, Result, Venue, create_fixture_from_row, win, loss
from model.leagues import league_register
from model.seasons import Season
from model.tables import LeagueTable
from model.teams import Team
from sql.sql import (ColumnNames,
                     Characters,
                     Keywords,
                     extract_picked_team,
                     load_league, load_teams,
                     get_finished_matches)
from sql.sql import Database


def parse_command_line():
    parser = ArgumentParser(description='Show head-to-head record')
    add_database_option(parser)
    add_logging_options(parser)
    add_league_option(parser, False)
    add_team_option(parser, True)
    add_block_option(parser)
    return parser.parse_args()


def add_image(ax, team: Team):
    try:
        url = 'https://media.api-sports.io/football/teams'
        img = plt.imread('{}/{}.png'.format(url, team.id))
        ax.imshow(img)
        ax.axis('off')
        return True
    except URLError:
        return False


def get_head_to_head_fixtures(database: str, left_team: Team, right_team: Team):
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


def decide_cell_color(result: Result,
                      left_color: Tuple[float, float, float],
                      right_color: Tuple[float, float, float],
                      neutral_color: Tuple[float, float, float]):
    if result:
        if win(result):
            return left_color
        elif loss(result):
            return right_color
        else:
            return neutral_color
    else:
        return neutral_color


def create_results_table(ax,
                         fixtures: List[Fixture],
                         left_team: Team,
                         right_team: Team,
                         left_color: Tuple[float, float, float],
                         right_color: Tuple[float, float, float],
                         neutral_color: Tuple[float, float, float]):
    colors = []
    table = []
    fixtures.sort(key=lambda row: row.date, reverse=True)
    for i, fixture in enumerate(fixtures):
        first_half = fixture.first_half()
        second_half = fixture.second_half()
        full_time = fixture.full_time()
        colors.append([neutral_color,
                       left_color,
                       right_color,
                       decide_cell_color(first_half, left_color, right_color, neutral_color),
                       decide_cell_color(second_half, left_color, right_color, neutral_color),
                       decide_cell_color(full_time, left_color, right_color, neutral_color)])
        row = [fixture.date.strftime('%Y %b'),
               left_team.name,
               right_team.name,
               str(first_half) if first_half else '',
               str(second_half) if second_half else '',
               str(full_time) if full_time else '']
        table.append(row)

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
    the_table.set_fontsize(8)
    ax.axis('off')


def create_form_table(ax,
                      fixtures: List[Fixture],
                      team: Team,
                      team_color: Tuple[float, float, float],
                      other_color: Tuple[float, float, float],
                      neutral_color: Tuple[float, float, float]):
    colors = []
    table = []
    fixtures.sort(key=lambda row: row.date, reverse=True)
    for i, fixture in enumerate(fixtures):
        first_half = fixture.first_half()
        second_half = fixture.second_half()
        full_time = fixture.full_time()

        if team == fixture.away_team:
            first_half = first_half.reverse()
            second_half = second_half.reverse()
            full_time = full_time.reverse()

        colors.append([neutral_color,
                       neutral_color if fixture.home_team != team else team_color,
                       neutral_color if fixture.away_team != team else team_color,
                       decide_cell_color(first_half, team_color, other_color, neutral_color),
                       decide_cell_color(second_half, team_color, other_color, neutral_color),
                       decide_cell_color(full_time, team_color, other_color, neutral_color)])

        if team == fixture.away_team:
            first_half = first_half.reverse()
            second_half = second_half.reverse()
            full_time = full_time.reverse()

        row = [fixture.date.strftime('%Y %b'),
               fixture.home_team.name,
               fixture.away_team.name,
               str(first_half) if first_half else '',
               str(second_half) if second_half else '',
               str(full_time) if full_time else '']
        table.append(row)

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
    the_table.set_fontsize(8)
    ax.axis('off')


def create_league_table(ax,
                        this_season: Season,
                        team_colors: Dict,
                        neutral_color: str):
    league_table = LeagueTable(this_season, [Half.full])
    display_table = []
    team_length = 1
    for i, league_row in enumerate(league_table, start=1):
        display_row = [league_row.TEAM.name,
                       league_row.W + league_row.D + league_row.L,
                       league_row.W, league_row.D, league_row.L, league_row.F, league_row.A]
        wins = league_row.W
        draws = league_row.D
        pts = wins * 3 + draws
        display_row.append(pts)
        display_table.append(display_row)
        team_length = max(team_length, len(league_row.TEAM.name))

    display_table.sort(key=lambda row: row[-1], reverse=True)

    colors = []
    for display_row in display_table:
        if display_row[0] in team_colors:
            colors.append([team_colors[display_row[0]]] * len(display_row))
        else:
            colors.append([neutral_color] * len(display_row))

    df = DataFrame(display_table)
    df.columns = ['Team', 'P', 'W', 'D', 'L', 'F', 'A', 'PTS']

    the_table = ax.table(cellText=df.values,
                         colLabels=df.columns,
                         colLoc='left',
                         colColours=[neutral_color] * len(df.columns),
                         colWidths=[0.3, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1],
                         cellColours=colors,
                         cellLoc='left',
                         loc='upper center')
    the_table.auto_set_font_size(False)
    the_table.set_fontsize(8)
    ax.axis('off')


def main(args: Namespace):
    load_teams(args.database)

    if args.league:
        league = league_register[get_unique_league(args)]
        load_league(args.database, league)

    background_color = '#d3d3d3'
    left_color = '#81d4fa'
    right_color = '#0086c3'
    other_color = '#ef5350'
    neutral_color = '#ffffff'

    nrows = 3
    ncols = 3
    fig = plt.figure(figsize=(15, 10), constrained_layout=True, facecolor=background_color)
    spec = gridspec.GridSpec(nrows=nrows, ncols=ncols, figure=fig, hspace=0.75)

    left_name, right_name = get_multiple_teams(args)
    (row,) = extract_picked_team(args.database, left_name, league)
    left_team = Team.inventory[row[0]]
    (row,) = extract_picked_team(args.database, right_name, league)
    right_team = Team.inventory[row[0]]

    ax = fig.add_subplot(spec[0, 0])
    if not add_image(ax, left_team):
        fig.delaxes(ax)

    ax = fig.add_subplot(spec[0, 2])
    if not add_image(ax, right_team):
        fig.delaxes(ax)

    left_fixtures = get_head_to_head_fixtures(args.database, left_team, right_team)
    right_fixtures = get_head_to_head_fixtures(args.database, right_team, left_team)

    if not left_fixtures and not right_fixtures:
        warning_message("No head-to-head between {} and {}".format(left_team.name, right_team.name))
    else:
        if left_fixtures:
            ax = fig.add_subplot(spec[1, 0])
            create_results_table(ax,
                                 left_fixtures,
                                 left_team,
                                 right_team,
                                 left_color,
                                 right_color,
                                 neutral_color)

        if right_fixtures:
            ax = fig.add_subplot(spec[1, 2])
            create_results_table(ax,
                                 right_fixtures,
                                 right_team,
                                 left_team,
                                 right_color,
                                 left_color,
                                 neutral_color)

    seasons = Season.seasons(league)
    this_season = seasons.pop()

    fixtures = get_finished_matches(args.database, this_season, left_team)
    ax = fig.add_subplot(spec[2, 0])
    create_form_table(ax,
                      fixtures,
                      left_team,
                      left_color,
                      other_color,
                      neutral_color)

    fixtures = get_finished_matches(args.database, this_season, right_team)
    ax = fig.add_subplot(spec[2, 2])
    create_form_table(ax,
                      fixtures,
                      right_team,
                      right_color,
                      other_color,
                      neutral_color)

    ax = fig.add_subplot(spec[1:, 1])
    colors = {left_team.name: left_color, right_team.name: right_color}
    create_league_table(ax, this_season, colors, neutral_color)

    plt.show(block=args.block)


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    set_matplotlib_defaults()
    main(args)

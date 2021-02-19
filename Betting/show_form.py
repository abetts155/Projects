from argparse import ArgumentParser, Namespace
from cli.cli import (add_database_option,
                     add_logging_options,
                     set_logging_options,
                     add_team_option,
                     add_league_option,
                     add_venue_option,
                     add_half_option,
                     add_block_option,
                     get_unique_league,
                     get_unique_team)
from matplotlib import pyplot as plt
from model.fixtures import Fixture, Half, Result, Venue, create_fixture_from_row, win, defeat
from model.leagues import league_register
from model.seasons import Season
from model.tables import LeagueTable
from model.teams import Team
from sql.sql import Database, load_database, extract_picked_team, ColumnNames, Characters, Keywords
from typing import List, Tuple

import pandas as pd


def parse_command_line():
    parser = ArgumentParser(description='Show recent form')
    add_database_option(parser)
    add_logging_options(parser)
    add_team_option(parser, True)
    add_league_option(parser, True)
    add_venue_option(parser)
    add_half_option(parser)
    add_block_option(parser)
    return parser.parse_args()


def get_fixtures(database: str, team: Team, this_season: Season, venue: Venue):
    fixtures = []
    with Database(database) as db:
        if venue == Venue.home:
            team_constraint = "{}={}".format(ColumnNames.Home_ID.name, team.id)
        elif venue == Venue.away:
            team_constraint = "{}={}".format(ColumnNames.Away_ID.name, team.id)
        else:
            team_constraint = "({}={} {} {}={})".format(ColumnNames.Home_ID.name,
                                                        team.id,
                                                        Keywords.OR.name,
                                                        ColumnNames.Away_ID.name,
                                                        team.id)

        finished_constraint = "{}={}".format(ColumnNames.Finished.name, Characters.TRUE.value)
        season_constraint = "{}={}".format(ColumnNames.Season_ID.name, this_season.id)
        constraints = [team_constraint, finished_constraint, season_constraint]
        fixtures_rows = db.fetch_all_rows(Fixture.sql_table(), constraints)

        for row in fixtures_rows:
            fixture = create_fixture_from_row(row)
            fixtures.append(fixture)
    return fixtures


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
                         team: Team,
                         fixtures: List[Fixture],
                         team_color: Tuple[float, float, float],
                         other_color: Tuple[float, float, float],
                         neutral_color: Tuple[float, float, float],
                         unknown_color: Tuple[float, float, float]):
    colors = []
    table = []
    fixtures.sort(key=lambda row: row.date)
    for i, fixture in enumerate(fixtures):
        first_half = fixture.canonicalise_result(team, fixture.first_half())
        second_half = fixture.canonicalise_result(team, fixture.second_half())
        full_time = fixture.canonicalise_result(team, fixture.full_time())
        colors.append([neutral_color,
                       neutral_color,
                       neutral_color,
                       decide_cell_color(first_half, team_color, other_color, neutral_color, unknown_color),
                       decide_cell_color(second_half, team_color, other_color, neutral_color, unknown_color),
                       decide_cell_color(full_time, team_color, other_color, neutral_color, unknown_color)])

        if fixture.home_team == team:
            opponent_team = fixture.away_team
            venue = Venue.home
        else:
            opponent_team = fixture.home_team
            venue = Venue.away

        row = ['{}'.format(fixture.date.strftime('%Y-%m-%d')),
               opponent_team.name,
               venue.name[0].upper(),
               str(first_half),
               str(second_half),
               str(full_time)]
        table.append(row)

    df = pd.DataFrame(table)
    df.columns = ['Date', 'Opponent', 'Venue', '1st', '2nd', 'FT']

    ax.table(cellText=df.values,
             colLabels=df.columns,
             colLoc='left',
             colWidths=[0.2, 0.4, 0.1, 0.1, 0.1, 0.1],
             cellColours=colors,
             cellLoc='left',
             loc='upper center')
    ax.set_title('Form')
    ax.axis('off')


def create_league_table(ax,
                        this_season: Season,
                        team: Team,
                        team_color: Tuple[float, float, float],
                        other_color: Tuple[float, float, float],
                        venue: Venue,
                        half: Half):
    league_table = LeagueTable(this_season, half)
    display_table = []
    colors = []
    team_length = 1
    for i, league_row in enumerate(league_table, start=1):
        display_row = [league_row.TEAM.name]
        if venue == Venue.home:
            display_row.extend([league_row.HW + league_row.HD + league_row.HL,
                                league_row.HW, league_row.HD, league_row.HL, league_row.HF, league_row.HA])
            wins = league_row.HW
            draws = league_row.HD
        elif venue == Venue.away:
            display_row.extend([league_row.AW + league_row.AD + league_row.AL,
                                league_row.AW, league_row.AD, league_row.AL, league_row.AF, league_row.AA])
            wins = league_row.AW
            draws = league_row.AD
        else:
            display_row.extend([league_row.W + league_row.D + league_row.L,
                                league_row.W, league_row.D, league_row.L, league_row.F, league_row.A])
            wins = league_row.W
            draws = league_row.D
        pts = wins * 3 + draws
        display_row.append(pts)
        display_table.append(display_row)
        team_length = max(team_length, len(league_row.TEAM.name))

    display_table.sort(key=lambda row: row[-1], reverse=True)

    for display_row in display_table:
        if display_row[0] == team.name:
            colors.append([team_color] * len(display_row))
        else:
            colors.append([other_color] * len(display_row))

    df = pd.DataFrame(display_table)
    df.columns = ['Team', 'Played', 'W', 'D', 'L', 'F', 'A', 'PTS']

    ax.table(cellText=df.values,
             colLabels=df.columns,
             colLoc='left',
             colWidths=[0.3, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1],
             cellColours=colors,
             cellLoc='left',
             loc='upper center')
    title = 'League table'
    if half:
        title = '{} ({} half)'.format(title, half.name)
    ax.set_title(title)
    ax.axis('off')


def main(args: Namespace):
    league = league_register[get_unique_league(args)]
    load_database(args.database, league)
    team_name = get_unique_team(args)
    team = extract_picked_team(args.database, team_name, league)
    seasons = Season.seasons(league)
    this_season = seasons.pop()
    fixtures = get_fixtures(args.database, team, this_season, args.venue)

    nrows = 2
    ncols = 1
    fig, axs = plt.subplots(nrows=nrows, ncols=ncols, figsize=(12, 10), squeeze=False, constrained_layout=True)

    team_color = (54 / 255, 104 / 255, 141 / 255)
    other_team_color = (240 / 255, 88 / 255, 55 / 255)
    neutral_color = (1, 1, 1)
    unknown_color = (0, 0, 0)

    create_results_table(axs[0, 0], team, fixtures, team_color, other_team_color, neutral_color, unknown_color)
    create_league_table(axs[1, 0], this_season, team, team_color, neutral_color, args.venue, args.half)

    title = '{} {}: {}'.format(league.country, league.name, team.name)
    if args.venue == Venue.any:
        title = '{} ({} or {})'.format(title, Venue.home.name, Venue.away.name)
    else:
        title = '{} ({} only)'.format(title, args.venue.name)

    fig.suptitle(title, fontweight='bold', fontsize=14)
    plt.show(block=args.block)


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)

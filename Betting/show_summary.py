from argparse import ArgumentParser, Namespace
from cli.cli import add_database_option, add_league_option, add_logging_options, set_logging_options, get_unique_league
from collections import OrderedDict
from lib import messages
from math import ceil
from matplotlib import pyplot as plt
from model.leagues import league_register
from model.seasons import Season
from sql.sql import load_database


def parse_command_line():
    parser = ArgumentParser(description='Show season summary data bar charts')
    add_database_option(parser)
    add_league_option(parser)
    add_logging_options(parser)
    return parser.parse_args()


class Summary:
    __slots__ = ['home_wins', 'away_wins', 'draws', 'home_goals', 'away_goals']

    def __init__(self):
        self.home_wins = 0
        self.away_wins = 0
        self.draws = 0
        self.home_goals = 0
        self.away_goals = 0


def compute_line_graph_data(season: Season):
    stats = Summary()
    for fixture in season.fixtures():
        result = fixture.full_time()
        if result is not None:
            stats.home_goals += result.left
            stats.away_goals += result.right
            if result.left > result.right:
                stats.home_wins += 1
            elif result.left < result.right:
                stats.away_wins += 1
            else:
                stats.draws += 1
    return stats


def main(arguments: Namespace):
    league = league_register[get_unique_league(arguments)]
    load_database(arguments.database, league)

    seasons = Season.seasons()
    if not seasons:
        messages.error_message("No season data found")

    season_to_stats = OrderedDict()
    ylim = 0
    for season in seasons:
        season_to_stats[season] = compute_line_graph_data(season)
        ylim = max(ylim, season_to_stats[season].home_goals, season_to_stats[season].away_goals)
    ylim += 25

    if len(seasons) <= 3:
        nrows = 1
        ncols = len(seasons)
    else:
        nrows = 2
        ncols = ceil(len(seasons) / nrows)

    fig, axes = plt.subplots(nrows=nrows, ncols=ncols, figsize=(20, 10))
    fig.suptitle('{} {}'.format(league.country, league.name), fontweight='bold', fontsize=14)

    row_id = 0
    col_id = 0
    for season in seasons:
        if nrows == 1:
            ax = axes[col_id]
        else:
            ax = axes[row_id, col_id]
        x_values = ['HW', 'D', 'AW', 'HG', 'AG']
        stats = season_to_stats[season]
        y_values = [stats.home_wins, stats.draws, stats.away_wins, stats.home_goals, stats.away_goals]
        ax.bar(x_values, y_values)
        ax.set_ylim(0, ylim)
        ax.set_yticks([])
        ax.set_title('{}: {} games'.format(season.year, stats.home_wins + stats.draws + stats.away_wins))

        for k, v in zip(x_values, y_values):
            ax.text(k, v, str(v), ha='center', fontsize=8)

        if col_id == ncols - 1:
            row_id += 1
            col_id = 0
        else:
            col_id += 1

    if 0 < col_id:
        for i in range(col_id, ncols):
            fig.delaxes(axes[row_id][col_id])
    plt.tight_layout()
    plt.show()


if __name__ == '__main__':
    arguments = parse_command_line()
    set_logging_options(arguments)
    main(arguments)

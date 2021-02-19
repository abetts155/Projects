from argparse import ArgumentParser, Namespace
from cli.cli import (add_database_option,
                     add_league_option,
                     add_logging_options,
                     set_logging_options,
                     add_block_option,
                     get_unique_league)
from collections import OrderedDict
from lib import messages
from lib.helpful import DisplayGrid
from matplotlib import pyplot as plt
from model.leagues import league_register
from model.seasons import Season
from sql.sql import load_database


def parse_command_line():
    parser = ArgumentParser(description='Show season summary data bar charts')
    add_database_option(parser)
    add_league_option(parser)
    add_logging_options(parser)
    add_block_option(parser)
    return parser.parse_args()


class Summary:
    __slots__ = ['home_wins', 'away_wins', 'draws', 'home_goals', 'away_goals']

    def __init__(self):
        self.home_wins = 0
        self.away_wins = 0
        self.draws = 0
        self.home_goals = 0
        self.away_goals = 0


def compute_summary(season: Season):
    summary = Summary()
    for fixture in season.fixtures():
        result = fixture.full_time()
        if result is not None:
            summary.home_goals += result.left
            summary.away_goals += result.right
            if result.left > result.right:
                summary.home_wins += 1
            elif result.left < result.right:
                summary.away_wins += 1
            else:
                summary.draws += 1
    return summary


def show(title: str, season_to_summary, ylim: int, block: bool):
    display = DisplayGrid(len(season_to_summary), 3)
    fig, axs = plt.subplots(nrows=display.nrows,
                            ncols=display.ncols,
                            figsize=(20, 10),
                            squeeze=False,
                            constrained_layout=True)

    for i, (season, summary) in enumerate(season_to_summary.items()):
        x_values_results = ['HW', 'D', 'AW']
        x_values_goals = ['HG', 'AG']
        x_values = x_values_results + x_values_goals
        y_values_results = [summary.home_wins, summary.draws, summary.away_wins]
        y_values_goals = [summary.home_goals, summary.away_goals]
        y_values = y_values_results + y_values_goals

        cell_x, cell_y = display.index(i)
        ax = axs[cell_x, cell_y]
        bar = ax.bar(x_values, y_values)
        ax.set_ylim(0, ylim + 20)
        ax.set_yticks([])
        total_games = summary.home_wins + summary.draws + summary.away_wins
        ax.set_title('{}: {} games'.format(season.year, total_games))

        if total_games:
            results_copy = y_values_results[:]
            results_copy.sort()
            colors = ['white', 'silver', 'gold']
            for x, y_value in enumerate(results_copy):
                index = y_values_results.index(y_value)
                bar[index].set_color(colors[x])
                bar[index].set_edgecolor('black')

            goals_copy = y_values_goals[:]
            goals_copy.sort()
            colors = ['white', 'dodgerblue']
            for x, y_value in enumerate(goals_copy):
                index = y_values_goals.index(y_value) + len(y_values_results)
                bar[index].set_color(colors[x])
                bar[index].set_edgecolor('black')

            for k, v in zip(x_values, y_values):
                if k in x_values_results:
                    percentage = round((v / total_games) * 100)
                    text = '{} ({}%)'.format(v, percentage)
                else:
                    text = str(v)
                ax.text(k, v + 5, text, ha='center', fontsize=8)

    for i in range(len(season_to_summary), display.nrows * display.ncols):
        cell_x, cell_y = display.index(i)
        ax = axs[cell_x][cell_y]
        fig.delaxes(ax)

    fig.suptitle(title, fontweight='bold', fontsize=14)
    plt.show(block=block)


def main(args: Namespace):
    league = league_register[get_unique_league(args)]
    load_database(args.database, league)

    seasons = Season.seasons(league)
    if not seasons:
        messages.error_message("No season data found")

    season_to_summary = OrderedDict()
    ylim = 0
    for season in seasons:
        season_to_summary[season] = compute_summary(season)
        ylim = max(ylim, season_to_summary[season].home_goals, season_to_summary[season].away_goals)
    ylim += 25

    title = '{} {}'.format(league.country, league.name)
    show(title, season_to_summary, ylim, args.block)


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)

from collections import OrderedDict
from matplotlib import dates, pyplot as plt
from model.seasons import Fixture, Season
from typing import List


class Summary:
    __slots__ = ['fixture', 'home_wins', 'away_wins', 'draws', 'home_goals', 'away_goals']

    def __init__(self, fixture: Fixture = None):
        self.fixture = fixture
        self.home_wins = 0
        self.away_wins = 0
        self.draws = 0
        self.home_goals = 0
        self.away_goals = 0

    def __str__(self):
        return 'HW={} AW={} DR={} HG={} AG={}'.format(self.home_wins, self.away_wins, self.draws, self.home_goals,
                                                      self.away_goals)


def compute_line_graph_data(season: Season):
    date_to_stats = OrderedDict()
    previous_stats = Summary()
    for fixture in season.fixtures():
        key = (fixture.date.day, fixture.date.month, fixture.date.year)
        if key not in date_to_stats:
            stats = Summary(fixture)
            stats.home_wins = previous_stats.home_wins
            stats.away_wins = previous_stats.away_wins
            stats.draws = previous_stats.draws
            stats.home_goals = previous_stats.home_goals
            stats.away_goals = previous_stats.away_goals
            date_to_stats[key] = stats

        stats = date_to_stats[key]
        result = fixture.full_time()
        stats.home_goals += result.left
        stats.away_goals += result.right
        if result.left > result.right:
            stats.home_wins += 1
        elif result.left < result.right:
            stats.away_wins += 1
        else:
            stats.draws += 1
        previous_stats = stats
    return date_to_stats


def display_line_graphs(seasons: List[Season]):
    for season in seasons:
        summary = compute_line_graph_data(season)
        fig, ax = plt.subplots(1)
        fig.autofmt_xdate()
        x_values = [record.fixture.date for record in summary.values()]
        ax.plot(x_values, [record.home_wins for record in summary.values()], label='Home wins', color='red')
        ax.plot(x_values, [record.away_wins for record in summary.values()], label='Away wins', color='blue')
        ax.plot(x_values, [record.draws for record in summary.values()], label='Draws', color='black')
        ax.xaxis.set_major_formatter(dates.DateFormatter('%B'))
        ax.grid()
        ax.legend()
        plt.title(season.year)
        plt.show()

from argparse import ArgumentParser, Namespace
from collections import Counter, OrderedDict
from lib import messages
from matplotlib import dates, pyplot as plt
from matplotlib.ticker import MaxNLocator
from model.competitions import league_register, Fixture, League, Season, Team, Venue
from sql.sql import load_database, extract_picked_team
from typing import List


def parse_command_line():
    parser = ArgumentParser(description='Betting predictions')

    parser.add_argument('--database',
                        help='read from this database',
                        metavar='<DATABASE>',
                        required=True)

    event_choices = [Fixture.is_draw.__name__, Fixture.is_loss.__name__, Fixture.is_win.__name__]
    parser.add_argument('--event',
                        choices=event_choices,
                        type=str.lower,
                        help='choose event to analyse',
                        required=True)

    parser.add_argument('-n',
                        '--negate-event',
                        action='store_true',
                        help='negate the chosen event',
                        default=False)

    parser.add_argument('--venue',
                        choices=list(Venue),
                        type=Venue.from_string,
                        help='filter fixtures according to the venue',
                        default=Venue.any)

    parser.add_argument('--league',
                        help='choose the league to analyse',
                        metavar='<NAME>',
                        choices=league_register.keys(),
                        type=str.upper,
                        required=True)

    parser.add_argument('--team',
                        help='choose the team to analyse',
                        metavar='<NAME>',
                        type=str.capitalize)

    parser.add_argument('--history',
                        help='only consider this number of completed seasons',
                        metavar='<INT>',
                        type=int)

    parser.add_argument('-d',
                        '--debug',
                        action='store_true',
                        help='print debug messages',
                        default=False)

    parser.add_argument('-v',
                        '--verbose',
                        action='store_true',
                        help='print verbose messages',
                        default=False)

    return parser.parse_args()


class Statistics(object):
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
    previous_stats = Statistics()
    for fixture in season.fixtures():
        key = (fixture.date.day, fixture.date.month, fixture.date.year)
        if key not in date_to_stats:
            stats = Statistics(fixture)
            stats.home_wins = previous_stats.home_wins
            stats.away_wins = previous_stats.away_wins
            stats.draws = previous_stats.draws
            stats.home_goals = previous_stats.home_goals
            stats.away_goals = previous_stats.away_goals
            date_to_stats[key] = stats

        stats = date_to_stats[key]
        stats.home_goals += fixture.full_time_home
        stats.away_goals += fixture.full_time_away
        if fixture.full_time_home > fixture.full_time_away:
            stats.home_wins += 1
        elif fixture.full_time_home < fixture.full_time_away:
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


class BarChart:
    __slots__ = ['counter', 'team', 'years', 'team', 'highlight']

    def __init__(self, counter: Counter, years: List[int], team: Team = None):
        self.counter = counter
        self.years = years
        self.team = team
        self.highlight = None


def count_events(season: Season, team: Team, venue: Venue, event: str, negate: bool, chart: BarChart):
    fixtures = []
    for fixture in season.fixtures():
        if fixture.full_time_home is not None and fixture.full_time_away is not None:
            if venue == Venue.any:
                if fixture.home_team == team or fixture.away_team == team:
                    fixtures.append(fixture)
            elif venue == Venue.away:
                if fixture.away_team == team:
                    fixtures.append(fixture)
            elif venue == Venue.home:
                if fixture.home_team == team:
                    fixtures.append(fixture)

    event_function = getattr(Fixture, event)
    sequence = []
    for fixture in fixtures:
        answer = event_function(fixture, team)
        if negate:
            answer = not answer
        if answer:
            sequence.append(fixture)
        else:
            chart.highlight = len(sequence)
            chart.counter[len(sequence)] += 1
            sequence = []
    if sequence:
        chart.highlight = len(sequence)
        chart.counter[len(sequence)] += 1
    else:
        chart.highlight = None


def display_bar_graphs(event: str, negate_event: bool, league: League, charts: List[BarChart]):
    x_limit = 0
    for chart in charts:
        if chart.counter:
            max_key = max(chart.counter)
            x_limit = max(x_limit, max_key)

    if len(charts) == 4:
        fig, axes = plt.subplots(2, 2, figsize=(20, 10))
    else:
        fig, axes = plt.subplots(2, figsize=(20, 10))

    if event == Fixture.is_win.__name__:
        prologue = 'Wins'
    elif event == Fixture.is_loss.__name__:
        prologue = 'Losses'
    elif event == Fixture.is_draw.__name__:
        prologue = 'Draws'
    else:
        prologue = ''

    if negate_event:
        prologue = 'No {}'.format(prologue.lower())

    fig.suptitle('{} in {} {}'.format(prologue, league.country, league.name))

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

        ax.set_title('{}:{}'.format(summary_string, year_string))
        bar = ax.bar(x_values, y_values)
        if chart.team is not None and len(chart.years) == 1 and chart.highlight is not None:
            bar[chart.highlight].set_color('red')
    plt.subplots_adjust(hspace=0.5)
    plt.show()


def main(arguments: Namespace):
    messages.verbose = arguments.verbose
    messages.debug = arguments.debug
    league = league_register[arguments.league]
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

    charts = []
    aggregated_history = BarChart(Counter(), [seasons[0].year, seasons[-1].year])
    charts.append(aggregated_history)
    for season in seasons:
        for team in season.teams():
            count_events(season,
                         team,
                         arguments.venue,
                         arguments.event,
                         arguments.negate_event,
                         aggregated_history)

    if selected_team is not None:
        team_history = BarChart(Counter(), [seasons[0].year, seasons[-1].year], selected_team)
        charts.append(team_history)
        for season in seasons:
            if not season.current:
                count_events(season,
                             team_history.team,
                             arguments.venue,
                             arguments.event,
                             arguments.negate_event,
                             team_history)

    if this_season is not None:
        aggregated_now = BarChart(Counter(), [this_season.year])
        charts.append(aggregated_now)
        for team in this_season.teams():
            count_events(this_season,
                         team,
                         arguments.venue,
                         arguments.event,
                         arguments.negate_event,
                         aggregated_now)

        if selected_team is not None:
            team_now = BarChart(Counter(), [this_season.year], selected_team)
            charts.append(team_now)
            count_events(this_season,
                         team_now.team,
                         arguments.venue,
                         arguments.event,
                         arguments.negate_event,
                         team_now)

    display_bar_graphs(arguments.event, arguments.negate_event, league, charts)


if __name__ == '__main__':
    main(parse_command_line())

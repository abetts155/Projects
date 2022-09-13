from collections import Counter
from datetime import datetime
from lib.helpful import split_into_contiguous_groups, to_string
from lib.messages import warning_message
from model.fixtures import Half, Venue
from model.seasons import Season
from model.teams import Team
from typing import Callable, List


class DataUnit:
    __slots__ = ['counter', 'team', 'seasons', 'team', 'positions', 'highlight', 'last']

    def __init__(self,
                 counter: Counter,
                 seasons: List[Season],
                 team: Team = None,
                 positions: List[int] = None,
                 highlight: bool = False):
        self.counter = counter
        self.seasons = seasons
        self.team = team
        self.positions = positions
        self.highlight = highlight
        self.last = None

    def title(self):
        years = split_into_contiguous_groups([season.year for season in self.seasons])
        title = '{}:{} ({} data points)'.format(self.team.name if self.team else 'Aggregated',
                                                to_string(years),
                                                sum(self.counter.values()))
        return title

    def y_label(self):
        if self.positions:
            positions = split_into_contiguous_groups([position + 1 for position in self.positions])
            return to_string(positions)

    def values(self, x_limit: int):
        x_values = []
        y_values = []
        for key in range(0, x_limit + 1):
            x_values.append(key)
            if key in self.counter:
                y_values.append(self.counter[key])
            else:
                y_values.append(0)

        return x_values, y_values


def count_events(season: Season,
                 team: Team,
                 venue: Venue,
                 halves: List[Half],
                 func: Callable,
                 negate: bool,
                 data: DataUnit):
    right_now = datetime.now()
    fixtures = []
    for fixture in season.fixtures():
        if datetime.date(fixture.date) <= right_now.date():
            if venue == Venue.any:
                if fixture.home_team == team or fixture.away_team == team:
                    fixtures.append(fixture)
            elif venue == Venue.away:
                if fixture.away_team == team:
                    fixtures.append(fixture)
            elif venue == Venue.home:
                if fixture.home_team == team:
                    fixtures.append(fixture)

    sequence = []
    for fixture in fixtures:
        results = []

        if Half.first in halves:
            if fixture.first_half() is not None:
                results.append(fixture.first_half())
            else:
                warning_message('No 1st half result for fixture {}'.format(fixture))

        if Half.second in halves:
            if fixture.second_half() is not None:
                results.append(fixture.second_half())
            else:
                warning_message('No 2nd half result for fixture {}'.format(fixture))

        if Half.full in halves:
            if fixture.full_time() is not None:
                results.append(fixture.full_time())
            else:
                warning_message('No full-time result for fixture {}'.format(fixture))

        if results:
            for result in results:
                if fixture.home_team != team:
                    result = result.reverse()

                if negate:
                    outcome = not func(result)
                else:
                    outcome = func(result)

                if outcome:
                    sequence.append(fixture)
                else:
                    data.last = len(sequence)
                    data.counter[len(sequence)] += 1
                    sequence = []

    if sequence:
        data.last = len(sequence)
        data.counter[len(sequence)] += 1
    else:
        data.last = None

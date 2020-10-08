from collections import Counter
from model.fixtures import Venue
from model.seasons import Season
from model.teams import Team
from typing import Callable, List


class BarChart:
    __slots__ = ['counter', 'team', 'years', 'team', 'last']

    def __init__(self, counter: Counter, years: List[int], team: Team = None):
        self.counter = counter
        self.years = years
        self.team = team
        self.last = None


def count_events(season: Season, team: Team, venue: Venue, event_function: Callable, chart: BarChart):
    fixtures = []
    for fixture in season.fixtures():
        if fixture.first_half() is not None and fixture.second_half() is not None:
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
        if fixture.home_team == team:
            result = fixture.full_time()
        else:
            result = fixture.full_time().reverse()

        if event_function(result):
            sequence.append(fixture)
        else:
            chart.last = len(sequence)
            chart.counter[len(sequence)] += 1
            sequence = []

    if sequence:
        chart.last = len(sequence)
        chart.counter[len(sequence)] += 1
    else:
        chart.last = None

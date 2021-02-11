from collections import Counter
from lib.messages import warning_message
from model.fixtures import Half, Venue
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


def count_events(season: Season,
                 team: Team,
                 venue: Venue,
                 half: Half,
                 event_function: Callable,
                 negate: bool,
                 chart: BarChart):
    fixtures = []
    for fixture in season.fixtures():
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
        result = None
        if half is not None:
            if half == Half.first:
                if fixture.first_half() is not None:
                    result = fixture.first_half()
                else:
                    warning_message('No 1st half result for fixture {}'.format(fixture))
            else:
                if fixture.second_half() is not None:
                    result = fixture.second_half()
                else:
                    warning_message('No 2nd half result for fixture {}'.format(fixture))
        else:
            result = fixture.full_time()

        if result:
            if fixture.home_team != team:
                result = result.reverse()

            if negate:
                outcome = not event_function(result)
            else:
                outcome = event_function(result)

            if outcome:
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

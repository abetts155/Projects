import collections

from lib.helpful import split_into_contiguous_groups, to_string
from lib.messages import warning_message
from model.fixtures import ContextualEvent, Fixture, Period, Venue
from model.seasons import Season
from model.teams import Team


class DataUnit:
    __slots__ = ['counter', 'team', 'seasons', 'team', 'positions', 'highlight', 'last']

    def __init__(self,
                 counter: collections.Counter,
                 seasons: list[Season],
                 team: Team = None,
                 positions: list[int] = None,
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


def count_events(team: Team,
                 fixtures: list[Fixture],
                 bets: list[ContextualEvent],
                 data: list[DataUnit]):
    bet_sequences = {bet: [] for bet in bets}
    for fixture in fixtures:
        for bet, datum in zip(bets, data):
            results = []

            if fixture.finished and (bet.venue == Venue.ANYWHERE or
                                     (bet.venue == Venue.HOME and fixture.home_team == team) or
                                     (bet.venue == Venue.AWAY and fixture.away_team == team)):

                if Period.FIRST in bet.periods:
                    if fixture.result(Period.FIRST) is not None:
                        results.append(fixture.result(Period.FIRST))
                    else:
                        warning_message('No 1st half result for fixture {}'.format(fixture))

                if Period.SECOND in bet.periods:
                    if fixture.result(Period.SECOND) is not None:
                        results.append(fixture.result(Period.SECOND))
                    else:
                        warning_message('No 2nd half result for fixture {}'.format(fixture))

                if Period.FULL in bet.periods:
                    if fixture.result(Period.FULL) is not None:
                        results.append(fixture.result(Period.FULL))
                    else:
                        warning_message('No full-time result for fixture {}'.format(fixture))

            if results:
                for result in results:
                    if fixture.home_team != team:
                        result = result.reverse()

                    if bet.negate:
                        outcome = not bet.func(result)
                    else:
                        outcome = bet.func(result)

                    if outcome:
                        bet_sequences[bet].append(fixture)
                    else:
                        datum.last = len(bet_sequences[bet])
                        datum.counter[len(bet_sequences[bet])] += 1
                        bet_sequences[bet].clear()

    for bet, datum in zip(bets, data):
        datum.last = len(bet_sequences[bet])
        datum.counter[len(bet_sequences[bet])] += 1

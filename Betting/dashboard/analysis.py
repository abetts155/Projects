import collections
import dataclasses
import functools
import operator
import typing

import model.fixtures
from dashboard.fixtures import Scoreline


def prettify(func: typing.Callable):
    return ' '.join([l.capitalize() for l in func.__name__.split('_')])


def create_total_predicate(op, bound: int) -> typing.Callable[[Scoreline], bool]:
    def total(op: typing.Callable, bound: int, scoreline: Scoreline):
        return op(scoreline.left + scoreline.right, bound)
    return functools.partial(total, getattr(operator, op), bound)


def create_scored_predicate(op, bound: int) -> typing.Callable[[Scoreline], bool]:
    def scored(op: typing.Callable, bound: int, scoreline: Scoreline):
        return op(scoreline.left, bound)
    return functools.partial(scored, getattr(operator, op), bound)


def create_conceded_predicate(op, bound: int) -> typing.Callable[[Scoreline], bool]:
    def conceded(op: typing.Callable, bound: int, scoreline: Scoreline):
        return op(scoreline.right, bound)
    return functools.partial(conceded, getattr(operator, op), bound)


def create_both_scored_predicate(op, bound: int) -> typing.Callable[[Scoreline], bool]:
    def both_scored(op: typing.Callable, bound: int, scoreline: Scoreline):
        return op(scoreline.left, bound) and op(scoreline.right, bound)
    return functools.partial(both_scored, getattr(operator, op), bound)


def won(scoreline: Scoreline) -> bool:
    return scoreline.left > scoreline.right


def drawn(scoreline: Scoreline) -> bool:
    return scoreline.left == scoreline.right


def lost(scoreline: Scoreline) -> bool:
    return scoreline.left < scoreline.right


@dataclasses.dataclass(slots=True)
class Event:
    func: typing.Callable[[Scoreline], bool]
    negate: bool = False

    def __hash__(self):
        return hash((self.func, self.negate))

    def __str__(self):
        if isinstance(self.func, functools.partial):
            func = self.func.func
            op, bound = self.func.args

            goal_relations = {
                'eq': '=',
                'ne': '≠',
                'lt': '<',
                'gt': '>',
                'le': '≤',
                'ge': '≥'
            }
            relation = goal_relations[op.__name__]

            return f"{prettify(func)} {relation} {bound}"
        else:
            return f"{'Not ' if self.negate else ''}{prettify(self.func)}"


def analyse_event(event: Event, scorelines: list[Scoreline]):
    counter: collections.Counter = collections.Counter()
    count = 0
    for s in scorelines:
        outcome = event.func(s)
        if event.negate:
            outcome = not outcome

        if outcome:
            count += 1
        else:
            counter[count] += 1
            count = 0

    counter[count] += 1
    return counter


def current_streak(event: Event, scorelines: list[Scoreline]):
    count = 0
    for s in scorelines:
        outcome = event.func(s)
        if event.negate:
            outcome = not outcome

        if outcome:
            count += 1
        else:
            count = 0

    return count


@dataclasses.dataclass(slots=True)
class Trend:
    event: str
    current: int
    max_team: int
    max_history: int


def analyse(
        period: model.fixtures.Period,
        teams_scorelines: dict[int, list[Scoreline]],
        index: int,
        chosen_team_id: int
) -> list[Trend]:
    if period == model.fixtures.Period.FULL:
        events = [
            Event(create_total_predicate("eq", 0)),
            Event(create_total_predicate("le", 1)),
            Event(create_total_predicate("le", 2)),
            Event(create_total_predicate("ge", 3)),
            Event(create_scored_predicate("eq", 0)),
            Event(create_scored_predicate("le", 1)),
            Event(create_scored_predicate("gt", 2)),
            Event(create_conceded_predicate("eq", 0)),
            Event(create_both_scored_predicate("gt", 0)),
            Event(create_both_scored_predicate("gt", 0), True),
            Event(won),
            Event(drawn),
            Event(lost),
            Event(won, True),
            Event(drawn, True),
            Event(lost, True)
        ]
    elif period == model.fixtures.Period.FIRST:
        events = [
            Event(create_total_predicate("eq", 0)),
            Event(create_total_predicate("ne", 0)),
            Event(create_total_predicate("le", 1)),
            Event(create_total_predicate("ge", 2)),
            Event(create_scored_predicate("eq", 0)),
            Event(create_conceded_predicate("eq", 0)),
            Event(drawn),
            Event(drawn, True)
        ]
    elif period == model.fixtures.Period.SECOND:
        events = [
            Event(create_total_predicate("eq", 0)),
            Event(create_total_predicate("ne", 0)),
            Event(create_total_predicate("le", 1)),
            Event(create_total_predicate("ge", 2)),
            Event(create_scored_predicate("eq", 0)),
            Event(create_conceded_predicate("eq", 0)),
            Event(drawn),
            Event(drawn, True)
        ]

    counters = {e: [] for e in events}
    for team_id, scorelines in teams_scorelines.items():
        for e in events:
            counter = analyse_event(e, scorelines)
            counters[e].append(counter)

    max_counters = {e: None for e in events}
    for e in events:
        for c in counters[e]:
            if max_counters[e] is None:
                max_counters[e] = c
            else:
                if max(c) > max(max_counters[e]):
                    max_counters[e] = c

    historical_scorelines = teams_scorelines[chosen_team_id]
    this_season_scorelines = teams_scorelines[chosen_team_id][index:]
    trends = []
    for e in events:
        historical_counter = analyse_event(e, historical_scorelines)
        current = current_streak(e, this_season_scorelines)
        t = Trend(str(e), current, max(historical_counter), max(max_counters[e]))
        trends.append(t)

    return trends

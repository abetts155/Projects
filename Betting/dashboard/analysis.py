import collections
import dataclasses
import functools
import operator
import pandas as pd
import typing

import dashboard.data
from dashboard.fixtures import Scoreline


def prettify(func: typing.Callable):
    return ' '.join(func.__name__.split('_'))


def Drawn(scoreline: Scoreline):
    return scoreline.left == scoreline.right


def Won(scoreline: Scoreline):
    return scoreline.left > scoreline.right


def Lost(scoreline: Scoreline):
    return scoreline.left < scoreline.right


result_predicates = [Won, Drawn, Lost]


def Both_Teams_Scored(scoreline: Scoreline):
    return scoreline.left > 0 and scoreline.right > 0


def Scored(scoreline: Scoreline):
    return scoreline.left > 0


def Conceded(scoreline: Scoreline):
    return scoreline.right > 0


goals_predicates = [Both_Teams_Scored, Scored, Conceded]


goal_relations = {
    'eq': '=',
    'ne': '≠',
    'lt': '<',
    'gt': '>',
    'le': '≤',
    'ge': '≥'
}


def total_goals(op: typing.Callable, bound: int, scoreline: Scoreline):
    return op(scoreline.left + scoreline.right, bound)


def create_total_goals_predicate(op, bound: int) -> typing.Callable[[Scoreline], bool]:
    return functools.partial(total_goals, getattr(operator, op), bound)


@dataclasses.dataclass(slots=True)
class Event:
    func: typing.Callable[[Scoreline], bool]
    negate: bool
    reverse: bool = False
    counter: collections.Counter = dataclasses.field(default_factory=collections.Counter)
    current_team_value: int = 0

    def analyse(self, scorelines: list[Scoreline], team_name: str = ''):
        truthy = []
        for scoreline in scorelines:
            if self.func(scoreline):
                truthy.append(False if self.negate else True)
            else:
                truthy.append(True if self.negate else False)

        count = 0
        for value in truthy:
            if value:
                count += 1
            else:
                self.counter[count] += 1
                count = 0

        if team_name:
            self.current_team_value = count
        else:
            self.counter[count] += 1

    def __str__(self):
        if self.func in [Won, Drawn, Lost, Both_Teams_Scored, Scored, Conceded]:
            if self.negate:
                return f'Not {prettify(self.func)}'
            else:
                return prettify(self.func)
        else:
            op, bound = self.func.args
            return f'Total goals {goal_relations[op.__name__]} {bound}'


def analyse(
        teams_df: pd.DataFrame,
        teams_scorelines: dict[int, list[Scoreline]],
        index: int,
        team_name: str,
        events: list[Event]
):
    for scorelines in teams_scorelines.values():
        for event in events:
            event.analyse(scorelines)

    team_id = dashboard.data.get_team_id(teams_df, team_name)
    scorelines = teams_scorelines[team_id][index:]
    for event in events:
        event.analyse(scorelines, team_name)

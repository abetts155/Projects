from enum import auto, Enum
from model.events import is_corner, is_pass, is_shot, Event, Period
from model.matches import Match, Side
from typing import List


class Metric(Enum):
    CORNERS = auto()
    PASSES = auto()
    POSSESSION = auto()
    SHOTS = auto()


class MatchMetrics:
    def __init__(self):
        self._data = {(side, metric): 0 for side in Side for metric in Metric}

    def increment(self, side: Side, metric: Metric):
        self._data[(side, metric)] += 1

    def query(self, side: Side, metric: Metric) -> int:
        if metric == Metric.POSSESSION:
            away_passes = self._data[(Side.AWAY, Metric.PASSES)]
            home_passes = self._data[(Side.HOME, Metric.PASSES)]
            total_passes = away_passes + home_passes
            assert total_passes > 0
            if side == Side.AWAY:
                return (away_passes / total_passes) * 100
            else:
                return (home_passes / total_passes) * 100
        else:
            return self._data[(side, metric)]

    def __add__(self, other: "MatchMetrics"):
        if type(other) is MatchMetrics:
            combination = MatchMetrics()
            for side in Side:
                for metric in Metric:
                    key = (side, metric)
                    combination._data[key] = self._data[key] + other._data[key]
            return combination
        else:
            return NotImplemented

    def __radd__(self, other: "MatchMetrics"):
        return self.__add__(other)


def compute_metrics(events: List[Event]):
    match: Match = events[0].match
    metrics = {period: MatchMetrics() for period in Period}
    for event in events:
        if event.team == match.home_team:
            side = Side.HOME
        else:
            side = Side.AWAY
        if is_shot(event):
            metrics[event.period].increment(side, Metric.SHOTS)
        if is_pass(event):
            metrics[event.period].increment(side, Metric.PASSES)
        if is_corner(event):
            metrics[event.period].increment(side, Metric.CORNERS)
    return metrics

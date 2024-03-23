from dataclasses import dataclass, field
from datetime import datetime
from enum import StrEnum


class Period(StrEnum):
    FULL = 'Full Time'
    FIRST = 'First Half'
    SECOND = 'Second Half'


class Venue(StrEnum):
    SOMEWHERE = 'Home or Away'
    HOME = 'Home'
    AWAY = 'Away'


@dataclass(slots=True)
class Scoreline:
    home_goals: int = None
    away_goals: int = None

    def __str__(self):
        if self.home_goals is not None and self.away_goals is not None:
            return f'{self.home_goals}-{self.away_goals}'
        else:
            return '?-?'


UNKNOWN_SCORELINE = Scoreline()


def create_scoreline_from_string(scoreline_string: str) -> Scoreline:
    scoreline = UNKNOWN_SCORELINE
    if scoreline_string:
        home_goals, away_goals = scoreline_string.split('-')
        if home_goals.isdigit() and away_goals.isdigit():
            home_goals, away_goals = int(home_goals), int(away_goals)
            scoreline = Scoreline(home_goals, away_goals)
    return scoreline


def reverse_scoreline(scoreline: Scoreline):
    return Scoreline(scoreline.away_goals, scoreline.home_goals)


@dataclass(kw_only=True)
class Fixture:
    date: datetime
    home_name: str
    away_name: str
    finished: bool
    full_time: Scoreline = field(default_factory=lambda: UNKNOWN_SCORELINE)
    first_half: Scoreline = field(default_factory=lambda: UNKNOWN_SCORELINE)
    second_half: Scoreline = field(default_factory=lambda: UNKNOWN_SCORELINE)

    def __post_init__(self):
        if self.full_time != UNKNOWN_SCORELINE and self.first_half != UNKNOWN_SCORELINE:
            self.second_half = Scoreline(
                self.full_time.home_goals - self.first_half.home_goals,
                self.full_time.away_goals - self.first_half.away_goals
            )

    def __str__(self):
        return f"{self.date.strftime('%d-%m-%y')}: {self.home_name} vs {self.away_name} " \
               f"({Period.FIRST}: {self.first_half}) " \
               f"({Period.SECOND}: {self.second_half}) " \
               f"({Period.FULL}: {self.full_time})"

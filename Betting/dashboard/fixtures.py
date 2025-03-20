import dataclasses
import datetime


@dataclasses.dataclass(slots=True)
class Scoreline:
    left: int = None
    right: int = None

    def __str__(self):
        if self.left is not None and self.right is not None:
            return f'{self.left}-{self.right}'
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
    return Scoreline(scoreline.right, scoreline.left)


@dataclasses.dataclass(kw_only=True)
class Fixture:
    date: datetime
    home_id: int
    away_id: int
    finished: bool
    full_time: Scoreline = dataclasses.field(default_factory=lambda: UNKNOWN_SCORELINE)
    first_half: Scoreline = dataclasses.field(default_factory=lambda: UNKNOWN_SCORELINE)
    second_half: Scoreline = dataclasses.field(default_factory=lambda: UNKNOWN_SCORELINE)

    def __post_init__(self):
        if self.full_time != UNKNOWN_SCORELINE and self.first_half != UNKNOWN_SCORELINE:
            self.second_half = Scoreline(
                self.full_time.left - self.first_half.left,
                self.full_time.right - self.first_half.right
            )


YEAR_DELIMITER = '/'


def get_start_and_end_years(season_str: str) -> tuple[int, int]:
    if YEAR_DELIMITER in season_str:
        start_year, end_year = map(int, season_str.split(YEAR_DELIMITER))
    else:
        start_year = end_year = int(season_str)
    return start_year + 2000, end_year + 2000


@dataclasses.dataclass(slots=True, frozen=True)
class Season:
    the_id: int
    start_year: int
    end_year: int

    def __str__(self):
        if self.start_year == self.end_year:
            return f'{self.start_year - 2000}'
        else:
            return f'{self.start_year - 2000}{YEAR_DELIMITER}{self.end_year - 2000}'

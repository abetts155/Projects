from collections import OrderedDict, namedtuple
from enum import auto, Enum
from lib.messages import error_message
from model.fixtures import Fixture, Half, draw, win, loss
from model.seasons import Season
from model.teams import Team
from typing import List, Tuple


class Data:
    __slots__ = ['wins', 'draws', 'losses', 'goals_for', 'goals_against']

    def __init__(self):
        self.wins = 0
        self.draws = 0
        self.losses = 0
        self.goals_for = 0
        self.goals_against = 0

    def games(self) -> int:
        return self.wins + self.draws + self.losses


def combine(left: Data, right: Data) -> Data:
    combined = Data()
    combined.wins = left.wins + right.wins
    combined.draws = left.draws + right.draws
    combined.losses = left.losses + right.losses
    combined.goals_for = left.goals_for + right.goals_for
    combined.goals_against = left.goals_against + right.goals_against
    return combined


class Position(Enum):
    top = auto()
    middle = auto()
    bottom = auto()
    top_but_not_best = auto()
    bottom_but_not_worst = auto()
    best = auto()
    worst = auto()
    top_half = auto()
    bottom_half = auto()
    worst_of_top_half = auto()
    best_of_bottom_half = auto()

    @staticmethod
    def pretty(position: "Position"):
        lexemes = position.name.split('_')
        lexemes[0] = lexemes[0].capitalize()
        return ' '.join(lexemes)

    @staticmethod
    def from_string(string: str):
        try:
            return Position[string.lower()]
        except KeyError:
            error_message("Position '{}' is not valid".format(string))


TableRow = namedtuple('TableRow', 'TEAM P HW HD HL HF HA AW AD AL AF AA W D L F A PTS')


class TableMap:
    def __init__(self, chunk_size: int, table_size: int):
        self._row_to_chunk = OrderedDict()
        self._chunk_to_rows = OrderedDict()
        self.__compute(chunk_size, table_size)

    def __compute(self, chunk_size: int, table_size: int):
        number_of_chunks = table_size // chunk_size
        chunk_to_capacity = {}
        for chunk_id in range(number_of_chunks):
            chunk_to_capacity[chunk_id] = chunk_size

        slack = table_size % chunk_size
        if slack / chunk_size > 0.5:
            chunk_id = number_of_chunks
            chunk_to_capacity[chunk_id] = slack
        else:
            for i in range(slack):
                chunk_id = i % number_of_chunks
                chunk_to_capacity[chunk_id] += 1

        chunk_id = 0
        for row_id in range(table_size):
            self._row_to_chunk[row_id] = chunk_id
            self._chunk_to_rows.setdefault(chunk_id, []).append(row_id)
            if len(self._chunk_to_rows[chunk_id]) == chunk_to_capacity[chunk_id]:
                chunk_id += 1

    def get_chunk(self, row_id: int) -> int:
        return self._row_to_chunk[row_id]

    def get_rows(self, chunk_id: int) -> List[int]:
        return self._chunk_to_rows[chunk_id]

    def number_of_chunks(self) -> int:
        return len(self._chunk_to_rows)


class LeagueTable(list):
    def __init__(self, season: Season, half: Half):
        list.__init__(self)
        self._season = season
        self.__fill(season.fixtures(), half)
        self.sort(key=lambda row: (row.PTS, row.F - row.A, row.F), reverse=True)
        self.__compute_slices()

    def __fill(self, fixtures: List[Fixture], half: Half):
        home_data = {}
        away_data = {}
        for fixture in fixtures:
            if fixture.full_time():
                if fixture.home_team not in home_data:
                    home_data[fixture.home_team] = Data()

                if fixture.away_team not in away_data:
                    away_data[fixture.away_team] = Data()

                home = home_data[fixture.home_team]
                away = away_data[fixture.away_team]

                results = []
                if half == Half.both:
                    if fixture.full_time() is not None:
                        results.append(fixture.full_time())
                elif half == Half.first:
                    if fixture.first_half() is not None:
                        results.append(fixture.first_half())
                elif half == Half.second:
                    if fixture.second_half() is not None:
                        results.append(fixture.second_half())
                elif half == Half.separate:
                    if fixture.first_half() is not None:
                        results.append(fixture.first_half())
                    if fixture.second_half() is not None:
                        results.append(fixture.second_half())
                else:
                    assert False

                if results:
                    for result in results:
                        if win(result):
                            home.wins += 1
                            away.losses += 1
                        elif loss(result):
                            home.losses += 1
                            away.wins += 1
                        else:
                            home.draws += 1
                            away.draws += 1

                        home.goals_for += result.left
                        home.goals_against += result.right
                        away.goals_for += result.right
                        away.goals_against += result.left

        assert home_data.keys() == away_data.keys()

        for team in home_data.keys():
            home = home_data[team]
            away = away_data[team]
            total = combine(home, away)
            row = TableRow(team, total.games(),
                           home.wins, home.draws, home.losses, home.goals_for, home.goals_against,
                           away.wins, away.draws, away.losses, away.goals_for, away.goals_against,
                           total.wins, total.draws, total.losses, total.goals_for, total.goals_against,
                           total.wins * 3 + total.draws)
            self.append(row)

    def __compute_slices(self):
        self._best_start = 0
        self._best_end = len(self) // 5
        self._worst_start = len(self) - len(self) // 5
        self._worst_end = len(self)
        self._bottom_half_start = len(self) // 2
        self._top_half_end = len(self) // 2

    def positions(self, position: Position) -> Tuple[int, int]:
        if position == Position.top:
            lower_bound = self._best_start
            upper_bound = self._best_end
        elif position == Position.top_but_not_best:
            lower_bound = self._best_start + 1
            upper_bound = self._best_end
        elif position == Position.middle:
            lower_bound = self._best_end
            upper_bound = self._worst_start
        elif position == Position.bottom:
            lower_bound = self._worst_start
            upper_bound = self._worst_end
        elif position == Position.bottom_but_not_worst:
            lower_bound = self._worst_start
            upper_bound = self._worst_end - 1
        elif position == Position.best:
            lower_bound = self._best_start
            upper_bound = self._best_start + 1
        elif position == Position.worst:
            lower_bound = self._worst_end - 1
            upper_bound = self._worst_end
        elif position == Position.top_half:
            lower_bound = self._best_start
            upper_bound = self._top_half_end
        elif position == Position.bottom_half:
            lower_bound = self._bottom_half_start
            upper_bound = self._worst_end
        elif position == Position.worst_of_top_half:
            lower_bound = self._best_end
            upper_bound = self._top_half_end
        elif position == Position.best_of_bottom_half:
            lower_bound = self._bottom_half_start
            upper_bound = self._worst_start + 1
        else:
            assert False
        return lower_bound + 1, upper_bound + 1

    def teams_by_position(self, positions: List[int]) -> List[Team]:
        teams = [self[i-1].TEAM for i in positions]
        return teams

    def team_position(self, team: Team) -> int:
        for i, row in enumerate(self):
            if row.TEAM == team:
                return i

        error_message("Unable to find team '{}' in the table".format(team.name))

    @property
    def season(self) -> Season:
        return self._season

    def group(self, chunk_size: int) -> TableMap:
        if chunk_size <= 0 or chunk_size > len(self):
            error_message('Chunk size {} is not for tables of size {}.'.format(chunk_size, len(self)))
        return TableMap(chunk_size, len(self))

    def played(self) -> int:
        return sum([row.P for row in self])

    def __str__(self):
        team_column_length = 0
        for row in self:
            team_column_length = max(len(row.TEAM.name), team_column_length)

        top = '|{}|{}|{:^16}|{:^16}|{:^18}|{}|'.format(' ' * (team_column_length + 1),
                                                       ' ' * 2,
                                                       'Home',
                                                       'Away',
                                                       'Overall',
                                                       ' ' * 5)
        bottom = '|{:<{}} |{:>2}|  ' \
                 '{:>2} {:>2} {:>2} {:>2} {:>2}|  ' \
                 '{:>2} {:>2} {:>2} {:>2} {:>2}|  ' \
                 '{:>2} {:>2} {:>2} {:>3} {:>3}|  ' \
                 '{:>3}|'.format('Team', team_column_length, 'P',
                                 'W', 'D', 'L', 'F', 'A',
                                 'W', 'D', 'L', 'F', 'A',
                                 'W', 'D', 'L', 'F', 'A', 'PTS')

        rows = []
        for row in self:
            data = '|{:<{}} |{:>2}|  ' \
                   '{:>2} {:>2} {:>2} {:>2} {:>2}|  ' \
                   '{:>2} {:>2} {:>2} {:>2} {:>2}|  ' \
                   '{:>2} {:>2} {:>2} {:>3} {:>3}|  ' \
                   '{:>3}|'.format(row.TEAM.name, team_column_length, row.P,
                                   row.HW, row.HD, row.HL, row.HF, row.HA,
                                   row.AW, row.AD, row.AL, row.AF, row.AA,
                                   row.W, row.D, row.L, row.F, row.A, row.PTS)
            rows.append(data)

        divider = '-' * len(bottom)
        return '{}\n{}\n{}\n{}\n{}\n{}\n{}\n'.format(divider, top, divider, bottom, divider, '\n'.join(rows), divider)

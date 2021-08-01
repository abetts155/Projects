from enum import auto, Enum
from model.fixtures import Fixture
from model.teams import Team
from sql import sql_tables
from sql.sql_columns import Affinity, Column, ColumnNames
from typing import Dict, List


class EventDetail(Enum):
    normal_goal = auto()
    own_goal = auto()
    penalty = auto()
    missed_penalty = auto()
    yellow_card = auto()
    red_card = auto()
    substitution = auto()
    var = auto()


def to_string(detail: EventDetail):
    return ' '.join(lex.capitalize() for lex in detail.name.split('_'))


def is_goal(detail: EventDetail):
    return detail in [EventDetail.normal_goal, EventDetail.own_goal, EventDetail.penalty]


class Event:
    table = None
    inventory = {}

    def __init__(self,
                 fixture: Fixture,
                 time: int,
                 extra_time: int,
                 team: Team,
                 left_id: int,
                 right_id: int,
                 detail: EventDetail):
        self._fixture = fixture
        self._time = time
        self._extra_time = extra_time
        self._team = team
        self._left_id = left_id
        self._right_id = right_id
        self._detail = detail

    @property
    def fixture(self) -> Fixture:
        return self._fixture

    @property
    def time(self) -> int:
        return self._time

    @property
    def extra_time(self) -> int:
        return self._extra_time

    @property
    def team(self) -> Team:
        return self._team

    @property
    def left_id(self) -> int:
        return self._left_id

    @property
    def right_id(self) -> int:
        return self._right_id

    @property
    def detail(self) -> EventDetail:
        return self._detail

    def sql_values(self):
        values = [self.fixture.id,
                  self.time,
                  self.extra_time,
                  self.team.id,
                  self.team.name,
                  self.left_id,
                  self.right_id,
                  self.detail.name]
        assert len(values) == len(self.__class__.sql_table().columns)
        return values

    @classmethod
    def sql_table(cls) -> sql_tables.Table:
        if cls.table is None:
            cls.table = sql_tables.Table(cls.__name__,
                                         None,
                                         [Column(ColumnNames.Fixture_ID.name, Affinity.INTEGER),
                                          Column(ColumnNames.Time.name, Affinity.INTEGER),
                                          Column(ColumnNames.Extra_Time.name, Affinity.INTEGER),
                                          Column(ColumnNames.Team_ID.name, Affinity.INTEGER),
                                          Column(ColumnNames.Name.name, Affinity.TEXT),
                                          Column(ColumnNames.Left_ID.name, Affinity.INTEGER),
                                          Column(ColumnNames.Right_ID.name, Affinity.INTEGER),
                                          Column(ColumnNames.Detail.name, Affinity.TEXT)])
        return cls.table

    def __eq__(self, other):
        if type(other) == type(self):
            return self.__dict__ == other.__dict__
        return NotImplemented

    def __str__(self):
        return '{:>2}+{}: {} {}'.format(self.time,
                                        self.extra_time,
                                        self.team.name,
                                        to_string(self.detail))


def create_event_from_json(data: Dict, fixture: Fixture):
    time = int(data['elapsed'])

    if data['elapsed_plus']:
        extra_time = int(data['elapsed_plus'])
    else:
        extra_time = 0

    team_id = int(data['team_id'])
    team_name = data['teamName']
    team = Team.find_team(team_id, team_name)

    if data['assist_id']:
        left_id = int(data['assist_id'])
    else:
        left_id = 0

    if data['player_id']:
        right_id = int(data['player_id'])
    else:
        right_id = 0

    if data['type'] == 'Goal':
        if data['detail'] == 'Normal Goal':
            detail = EventDetail.normal_goal
        elif data['detail'] == 'Own Goal':
            detail = EventDetail.own_goal
        elif data['detail'] == 'Penalty':
            detail = EventDetail.penalty
        elif data['detail'] == 'Missed Penalty':
            detail = EventDetail.missed_penalty
        else:
            assert False
    elif data['type'] == 'subst':
        detail = EventDetail.substitution
    elif data['type'] == 'Var':
        detail = EventDetail.var
    elif data['type'] == 'Card':
        if data['detail'] == 'Yellow Card':
            detail = EventDetail.yellow_card
        elif data['detail'] == 'Red Card':
            detail = EventDetail.red_card
        else:
            assert False
    else:
        assert False

    Event.inventory[(fixture.id, time, extra_time)] = Event(fixture, time, extra_time, team, left_id, right_id, detail)


def create_event_from_row(row: List, fixture: Fixture):
    time = int(row[1])
    extra_time = int(row[2])
    team_id = int(row[3])
    team_name = row[4]
    team = Team.find_team(team_id, team_name)
    left_id = int(row[5])
    right_id = int(row[6])
    detail = EventDetail[row[7]]
    event = Event(fixture, time, extra_time, team, left_id, right_id, detail)
    return event

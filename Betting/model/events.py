from enum import auto, Enum
from football_api.football_api import get_events
from football_api.structure import get_events_json, store
from json import load
from model.fixtures import Fixture
from model.teams import Team
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


def create_events_json(fixture_id: int):
    events_json = get_events_json(fixture_id)
    if not events_json.exists():
        store(events_json, get_events(fixture_id))
    else:
        restore = False
        with events_json.open() as in_file:
            json_text = load(in_file)
            events = json_text['api']['events']
            restore = not events

        if restore:
            store(events_json, get_events(fixture_id))


def get_events_for_fixture(fixture):
    create_events_json(fixture.id)
    events = []
    events_json = get_events_json(fixture.id)
    with events_json.open() as in_file:
        json_text = load(in_file)
        for data in json_text['api']['events']:
            event = create_event_from_json(data, fixture)
            events.append(event)
    return events


class Event:
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
            detail = None
    else:
        assert False

    return Event(fixture, time, extra_time, team, left_id, right_id, detail)

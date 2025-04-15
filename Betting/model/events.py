import dataclasses
import enum
import pathlib

import model.fixtures
import model.teams
import sql.sql
import sql.sql_tables
from sql.sql_columns import Affinity, Column, ColumnNames


class EventDetail(enum.StrEnum):
    GOAL = "Goal"
    OWN_GOAL = "Own Goal"
    PENALTY = "Penalty"
    MISSED_PENALTY = "Missed Penalty"
    YELLOW_CARD = "Yellow Card"
    RED_CARD = "Red Card"
    SUBSTITUTION = "Sub"
    GOAL_CANCELLED = "Goal cancelled"
    GOAL_DISALLOWED = "Goal Disallowed"
    GOAL_DISALLOWED_HANDBALL = "Goal Disallowed - handball"
    GOAL_DISALLOWED_OFFSIDE = "Goal Disallowed - offside"
    GOAL_DISALLOWED_FOUL = "Goal Disallowed - Foul"
    PENALTY_CANCELLED = "Penalty cancelled"
    PENALTY_CONFIRMED = "Penalty confirmed"
    CARD_UPGRADE = "Card upgrade"
    RED_CARD_CANCELLED = "Red card cancelled"
    CARD_REVIEWED = "Card reviewed"
    GOAL_CONFIRMED = "Goal confirmed"
    UNKNOWN = "Unknown"


def is_goal(detail: EventDetail):
    return detail in [EventDetail.GOAL, EventDetail.OWN_GOAL, EventDetail.PENALTY]


class Event:
    def __init__(
            self,
            id_: int,
            fixture: model.fixtures.Fixture,
            team: model.teams.Team,
            time: int,
            extra_time: int,
            left_player_id: int,
            right_player_id: int,
            detail: EventDetail,
            var_intervention: bool
    ):
        self.id = id_
        self.fixture = fixture
        self.team = team
        self.time = time
        self.extra_time = extra_time
        self.left_player_id = left_player_id
        self.right_player_id = right_player_id
        self.detail = detail
        self.var_intervention = var_intervention

    def sql_values(self):
        values = [
            self.id,
            self.fixture.id,
            self.team.id,
            self.time,
            self.extra_time,
            self.left_player_id,
            self.right_player_id,
            self.detail.name,
            self.var_intervention
        ]
        assert len(values) == len(self.__class__.sql_table().columns)
        return values

    @classmethod
    def sql_table(cls) -> sql.sql_tables.Table:
        id_col = Column(ColumnNames.ID.name, Affinity.INTEGER)
        fixture_id_col = Column(ColumnNames.Fixture_ID.name, Affinity.INTEGER)

        return sql.sql_tables.Table(
            cls.__name__,
            [
                id_col,
                fixture_id_col
            ],
            [
                id_col,
                fixture_id_col,
                Column(ColumnNames.Team_ID.name, Affinity.INTEGER),
                Column(ColumnNames.Time.name, Affinity.INTEGER),
                Column(ColumnNames.Extra_Time.name, Affinity.INTEGER),
                Column(ColumnNames.Left_Player_ID.name, Affinity.INTEGER),
                Column(ColumnNames.Right_Player_ID.name, Affinity.INTEGER),
                Column(ColumnNames.Detail.name, Affinity.TEXT),
                Column(ColumnNames.VAR_Intervention.name, Affinity.INTEGER)
            ]
        )

    def __eq__(self, other):
        if type(other) == type(self):
            return self.id == other.id
        return NotImplemented

    def __hash__(self):
        return hash(self.id)

    def __lt__(self, other):
        if type(other) == type(self):
            return self.id < other.id
        return NotImplemented

    def __le__(self, other):
        if type(other) == type(self):
            return self.id <= other.id
        return NotImplemented

    def __str__(self):
        return f'{self.time:>2}+{self.extra_time}: {self.team.name} {self.detail}'


def create_event_from_json(json_data: dict, event_id: int, fixture: model.fixtures.Fixture):
    time = int(json_data['time']['elapsed'])

    if json_data['time']['extra']:
        extra_time = int(json_data['time']['extra'])
    else:
        extra_time = 0

    team_id = int(json_data['team']['id'])
    if team_id == fixture.home_team.id:
        team = fixture.home_team
    else:
        team = fixture.away_team

    left_player_id = int(json_data['player']['id']) if json_data['player']['id'] else None
    right_player_id = int(json_data['assist']['id']) if json_data['assist']['id'] else None

    var_intervention = False
    if json_data['type'] == 'Goal':
        if json_data['detail'] == 'Normal Goal':
            detail = EventDetail.GOAL
        elif json_data['detail'] == 'Own Goal':
            detail = EventDetail.OWN_GOAL
        elif json_data['detail'] == 'Penalty':
            detail = EventDetail.PENALTY
        elif json_data['detail'] == 'Missed Penalty':
            detail = EventDetail.MISSED_PENALTY
        else:
            assert False
    elif json_data['type'] == 'subst':
        detail = EventDetail.SUBSTITUTION
    elif json_data['type'] == 'Var':
        var_intervention = True
        if json_data['detail'] == EventDetail.GOAL_CANCELLED:
            detail = EventDetail.GOAL_CANCELLED
        elif json_data['detail'] == EventDetail.GOAL_DISALLOWED:
            detail = EventDetail.GOAL_CANCELLED
        elif json_data['detail'] == EventDetail.GOAL_DISALLOWED_HANDBALL:
            detail = EventDetail.GOAL_CANCELLED
        elif json_data['detail'] == EventDetail.GOAL_DISALLOWED_OFFSIDE:
            detail = EventDetail.GOAL_CANCELLED
        elif json_data['detail'] == EventDetail.GOAL_DISALLOWED_FOUL:
            detail = EventDetail.GOAL_CANCELLED
        elif json_data['detail'] == EventDetail.PENALTY_CANCELLED:
            detail = EventDetail.PENALTY_CANCELLED
        elif json_data['detail'] == EventDetail.GOAL_CONFIRMED:
            detail = EventDetail.GOAL
        elif json_data['detail'] == EventDetail.PENALTY_CONFIRMED:
            detail = EventDetail.PENALTY_CONFIRMED
        elif json_data['detail'] == EventDetail.CARD_UPGRADE:
            detail = EventDetail.CARD_UPGRADE
        elif json_data['detail'] == EventDetail.RED_CARD_CANCELLED:
            detail = EventDetail.RED_CARD_CANCELLED
        elif json_data['detail'] == EventDetail.CARD_REVIEWED:
            detail = EventDetail.CARD_REVIEWED
        elif json_data['detail'] is None:
            detail = EventDetail.UNKNOWN
        else:
            assert False, json_data['detail']
    elif json_data['type'] == 'Card':
        if json_data['detail'] == 'Yellow Card':
            detail = EventDetail.YELLOW_CARD
        elif json_data['detail'] == 'Red Card':
            detail = EventDetail.RED_CARD
        else:
            assert False
    else:
        assert False

    return Event(event_id, fixture, team, time, extra_time, left_player_id, right_player_id, detail, var_intervention)



def create_event_from_row(row: list, fixture: model.fixtures.Fixture) -> Event:
    id_ = int(row[0])
    fixture_id = int(row[1])
    team_id = int(row[2])

    if team_id == fixture.home_team.id:
        team = fixture.home_team
    else:
        team = fixture.away_team

    time = int(row[3])
    if time == 0:
        time += 1

    extra_time = int(row[4])
    left_player_id = int(row[5]) if row[5] is not None else None
    right_player_id = int(row[6]) if row[6] is not None else None
    detail = EventDetail[row[7]]
    var_intervention = bool(row[8])
    event = Event(id_, fixture, team, time, extra_time, left_player_id, right_player_id, detail, var_intervention)
    return event


def load_events(database: pathlib.Path, fixture: model.fixtures.Fixture):
    events = []
    goals = 0
    with sql.sql.Database(database) as db:
        fixture_id_constraint = f"{ColumnNames.Fixture_ID.name}={fixture.id}"
        event_rows = db.fetch_all_rows(Event.sql_table(), [fixture_id_constraint])
        for row in event_rows:
            event = create_event_from_row(row, fixture)
            events.append(event)

            if is_goal(event.detail):
                goals += 1

    result = fixture.result(model.fixtures.Period.FULL)
    #assert goals == result.left + result.right, fixture

    events.sort(key=lambda event: event.id)
    return events



@dataclasses.dataclass(slots=True, frozen=True)
class TimeKey:
    time: int
    extra_time: int


@dataclasses.dataclass(slots=True)
class Interval:
    left: int
    right: int
    total: int = 0

    def belongs(self, time: int, goals: int) -> bool:
        if self.left <= time <= self.right:
            self.total += goals
            return True
        else:
            return False

    def __str__(self):
        if self.right in [45, 90]:
            return f"{self.left}-{self.right}+"
        else:
            return f"{self.left}-{self.right}"

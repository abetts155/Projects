import enum
import pickle
import typing

from miscellaneous import wyscout
from model import teams
from sql.sql_columns import Affinity, Column, ColumnNames
from sql import sql_columns, sql_tables


class Period(enum.Enum):
    FIRST_HALF = '1st'
    SECOND_HALF = '2nd'
    FIRST_HALF_ET = '1ET'
    SECOND_HALF_ET = '2ET'
    PENALTIES = 'P'

    @staticmethod
    def from_mnemonic(mnemonic):
        if mnemonic == '1H':
            return Period.FIRST_HALF
        elif mnemonic == '2H':
            return Period.SECOND_HALF
        elif mnemonic == 'E1':
            return Period.FIRST_HALF_ET
        elif mnemonic == 'E2':
            return Period.SECOND_HALF_ET
        elif mnemonic == 'P':
            return Period.PENALTIES


class Position:
    def __init__(self, x: str, y: str):
        self._x = max(0, int(x))
        self._y = max(0, int(y))

    @property
    def x(self) -> int:
        return self._x

    @property
    def y(self) -> int:
        return self._y

    def __str__(self):
        return '({},{})'.format(self.x, self.y)


class Timestamp:
    def __init__(self, time: float):
        self._time = time

    @property
    def time(self) -> float:
        return self._time

    def __str__(self):
        return '{:2f}'.format(self.time)


class EventID(enum.Enum):
    DUEL = enum.auto()
    FOUL = enum.auto()
    FREE_KICK = enum.auto()
    OFF_LINE = enum.auto()
    INTERRUPTION = enum.auto()
    OFFSIDE = enum.auto()
    ON_BALL = enum.auto()
    PASS = enum.auto()
    SAVE_ATTEMPT = enum.auto()
    SHOT = enum.auto()


class SubEventID(enum.Enum):
    AIR = enum.auto()
    GROUND_ATTACKING = enum.auto()
    GROUND_DEFENDING = enum.auto()
    GROUND_LOOSE_BALL = enum.auto()


events_table = {(EventID.DUEL, 10): 'Air duel',
                (EventID.DUEL, 11): 'Ground attacking duel',
                (EventID.DUEL, 12): 'Ground defending duel',
                (EventID.DUEL, 13): 'Ground loose ball duel',
                (EventID.FOUL, 20): 'Foul',
                (EventID.FOUL, 21): 'Hand foul',
                (EventID.FOUL, 22): 'Late card foul',
                (EventID.FOUL, 23): 'Out of game foul',
                (EventID.FOUL, 24): 'Protest',
                (EventID.FOUL, 25): 'Simulation',
                (EventID.FOUL, 26): 'Time lost foul',
                (EventID.FOUL, 27): 'Violent foul',
                (EventID.FREE_KICK, 30): 'Corner',
                (EventID.FREE_KICK, 31): 'Free kick',
                (EventID.FREE_KICK, 32): 'Free kick cross',
                (EventID.FREE_KICK, 33): 'Free kick shot',
                (EventID.FREE_KICK, 34): 'Goal kick',
                (EventID.FREE_KICK, 35): 'Penalty',
                (EventID.FREE_KICK, 36): 'Throw in',
                (EventID.OFF_LINE, 40): 'Goalkeeper leaving line',
                (EventID.INTERRUPTION, 50): 'Ball out of field',
                (EventID.INTERRUPTION, 51): 'Whistle',
                (EventID.OFFSIDE, 60): 'Offside',
                (EventID.ON_BALL, 70): 'Acceleration',
                (EventID.ON_BALL, 71): 'Clearance',
                (EventID.ON_BALL, 72): 'Touch',
                (EventID.PASS, 80): 'Cross',
                (EventID.PASS, 81): 'Hand pass',
                (EventID.PASS, 82): 'Head pass',
                (EventID.PASS, 83): 'High pass',
                (EventID.PASS, 84): 'Launch',
                (EventID.PASS, 85): 'Simple pass',
                (EventID.PASS, 86): 'Smart pass',
                (EventID.SAVE_ATTEMPT, 90): 'Reflex save attempt',
                (EventID.SAVE_ATTEMPT, 91): 'Normal save attempt',
                (EventID.SHOT, 100): 'Shot'}


def what_happened(event_id: int, sub_event_id: int = 0) -> str:
    if sub_event_id == 0:
        (candidate,) = [event for (left_index, _), event in events_table.items() if left_index == EventID(event_id)]
        return candidate
    else:
        return events_table[(EventID(event_id), sub_event_id)]


class Tag:
    def __init__(self, label: str, description: str):
        self._label = label
        self._description = description

    @property
    def label(self) -> str:
        return self._label

    @property
    def description(self) -> str:
        return self._description

    def __str__(self):
        return self.description


tags_table = {101: Tag('goal', 'goal'),
              102: Tag('own_goal', 'own goal'),
              201: Tag('opportunity', 'opportunity'),
              301: Tag('assist', 'assist'),
              302: Tag('keyPass', 'key pass'),
              401: Tag('Left', 'left foot'),
              402: Tag('Right', 'right foot'),
              403: Tag('head_body', 'head (maybe body)'),
              501: Tag('free_space_r', 'free space right'),
              502: Tag('free_space_l', 'free space left'),
              503: Tag('take_on_l', 'take on left'),
              504: Tag('take_on_r', 'take on right'),
              601: Tag('anticipated', 'anticipated'),
              602: Tag('anticipation', 'anticipation'),
              701: Tag('lost', 'lost'),
              702: Tag('neutral', 'neutral'),
              703: Tag('won', 'won'),
              801: Tag('high', 'high'),
              802: Tag('low', 'low'),
              901: Tag('through', 'through'),
              1001: Tag('fairplay', 'fair play'),
              1101: Tag('direct', 'direct'),
              1102: Tag('indirect', 'indirect'),
              1201: Tag('gb', 'goal low center'),
              1202: Tag('gbr', 'goal low right'),
              1203: Tag('gc', 'goal center'),
              1204: Tag('gl', 'goal center left'),
              1205: Tag('glb', 'goal low left'),
              1206: Tag('gr', 'goal center right'),
              1207: Tag('gt', 'goal high center'),
              1208: Tag('gtl', 'goal high left'),
              1209: Tag('gtr', 'goal high right'),
              1210: Tag('obr', 'out low right'),
              1211: Tag('ol', 'out center left'),
              1212: Tag('olb', 'out low left'),
              1213: Tag('or', 'out center right'),
              1214: Tag('ot', 'out high center'),
              1215: Tag('otl', 'out high left'),
              1216: Tag('otr', 'out high right'),
              1217: Tag('pbr', 'post low right'),
              1218: Tag('pl', 'post center left'),
              1219: Tag('plb', 'post low left'),
              1220: Tag('pr', 'post center right'),
              1221: Tag('pt', 'post high center'),
              1222: Tag('ptl', 'post high left'),
              1223: Tag('ptr', 'post high right'),
              1301: Tag('feint', 'feint'),
              1302: Tag('missed_ball', 'missed ball'),
              1401: Tag('interception', 'interception'),
              1501: Tag('clearance', 'clearance'),
              1601: Tag('sliding_tackle', 'sliding tackle'),
              1701: Tag('red_card', 'red card'),
              1702: Tag('yellow_card', 'yellow card'),
              1703: Tag('second_yellow_card', 'second yellow card'),
              1801: Tag('accurate', 'accurate'),
              1802: Tag('not_accurate', 'not accurate'),
              1901: Tag('counter_attack', 'counter attack'),
              2001: Tag('dangerous_ball_lost', 'dangerous ball lost'),
              2101: Tag('blocked', 'blocked')}


class Event:
    table = None
    inventory = {}

    def __init__(self,
                 id_: int,
                 period: Period,
                 timestamp: Timestamp,
                 player: teams.Player,
                 team: teams.Team,
                 event_id: int,
                 sub_event_id: int,
                 tags: typing.List[int],
                 positions: typing.List[Position]):
        self._id = id_
        self._period = period
        self._timestamp = timestamp
        self._player = player
        self._team = team
        self._event_id = event_id
        self._sub_event_id = sub_event_id
        self._tags = tags
        self._positions = positions
        assert self.id not in Event.inventory
        Event.inventory[self.id] = self

    @property
    def id(self) -> int:
        return self._id

    @property
    def event_id(self) -> int:
        return self._event_id

    @property
    def period(self) -> Period:
        return self._period

    @property
    def player(self) -> teams.Player:
        return self._player

    @property
    def positions(self) -> typing.List[Position]:
        return self._positions

    @property
    def sub_event_id(self) -> int:
        return self._sub_event_id

    @property
    def team(self) -> teams.Team:
        return self._team

    @property
    def tags(self) -> typing.List[int]:
        return self._tags

    @property
    def timestamp(self) -> Timestamp:
        return self._timestamp

    def sql_values(self):
        values = [self.id,
                  self.period.value,
                  self.timestamp.time,
                  self.player.id if self.player else None,
                  self.team.id,
                  self.event_id,
                  self.sub_event_id,
                  pickle.dumps(self.tags),
                  pickle.dumps(self.positions[0]),
                  pickle.dumps(self.positions[1])]
        assert len(values) == len(self.__class__.table.columns)
        return values

    @classmethod
    def sql_table(cls) -> sql_tables.Table:
        if cls.table is None:
            player_column = Column(ColumnNames.Player_ID.name, Affinity.INTEGER)
            team_column = Column(ColumnNames.Team_ID.name, Affinity.INTEGER)
            cls.table = sql_tables.Table(cls.__name__,
                                         sql_columns.id_column(),
                                         [sql_columns.id_column(),
                                          Column(ColumnNames.Period.name, Affinity.INTEGER),
                                          Column(ColumnNames.Timestamp.name, Affinity.REAL),
                                          player_column,
                                          team_column,
                                          Column(ColumnNames.Event_ID.name, Affinity.INTEGER),
                                          Column(ColumnNames.Sub_Event_ID.name, Affinity.INTEGER),
                                          Column(ColumnNames.Tags.name, Affinity.BLOB),
                                          Column(ColumnNames.From_Position.name, Affinity.BLOB),
                                          Column(ColumnNames.To_Position.name, Affinity.BLOB)])
            cls.table.add_foreign_key(player_column, teams.Player.sql_table())
            cls.table.add_foreign_key(team_column, teams.Team.sql_table())
        return cls.table

    def __str__(self):
        value = '[{}:{}]'.format(self.period.value, self.timestamp)
        value += '[{:<22}] '.format(' to '.join(str(position) for position in self._positions))
        if self.player:
            value += '{} for {}. '.format(self.player, self.team)
        else:
            value += '{}. '.format(str(self.team))
        value += what_happened(self.event_id, self.sub_event_id)
        if self.tags:
            value += ': ' + ','.join(str(tags_table[id_]) for id_ in self._tags)
        value += '.'
        return value


def is_shot(event: Event):
    return event.event_id == EventID.SHOT.value


def is_pass(event: Event):
    return event.event_id == EventID.PASS.value


def is_corner(event: Event):
    return event.event_id == EventID.FREE_KICK.value and event.sub_event_id == 30


def create_event_from_json(data: typing.Dict):
    id_ = int(data[wyscout.JSON_Keys.id_])
    period = Period.from_mnemonic(data[wyscout.JSON_Keys.matchPeriod])
    timestamp = Timestamp(data[wyscout.JSON_Keys.eventSec])
    player_id = int(data[wyscout.JSON_Keys.playerId])
    if player_id in teams.Player.inventory:
        player = teams.Player.inventory[player_id]
    else:
        player = None
    team_id = int(data[wyscout.JSON_Keys.teamId])
    team = teams.Team.inventory[team_id]
    event_id = int(data[wyscout.JSON_Keys.eventId])
    if data[wyscout.JSON_Keys.subEventId]:
        sub_event_id = int(data[wyscout.JSON_Keys.subEventId])
    else:
        sub_event_id = 0
    tags = []
    for tag in data[wyscout.JSON_Keys.tags]:
        tags.append(int(tag[wyscout.JSON_Keys.id_]))
    positions = []
    if len(data[wyscout.JSON_Keys.positions]) == 2:
        position_one, position_two = data[wyscout.JSON_Keys.positions]
        coordinates_one = Position(position_one[wyscout.JSON_Keys.x], position_two[wyscout.JSON_Keys.y])
        coordinates_two = Position(position_two[wyscout.JSON_Keys.x], position_two[wyscout.JSON_Keys.y])
    else:
        (position,) = data[wyscout.JSON_Keys.positions]
        coordinates_one = Position(position[wyscout.JSON_Keys.x], position[wyscout.JSON_Keys.y])
        coordinates_two = Position(position[wyscout.JSON_Keys.x], position[wyscout.JSON_Keys.y])
    positions.append(coordinates_one)
    positions.append(coordinates_two)
    event = Event(id_, period, timestamp, player, team, event_id, sub_event_id, tags, positions)
    match_id = int(data[wyscout.JSON_Keys.matchId])
    return match_id, event

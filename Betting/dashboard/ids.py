import enum


class Table(enum.StrEnum):
    LEAGUE_TABLE = enum.auto()
    LEAGUE_RESULTS_TABLE = enum.auto()
    LEAGUE_FIXTURES_TABLE = enum.auto()
    TEAM_RESULTS_TABLE = enum.auto()
    TEAM_FIXTURES_TABLE = enum.auto()


class Pie(enum.StrEnum):
    RESULTS_PIE = enum.auto()
    GOALS_PIE = enum.auto()
    TEAM_RESULTS_PIE = enum.auto()
    TEAM_GOALS_PIE = enum.auto()


class Dropdown(enum.StrEnum):
    COUNTRY_DROPDOWN = enum.auto()
    LEAGUE_DROPDOWN = enum.auto()
    LEAGUE_ACCORDION = enum.auto()
    SEASON_DROPDOWN = enum.auto()
    TEAM_DROPDOWN = enum.auto()


class Radio(enum.StrEnum):
    HALF_RADIO = enum.auto()
    VENUE_RADIO = enum.auto()
    RESULTS_RADIO = enum.auto()
    GOALS_RADIO = enum.auto()
    TOTAL_GOALS_RELATIONS_RADIO = enum.auto()
    TOTAL_GOALS_RADIO = enum.auto()


class Switch(enum.StrEnum):
    RESULTS_SWITCH = enum.auto()
    GOALS_SWITCH = enum.auto()


class Slider(enum.StrEnum):
    HISTORY_SLIDER = enum.auto()


class Tab(enum.StrEnum):
    LEAGUE_OVERVIEW_TAB = enum.auto()
    TEAM_OVERVIEW_TAB = enum.auto()
    TEAM_NOW_TAB = enum.auto()


class Graph(enum.StrEnum):
    GOALS_GRAPH = enum.auto()
    BTS_GRAPH = enum.auto()
    RESULTS_GRAPH = enum.auto()
    SCORES_HEATMAP = enum.auto()
    TEAM_GOALS_GRAPH = enum.auto()
    TEAM_SCORES_HEATMAP = enum.auto()
    TEAM_SCORED_AND_CONCEDED_GRAPH = enum.auto()
    TEAM_RESULTS_GRAPH = enum.auto()
    TEAM_GOALS_TREND = enum.auto()
    TEAM_RESULTS_TREND = enum.auto()


class Bar(enum.StrEnum):
    RESULTS_BAR = enum.auto()
    GOALS_BAR = enum.auto()
    TOTAL_GOALS_BAR = enum.auto()


class Miscellaneous(enum.StrEnum):
    LEAGUE_ACCORDION = enum.auto()
    CONTAINER = enum.auto()
    LOADER = enum.auto()
    DATA_STORE = enum.auto()
    TEAM_FIXTURES_CARD = enum.auto()
    TEAM_RESULTS_CARD = enum.auto()

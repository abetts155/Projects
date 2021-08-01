from model.fixtures import Fixture
from model.leagues import League
from model.teams import Team
from sql import sql_columns, sql_tables
from sql.sql_columns import Affinity, Column, ColumnNames
from typing import Dict, List, Set


class Season:
    table = None
    inventory = {}

    def __init__(self, id_: int, year: int, name: str, country: str, country_code: str, flag: str, current: bool):
        self._id = id_
        self._year = year
        self._name = name
        self._country = country
        self._country_code = country_code
        self._flag = flag
        self._current = current
        self._fixtures = []

    @property
    def id(self) -> int:
        return self._id

    @property
    def year(self) -> int:
        return self._year

    @property
    def name(self) -> str:
        return self._name

    @property
    def country(self) -> str:
        return self._country

    @property
    def country_code(self) -> str:
        return self._country_code

    @property
    def flag(self) -> str:
        return self._flag

    @property
    def current(self) -> bool:
        return self._current

    def add_fixture(self, fixture: Fixture):
        self._fixtures.append(fixture)

    def teams(self) -> Set[Team]:
        teams = set()
        for fixture in self.fixtures():
            teams.add(fixture.home_team)
            teams.add(fixture.away_team)
        return teams

    def sort_fixtures(self):
        self._fixtures.sort(key=lambda fixture: fixture.date)

    def fixtures(self) -> List[Fixture]:
        return self._fixtures

    def fixtures_per_team(self) -> Dict[Team, List[Fixture]]:
        team_fixtures = {team: [] for team in self.teams()}
        for fixture in self.fixtures():
            team_fixtures[fixture.home_team].append(fixture)
            team_fixtures[fixture.away_team].append(fixture)
        return team_fixtures

    def sql_values(self):
        values = [self.id, self.year, self.name, self.country, self.country_code, self.flag, self.current]
        assert len(values) == len(self.__class__.sql_table().columns)
        return values

    @classmethod
    def sql_table(cls) -> sql_tables.Table:
        if cls.table is None:
            cls.table = sql_tables.Table(cls.__name__,
                                         sql_columns.id_column(),
                                         [sql_columns.id_column(),
                                          Column(ColumnNames.Year.name, Affinity.INTEGER),
                                          Column(ColumnNames.Code.name, Affinity.TEXT),
                                          Column(ColumnNames.Country.name, Affinity.TEXT),
                                          Column(ColumnNames.Country_Code.name, Affinity.TEXT),
                                          Column(ColumnNames.Flag.name, Affinity.TEXT),
                                          Column(ColumnNames.Current.name, Affinity.INTEGER)])
        return cls.table

    @classmethod
    def seasons(cls, league: League, ordered: bool = True):
        seasons = [season for season in Season.inventory.values()
                   if season.country == league.country and season.name == league.name.replace("''", "'")]
        if ordered:
            seasons.sort(key=lambda season: season.year)
        return seasons

    def __eq__(self, other):
        if type(other) == type(self):
            return self.year == other.year
        return NotImplemented

    def __hash__(self):
        return self.id

    def __lt__(self, other):
        if type(other) == type(self):
            return self.year < other.year
        return NotImplemented

    def __le__(self, other):
        if type(other) == type(self):
            return self.year <= other.year
        return NotImplemented

    def __str__(self):
        return '{} ({})'.format(self.name, self.year)


def create_season_from_json(data: Dict):
    if data['type'] == 'League':
        id_ = int(data['league_id'])
        year = int(data['season'])
        current = bool(data['is_current'])
        season = Season(id_, year, data['name'], data['country'], data['country_code'], data['flag'], current)
        Season.inventory[season.id] = season


def create_season_from_row(row: List):
    id_ = int(row[0])
    year = int(row[1])
    name = row[2]
    country = row[3]
    country_code = row[4]
    flag = row[5]
    current = bool(row[6])
    season = Season(id_, year, name, country, country_code, flag, current)
    Season.inventory[season.id] = season
    return season

import datetime
import pathlib
import typing

import lib.structure
import model.fixtures
import model.competitions
import model.teams
import sql.sql
import sql.sql_columns
import sql.sql_tables

from sql.sql_columns import Affinity, Column, ColumnNames
from sql.sql_language import Characters


class Season:
    def __init__(
            self,
            competition_id: int,
            year: int,
            start_date: datetime.date,
            end_date: datetime.date,
            current: bool,
            lineups: bool,
            events: bool,
            statistics_fixtures: bool,
            statistics_players: bool
    ):
        self.competition_id = competition_id
        self.year = year
        self.start_date = start_date
        self.end_date = end_date
        self.current = current
        self.lineups = lineups
        self.events = events
        self.statistics_fixtures = statistics_fixtures
        self.statistics_players = statistics_players

    def sql_values(self):
        values = [
            self.competition_id,
            self.year,
            self.start_date,
            self.end_date,
            self.current,
            self.lineups,
            self.events,
            self.statistics_fixtures,
            self.statistics_players,
        ]
        assert len(values) == len(self.__class__.sql_table().columns)
        return values

    @classmethod
    def sql_table(cls) -> sql.sql_tables.Table:
        competition_id_col = Column(ColumnNames.Competition_ID.name, Affinity.INTEGER)
        year_col = Column(ColumnNames.Year.name, Affinity.INTEGER)

        return sql.sql_tables.Table(
            cls.__name__,
            [
                competition_id_col,
                year_col
            ],
            [
                competition_id_col,
                year_col,
                Column(ColumnNames.Start_Date.name, Affinity.TEXT),
                Column(ColumnNames.End_Date.name, Affinity.TEXT),
                Column(ColumnNames.Current.name, Affinity.INTEGER),
                Column(ColumnNames.Lineups.name, Affinity.INTEGER),
                Column(ColumnNames.Events.name, Affinity.INTEGER),
                Column(ColumnNames.Statistics_Fixtures.name, Affinity.INTEGER),
                Column(ColumnNames.Statistics_Players.name, Affinity.INTEGER)
            ]
        )

    def __eq__(self, other):
        if type(other) == type(self):
            return self.year == other.year
        return NotImplemented

    def __hash__(self):
        return hash(self.competition_id + self.year)

    def __lt__(self, other):
        if type(other) == type(self):
            return self.year < other.year
        return NotImplemented

    def __le__(self, other):
        if type(other) == type(self):
            return self.year <= other.year
        return NotImplemented


def create_season_from_json(competition: model.competitions.Competition, json_data: dict) -> Season:
    year = int(json_data['year'])
    start_date = datetime.datetime.fromisoformat(json_data['start'])
    end_date = datetime.datetime.fromisoformat(json_data['end'])
    current = bool(json_data['current'])
    lineups = bool(json_data['coverage']['fixtures']['lineups'])
    events = bool(json_data['coverage']['fixtures']['events'])
    statistics_fixtures = bool(json_data['coverage']['fixtures']['statistics_fixtures'])
    statistics_players = bool(json_data['coverage']['fixtures']['statistics_players'])

    return Season(
        competition.id,
        year,
        start_date,
        end_date,
        current,
        lineups,
        events,
        statistics_fixtures,
        statistics_players
    )


def create_season_from_row(row: list) -> Season:
    competition_id = int(row[0])
    year = int(row[1])
    start_date = datetime.datetime.fromisoformat(row[2])
    end_date = datetime.datetime.fromisoformat(row[3])
    current = bool(row[4])
    lineups = bool(row[5])
    events = bool(row[6])
    statistics_fixtures = bool(row[7])
    statistics_players = bool(row[8])

    return Season(
        competition_id,
        year,
        start_date,
        end_date,
        current,
        lineups,
        events,
        statistics_fixtures,
        statistics_players
    )


def load_fixtures(
        database: pathlib.Path,
        competition: model.competitions.Competition,
        season: Season
) -> list[model.fixtures.Fixture]:
    if competition.type == model.competitions.CompetitionType.LEAGUE:
        table = model.fixtures.Fixture.sql_table()
    else:
        table = model.fixtures.CupFixture.sql_table()

    fixtures = []
    with sql.sql.Database(database) as db:
        competition_constraint = f'{ColumnNames.Competition_ID.name}={competition.id}'
        season_constraint = f'{ColumnNames.Season_ID.name}={season.year}'
        fixture_rows = db.fetch_all_rows(table, [competition_constraint, season_constraint])

        team_ids = model.fixtures.get_team_ids(fixture_rows)
        teams = model.teams.load_teams(database, team_ids)

        for fixture_row in fixture_rows:
            if competition.type == model.competitions.CompetitionType.LEAGUE:
                fixture = model.fixtures.create_fixture_from_row(fixture_row, teams)
            else:
                fixture = model.fixtures.create_cup_fixture_from_row(fixture_row, teams)

            assert fixture is not None
            fixtures.append(fixture)

    model.fixtures.sort_fixtures(fixtures)
    return fixtures


def load_current_season(database: pathlib.Path, competition: model.competitions.Competition) -> Season:
    with sql.sql.Database(database) as db:
        competition_constraint = f"{ColumnNames.Competition_ID.name}={competition.id}"
        current_constraint = f"{ColumnNames.Current.name}={Characters.TRUE.value}"
        season_rows = db.fetch_all_rows(Season.sql_table(), [competition_constraint, current_constraint])
        assert season_rows
        (season_row,) = season_rows
        return create_season_from_row(season_row)


def load_season(
        database: pathlib.Path,
        competition: model.competitions.Competition,
        year: int
) -> typing.Optional[Season]:
    with sql.sql.Database(database) as db:
        competition_constraint = f"{ColumnNames.Competition_ID.name}={competition.id}"
        year_constraint = f"{ColumnNames.Year.name}={year}"
        season_rows = db.fetch_all_rows(Season.sql_table(), [competition_constraint, year_constraint])
        if season_rows:
            (row,) = season_rows
            return create_season_from_row(row)


def load_seasons(
        database: pathlib.Path,
        competition: model.competitions.Competition
) -> list[Season]:
    seasons = []
    with sql.sql.Database(database) as db:
        competition_constraint = f"{ColumnNames.Competition_ID.name}={competition.id}"
        season_rows = db.fetch_all_rows(Season.sql_table(), [competition_constraint])
        for season_row in season_rows:
            season = create_season_from_row(season_row)
            seasons.append(season)

    seasons.sort(key=lambda season: season.year)
    return seasons

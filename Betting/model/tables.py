import collections
import dataclasses

import lib.messages
import model.competitions
import model.fixtures
import model.seasons
import model.teams


@dataclasses.dataclass(slots=True)
class Data:
    wins: int = 0
    draws: int = 0
    losses: int = 0
    goals_for: int = 0
    goals_against: int = 0

    def games(self) -> int:
        return self.wins + self.draws + self.losses


TableRow = collections.namedtuple('TableRow', 'TEAM P W D L F A PTS')


class LeagueTable(list):
    def __init__(
            self,
            competition: model.competitions.Competition,
            season: model.seasons.Season,
            period: model.fixtures.Period = model.fixtures.Period.FULL,
            venue: model.fixtures.Venue = model.fixtures.Venue.ANYWHERE
    ):
        assert period in [model.fixtures.Period.FIRST, model.fixtures.Period.SECOND, model.fixtures.Period.FULL]
        list.__init__(self)
        self.competition = competition
        self.season = season
        self.__fill(period, venue)
        self.sort(key=lambda row: (row.PTS, row.F - row.A, row.F), reverse=True)

    def __fill(self, period: model.fixtures.Period, venue: model.fixtures.Venue):
        fixtures = model.seasons.load_fixtures(self.competition, self.season)

        home_data = {}
        away_data = {}
        for fixture in fixtures:
            if fixture.home_team not in home_data:
                home_data[fixture.home_team] = Data()
                away_data[fixture.home_team] = Data()

            if fixture.away_team not in home_data:
                home_data[fixture.away_team] = Data()
                away_data[fixture.away_team] = Data()

        for fixture in fixtures:
            if fixture.result(model.fixtures.Period.FULL):
                if period == model.fixtures.Period.FULL:
                    result = fixture.result(model.fixtures.Period.FULL)
                elif period == model.fixtures.Period.FIRST:
                    result = fixture.result(model.fixtures.Period.FIRST)
                else:
                    result = fixture.result(model.fixtures.Period.SECOND)

                if result is not None:
                    if model.fixtures.win(result):
                        home_data[fixture.home_team].wins += 1
                        away_data[fixture.away_team].losses += 1
                    elif model.fixtures.loss(result):
                        home_data[fixture.home_team].losses += 1
                        away_data[fixture.away_team].wins += 1
                    else:
                        home_data[fixture.home_team].draws += 1
                        away_data[fixture.away_team].draws += 1

                    home_data[fixture.home_team].goals_for += result.left
                    home_data[fixture.home_team].goals_against += result.right
                    away_data[fixture.away_team].goals_for += result.right
                    away_data[fixture.away_team].goals_against += result.left

        for team in home_data.keys():
            if venue == model.fixtures.Venue.AWAY:
                row = TableRow(
                    team,
                    away_data[team].games(),
                    away_data[team].wins,
                    away_data[team].draws,
                    away_data[team].losses,
                    away_data[team].goals_for,
                    away_data[team].goals_against,
                    3 * away_data[team].wins + away_data[team].draws
                )
            elif venue == model.fixtures.Venue.HOME:
                row = TableRow(
                    team,
                    home_data[team].games(),
                    home_data[team].wins,
                    home_data[team].draws,
                    home_data[team].losses,
                    home_data[team].goals_for,
                    home_data[team].goals_against,
                    3 * home_data[team].wins + home_data[team].draws
                )
            else:
                row = TableRow(
                    team,
                    home_data[team].games() + away_data[team].games(),
                    home_data[team].wins + away_data[team].wins,
                    home_data[team].draws + away_data[team].draws,
                    home_data[team].losses + away_data[team].losses,
                    home_data[team].goals_for + away_data[team].goals_for,
                    home_data[team].goals_against + away_data[team].goals_against,
                    3 * home_data[team].wins + home_data[team].draws + 3 * away_data[team].wins + away_data[team].draws
                )
            self.append(row)

    def team_position(self, team: model.teams.Team) -> int:
        for i, row in enumerate(self):
            if row.TEAM == team:
                return i
        lib.messages.error_message("Unable to find team '{}' in the table".format(team.name))

    def played(self) -> int:
        return sum([row.P for row in self])

    def __str__(self):
        team_column_length = 0
        for row in self:
            team_column_length = max(len(row.TEAM.name), team_column_length)

        bottom = f"|{'Team':<{team_column_length}} |{'P':>2}| {'W':>2} {'L':>2} {'D':>2} {'F':>2} {'A':>2} {'PTS':>3}|"
        rows = []
        for row in self:
            data = f"|{row.TEAM.name:<{team_column_length}} |{row.P:>2}| {row.W:>2} {row.D:>2} {row.L:>2} {row.F:>2} {row.A:>2} {row.PTS:>3}|"
            rows.append(data)

        divider = '-' * len(bottom)
        return f"{divider}\n{bottom}\n{divider}\n{'\n'.join(rows)}\n{divider}\n"

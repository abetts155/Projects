import dataclasses
import pandas as pd

import tabulate

import lib.messages
import lib.structure
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
    games_scored: int = 0
    games_conceded: int = 0
    games_both_teams_scored: int = 0
    games_at_least_one_team_scored: int = 0
    over_1_5: int = 0
    over_2_5: int = 0

    def games(self) -> int:
        return self.wins + self.draws + self.losses


TEAM_COL     = 'TEAM'
P_COL        = 'P'
W_COL        = 'W'
D_COL        = 'D'
L_COL        = 'L'
PTS_COL      = 'PTS'
S_COL        = 'S'
C_COL        = 'C'
BTS_COL      = 'BTS'
OTS_COL      = 'OTS'
F_COL        = 'F'
A_COL        = 'A'
GR_COL       = 'GR'
OVER_1_5_COL = '≥ 1.5'
OVER_2_5_COL = '≥ 2.5'

COLS = [
    TEAM_COL, P_COL, W_COL, D_COL, L_COL, PTS_COL, S_COL, C_COL,
    BTS_COL, OTS_COL, OVER_1_5_COL, OVER_2_5_COL, F_COL, A_COL, GR_COL
]
PERCENTAGE_COLS = [S_COL, C_COL, BTS_COL, OTS_COL, OVER_1_5_COL, OVER_2_5_COL]
FLOAT_COLS = [F_COL, A_COL, GR_COL]


def translate(col: str) -> str:
    if col == F_COL:
        return "Goals For Rate"
    elif col == A_COL:
        return "Goals Against Rate"
    elif col == S_COL:
        return "Games Scored"
    elif col == C_COL:
        return "Games Conceded"
    elif col == BTS_COL:
        return "Both Scored"
    elif col == OTS_COL:
        return "Not 0-0"
    elif col == OVER_1_5_COL:
        return "Over 1.5"
    elif col == OVER_2_5_COL:
        return "Over 2.5"
    elif col == GR_COL:
        return "Total Goals Rate"


def create_default_row(team: model.teams.Team) -> dict:
    row = {col: 0 for col in COLS if col != TEAM_COL}
    row[TEAM_COL] = team
    return row


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
        self.df = pd.DataFrame(self, columns=COLS)
        self.df.sort_values(
            by=[PTS_COL, F_COL, A_COL],
            ascending=[False, False, True],
            inplace=True
        )
        self.df[PERCENTAGE_COLS] = self.df[PERCENTAGE_COLS].round(0).astype(int)
        self.df[FLOAT_COLS] = self.df[FLOAT_COLS].round(1)

    def __fill(self, period: model.fixtures.Period, venue: model.fixtures.Venue):
        database = lib.structure.get_database(self.competition.country)
        fixtures = model.seasons.load_fixtures(database, self.competition, self.season)

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

                    if result.left > 0:
                        home_data[fixture.home_team].games_scored += 1
                        away_data[fixture.away_team].games_conceded += 1

                    if result.right > 0:
                        home_data[fixture.home_team].games_conceded += 1
                        away_data[fixture.away_team].games_scored += 1

                    if result.left > 0 and result.right > 0:
                        home_data[fixture.home_team].games_both_teams_scored += 1
                        away_data[fixture.away_team].games_both_teams_scored += 1

                    if result.left > 0 or result.right > 0:
                        home_data[fixture.home_team].games_at_least_one_team_scored += 1
                        away_data[fixture.away_team].games_at_least_one_team_scored += 1

                    if result.left + result.right > 1:
                        home_data[fixture.home_team].over_1_5 += 1
                        away_data[fixture.away_team].over_1_5 += 1

                    if result.left + result.right > 2:
                        home_data[fixture.home_team].over_2_5 += 1
                        away_data[fixture.away_team].over_2_5 += 1

        for team in home_data.keys():
            if venue == model.fixtures.Venue.AWAY:
                away = away_data[team]
                total_games = away.games()
                if total_games:
                    row = {
                        TEAM_COL: team,
                        P_COL: total_games,
                        W_COL: away.wins,
                        D_COL: away.draws,
                        L_COL: away.losses,
                        PTS_COL: 3 * away.wins + away.draws,
                        S_COL: 100 * away.games_scored / total_games,
                        C_COL: 100 * away.games_conceded / total_games,
                        BTS_COL: 100 * away.games_both_teams_scored / total_games,
                        OTS_COL: 100 * away.games_at_least_one_team_scored / total_games,
                        OVER_1_5_COL: 100 * away.over_1_5 / total_games,
                        OVER_2_5_COL: 100 * away.over_2_5 / total_games,
                        F_COL: away.goals_for / total_games,
                        A_COL: away.goals_against / total_games,
                        GR_COL: (away.goals_for + away.goals_against) / total_games,
                    }
                else:
                    row = create_default_row(team)

            elif venue == model.fixtures.Venue.HOME:
                home = home_data[team]
                total_games = home.games()
                if total_games:
                    row = {
                        TEAM_COL: team,
                        P_COL: total_games,
                        W_COL: home.wins,
                        D_COL: home.draws,
                        L_COL: home.losses,
                        PTS_COL: 3 * home.wins + home.draws,
                        S_COL: 100 * home.games_scored / total_games,
                        C_COL: 100 * home.games_conceded / total_games,
                        BTS_COL: 100 * home.games_both_teams_scored / total_games,
                        OTS_COL: 100 * home.games_at_least_one_team_scored / total_games,
                        OVER_1_5_COL: 100 * home.over_1_5 / total_games,
                        OVER_2_5_COL: 100 * home.over_2_5 / total_games,
                        F_COL: home.goals_for / total_games,
                        A_COL: home.goals_against / total_games,
                        GR_COL: (home.goals_for + home.goals_against) / total_games,
                    }
                else:
                    row = create_default_row(team)

            else:
                home = home_data[team]
                away = away_data[team]
                total_games = home.games() + away.games()

                if total_games:
                    row = {
                        TEAM_COL: team,
                        P_COL: total_games,
                        W_COL: home.wins + away.wins,
                        D_COL: home.draws + away.draws,
                        L_COL: home.losses + away.losses,
                        PTS_COL: 3 * home.wins + home.draws + 3 * away.wins + away.draws,
                        S_COL: 100 * (home.games_scored + away.games_scored) / total_games,
                        C_COL: 100 * (home.games_conceded + away.games_conceded) / total_games,
                        BTS_COL: 100 * (home.games_both_teams_scored + away.games_both_teams_scored) / total_games,
                        OTS_COL: 100 * (home.games_at_least_one_team_scored + away.games_at_least_one_team_scored) / total_games,
                        OVER_1_5_COL: 100 * (home.over_1_5 + away.over_1_5) / total_games,
                        OVER_2_5_COL: 100 * (home.over_2_5 + away.over_2_5) / total_games,
                        F_COL: (home.goals_for + away.goals_for) / total_games,
                        A_COL: (home.goals_against + away.goals_against) / total_games,
                        GR_COL: (home.goals_for + home.goals_against + away.goals_for + away.goals_against) / total_games,
                    }
                else:
                    row = create_default_row(team)

            self.append(row)


    def __str__(self):
        return tabulate.tabulate(self.df, headers='keys', showindex=False, numalign="right", stralign="left")

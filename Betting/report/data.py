import dataclasses
import pathlib

import model.fixtures
import model.lineups
import model.players
import model.statistics
import model.teams


@dataclasses.dataclass(slots=True)
class GameState:
    won: list[model.teams.Team] = dataclasses.field(default_factory=list)
    drew: list[model.teams.Team] = dataclasses.field(default_factory=list)
    lost: list[model.teams.Team] = dataclasses.field(default_factory=list)

    def collect(self, score: model.fixtures.Scoreline, opposition: model.teams.Team):
        if model.fixtures.win(score):
            self.won.append(opposition)
        elif model.fixtures.draw(score):
            self.drew.append(opposition)
        else:
            self.lost.append(opposition)


@dataclasses.dataclass(slots=True)
class GoalData:
    total: int = 0
    goals_0: list[model.teams.Team] = dataclasses.field(default_factory=list)
    goals_1: list[model.teams.Team] = dataclasses.field(default_factory=list)
    goals_2: list[model.teams.Team] = dataclasses.field(default_factory=list)
    goals_3_or_more: list[model.teams.Team] = dataclasses.field(default_factory=list)

    def collect(self, goals: int, opposition: model.teams.Team):
        self.total += goals
        if goals == 0:
            self.goals_0.append(opposition)
        elif goals == 1:
            self.goals_1.append(opposition)
        elif goals == 2:
            self.goals_2.append(opposition)
        else:
            self.goals_3_or_more.append(opposition)

    def at_least(self, goals: int) -> int:
        if goals == 0:
            lists = [self.goals_0, self.goals_1, self.goals_2, self.goals_3_or_more]
        elif goals == 1:
            lists = [self.goals_1, self.goals_2, self.goals_3_or_more]
        elif goals == 2:
            lists = [self.goals_2, self.goals_3_or_more]
        else:
            lists = [self.goals_3_or_more]
        return sum(len(l) for l in lists)

    def at_most(self, goals: int) -> int:
        if goals == 0:
            lists = [self.goals_0]
        elif goals == 1:
            lists = [self.goals_0, self.goals_1]
        elif goals == 2:
            lists = [self.goals_0, self.goals_1, self.goals_2]
        else:
            lists = [self.goals_0, self.goals_1, self.goals_2, self.goals_3_or_more]
        return sum(len(l) for l in lists)


@dataclasses.dataclass(slots=True)
class GoalContributions:
    goals: int = 0
    assists: int = 0


@dataclasses.dataclass(slots=True)
class PlayerData:
    contributions: dict[model.players.Player, GoalContributions] = dataclasses.field(default_factory=dict)

    def collect(self, player: model.players.Player, player_stats: model.statistics.PlayerStats):
        if player not in self.contributions:
            self.contributions[player] = GoalContributions()

        contributions = self.contributions[player]
        contributions.goals += player_stats.goals
        contributions.assists += player_stats.assists


@dataclasses.dataclass(slots=True)
class TeamData:
    team: model.teams.Team
    unknown_scores: int = 0
    won: list[model.teams.Team] = dataclasses.field(default_factory=list)
    drawn: list[model.teams.Team] = dataclasses.field(default_factory=list)
    lost: list[model.teams.Team] = dataclasses.field(default_factory=list)
    bts: list[model.teams.Team] = dataclasses.field(default_factory=list)
    nts: list[model.teams.Team] = dataclasses.field(default_factory=list)
    goals_for: GoalData = dataclasses.field(default_factory=GoalData)
    goals_against: GoalData = dataclasses.field(default_factory=GoalData)
    total_goals: GoalData = dataclasses.field(default_factory=GoalData)
    half_time_winning: GameState = dataclasses.field(default_factory=GameState)
    half_time_drawing: GameState = dataclasses.field(default_factory=GameState)
    half_time_losing: GameState = dataclasses.field(default_factory=GameState)
    half_time_0_goals: GoalData = dataclasses.field(default_factory=GoalData)
    half_time_1_goal: GoalData = dataclasses.field(default_factory=GoalData)
    half_time_2_goals: GoalData = dataclasses.field(default_factory=GoalData)
    player_data: PlayerData = dataclasses.field(default_factory=PlayerData)

    def played(self) -> int:
        return len(self.won) + len(self.drawn) + len(self.lost)

    def points(self) -> int:
        return 3 * len(self.won) + len(self.drawn)

    def goal_rate(self) -> float:
        return (self.goals_for.total + self.goals_against.total) / self.played()

    def categorise_ht_ft_transitions(self, fixture: model.fixtures.Fixture, opposition: model.teams.Team):
        half_time_score = fixture.result(model.fixtures.Period.FIRST)
        full_time_score = fixture.result(model.fixtures.Period.FULL)
        if half_time_score is not None and full_time_score is not None:
            half_time_score = model.fixtures.canonicalise_scoreline(fixture, self.team, half_time_score)
            full_time_score = model.fixtures.canonicalise_scoreline(fixture, self.team, full_time_score)

            if model.fixtures.win(half_time_score):
                self.half_time_winning.collect(full_time_score, opposition)
            elif model.fixtures.draw(half_time_score):
                self.half_time_drawing.collect(full_time_score, opposition)
            else:
                self.half_time_losing.collect(full_time_score, opposition)

            half_time_goals = half_time_score.left + half_time_score.right
            full_time_goals = full_time_score.left + full_time_score.right
            if half_time_goals == 0:
                self.half_time_0_goals.collect(full_time_goals, opposition)
            elif half_time_goals == 1:
                self.half_time_1_goal.collect(full_time_goals - half_time_goals, opposition)
            else:
                self.half_time_2_goals.collect(full_time_goals - half_time_goals, opposition)

    def tally_individual_contributions(
            self,
            database: pathlib.Path,
            fixture: model.fixtures.Fixture
    ):
        lineup = model.lineups.load_lineup(database, fixture, self.team)
        if lineup is not None:
            for player_id in lineup.starting_11 + lineup.substitutes:
                player = model.players.load_player(player_id)
                if player:
                    player_stats = model.statistics.load_player_stats(database, fixture, player)
                    if player_stats:
                        self.player_data.collect(player, player_stats)

    def summarise_period_outcome(
            self,
            fixture: model.fixtures.Fixture,
            period: model.fixtures.Period,
            opposition: model.teams.Team
    ):
        score = fixture.result(period)
        if score is None:
            self.unknown_scores += 1
        else:
            score = model.fixtures.canonicalise_scoreline(fixture, self.team, score)
            if model.fixtures.win(score):
                self.won.append(opposition)
            elif model.fixtures.draw(score):
                self.drawn.append(opposition)
            else:
                self.lost.append(opposition)

            if score.left > 0 and score.right > 0:
                self.bts.append(opposition)

            if score.left == 0 and score.right == 0:
                self.nts.append(opposition)

            self.goals_for.collect(score.left, opposition)
            self.goals_against.collect(score.right, opposition)
            self.total_goals.collect(score.left + score.right, opposition)

    def collect(
            self,
            database: pathlib.Path,
            fixture: model.fixtures.Fixture,
            period: model.fixtures.Period,
            venue: model.fixtures.Venue
    ):
        if venue == model.fixtures.Venue.ANYWHERE:
            right_venue = True
        elif venue == model.fixtures.Venue.HOME and fixture.home_team == self.team:
            right_venue = True
        elif venue == model.fixtures.Venue.AWAY and fixture.away_team == self.team:
            right_venue = True
        else:
            right_venue = False

        if right_venue:
            if fixture.home_team == self.team:
                opposition = fixture.away_team
            else:
                opposition = fixture.home_team

            self.categorise_ht_ft_transitions(fixture, opposition)
            self.tally_individual_contributions(database, fixture)
            self.summarise_period_outcome(fixture, period, opposition)


@dataclasses.dataclass(slots=True, frozen=True)
class LeagueTable:
    col_goals_for_not_0: str
    col_goals_against_not_0: str
    col_team: str = "Team"
    col_played: str = "P"
    col_won: str = "W"
    col_drawn: str = "D"
    col_lost: str = "L"
    col_goals_for: str = "F"
    col_goals_against: str = "A"
    col_points: str = "PTS"
    col_goals_for_0: str = "F 0"
    col_goals_against_0: str = "A 0"
    col_bts: str = "BTS"
    col_nts: str = "NTS"
    col_goal_rate: str = "AGR"

    def columns(self) -> list[str]:
        return [
            self.col_team,
            self.col_played,
            self.col_won,
            self.col_drawn,
            self.col_lost,
            self.col_goals_for,
            self.col_goals_against,
            self.col_points,
            self.col_goals_for_0,
            self.col_goals_against_0,
            self.col_goals_for_not_0,
            self.col_goals_against_not_0,
            self.col_bts,
            self.col_nts,
            self.col_goal_rate
        ]

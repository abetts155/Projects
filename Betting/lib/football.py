import collections
import tabulate
import enum

from lib.betting import utils


class Result(enum.Enum):
    WIN = 'W'
    DRAW = 'D'
    LOSS = 'L'


class Fixture:
    HOME_GAME = 'H'
    AWAY_GAME = 'A'
       
    @staticmethod
    def get_home_and_away_goals(score):
        goals = score.split(':')
        return utils.parse_int(goals[0]), utils.parse_int(goals[1])

    @staticmethod
    def compute_match_outcome(score, venue):
        home_goals, away_goals = Fixture.get_home_and_away_goals(score)
        if venue == Fixture.HOME_GAME:
            if home_goals > away_goals:
                return Result.WIN
            elif home_goals == away_goals:
                return Result.DRAW
            else:
                return Result.LOSS
        else:
            if home_goals > away_goals:
                return Result.LOSS
            elif home_goals == away_goals:
                return Result.DRAW
            else:
                return Result.WIN
    
    def __init__(self, score, opponent, venue):
        self.__score = score
        self.__opponent = opponent
        self.__venue = venue

    @property
    def score(self):
        return self.__score

    @property
    def opponent(self):
        return self.__opponent

    @property
    def venue(self):
        return self.__venue


class Team(list):
    def __init__(self, name, id_, season):
        list.__init__(self)
        self.__name = name
        self.__id = id_
        self.__season = season

    @property
    def name(self):
        return self.__name

    @property
    def id_(self):
        return self.__id
    
    def event_outcomes(self, func, venues):
        home_away = []
        for _, f in enumerate(self, start=1):
            if f.venue in venues:
                home_away.append(func(f))
        return home_away
    
    def since_event(self, func, venues):
        sequence = []
        for s in reversed(self.event_outcomes(func, venues)):
            if not s:
                sequence.append(s)
            else:
                break
        sequence.reverse()
        return sequence

    def get_record(self, venue):
        wins = draws = losses = 0
        goals_for = goals_against = 0
        for _, fixture in enumerate(self, start=1):
            if fixture.score is not None:
                if fixture.venue == venue:
                    match_outcome = Fixture.compute_match_outcome(fixture.score,
                                                                  fixture.venue)
                    if match_outcome == Result.WIN:
                        wins += 1
                    elif match_outcome == Result.LOSS:
                        losses += 1
                    else:
                        draws += 1

                    home_goals, away_goals = Fixture.get_home_and_away_goals(fixture.score)
                    if venue == Fixture.HOME_GAME:
                        goals_for += home_goals
                        goals_against += away_goals
                    else:
                        goals_for += away_goals
                        goals_against += home_goals
        return wins, draws, losses, goals_for, goals_against

    def get_points(self):
        points = 0
        for f in self:
            match_outcome = Fixture.compute_match_outcome(f.score,f.venue)
            if match_outcome == Result.WIN:
                points += 3
            elif match_outcome == Result.DRAW:
                points += 1
        return points
    
    def print_fixtures(self):
        # Work out column sizes
        col_1_length = 0
        col_2_length = 0
        for fixture in self:
            col_1_length = max(col_1_length, len(fixture.score))
            col_2_length = max(col_2_length, len(fixture.opponent))        
        # Print the table
        for matchday, fixture in enumerate(self, start=1):
            print('{}{}{}{}{}'.format(fixture.score, 
                                      ' ' * (1 + col_1_length - len(fixture.score)), 
                                      fixture.opponent, 
                                      ' ' * (1 + col_2_length - len(fixture.opponent)), 
                                      fixture.venue))

    def print_record(self, venue):
        wins, draws, losses, goals_for, goals_against = self.get_record(venue)
        print('Record ({0}) W:{1} D:{2} L:{3} F:{4} A:{5}'.format(venue,
                                                                  wins, 
                                                                  draws, 
                                                                  losses,
                                                                  goals_for,
                                                                  goals_against))
        
    def print_season_record(self):
        header = '{}: {}'.format(self.__season, self.__name)
        print('-' * len(header))
        print(header)
        print('-' * len(header))
        self.print_fixtures()
        self.print_record(Fixture.HOME_GAME)
        self.print_record(Fixture.AWAY_GAME)

    def __str__(self):
        return self.__name

    def __hash__(self):
        return self.__id

    def __eq__(self, other):
        if isinstance(other, self.__class__):
            return self.__id == other.id_
        return NotImplemented
    

class LeagueTable:
    def __init__(self, season, teams):
        self._season = season
        self._order = []
        self.__create(teams)

    def __create(self, teams):
        LeagueTableRow = collections.namedtuple('LeagueTableRow',
                                                'TEAM P HW HD HL HF HA AW AD AL AF AA TW TD TL TF TA PTS')

        games_played = {}
        for team in teams:
            home_record = team.get_record(Fixture.HOME_GAME)
            away_record = team.get_record(Fixture.AWAY_GAME)
            total_points = ((3 * home_record[0]) + 
                            (3 * away_record[0]) + 
                            home_record[1] + 
                            away_record[1])
            games_played[team] = (home_record[0] + home_record[1] + home_record[2] +
                                  away_record[0] + away_record[1] + away_record[2])

            self._order.append(LeagueTableRow(team,
                                              games_played[team], 
                                              home_record[0], 
                                              home_record[1], 
                                              home_record[2], 
                                              home_record[3], 
                                              home_record[4],
                                              away_record[0], 
                                              away_record[1], 
                                              away_record[2], 
                                              away_record[3], 
                                              away_record[4],
                                              home_record[0] + away_record[0],
                                              home_record[1] + away_record[1],
                                              home_record[2] + away_record[2],
                                              home_record[3] + away_record[3],
                                              home_record[4] + away_record[4],
                                              total_points))
        self._order.sort(key=lambda row: row[-1], reverse=True)
    
    def get_position(self, name):
        for pos, row in enumerate(self._order, start=1):
            if row.TEAM.name == name:
                return pos
        return None

    def get_team(self, pos):
        try:
            return self._order[pos-1][0]
        except IndexError:
            utils.exit_message('No position {} in league table'.format(pos))

    def __getitem__(self, item):
        return self._order[item]
        
    def __len__(self):
        return len(self._order)
    
    def __iter__(self):
        return enumerate(self._order, start=1)
    
    def __str__(self):
        team = 'Team'
        played = 'P'
        goals_for = 'F'
        goal_against = 'A'
        points = 'PTS'
        table_header = [team, played,
                        Result.WIN.value, Result.DRAW.value, Result.LOSS.value, goals_for, goal_against,
                        Result.WIN.value, Result.DRAW.value, Result.LOSS.value, goals_for, goal_against,
                        Result.WIN.value, Result.DRAW.value, Result.LOSS.value, goals_for, goal_against,
                        points]
        return '>>> Season {}\n{}'.format(self._season,
                                          tabulate.tabulate(self._order, 
                                                            headers=table_header))

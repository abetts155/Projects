from collections import namedtuple


class LeagueTable:
    def __init__(self, season, teams):
        self._season = season
        self._order = []
        self.__create(teams)

    def __create(self, teams):
        LeagueTableRow = namedtuple('LeagueTableRow', 'TEAM P HW HD HL HF HA AW AD AL AF AA TW TD TL TF TA PTS')

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
            return self._order[pos - 1][0]
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
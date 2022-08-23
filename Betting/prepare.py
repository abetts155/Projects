from collections import OrderedDict
from datetime import datetime
from re import compile

sequence_header_re = compile(r'Analysing sequences in (.*)')
next_match_re = compile(r'Next match: (\d{4}-\d{2}-\d{2}) (\d{2}).(\d{2}) (.*) vs. (.*)$')
event_re = compile(r'>>>>>>>>>> (.*)')


class Game:
    __slots__ = ['league', 'date', 'hour', 'minutes', 'home_team', 'away_team', 'events']

    def __init__(self, league, date, hour, minutes, home_team, away_team):
        self.league = league
        self.date = date
        self.hour = int(hour)
        self.minutes = int(minutes)
        self.home_team = home_team
        self.away_team = away_team
        self.events = []

    def __str__(self):
        header = '{} {:02d}.{:02d} {} vs. {}'.format(self.date,
                                                     self.hour,
                                                     self.minutes,
                                                     self.home_team,
                                                     self.away_team)
        return header


game_index = {}
with open('output.txt', 'r') as in_file:
    for line in in_file:
        sequence_header = sequence_header_re.match(line)
        next_match = next_match_re.match(line)
        event = event_re.match(line)

        if sequence_header:
            current_league = sequence_header.group(1)

        if next_match:
            date = next_match.group(1)
            hour = next_match.group(2)
            minutes = next_match.group(3)
            home_team = next_match.group(4)
            away_team = next_match.group(5)

            key = (home_team, away_team)
            if key not in game_index:
                game_index[key] = Game(current_league, date, hour, minutes, home_team, away_team)
            current_game = game_index[key]

        if event:
            current_game.events.append(event.group(1))


game_times = OrderedDict({hour: [] for hour in range(0, 24)})

for game in game_index.values():
    game_times[game.hour].append(game)


today = datetime.today()
with open('{:02d}_{:02d}_{:02d}_{:02d}.txt'.format(today.month, today.day, today.hour, today.minute), 'w') as out_file:
    for hour, games in game_times.items():
        if games:
            out_file.write('{}\nGames starting at {}\n{}'.format('-' * 80, hour, '-' * 80))
            out_file.write('\n')
            games.sort(key=lambda game: (game.minutes, game.league))
            last_league = None
            for game in games:
                if last_league is None or game.league != last_league:
                    last_league = game.league
                    out_file.write('>' * 10)
                    out_file.write(' ')
                    out_file.write(game.league)
                    out_file.write('\n')

                out_file.write(str(game))
                out_file.write('\n')
                out_file.write('\n'.join(game.events))
                out_file.write('\n')
                out_file.write('\n')

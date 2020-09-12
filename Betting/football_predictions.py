import sys
import collections
import itertools
import math
import argparse
import inspect

from lib import scraper
from lib import utils
from lib.football import Fixture, LeagueTable, Result
import lib.events


class Prediction:
    def __init__(self, event_func, analysis_func, venues, sequence, probability):
        self._event_func = event_func
        self._analysis_func = analysis_func
        self._venues = venues
        self._sequence = sequence
        self._probability = probability

    @property
    def event_func(self):
        return self._event_func

    @property
    def analysis_func(self):
        return self._analysis_func

    @property
    def venues(self):
        return self._venues
    
    @property
    def sequence(self):
        return self._sequence

    @property
    def probability(self):
        return self._probability

    def __repr__(self):
        return 'No {} @ {} in {}: Probability of event extending is {:.5f}, according to {}'.\
                format(self._event_func.__doc__, 
                       ' or '.join(v for v in self._venues),
                       len(self._sequence),
                       self._probability,
                       self._analysis_func.__doc__)


def count_frequencies_of_subsequence_lengths(subsequences):
    bins = {}
    for s in subsequences:
        bins.setdefault(len(s), []).append(s)

    data = []
    for length in range(1, max(bins.keys())+1):
        if length in bins:
            data.append(len(bins[length]))
        else:
            data.append(0)
    return data


def split_sequences(sequences):
    subsequences = []
    for s in sequences:
        split = [list(g) for k,g in itertools.groupby(s, lambda x: x) if not k]
        subsequences.extend(split)
    return subsequences
        

def positions_analysis(previous_seasons, highest_position, lowest_position, league_tables, func, venues):
    """data on a per-position analysis"""
    position_by_position = {}
    for season in previous_seasons:
        league_table = league_tables[season]
        for pos, row in league_table:
            if highest_position <= pos <= lowest_position:
                if pos not in position_by_position:
                    position_by_position[pos] = []
                position_by_position[pos].extend(row.TEAM.event_outcomes(func, venues))
    return position_by_position.values()
        
        
def seasons_analysis(previous_seasons, highest_position, lowest_position, league_tables, func, venues):
    """data on a per-season analysis"""
    season_by_season = {}
    for season in previous_seasons:
        season_by_season[season] = []
        league_table = league_tables[season]
        for pos, row in league_table:
            if highest_position <= pos <= lowest_position:
                season_by_season[season].extend(row.TEAM.event_outcomes(func, venues))
    return season_by_season.values()


def teams_analysis(previous_seasons, highest_position, lowest_position, league_tables, func, venues):
    """data on a per-team basis"""
    team_by_team = {}
    for season in previous_seasons:
        league_table = league_tables[season]
        for pos, row in league_table:
            if highest_position <= pos <= lowest_position:
                if row.TEAM not in team_by_team:
                    team_by_team[row.TEAM] = []
                team_by_team[row.TEAM].extend(row.TEAM.event_outcomes(func, venues)) 
    return team_by_team.values()
                    
                    
def make_team_prediction(team, season, previous_seasons, probability_threshold, highest_position, lowest_position,
                         tables, analysis_func, event_func, venues):
    sequences = analysis_func(previous_seasons, highest_position, lowest_position, tables, event_func, venues)
    subsequences = split_sequences(sequences)
    column = count_frequencies_of_subsequence_lengths(subsequences)
    predictions = []
    pos = tables[season].get_position(team.name)
    if highest_position <= pos <= lowest_position:
        sequence = team.since_event(event_func, venues)
        event_probability = float(sum(column[len(sequence):]))/sum(column)
        if event_probability <= probability_threshold:
            predictions.append(Prediction(event_func, analysis_func, venues, sequence, event_probability))
    return predictions


def league_table_analysis(table):
    def draw_breakdown(highest, lowest):
        valid_positions = [pos for pos, _ in table if highest <= pos <= lowest]
        draws = sum([row.HD + row.AD for pos, row in table if pos in valid_positions])
        percentage = 100 * (draws / (total_draws * 2.0))
        print('Draws = {} out of {} ({:.1f}%) (positions {} to {})'.
              format(draws, total_draws * 2, percentage,
                     min(valid_positions),
                     max(valid_positions)))

    print(table)
    print('-' * 80)
    total_midtable_teams = math.floor(len(table)* (2/3))
    total_draws = sum([row.HD for _, row in table])
    print('Total draws = {}'.format(total_draws))

    if total_midtable_teams % 2 == 0:
        remaining = (len(table) - total_midtable_teams)/2
    else:
        remaining = math.ceil((len(table) - total_midtable_teams) / 2)

    top_range = (1, remaining)
    mid_range = (remaining + 1, len(table) - remaining)
    bot_range = (len(table) - remaining + 1, len(table))

    draw_breakdown(*top_range)
    draw_breakdown(*mid_range)
    draw_breakdown(*bot_range)

    draws = []
    for pos, row in table:
        for fixture in row.TEAM:
            if (fixture.venue == fixture.HOME_GAME and
                        Fixture.compute_match_outcome(fixture.score, fixture.venue) == Result.DRAW):
                draws.append([row.TEAM.name, fixture.opponent.lower(), fixture.score])


def make_predictions(events, season, previous_seasons, probability_threshold, highest_position, lowest_position,
                     tables, teams):
    predictions = {team: [] for team in teams}
    for venues in [[Fixture.HOME_GAME, Fixture.AWAY_GAME], [Fixture.HOME_GAME], [Fixture.AWAY_GAME]]:
        for f_name in events:
            for team in teams:
                predictions[team].extend(make_team_prediction(team,
                                                              season,
                                                              previous_seasons,
                                                              probability_threshold,
                                                              highest_position,
                                                              lowest_position,
                                                              tables,
                                                              positions_analysis,
                                                              getattr(lib.betting.events, f_name),
                                                              venues))

                predictions[team].extend(make_team_prediction(team,
                                                              season,
                                                              previous_seasons,
                                                              probability_threshold,
                                                              highest_position,
                                                              lowest_position,
                                                              tables,
                                                              seasons_analysis,
                                                              getattr(lib.betting.events, f_name),
                                                              venues))

                predictions[team].extend(make_team_prediction(team,
                                                              season,
                                                              previous_seasons,
                                                              probability_threshold,
                                                              highest_position,
                                                              lowest_position,
                                                              tables,
                                                              teams_analysis,
                                                              getattr(lib.betting.events, f_name),
                                                              venues))

    return predictions


def compute_league_tables(league, previous_seasons):
    tables = collections.OrderedDict()
    for season in previous_seasons:
        tables[season] = LeagueTable(season, scraper.collect_data(league, season))
        league_table_analysis(tables[season])
    return tables


def main(**kwargs):
    previous_seasons = [kwargs['season'] - i for i in reversed(range(1, kwargs['history'] + 1))]
    tables = compute_league_tables(kwargs['league'], previous_seasons)
    
    teams = scraper.collect_data(kwargs['league'], kwargs['season'])
    tables[kwargs['season']] = LeagueTable(kwargs['season'], teams)
    league_table_analysis(tables[kwargs['season']])
    
    all_predictions = make_predictions(kwargs['events'], kwargs['season'], previous_seasons, kwargs['probability'],
                                       kwargs['highest_position'], kwargs['lowest_position'], tables, teams)

    for team, predictions in all_predictions.items():
        if predictions:
            team.print_season_record()
            print('\n'.join(str(p) for p in predictions))

    if kwargs['replay']:
        replay_season(all_predictions)
    elif kwargs['season'] == utils.this_season():
        return 0
        for team in all_predictions.keys():
            if predictions[team]:
                refined_predictions = refine_analysis(team, 
                                                      previous_seasons, 
                                                      tables)
                if refined_predictions:
                    team.print_season_record()
                    print('\n'.join(str(p) for p in predictions[team]))
                    print('>>> Refining analysis, concentrating on table between '
                          'positions {} and {}'.format(kwargs['highest_position'],
                                                       kwargs['lowest_position']))
                
                    print('\n'.join(str(p) for p in refined_predictions))

    return 0


def parse_command_line():
    parser = argparse.ArgumentParser(formatter_class=argparse.RawTextHelpFormatter,
                                     description=
                                     'Football betting data analysis')

    parser.add_argument('--history',
                        type=int,
                        help='pick number of previous seasons to base analysis upon',
                        metavar='<INT>')

    parser.add_argument('--season',
                        type=int,
                        help='pick season to make football_predictions on, using the'
                             ' start year of the season (e.g., 2016 for 2016/2017)',
                        metavar='<INT>',
                        default=utils.this_season())

    parser.add_argument('--matchday',
                        type=int,
                        help='pick a matchday where the analysis is performed',
                        metavar='<INT>')

    parser.add_argument('--events',
                        choices=['{}'.format(f[0]) for f in inspect.getmembers(lib.events, inspect.isfunction)],
                        type=str.lower,
                        nargs='+',
                        help='choose betting events')

    parser.add_argument('-HP',
                        dest='highest_position',
                        type=int,
                        help='when collecting data, only use teams that finished'
                             ' at or BELOW this position',
                        metavar='<INT>',
                        default=0)

    parser.add_argument('-LP',
                        dest='lowest_position',
                        type=int,
                        help='when collecting data, only use teams that finished'
                             ' at or ABOVE this position',
                        metavar='<INT>',
                        default=10**6)

    parser.add_argument('--probability',
                        type=float,
                        help='choose a probability threshold that a prediction'
                             ' must satisfy to be considered',
                        metavar='<FLOAT>',
                        required=True)

    parser.add_argument('-R',
                        '--replay',
                        action='store_true',
                        help='replay the season to see how football predictions turn out',
                        default=False)

    league_choices = collections.OrderedDict()
    with open('leagues.txt', 'r') as the_file:
        for line in the_file.readlines():
            lexemes = line.split('=')
            code = lexemes[0].strip()
            description = lexemes[1].strip()
            league_choices[code] = description

    parser.add_argument('--league',
                        choices=league_choices.keys(),
                        type=str.upper,
                        required=True,
                        help='choose league:\n{}'.format('\n'.join(['%s : %s' % (key, value)
                                                                    for (key, value) in league_choices.items()])),
                        metavar='<CODE>')

    return vars(parser.parse_args())
       

if __name__ == '__main__':
    args = parse_command_line()
    sys.exit(main(**args))


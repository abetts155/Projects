from argparse import Namespace
from re import compile
from typing import Dict

from PySimpleGUI import theme, PopupError, PopupAutoClose, Window, WIN_CLOSED

import head_to_head
import show_sequences
import show_matrix
import show_season_sequences
import show_projection
import show_form
import show_team
import show_heatmap
import show_season_summary
import show_goal_events

from gui.widgets import (aggregated_sequences_submit,
                         betting_file,
                         betting_text,
                         chunks_choice,
                         country_choice,
                         event_choice,
                         event_clear,
                         event_matrix_submit,
                         event_negation,
                         expression_text,
                         half_both,
                         half_first,
                         half_second,
                         half_separate,
                         heatmap_analysis,
                         heatmap_submit,
                         history_choice,
                         h2h_submit,
                         league_analysis_submit,
                         league_choice,
                         make_window,
                         performance_analysis_submit,
                         performance_average,
                         performance_individual,
                         performance_relative,
                         performance_positions,
                         performance_positions_choice,
                         performance_relative_choice,
                         result_text,
                         season_sequences_submit,
                         team_analysis_game_states,
                         team_analysis_form,
                         team_analysis_goals,
                         team_analysis_submit,
                         team_analysis_summary,
                         team_choice_one,
                         team_choice_two,
                         team_radio_one,
                         team_radio_two,
                         venue_away,
                         venue_home)
from lib import messages
from model.fixtures import Half, Venue, win, loss, draw, bts
from model.leagues import League, get_league_code, league_register, uglify
from model.seasons import Season
from model.tables import LeagueTable
from model.teams import Team
from sql.sql import extract_picked_team, get_team_fixtures, load_league, load_teams

messages.warnings = False
database = 'football.db'


def get_league(values: Dict):
    return League(values[country_choice.Key], values[league_choice.Key])


def set_venue(values: Dict, args: Namespace):
    if values[venue_home.Key]:
        args.venue = Venue.home
    elif values[venue_away.Key]:
        args.venue = Venue.away
    else:
        args.venue = Venue.any


def set_half(values: Dict, args: Namespace):
    if values[half_first.Key]:
        args.half = Half.first
    elif values[half_second.Key]:
        args.half = Half.second
    elif values[half_both.Key]:
        args.half = Half.both
    elif values[half_separate.Key]:
        args.half = Half.separate
    else:
        assert False


def set_history(values: Dict, args: Namespace):
    args.history = int(values[history_choice.Key])


def set_chunks(values: Dict, args: Namespace):
    if values[chunks_choice.Key]:
        args.chunks = int(values[chunks_choice.Key])
    else:
        args.chunks = None


def get_event(values: Dict) -> str:
    goals_re = compile(r'\s*(gfa|gf|ga)\s*(>=|<=|>|<|==|!=)\s*(\d+)\s*')
    match = goals_re.match(values[event_choice.Key])
    if match:
        infix = match.group(2)
        if infix == '>=':
            infix = 'ge'
        elif infix == '<=':
            infix = 'le'
        elif infix == '>':
            infix = 'gt'
        elif infix == '<':
            infix = 'lt'
        elif infix == '==':
            infix = 'eq'
        elif infix == '!=':
            infix = 'ne'

        return '{}_{}_{}'.format(match.group(1), infix, match.group(3))

    other_re = compile(r'\s*({}|{}|{}|{})\s*'.format(draw.__name__, win.__name__, loss.__name__, bts.__name__))
    match = other_re.match(values[event_choice.Key])
    if match:
        return match.group(1)

    if values[event_choice.Key] == 'won':
        event_choice.update('win')
        PopupAutoClose("Fixing event 'won' to 'win'",
                       title='',
                       auto_close=True,
                       auto_close_duration=2,
                       non_blocking=True)
    elif values[event_choice.Key] == 'lost':
        event_choice.update('loss')
        PopupAutoClose("Fixing event 'lost' to 'loss'",
                       title='',
                       auto_close=True,
                       auto_close_duration=2,
                       non_blocking=True)
    elif values[event_choice.Key] == 'drew':
        event_choice.update('draw')
        PopupAutoClose("Fixing event 'drew' to 'draw'",
                       title='',
                       auto_close=True,
                       auto_close_duration=2,
                       non_blocking=True)
    else:
        PopupError("Do not recognise '{}' as an event".format(values[event_choice.Key].strip()),
                   title='',
                   auto_close=True,
                   auto_close_duration=2,
                   non_blocking=True)
    return ''


def team_one_is_valid(values: Dict) -> str:
    if values[team_radio_one.Key] and values[team_choice_one.Key]:
        return values[team_choice_one.Key]
    return ''


def team_two_is_valid(values: Dict) -> str:
    if values[team_radio_two.Key] and values[team_choice_two.Key]:
        return values[team_choice_two.Key]
    return ''


def team_selected(values: Dict) -> str:
    team_one = team_one_is_valid(values)
    if team_one:
        return team_one

    team_two = team_two_is_valid(values)
    if team_two:
        return team_two

    return ''


def get_league(values: Dict) -> League:
    return League(uglify(values[country_choice.Key]), values[league_choice.Key])


def set_league(values: Dict, args: Namespace):
    if values[league_choice.Key]:
        league = get_league(values)
        args.league = [get_league_code(league)]
    else:
        args.league = None


def run_head_to_head(values: Dict):
    if values[team_choice_one.Key] and values[team_choice_two.Key]:
        args = Namespace()
        args.database = database
        args.block = False
        set_league(values, args)
        args.team = '{}:{}'.format(values[team_choice_one.Key], values[team_choice_two.Key])
        head_to_head.main(args)


def run_sequences(values: Dict, event):
    valid_event = get_event(values)
    if values[league_choice.Key] and valid_event:
        args = Namespace()
        args.database = database
        args.block = False
        args.event = [valid_event]
        args.negate = values[event_negation.Key]
        args.team = team_selected(values)
        set_league(values, args)
        set_history(values, args)
        set_venue(values, args)
        set_half(values, args)
        args.lines = None

        if event == aggregated_sequences_submit.Key:
            set_chunks(values, args)
            show_sequences.main(args)
        else:
            show_season_sequences.main(args)


def run_team_analysis(values: Dict):
    if values[league_choice.Key]:
        if team_one_is_valid(values) or team_two_is_valid(values):
            args = Namespace()
            args.database = database
            args.block = False
            args.team = team_selected(values)
            set_league(values, args)
            set_history(values, args)
            set_venue(values, args)
            set_half(values, args)

            if values[team_analysis_summary.Key]:
                if values[team_analysis_game_states.Key]:
                    args.game_states = values[team_analysis_game_states.Key].split()
                else:
                    args.game_states = None

                args.averages = None
                show_team.main(args)
            elif values[team_analysis_form.Key]:
                show_form.main(args)
            elif values[team_analysis_goals.Key]:
                args.intervals = 3
                show_goal_events.main(args)


def run_performance_analysis(values: Dict):
    if values[league_choice.Key]:
        selected_team = team_selected(values)
        if selected_team:
            args = Namespace()
            args.database = database
            args.block = False
            args.team = selected_team
            set_league(values, args)
            set_history(values, args)
            set_venue(values, args)
            set_half(values, args)

            args.average = None
            args.relative = None
            args.position = None

            if values[performance_individual.Key]:
                show_projection.main(args)
            elif values[performance_average.Key]:
                args.average = True
                show_projection.main(args)
            elif values[performance_positions.Key] and values[performance_positions_choice.Key]:
                positions = values[performance_positions_choice.Key].strip().split(' ')

                args.position = []
                for value in positions:
                    try:
                        args.position.append(int(value))
                    except ValueError:
                        pass

                show_projection.main(args)
            elif values[performance_relative.Key] and values[performance_relative_choice.Key]:
                args.relative = '_'.join(values[performance_relative_choice.Key][0].split())
                show_projection.main(args)


def run_heatmap_analysis(values: Dict):
    if values[league_choice.Key] and values[heatmap_analysis.Key] and values[chunks_choice.Key]:
        args = Namespace()
        args.database = database
        args.block = False
        set_league(values, args)
        set_history(values, args)
        set_venue(values, args)
        set_half(values, args)
        set_chunks(values, args)
        args.analysis = show_heatmap.Analysis.from_string(values[heatmap_analysis.Key])
        show_heatmap.main(args)


def run_event_matrix_analysis(values: Dict):
    if values[league_choice.Key] and values[event_choice.Key] and values[chunks_choice.Key]:
        args = Namespace()
        args.database = database
        args.block = False
        args.event = [get_event(values)]
        args.negate = values[event_negation.Key]
        set_league(values, args)
        set_history(values, args)
        set_venue(values, args)
        set_half(values, args)
        set_chunks(values, args)
        if args.event in [win, loss]:
            args.symmetry = False
        else:
            args.symmetry = True
        show_matrix.main(args)


def run_league_analysis(values: Dict):
    if values[league_choice.Key]:
        args = Namespace()
        args.database = database
        args.block = False
        set_league(values, args)
        set_half(values, args)
        set_venue(values, args)
        args.team = team_selected(values)
        args.history = None
        show_season_summary.main(args)


def run_expression_evaluation(values: Dict):
    try:
        answer = eval(values[expression_text.Key])
        result_text.update('{:.5f}'.format(answer))
    except (NameError, SyntaxError, TypeError, ValueError, ZeroDivisionError):
        result_text.update('')


def update_team_choices(values: Dict):
    league = get_league(values)
    load_league(database, league)
    league_seasons = Season.seasons(league)
    if league_seasons:
        this_season = league_seasons[-1]
        candidates = ['']
        width = 0
        for team in this_season.teams():
            candidates.append(team.name)
            width = max(width, len(candidates[-1]))

        candidates.sort()
        team_choice_one.set_size([width, None])
        team_choice_two.set_size([width, None])
        team_choice_one.update(values=candidates, set_to_index=0)
        team_choice_two.update(values=candidates, set_to_index=0)


def update_league_choices(values: Dict):
    candidates = []
    width = 0
    for league in league_register.values():
        if league.country == uglify(values[country_choice.Key]):
            candidates.append(league.name)
            width = max(width, len(candidates[-1]))
    league_choice.set_size([width, None])
    league_choice.update(values=candidates, set_to_index=0)
    values[league_choice.Key] = candidates[0]


def update_other_team(values: Dict, team_choice):
    league = get_league(values)
    league_seasons = Season.seasons(league)
    if league_seasons:
        (row,) = extract_picked_team(database, values[team_choice.Key], league)
        team = Team.inventory[row[0]]
        fixtures = get_team_fixtures(database, league_seasons[-1], team)
        if fixtures:
            next_match = fixtures[0]

            if next_match.home_team.name == values[team_choice.Key]:
                team_radio_one.update(True)
            else:
                team_radio_two.update(True)

            if team == next_match.home_team:
                team_choice_one.update(value=team.name)
                team_choice_two.update(value=next_match.away_team.name)
            else:
                team_choice_one.update(value=next_match.home_team.name)
                team_choice_two.update(value=team.name)


def update_history_bar(values: Dict):
    league = get_league(values)
    history = len(Season.seasons(league))
    history_choice.update(range=(2, history))
    history_choice.update(value=history)
    history_choice.update(disabled=False)


def update_chunk_bar(values: Dict):
    league = get_league(values)
    history = Season.seasons(league)
    table = LeagueTable(history[-1], Half.both)
    divisors = [i for i in range(2, len(table) + 1) if len(table) % i == 0]
    chunks_choice.update(values=[''] + divisors, set_to_index=0)
    chunks_choice.update(disabled=False)
    chunks_choice.set_size([2, None])


def run(window: Window):
    while True:
        event, values = window.read()

        if event in [WIN_CLOSED, 'Exit']:
            break
        elif event == country_choice.Key:
            update_league_choices(values)
            update_team_choices(values)
            update_history_bar(values)
            update_chunk_bar(values)
        elif event == league_choice.Key:
            update_team_choices(values)
            update_history_bar(values)
            update_chunk_bar(values)
        elif event == team_choice_one.Key and values[team_choice_one.Key]:
            update_other_team(values, team_choice_one)
        elif event == team_choice_two.Key and values[team_choice_two.Key]:
            update_other_team(values, team_choice_two)
        elif event == event_clear.Key:
            event_choice.update('')
        elif event == h2h_submit.Key:
            run_head_to_head(values)
        elif event == team_analysis_submit.Key:
            run_team_analysis(values)
        elif event == performance_analysis_submit.Key:
            run_performance_analysis(values)
        elif event in [aggregated_sequences_submit.Key, season_sequences_submit.Key]:
            run_sequences(values, event)
        elif event == heatmap_submit.Key:
            run_heatmap_analysis(values)
        elif event == event_matrix_submit.Key:
            run_event_matrix_analysis(values)
        elif event == league_analysis_submit.Key:
            run_league_analysis(values)
        elif event in [performance_individual.Key, performance_average.Key]:
            performance_positions_choice.update(disabled=True)
            performance_relative_choice.update(disabled=True)
        elif event == performance_positions.Key:
            performance_positions_choice.update(disabled=False)
            performance_relative_choice.update(disabled=True)
        elif event == performance_relative.Key:
            performance_positions_choice.update(disabled=True)
            performance_relative_choice.update(set_to_index=0, disabled=False)
        elif event == team_analysis_summary.Key:
            team_analysis_game_states.update(disabled=False)
        elif event in [team_analysis_form.Key, team_analysis_goals.Key]:
            team_analysis_game_states.update(disabled=True)
        elif event == expression_text.Key:
            run_expression_evaluation(values)
        elif event == betting_file.Key:
            text = ''
            with open(values[betting_file.Key], 'r') as in_file:
                for line in in_file:
                    text += line
            betting_text.update(value=text)


if __name__ == '__main__':
    theme('DarkBlue')
    load_teams(database)
    window = make_window()
    run(window)
    window.close()

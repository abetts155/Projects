from argparse import Namespace
from datetime import datetime
from typing import Dict

from PySimpleGUI import set_options, theme, Popup, Window, WIN_CLOSED

import head_to_head
import regression
import show_sequences
import show_season_sequences
import show_projection
import show_team
import show_season_summary
import show_goal_events
import show_history

from gui.widgets import (aggregated_sequences_submit,
                         chunks_choice,
                         country_choice,
                         win_event,
                         draw_event,
                         loss_event,
                         bts_event,
                         for_against_0_event,
                         for_against_1_event,
                         for_against_2_event,
                         for_0_event,
                         for_1_event,
                         against_0_event,
                         against_1_event,
                         evaluation_text,
                         event_negation,
                         expression_text,
                         history_choice,
                         h2h_submit,
                         league_choice,
                         make_window,
                         performance_analysis_submit,
                         performance_average,
                         performance_individual,
                         performance_relative,
                         performance_positions,
                         performance_positions_choice,
                         performance_relative_choice,
                         regression_submit,
                         result_first,
                         result_full,
                         result_second,
                         results_submit,
                         season_sequences_submit,
                         team_analysis_submit,
                         team_goals_submit,
                         team_history_submit,
                         team_choice_one,
                         team_choice_two,
                         team_clear_one,
                         team_clear_two,
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
from sql.sql import extract_picked_team, get_unfinished_matches, load_league, load_teams

messages.warnings = False
database = 'football.db'


def set_venue(values: Dict, args: Namespace):
    if values[venue_home.Key]:
        args.venue = Venue.home
    elif values[venue_away.Key]:
        args.venue = Venue.away
    else:
        args.venue = Venue.anywhere


def set_half(values: Dict, args: Namespace):
    args.half = []
    if values[result_first.Key]:
        args.half.append(Half.first)
    if values[result_second.Key]:
        args.half.append(Half.second)
    if values[result_full.Key]:
        args.half.append(Half.full)


def half_selected(values: Dict):
    if not values[result_first.Key] and not values[result_second.Key] and not values[result_full.Key]:
        Popup('No result selected', title='', auto_close=True, auto_close_duration=2, non_blocking=True)
        return False
    else:
        return True


def set_history(values: Dict, args: Namespace):
    args.history = int(values[history_choice.Key])


def set_chunks(values: Dict, args: Namespace):
    if values[chunks_choice.Key]:
        args.chunks = int(values[chunks_choice.Key])
    else:
        args.chunks = None


def get_event(values: Dict) -> str:
    if values[draw_event.Key]:
        return draw.__name__
    elif values[win_event.Key]:
        return win.__name__
    elif values[loss_event.Key]:
        return loss.__name__
    elif values[bts_event.Key]:
        return bts.__name__
    elif values[for_0_event.Key]:
        return 'gf_eq_0'
    elif values[for_1_event.Key]:
        return 'gf_le_1'
    elif values[against_0_event.Key]:
        return 'ga_eq_0'
    elif values[against_1_event.Key]:
        return 'ga_le_1'
    elif values[for_against_0_event.Key]:
        return 'gfa_eq_0'
    elif values[for_against_1_event.Key]:
        return 'gfa_le_1'
    elif values[for_against_2_event.Key]:
        return 'gfa_le_2'
    else:
        assert False


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
    for code, league in league_register.items():
        if league.country == uglify(values[country_choice.Key]) and league.name == values[league_choice.Key]:
            return league
    assert False


def set_league(values: Dict, args: Namespace):
    if values[league_choice.Key]:
        league = get_league(values)
        args.league = [league.code]
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
    if values[league_choice.Key] and half_selected(values):
        args = Namespace()
        args.database = database
        args.block = False
        args.event = [get_event(values)]
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
    if values[league_choice.Key] and team_selected(values):
        args = Namespace()
        args.database = database
        args.block = False
        args.team = team_selected(values)
        set_league(values, args)
        set_history(values, args)
        set_venue(values, args)
        args.game_states = []
        args.averages = None
        show_team.main(args)


def run_team_goals_analysis(values: Dict):
    if values[league_choice.Key] and team_selected(values):
        args = Namespace()
        args.database = database
        args.block = False
        args.team = team_selected(values)
        set_league(values, args)
        set_history(values, args)
        set_venue(values, args)
        args.intervals = 3
        show_goal_events.main(args)


def run_team_history(values: Dict):
    if values[league_choice.Key] and team_selected(values):
        args = Namespace()
        args.database = database
        args.block = False
        args.team = team_selected(values)
        set_league(values, args)
        show_history.main(args)


def run_performance_analysis(values: Dict):
    if values[league_choice.Key] and half_selected(values):
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


def run_results_analysis(values: Dict):
    if values[league_choice.Key] and half_selected(values):
        args = Namespace()
        args.database = database
        args.block = False
        set_league(values, args)
        set_half(values, args)
        set_venue(values, args)
        args.team = team_selected(values)
        args.history = None
        show_season_summary.main(args)


def run_regression_analysis(values: Dict):
    if values[league_choice.Key] and half_selected(values):
        args = Namespace()
        args.database = database
        set_league(values, args)
        set_half(values, args)
        args.country = None
        args.team = team_selected(values)
        args.no_colors = True
        set_history(values, args)
        buffer = regression.main(args)
        Popup(buffer, background_color='#add8e6', title='', non_blocking=True)


def run_expression_evaluation(values: Dict):
    try:
        answer = eval(values[expression_text.Key])
        evaluation_text.update('{:.5f}'.format(answer))
    except (NameError, SyntaxError, TypeError, ValueError, ZeroDivisionError):
        evaluation_text.update('')


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
        fixtures = get_unfinished_matches(database, league_seasons[-1], team)
        now = datetime.now()
        cutoff = datetime(now.year, now.month, now.day, now.hour - 2)
        fixtures = [fixture for fixture in fixtures
                    if datetime.fromisoformat(str(fixture.date)).replace(tzinfo=None) >= cutoff]
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
    table = LeagueTable(history[-1], [Half.full])
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
        elif event == team_clear_one.Key:
            team_choice_one.update(set_to_index=0)
        elif event == team_clear_two.Key:
            team_choice_two.update(set_to_index=0)
        elif event == h2h_submit.Key:
            run_head_to_head(values)
        elif event == team_analysis_submit.Key:
            run_team_analysis(values)
        elif event == team_goals_submit.Key:
            run_team_goals_analysis(values)
        elif event == team_history_submit.Key:
            run_team_history(values)
        elif event == performance_analysis_submit.Key:
            run_performance_analysis(values)
        elif event in [aggregated_sequences_submit.Key, season_sequences_submit.Key]:
            run_sequences(values, event)
        elif event == results_submit.Key:
            run_results_analysis(values)
        elif event == regression_submit.Key:
            run_regression_analysis(values)
        elif event in [performance_individual.Key, performance_average.Key]:
            performance_positions_choice.update(disabled=True)
            performance_relative_choice.update(disabled=True)
        elif event == performance_positions.Key:
            performance_positions_choice.update(disabled=False)
            performance_relative_choice.update(disabled=True)
        elif event == performance_relative.Key:
            performance_positions_choice.update(disabled=True)
            performance_relative_choice.update(set_to_index=0, disabled=False)
        elif event == expression_text.Key:
            run_expression_evaluation(values)


if __name__ == '__main__':
    theme('LightGrey3')
    set_options(font=('Open Sans', 12))
    load_teams(database)
    window = make_window()
    run(window)
    window.close()

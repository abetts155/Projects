from argparse import Namespace
from lib import messages
from model.fixtures import Half, Venue, win, loss, draw, bts
from model.leagues import League, league_register, get_league_code
from model.tables import Position
from PySimpleGUI import (InputText,
                         Text,
                         Submit,
                         Button,
                         Slider,
                         Checkbox,
                         Radio,
                         Listbox,
                         Window,
                         Combo,
                         PopupError,
                         theme,
                         WIN_CLOSED)
from re import compile
from sql.sql import extract_picked_team, load_teams
from typing import Dict, List

import head_to_head
import show_breakdown
import show_goal_events
import show_heatmap
import show_form
import show_matrix
import show_projection
import show_sequences
import show_season_sequences
import show_season_summary
import show_team

messages.warnings = False
database = 'football.db'
width = 10
height = 1
history_min = 1
history_max = 12
chunks_min = 0
chunks_max = 12
font = ('Helvetica', 14)
default_size = (width, height)

radio_venue = 1
radio_half = 2
radio_team_analysis = 3
radio_performance = 4
radio_team_choice = 5


def leagues() -> List[str]:
    options = []
    for key in list(league_register.keys()):
        league = league_register[key]
        options.append('{} {}'.format(league.country, league.name))
    return options


league_choice = Listbox(leagues(),
                        size=(34, 16),
                        font=font,
                        enable_events=True,
                        key='-LEAGUE-')

history_choice = Slider(range=(history_min, history_max),
                        default_value=history_max,
                        orientation='h',
                        font=font,
                        key='-HISTORY-')


team_radio_one = Radio('', radio_team_choice, font=font, default=True)
team_radio_two = Radio('', radio_team_choice, font=font)
team_choice_one = InputText(font=font, size=(30, 1), key='-TEAM1-')
team_choice_two = InputText(font=font, size=(30, 1), key='-TEAM2-')
team_clear_one = Button('Clear', button_color=('black', 'white'), font=font, key='-CLEAR-TEAM1-')
team_clear_two = Button('Clear', button_color=('black', 'white'), font=font, key='-CLEAR-TEAM2-')

venue_any = Radio(Venue.any.name.capitalize(), radio_venue, font=font, default=True)
venue_home = Radio(Venue.home.name.capitalize(), radio_venue, font=font)
venue_away = Radio(Venue.away.name.capitalize(), radio_venue, font=font)

half_both = Radio(Half.both.name.capitalize(), radio_half, font=font, default=True)
half_first = Radio(Half.first.name.capitalize(), radio_half, font=font)
half_second = Radio(Half.second.name.capitalize(), radio_half, font=font)
half_separate = Radio(Half.separate.name.capitalize(), radio_half, font=font)

chunks_choice = Slider(range=(chunks_min, chunks_max),
                       default_value=chunks_min,
                       orientation='h',
                       font=font,
                       key='-CHUNKS-')

event_choice = InputText(font=font, size=(16, 1), key='-EVENT-')
event_negation = Checkbox('Negate', font=font, key='-NEGATE-EVENT-')
event_clear = Button('Clear', button_color=('black', 'white'), font=font, key='-CLEAR-EVENT-')

aggregated_sequences_submit = Button('Aggregated sequences', font=font, key='-AGG-SEQ-SUBMIT-')
season_sequences_submit = Button('Per-season sequences', font=font, key='-PER-SEQ-SUBMIT-')


team_analysis_goals = Radio('Goals',
                            radio_team_analysis,
                            font=font,
                            key='-TEAM-ANALYSIS-GOALS-',
                            default=True,
                            enable_events=True)
team_analysis_form = Radio('Form',
                           radio_team_analysis,
                           font=font,
                           key='-TEAM-ANALYSIS-FORM-',
                           enable_events=True)
team_analysis_margins = Radio('Margins',
                              radio_team_analysis,
                              font=font,
                              key='-TEAM-ANALYSIS-MARGINS-',
                              enable_events=True)
team_analysis_summary = Radio('Summary',
                              radio_team_analysis,
                              font=font,
                              key='-TEAM-ANALYSIS-SUMMARY-',
                              enable_events=True)
team_analysis_game_states = InputText(font=font,
                                      key='-TEAM-ANALYSIS-GAME-STATES-',
                                      disabled=True)
team_analysis_submit = Submit(font=font, key='-TEAM-ANALYSIS-SUBMIT-')

h2h_submit = Button('Head to head', font=font, key='-H2H-SUBMIT-')

league_analysis_submit = Button('League Analysis', font=font, key='-LEAGUE-ANALYSIS-SUBMIT-')

heatmap_analysis = Combo([analysis.name.lower().capitalize() for analysis in show_heatmap.Analysis],
                         default_value=show_heatmap.Analysis.RESULT.name.lower().capitalize(),
                         key='-HEATMAP-')
heatmap_submit = Submit(font=font, key='-HEATMAP-SUBMIT-')

event_matrix_submit = Submit(font=font, key='-EVENT-MATRIX-SUBMIT-')

performance_individual = Radio('Individual', radio_performance, font=font, default=True, enable_events=True)
performance_average = Radio('Average', radio_performance, font=font, enable_events=True)
performance_positions = Radio('Absolute', radio_performance, font=font, enable_events=True)
performance_positions_choice = InputText(font=font,
                                         key='-PERFORMANCE-POSITIONS-CHOICE-',
                                         disabled=True,
                                         enable_events=True)
performance_relative = Radio('Relative', radio_performance, font=font, enable_events=True)
performance_relative_choice = Listbox(sorted([Position.pretty(position) for position in Position]),
                                      size=(20, 5),
                                      font=font,
                                      key='-PERFORMANCE-RELATIVE-CHOICE-',
                                      disabled=True,
                                      enable_events=True)
performance_analysis_submit = Submit(font=font, key='-PERFORMANCE-ANALYSIS-SUBMIT-')

expression = InputText(font=font, size=(16, 1), key='-EXPR-', enable_events=True)
equals = Text('=', font=font, justification='center', key='-EQUALS-')
result = Text(font=font, size=(10, 1), key='-RESULT-')


def make():
    divider = 105

    layout = [
        [Text('League', font=font, size=default_size), league_choice],
        [Text('Team #1', font=font, size=default_size), team_radio_one, team_choice_one, team_clear_one],
        [Text('Team #2', font=font, size=default_size), team_radio_two, team_choice_two, team_clear_two],
        [Text('History', font=font, size=default_size), history_choice],
        [Text('Event', font=font, size=default_size), event_choice, event_negation, event_clear],
        [Text('Venue', font=font, size=default_size), venue_any, venue_home, venue_away],
        [Text('Half', font=font, size=default_size), half_both, half_first, half_second, half_separate],
        [Text('Chunks', font=font, size=default_size), chunks_choice],
        [Text('_' * divider)],
        [aggregated_sequences_submit, season_sequences_submit, h2h_submit, league_analysis_submit],
        [Text('_' * divider)],
        [Text('Individual team analysis', font=font)],
        [Text('Analysis', font=font, size=default_size),
         team_analysis_goals,
         team_analysis_form,
         team_analysis_margins,
         team_analysis_summary],
        [Text('Game states', font=font, size=default_size), team_analysis_game_states],
        [team_analysis_submit],
        [Text('_' * divider)],
        [Text('Performance analysis', font=font)],
        [Text('Type', font=font, size=default_size),
         performance_individual, performance_average, performance_positions, performance_relative],
        [Text('Relative', font=font, size=default_size), performance_relative_choice],
        [Text('Absolute', font=font, size=default_size), performance_positions_choice],
        [performance_analysis_submit],
        [Text('_' * divider)],
        [Text('Heatmap analysis', font=font), heatmap_analysis],
        [heatmap_submit],
        [Text('_' * divider)],
        [Text('Event matrix analysis', font=font)],
        [event_matrix_submit],
        [Text('_' * divider)],
        [Text('Quick calculation', font=font)],
        [expression, equals, result]
    ]

    return Window('', layout=layout, finalize=True)


def get_league(value: str):
    country, *others = value.split()
    return League(country, ' '.join(others))


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


def set_league(values: Dict, args: Namespace):
    if values[league_choice.Key]:
        value = values[league_choice.Key][0]
        league = get_league(value)
        args.league = [get_league_code(league)]
    else:
        args.league = None


def get_event(value: str) -> str:
    goals_re = compile(r'\s*(gfa|gf|ga)\s*(>=|<=|>|<|==|!=)\s*(\d+)\s*')
    match = goals_re.match(value)
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
    match = other_re.match(value)
    if match:
        return match.group(1)

    PopupError("Do not recognise '{}' as an event".format(value.strip()),
               auto_close=True,
               auto_close_duration=2,
               non_blocking=True)


def check_team(values: Dict, team_key: str):
    selected_name = values[team_key].strip()
    if values[league_choice.Key]:
        league = get_league(values[league_choice.Key][0])
    else:
        league = None

    rows = extract_picked_team(database, selected_name, league=league, error=False)
    if len(rows) == 0:
        PopupError("No team '{}' found in the database.".format(selected_name),
                   auto_close=True,
                   non_blocking=True)
        return ''
    elif len(rows) > 1:
        if len(rows) > 10:
            PopupError("Too many teams ({}) match the name '{}' in the database.".format(len(rows), selected_name),
                       auto_close=True,
                       non_blocking=True)
        else:
            names = [row[1] for row in rows]
            PopupError("Too many teams match the name '{}' in the database: {}.".format(selected_name,
                                                                                        ', '.join(names)),
                       auto_close=True,
                       non_blocking=True)
        return ''
    else:
        (row,) = rows
        return row[1]


def team_one_is_valid(values: Dict) -> str:
    if values[team_radio_one.Key] and values[team_choice_one.Key]:
        return check_team(values, team_choice_one.Key)
    return ''


def team_two_is_valid(values: Dict) -> str:
    if values[team_radio_two.Key] and values[team_choice_two.Key]:
        return check_team(values, team_choice_two.Key)
    return ''


def team_selected(values: Dict) -> str:
    team_one = team_one_is_valid(values)
    if team_one:
        return team_one

    team_two = team_two_is_valid(values)
    if team_two:
        return team_two

    return ''


def run_head_to_head(values: Dict):
    team_one = check_team(values, team_choice_one.Key)
    team_two = check_team(values, team_choice_two.Key)
    if team_one and team_two:
        args = Namespace()
        args.database = database
        args.block = False
        set_league(values, args)
        args.team = '{}:{}'.format(team_one, team_two)
        head_to_head.main(args)


def run_sequences(values: Dict, event):
    valid_event = get_event(values[event_choice.Key])
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

            if values[team_analysis_margins.Key]:
                show_breakdown.main(args)
            elif values[team_analysis_summary.Key]:
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
        args.event = [get_event(values[event_choice.Key])]
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
        args.history = None
        show_season_summary.main(args)


def run_expression_evaluation(values: Dict):
    try:
        answer = eval(values[expression.Key])
        result.update('{:.5f}'.format(answer))
    except (SyntaxError, TypeError, ValueError, ZeroDivisionError):
        result.update('')


def run(window: Window):
    while True:
        event, values = window.read()

        if event in [WIN_CLOSED, 'Exit']:
            break
        elif event == team_clear_one.Key:
            team_choice_one.Update('')
        elif event == team_clear_two.Key:
            team_choice_two.Update('')
        elif event == event_clear.Key:
            event_choice.Update('')
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
            performance_relative_choice.update(disabled=False)
        elif event == team_analysis_summary.Key:
            team_analysis_game_states.update(disabled=False)
        elif event in [team_analysis_form.Key, team_analysis_margins.Key, team_analysis_goals.Key]:
            team_analysis_game_states.update(disabled=True)
        elif event == expression.Key:
            run_expression_evaluation(values)


if __name__ == '__main__':
    theme('DarkBlue')
    load_teams(database)
    window = make()
    run(window)
    window.close()

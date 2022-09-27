from PySimpleGUI import (Button,
                         Checkbox,
                         Combo,
                         HorizontalSeparator,
                         InputText,
                         Radio,
                         Slider,
                         Submit,
                         Text,
                         Window)

from model.fixtures import Venue
from model.leagues import league_register, prettify
from model.tables import Position
from show_heatmap import Analysis

radio_venue = 1
radio_half = 2
radio_event = 3
radio_performance = 4
radio_team_choice = 5

countries = sorted({league.country for league in league_register.values()})
country_choice = Combo([prettify(country) for country in countries],
                       enable_events=True,
                       readonly=True,
                       key='-COUNTRY-')

league_choice = Combo([], enable_events=True, readonly=True, key='-LEAGUE-')

history_choice = Slider(range=(0, 0),
                        disabled=True,
                        enable_events=True,
                        orientation='h',
                        key='-HISTORY-')

team_radio_one = Radio('', radio_team_choice, default=True)
team_radio_two = Radio('', radio_team_choice)
team_choice_one = Combo([], enable_events=True, readonly=True, key='-TEAM1-')
team_choice_two = Combo([], enable_events=True, readonly=True, key='-TEAM2-')
team_clear_one = Button('Clear', button_color=('black', 'white'), key='-TEAM1-CLEAR-')
team_clear_two = Button('Clear', button_color=('black', 'white'), key='-TEAM2-CLEAR-')

venue_any = Radio(Venue.anywhere.name.capitalize(), radio_venue, default=True)
venue_home = Radio(Venue.home.name.capitalize(), radio_venue)
venue_away = Radio(Venue.away.name.capitalize(), radio_venue)

event_negation = Checkbox('Negate', background_color='black', key='-NEGATE-EVENT-')
win_event = Radio('Win', radio_event)
draw_event = Radio('Draw', radio_event)
loss_event = Radio('Loss', radio_event)
bts_event = Radio('BTS', radio_event)
for_0_event = Radio('For = 0', radio_event)
for_1_event = Radio('For <= 1', radio_event)
against_0_event = Radio('Against = 0', radio_event)
against_1_event = Radio('Against <= 1', radio_event)
for_against_0_event = Radio('Goals = 0', radio_event, default=True)
for_against_1_event = Radio('Goals <= 1', radio_event)
for_against_2_event = Radio('Goals <= 2', radio_event)

result_full = Checkbox('Full-time', enable_events=True, default=True)
result_first = Checkbox('1st-half', enable_events=True)
result_second = Checkbox('2nd-half', enable_events=True)

chunks_choice = Combo([],
                      disabled=True,
                      enable_events=True,
                      readonly=True,
                      key='-CHUNKS-')

aggregated_sequences_submit = Button('Sequences (aggregated)', key='-AGG-SEQ-SUBMIT-')
season_sequences_submit = Button('Sequences (per-season)', key='-PER-SEQ-SUBMIT-')
team_analysis_submit = Button('Summary', key='-TEAM-ANALYSIS-SUMMARY-')
team_history_submit = Button('History', key='-TEAM-HISTORY_')
team_goals_submit = Button('Goals', key='-TEAM-ANALYSIS-GOALS-')
h2h_submit = Button('Head to head', key='-H2H-SUBMIT-')
results_submit = Button('Results', key='-RESULTS-SUBMIT-')
regression_submit = Button('Regression', key='-REGRESSION-SUBMIT-')

performance_individual = Radio('Individual', radio_performance, default=True, enable_events=True)
performance_average = Radio('Average', radio_performance, enable_events=True)
performance_positions = Radio('Absolute', radio_performance, enable_events=True)
performance_positions_choice = InputText(key='-PERFORMANCE-POSITIONS-CHOICE-',
                                         disabled=True,
                                         enable_events=True)
performance_relative = Radio('Relative', radio_performance, enable_events=True)
performance_relative_choice = Combo(sorted([Position.pretty(position) for position in Position]),
                                    key='-PERFORMANCE-RELATIVE-CHOICE-',
                                    disabled=True,
                                    enable_events=True)
performance_analysis_submit = Submit(key='-PERFORMANCE-ANALYSIS-SUBMIT-')

expression_text = InputText(size=(16, 1), key='-EXPR-', enable_events=True)
equals_text = Text('=', justification='center', key='-EQUALS-')
evaluation_text = Text(size=(10, 1), key='-RESULT-')


def make_window():
    default_size = (10, 1)
    layout = [[Text('Country', size=default_size), country_choice],
              [Text('League', size=default_size), league_choice],
              [Text('Team #1', size=default_size), team_radio_one, team_choice_one, team_clear_one],
              [Text('Team #2', size=default_size), team_radio_two, team_choice_two, team_clear_two],
              [Text('History', size=default_size), history_choice],
              [HorizontalSeparator()],
              [Text('Event', size=default_size)],
              [event_negation],
              [win_event, draw_event, loss_event],
              [bts_event],
              [for_against_0_event, for_against_1_event, for_against_2_event],
              [for_0_event, for_1_event],
              [against_0_event, against_1_event],
              [HorizontalSeparator()],
              [Text('Venue', size=default_size), venue_any, venue_home, venue_away],
              [Text('Result', size=default_size), result_full, result_first, result_second],
              [Text('Chunks', size=default_size), chunks_choice],
              [HorizontalSeparator()],
              [aggregated_sequences_submit, season_sequences_submit, results_submit, regression_submit],
              [HorizontalSeparator()],
              [Text('Individual team analysis', )],
              [team_analysis_submit, h2h_submit, team_history_submit, team_goals_submit],
              [HorizontalSeparator()],
              [Text('Performance analysis', )],
              [Text('Type', size=default_size),
               performance_individual, performance_average, performance_positions, performance_relative],
              [Text('Relative', size=default_size), performance_relative_choice],
              [Text('Absolute', size=default_size), performance_positions_choice],
              [performance_analysis_submit],
              [HorizontalSeparator()],
              [Text('Calculator', )],
              [expression_text, equals_text, evaluation_text]]

    return Window('', layout=layout, finalize=True, grab_anywhere=True, resizable=True)

from PySimpleGUI import (Button,
                         Checkbox,
                         Combo,
                         FileBrowse,
                         HorizontalSeparator,
                         InputText,
                         Multiline,
                         Radio,
                         Slider,
                         Submit,
                         Tab,
                         TabGroup,
                         Text,
                         Window)

from model.fixtures import Venue
from model.leagues import league_register, prettify
from model.tables import Position
from show_heatmap import Analysis

radio_venue = 1
radio_half = 2
radio_team_analysis = 3
radio_performance = 4
radio_team_choice = 5
radio_event = 6

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

aggregated_sequences_submit = Button('Aggregated sequences', key='-AGG-SEQ-SUBMIT-')
season_sequences_submit = Button('Per-season sequences', key='-PER-SEQ-SUBMIT-')

team_analysis_summary = Radio('Summary',
                              radio_team_analysis,
                              key='-TEAM-ANALYSIS-SUMMARY-',
                              enable_events=True,
                              default=True)
team_analysis_goals = Radio('Goals',
                            radio_team_analysis,
                            key='-TEAM-ANALYSIS-GOALS-',
                            enable_events=True)
team_analysis_game_states = InputText(key='-TEAM-ANALYSIS-GAME-STATES-',
                                      disabled=True)
team_analysis_submit = Submit(key='-TEAM-ANALYSIS-SUBMIT-')

h2h_submit = Button('Head to head', key='-H2H-SUBMIT-')

league_analysis_submit = Button('League Analysis', key='-LEAGUE-ANALYSIS-SUBMIT-')

heatmap_analysis = Combo([analysis.name.lower().capitalize() for analysis in Analysis],
                         default_value=Analysis.RESULT.name.lower().capitalize(),
                         key='-HEATMAP-')
heatmap_submit = Submit(key='-HEATMAP-SUBMIT-')

event_matrix_submit = Submit(key='-EVENT-MATRIX-SUBMIT-')

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

betting_file = FileBrowse('Load betting file', key='-BETTING-FILE-', enable_events=True)
betting_text = Multiline(size=[60, 50], key='-BETTING-')


def make_window():
    default_size = (10, 1)
    layout1 = [[Text('Country', size=default_size), country_choice],
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
               [aggregated_sequences_submit, season_sequences_submit, h2h_submit, league_analysis_submit],
               [HorizontalSeparator()],
               [Text('Individual team analysis', )],
               [Text('Analysis', size=default_size),
                team_analysis_summary,
                team_analysis_goals],
               [Text('Game states', size=default_size), team_analysis_game_states],
               [team_analysis_submit],
               [HorizontalSeparator()],
               [Text('Performance analysis', )],
               [Text('Type', size=default_size),
                performance_individual, performance_average, performance_positions, performance_relative],
               [Text('Relative', size=default_size), performance_relative_choice],
               [Text('Absolute', size=default_size), performance_positions_choice],
               [performance_analysis_submit],
               [HorizontalSeparator()],
               [Text('Heatmap analysis', ), heatmap_analysis],
               [heatmap_submit],
               [HorizontalSeparator()],
               [Text('Event matrix analysis', )],
               [event_matrix_submit],
               [HorizontalSeparator()],
               [Text('Quick calculation', )],
               [expression_text, equals_text, evaluation_text]]
    tab1 = Tab('Analysis', layout1)

    layout2 = [[betting_file],
               [betting_text]]
    tab2 = Tab('Betting information', layout2)

    tab_group = TabGroup([[tab1, tab2]])
    master_layout = [[tab_group]]

    return Window('', layout=master_layout, finalize=True, grab_anywhere=True)

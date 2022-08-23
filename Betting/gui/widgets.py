from PySimpleGUI import (Button,
                         Checkbox,
                         Combo,
                         FileBrowse,
                         InputText,
                         Multiline,
                         Radio,
                         Slider,
                         Submit,
                         Tab,
                         TabGroup,
                         Text,
                         Window)

from model.fixtures import Half, Venue
from model.leagues import country_register, prettify
from model.tables import Position
from show_heatmap import Analysis

radio_venue = 1
radio_half = 2
radio_team_analysis = 3
radio_performance = 4
radio_team_choice = 5

country_choice = Combo([prettify(country) for country in country_register],
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

venue_any = Radio(Venue.any.name.capitalize(), radio_venue, default=True)
venue_home = Radio(Venue.home.name.capitalize(), radio_venue)
venue_away = Radio(Venue.away.name.capitalize(), radio_venue)

half_both = Radio(Half.both.name.capitalize(), radio_half, default=True)
half_first = Radio(Half.first.name.capitalize(), radio_half)
half_second = Radio(Half.second.name.capitalize(), radio_half)
half_separate = Radio(Half.separate.name.capitalize(), radio_half)

chunks_choice = Combo([],
                      disabled=True,
                      enable_events=True,
                      readonly=True,
                      key='-CHUNKS-')

event_choice = InputText(size=(16, 1), key='-EVENT-')
event_negation = Checkbox('Negate', key='-NEGATE-EVENT-')
event_clear = Button('Clear', button_color=('black', 'white'), key='-CLEAR-EVENT-')

aggregated_sequences_submit = Button('Aggregated sequences', key='-AGG-SEQ-SUBMIT-')
season_sequences_submit = Button('Per-season sequences', key='-PER-SEQ-SUBMIT-')

team_analysis_goals = Radio('Goals',
                            radio_team_analysis,
                            key='-TEAM-ANALYSIS-GOALS-',
                            default=True,
                            enable_events=True)
team_analysis_form = Radio('Form',
                           radio_team_analysis,
                           key='-TEAM-ANALYSIS-FORM-',
                           enable_events=True)
team_analysis_margins = Radio('Margins',
                              radio_team_analysis,
                              key='-TEAM-ANALYSIS-MARGINS-',
                              enable_events=True)
team_analysis_summary = Radio('Summary',
                              radio_team_analysis,
                              key='-TEAM-ANALYSIS-SUMMARY-',
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
result_text = Text(size=(10, 1), key='-RESULT-')

betting_file = FileBrowse('Load betting file', key='-BETTING-FILE-', enable_events=True)
betting_text = Multiline(size=[80, 65], key='-BETTING-')


def make_window():
    default_size = (10, 1)
    divider = 80

    layout1 = [[Text('Country', size=default_size), country_choice],
               [Text('League', size=default_size), league_choice],
               [Text('Team #1', size=default_size), team_radio_one, team_choice_one],
               [Text('Team #2', size=default_size), team_radio_two, team_choice_two],
               [Text('History', size=default_size), history_choice],
               [Text('Event', size=default_size), event_choice, event_negation, event_clear],
               [Text('Venue', size=default_size), venue_any, venue_home, venue_away],
               [Text('Half', size=default_size), half_both, half_first, half_second, half_separate],
               [Text('Chunks', size=default_size), chunks_choice],
               [Text('_' * divider)],
               [aggregated_sequences_submit, season_sequences_submit, h2h_submit, league_analysis_submit],
               [Text('_' * divider)],
               [Text('Individual team analysis', )],
               [Text('Analysis', size=default_size),
                team_analysis_goals,
                team_analysis_form,
                team_analysis_margins,
                team_analysis_summary],
               [Text('Game states', size=default_size), team_analysis_game_states],
               [team_analysis_submit],
               [Text('_' * divider)],
               [Text('Performance analysis', )],
               [Text('Type', size=default_size),
                performance_individual, performance_average, performance_positions, performance_relative],
               [Text('Relative', size=default_size), performance_relative_choice],
               [Text('Absolute', size=default_size), performance_positions_choice],
               [performance_analysis_submit],
               [Text('_' * divider)],
               [Text('Heatmap analysis', ), heatmap_analysis],
               [heatmap_submit],
               [Text('_' * divider)],
               [Text('Event matrix analysis', )],
               [event_matrix_submit],
               [Text('_' * divider)],
               [Text('Quick calculation', )],
               [expression_text, equals_text, result_text],
               [Text('_' * divider)]]
    tab1 = Tab('Analysis', layout1)

    layout2 = [[betting_file],
               [betting_text]]
    tab2 = Tab('Betting information', layout2)

    tab_group = TabGroup([[tab1, tab2]])
    master_layout = [[tab_group]]

    return Window('', layout=master_layout, finalize=True)

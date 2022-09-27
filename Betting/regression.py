from argparse import ArgumentParser, Namespace
from cli.cli import (add_database_option,
                     add_history_option,
                     add_league_option,
                     add_country_option,
                     add_logging_options,
                     set_logging_options,
                     add_team_option,
                     get_unique_league)
from colorama import Fore, Style
from lib import messages
from model.fixtures import Event, Fixture, Half
from model.leagues import League, league_register
from model.seasons import Season
from model.teams import Team
from sql.sql import extract_picked_team, load_league, load_teams
from typing import Callable, List, Tuple


def parse_command_line():
    parser = ArgumentParser(description='Analyse where events are deviating from standard behaviour')
    add_database_option(parser)
    add_history_option(parser)
    add_league_option(parser, False)
    add_team_option(parser, False)
    add_country_option(parser, False)
    add_logging_options(parser)

    parser.add_argument('--no-colors',
                        help='do not color the generated text',
                        action='store_true',
                        default=False)

    return parser.parse_args()


def count_event(fixtures: List[Fixture], func: Callable, negate: bool, halves: List[Half]) -> Tuple[int, int]:
    proportion = 0
    total = 0
    for fixture in fixtures:
        results = []

        if Half.first in halves:
            results.append(fixture.first_half())

        if Half.second in halves:
            results.append(fixture.second_half())

        if Half.full in halves:
            results.append(fixture.full_time())

        results = [result for result in results if result is not None]
        total += len(results)
        for result in results:
            if negate and not func(result):
                proportion += 1

            if not negate and func(result):
                proportion += 1

    return proportion, total


def output_digression(func: Callable,
                      negate: bool,
                      halves: List[Half],
                      history: float,
                      current: float,
                      above_or_below: str,
                      color: str):
    message = '> {} ({}) is {}{}{}\n'.format(Event.name(func, negate),
                                             Half.to_string(halves),
                                             color,
                                             above_or_below,
                                             Style.RESET_ALL if color else '')
    message += '> History {:.1f}%\n'.format(history)
    message += '> Current {:.1f}%\n'.format(current)
    message += '\n'
    return message


def analyse(event, past_dividend: int, past_divisor: int, current_dividend: int, current_divisor: int, colors: bool):
    if past_divisor and current_divisor:
        past_percentage = 100 * past_dividend / past_divisor
        current_percentage = 100 * current_dividend / current_divisor
        if past_percentage < current_percentage and current_percentage > past_percentage * 1.2:
            return output_digression(event[0],
                                     event[1],
                                     event[2],
                                     past_percentage,
                                     current_percentage,
                                     'above average',
                                     Fore.RED if colors else '')

        if current_percentage < past_percentage and current_percentage < past_percentage * 0.8:
            return output_digression(event[0],
                                     event[1],
                                     event[2],
                                     past_percentage,
                                     current_percentage,
                                     'below average',
                                     Fore.BLUE if colors else '')

    return ''


def main(args: Namespace):
    load_teams(args.database)

    leagues = []
    if args.country:
        for country in args.country:
            leagues.extend([code for code, league in league_register.items() if league.country == country.capitalize()])

    if args.league:
        leagues.extend(list(args.league))

    if not args.country and not args.league:
        leagues.extend(list(league_register.keys()))

    events = [(Event.get('draw'), False, [Half.full]),
              (Event.get('draw'), False, [Half.first]),
              (Event.get('draw'), False, [Half.second]),
              (Event.get('gfa_eq_0'), False, [Half.first]),
              (Event.get('gfa_eq_0'), False, [Half.second]),
              (Event.get('gfa_le_1'), False, [Half.full]),
              (Event.get('gfa_le_1'), False, [Half.first, Half.second]),
              (Event.get('gfa_ne_0'), False, [Half.first])]

    buffer = ''
    if args.team:
        events.append((Event.get('draw'), False, [Half.first, Half.second]))

        league = league_register[get_unique_league(args)]
        load_league(args.database, league)

        (row,) = extract_picked_team(args.database, args.team, league)
        selected_team = Team.inventory[row[0]]

        seasons = Season.seasons(league)
        if not seasons:
            messages.warning_message('No season data found for {}'.format(league))
        else:
            if args.history:
                seasons = seasons[-args.history:]

            if seasons[-1].current:
                infix = '{} in {}'.format(selected_team.name, league)
                delimiter = '*' * (len(infix) + 2)
                buffer += '{}\n {} \n{}\n'.format(delimiter, infix, delimiter)

                this_season = seasons.pop()
                for event in events:
                    past_dividend = 0
                    past_divisor = 0
                    for season in seasons:
                        if selected_team in season.fixtures_per_team():
                            fixtures = season.fixtures_per_team()[selected_team]
                            proportion, total = count_event(fixtures, event[0], event[1], event[2])
                            past_dividend += proportion
                            past_divisor += total

                    current_dividend = 0
                    current_divisor = 0
                    if selected_team in this_season.fixtures_per_team():
                        fixtures = this_season.fixtures_per_team()[selected_team]
                        proportion, total = count_event(fixtures, event[0], event[1], event[2])
                        current_dividend += proportion
                        current_divisor += total

                    buffer += analyse(event,
                                      past_dividend,
                                      past_divisor,
                                      current_dividend,
                                      current_divisor,
                                      not args.no_colors)
    else:
        events.append((Event.get('gfa_eq_0'), False, [Half.full]))

        for league_code in leagues:
            league = league_register[league_code]
            load_league(args.database, league)

            seasons = Season.seasons(league)
            if not seasons:
                messages.warning_message('No season data found for {}'.format(league))
            else:
                if args.history:
                    seasons = seasons[-args.history:]

                if seasons[-1].current:
                    infix = '{}'.format(league)
                    delimiter = '*' * (len(infix) + 2)
                    buffer += '{}\n {} \n{}\n'.format(delimiter, infix, delimiter)

                    this_season = seasons.pop()
                    for event in events:
                        past_dividend = 0
                        past_divisor = 0
                        for season in seasons:
                            for _, fixtures in season.fixtures_per_team().items():
                                proportion, total = count_event(fixtures, event[0], event[1], event[2])
                                past_dividend += proportion
                                past_divisor += total

                        current_dividend = 0
                        current_divisor = 0
                        for _, fixtures in this_season.fixtures_per_team().items():
                            proportion, total = count_event(fixtures, event[0], event[1], event[2])
                            current_dividend += proportion
                            current_divisor += total

                        buffer += analyse(event,
                                          past_dividend,
                                          past_divisor,
                                          current_dividend,
                                          current_divisor,
                                          not args.no_colors)

    return buffer


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    buffer = main(args)
    print(buffer, end='')

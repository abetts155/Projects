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
from model.fixtures import ContextualEvent, Event, Fixture, Half, Venue
from model.leagues import league_register
from model.seasons import Season
from model.teams import Team
from sql.sql import extract_picked_team, load_league, load_teams
from typing import List, Tuple


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


def count_event(fixtures: List[Fixture], event: ContextualEvent) -> Tuple[int, int]:
    proportion = 0
    total = 0
    for fixture in fixtures:
        results = []

        if Half.first in event.halves:
            results.append(fixture.first_half())

        if Half.second in event.halves:
            results.append(fixture.second_half())

        if Half.full in event.halves:
            results.append(fixture.full_time())

        results = [result for result in results if result is not None]
        total += len(results)
        for result in results:
            if event.negate and not event.func(result):
                proportion += 1

            if not event.negate and event.func(result):
                proportion += 1

    return proportion, total


def analyse(event: ContextualEvent, data: List[float], colors: bool):
    this_season = data.pop()
    data.sort()
    if this_season < 0.8 * data[0]:
        return '> {} ({}): {}{:.2f}% < {:.2f}%{}\n'.format(Event.name(event.func, event.negate),
                                                           Half.to_string(event.halves),
                                                           Fore.RED if colors else '',
                                                           this_season,
                                                           data[0],
                                                           Style.RESET_ALL if colors else '')

    if this_season > 1.2 * data[-1]:
        return '> {} ({}): {}{:.2f}% > {:.2f}%{}\n'.format(Event.name(event.func, event.negate),
                                                           Half.to_string(event.halves),
                                                           Fore.BLUE if colors else '',
                                                           this_season,
                                                           data[-1],
                                                           Style.RESET_ALL if colors else '')

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

    events = [ContextualEvent(Event.get('draw'), False, Venue.anywhere, [Half.full]),
              ContextualEvent(Event.get('draw'), False, Venue.anywhere, [Half.first]),
              ContextualEvent(Event.get('draw'), False, Venue.anywhere, [Half.second]),
              ContextualEvent(Event.get('gfa_eq_0'), False, Venue.anywhere, [Half.first]),
              ContextualEvent(Event.get('gfa_eq_0'), False, Venue.anywhere, [Half.second]),
              ContextualEvent(Event.get('gfa_le_1'), False, Venue.anywhere, [Half.full]),
              ContextualEvent(Event.get('gfa_le_1'), False, Venue.anywhere, [Half.first, Half.second]),
              ContextualEvent(Event.get('gfa_ne_0'), False, Venue.anywhere, [Half.first])]

    output = ''
    if args.team:
        league = league_register[get_unique_league(args)]
        load_league(args.database, league)

        (row,) = extract_picked_team(args.database, args.team, league)
        selected_team = Team.inventory[row[0]]

        seasons = Season.seasons(league)
        assert seasons

        if args.history:
            seasons = seasons[-args.history:]

        if seasons[-1].current:
            infix = '{} in {}'.format(selected_team.name, league)
            delimiter = '*' * (len(infix) + 2)
            output += '{}\n {} \n{}\n'.format(delimiter, infix, delimiter)

            for event in events:
                data = []
                for season in seasons:
                    if selected_team in season.fixtures_per_team():
                        fixtures = season.fixtures_per_team()[selected_team]
                        proportion, total = count_event(fixtures, event)
                        if total:
                            data.append(100 * proportion / total)

                if len(data) > 1:
                    output += analyse(event, data, not args.no_colors)
    else:
        events.append(ContextualEvent(Event.get('gfa_eq_0'), False, Venue.anywhere, [Half.full]))
        events.append(ContextualEvent(Event.get('bts'), False, Venue.anywhere, [Half.full]))

        for league_code in leagues:
            league = league_register[league_code]
            load_league(args.database, league)

            seasons = Season.seasons(league)
            assert seasons

            if args.history:
                seasons = seasons[-args.history:]

            if seasons[-1].current:
                infix = '{}'.format(league)
                delimiter = '*' * (len(infix) + 2)
                output += '{}\n {} \n{}\n'.format(delimiter, infix, delimiter)

                for event in events:
                    data = []
                    for season in seasons:
                        proportion, total = count_event(season.fixtures(), event)
                        if total:
                            data.append(100 * proportion/total)

                    if len(data) > 1:
                        output += analyse(event, data, not args.no_colors)

            output += '\n'
    return output


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    buffer = main(args)
    print(buffer, end='')

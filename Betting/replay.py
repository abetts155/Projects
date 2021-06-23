from argparse import ArgumentParser, Namespace
from cli.cli import (add_database_option,
                     add_league_option,
                     add_logging_options,
                     set_logging_options,
                     add_events_option,
                     add_half_option,
                     add_venue_option,
                     get_unique_event,
                     get_unique_league)
from collections import Counter
from model.fixtures import Event, Fixture, Half, Result, Venue, draw, win, bts
from model.leagues import league_register
from model.seasons import Season
from model.tables import LeagueTable
from sql.sql import load_database
from typing import Callable


def parse_command_line():
    parser = ArgumentParser(description='Replay season to assess predicition accuracy')
    add_database_option(parser)
    add_half_option(parser)
    add_venue_option(parser)
    add_events_option(parser, True, 1)
    add_league_option(parser)
    add_logging_options(parser)
    return parser.parse_args()


class Prediction:
    __slots__ = ['func', 'minimum']

    def __init__(self, func: Callable, minimum: int):
        self.func = func
        self.minimum = minimum


class TeamState:
    __slots__ = ['fixtures', 'alive']

    def __init__(self):
        self.fixtures = []
        self.alive = False


def main(args: Namespace):
    league = league_register[get_unique_league(args)]
    load_database(args.database, league)

    seasons = Season.seasons(league)
    this_season = seasons.pop()
    assert this_season.current

    func = Event.get(get_unique_event(args))
    for i in range(1, 38):
        prediction = Prediction(func, i)
        penalties = Counter()
        for season in seasons:
            table = LeagueTable(season, Half.both)
            states = {team: TeamState() for team in season.teams()}
            season.sort_fixtures()

            fixture: Fixture
            for fixture in season.fixtures():
                if args.half == Half.both:
                    result = fixture.full_time()
                elif args.half == Half.first:
                    result = fixture.first_half()
                elif args.half == Half.second:
                    result = fixture.second_half()

                if result:
                    home_result = fixture.canonicalise_result(fixture.home_team, result)
                    home_outcome = prediction.func(home_result)
                    home_state = states[fixture.home_team]

                    if not home_outcome:
                        home_state.fixtures.append(fixture)
                    else:
                        if home_state.alive:
                            index = len(home_state.fixtures) - prediction.minimum + 1
                            penalties[index] += 1
                        states[fixture.home_team] = TeamState()

                    if len(home_state.fixtures) == prediction.minimum:
                        final_position = table.team_position(fixture.home_team)
                        if final_position not in [0, 1, len(table) - 2, len(table) - 1]:
                            home_state.alive = True

                    away_result = fixture.canonicalise_result(fixture.away_team, result)
                    away_outcome = prediction.func(away_result)
                    away_state = states[fixture.away_team]
                    if not away_outcome:
                        away_state.fixtures.append(fixture)
                    else:
                        if away_state.alive:
                            index = len(away_state.fixtures) - prediction.minimum + 1
                            penalties[index] += 1
                        states[fixture.away_team] = TeamState()

                        away_state.fixtures.append(fixture)

                    if len(away_state.fixtures) == prediction.minimum:
                        final_position = table.team_position(fixture.away_team)
                        if final_position not in [0, 1, len(table) - 2, len(table) - 1]:
                            away_state.alive = True

        total_penalty = 0
        total_correct = 0
        for distance, correct in penalties.items():
            exponent = distance
            this_penalty = 2 ** exponent - 1
            total_penalty += this_penalty * correct
            total_correct += correct
        if total_penalty:
            print('Betting from sequences of {} returns {} right with a penalty of {}'.format(i,
                                                                                              total_correct,
                                                                                              total_penalty))


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)

from argparse import ArgumentParser, Namespace
from cli.cli import (add_database_option,
                     add_half_option,
                     add_history_option,
                     add_league_option,
                     add_country_option,
                     add_minimum_option,
                     add_logging_options,
                     add_venue_option,
                     add_events_option,
                     set_logging_options)
from collections import Counter
from colorama import Fore, Style
from datetime import datetime, timedelta
from lib import messages
from model.fixtures import ContextualEvent, Event, Half, Fixture, Venue
from model.leagues import League, league_register
from model.seasons import Season
from model.sequences import count_events, DataUnit
from model.teams import Team
from sql.sql import load_league, load_teams
from typing import Callable, Dict, List


def parse_command_line():
    parser = ArgumentParser(description='Analyse events')
    add_database_option(parser)
    add_half_option(parser)
    add_history_option(parser)
    add_league_option(parser, False)
    add_country_option(parser, False)
    add_minimum_option(parser, False)
    add_venue_option(parser)
    add_events_option(parser, False)
    add_logging_options(parser)

    parser.add_argument('-l',
                        '--lower',
                        help='left-side of the time window to consider',
                        metavar='<INT>',
                        type=int,
                        default=0)

    parser.add_argument('-u',
                        '--upper',
                        help='right-side of the time window to consider',
                        metavar='<INT>',
                        type=int,
                        default=0)

    parser.add_argument('-c',
                        '--classics',
                        action='store_true',
                        help='also consider the classical betting signals')

    parser.add_argument('-T',
                        '--time',
                        action='store_true',
                        help='sort by time rather than by league')

    return parser.parse_args()


class BettingEvent(ContextualEvent):
    elsewhere = ''

    def __init__(self, func: Callable, negate: bool, venue: Venue, halves: List[Half], minimums: Dict[str, int]):
        ContextualEvent.__init__(self, func, negate, venue, halves)
        assert BettingEvent.elsewhere in minimums
        self.minimums = minimums


def satisfies_venue_constraint(team: Team,
                               fixture: Fixture,
                               bet: BettingEvent):
    return ((bet.venue == Venue.anywhere) or
            (bet.venue == Venue.home and fixture.home_team == team) or
            (bet.venue == Venue.away and fixture.away_team == team))


class Prediction:
    def __init__(self, league: League, team: Team, fixture: Fixture, bet: BettingEvent, run: int):
        self.league = league
        self.team = team
        self.fixture = fixture
        self.bet = bet
        self.run = run


def analyse_teams(league: League,
                  season: Season,
                  bets: List[BettingEvent],
                  left_window: datetime,
                  right_window: datetime):
    fixture_predictions = {}
    for team, fixtures in season.fixtures_per_team().items():
        filtered = []
        for fixture in fixtures:
            if not fixture.finished:
                fixture_datetime = datetime.fromisoformat(str(fixture.date)).replace(tzinfo=None)
                if left_window <= fixture_datetime and (right_window is None or fixture_datetime <= right_window):
                    filtered.append(fixture)

        if filtered:
            next_match = filtered[0]
            data = [DataUnit(Counter(), [season], team=team) for _ in bets]
            count_events(team,
                         fixtures,
                         bets,
                         data)

            for bet, datum in zip(bets, data):
                if league.country in bet.minimums:
                    minimum = bet.minimums[league.country]
                elif league.code in bet.minimums:
                    minimum = bet.minimums[league.code]
                else:
                    minimum = bet.minimums[BettingEvent.elsewhere]

                if datum.last and datum.last >= minimum and satisfies_venue_constraint(team, next_match, bet):
                    prediction = Prediction(league, team, next_match, bet, datum.last)
                    fixture_predictions.setdefault(next_match, []).append(prediction)
    return fixture_predictions


def create_fixture_header(fixture: Fixture, home_color, away_color):
    return '[{}] {}{}{} vs. {}{}{}'.format(fixture.date.strftime('%H.%M: %d %b %Y'),
                                           home_color, fixture.home_team.name, Style.RESET_ALL,
                                           away_color, fixture.away_team.name, Style.RESET_ALL)


def create_league_header(league: League):
    delimiter = '*' * (len(str(league)) + 2)
    return "{}\n {} \n{}".format(delimiter, league, delimiter)


def create_date_header(date: datetime):
    delimiter = '-' * 80
    return '{}\n{} on {}\n{}'.format(delimiter, date.strftime('%-I %p'), date.strftime('%d %B %Y'), delimiter)


def create_prediction(prediction: Prediction, home_color, away_color):
    if prediction.team == prediction.fixture.home_team:
        color = home_color
    else:
        color = away_color
    return '> {}{}{}: {} in the last {} ({}) ({})'.format(color,
                                                          prediction.team.name,
                                                          Style.RESET_ALL,
                                                          Event.name(prediction.bet.func, prediction.bet.negate),
                                                          prediction.run,
                                                          prediction.bet.venue.name,
                                                          Half.to_string(prediction.bet.halves))


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

    bets = []
    if args.event:
        for event in args.event:
            bets.append(BettingEvent(Event.get(event),
                                     args.negate,
                                     args.venue,
                                     args.half,
                                     {BettingEvent.elsewhere: args.minimum}))

    if args.classics:
        classic_bets = [BettingEvent(Event.get('draw'),
                                     True,
                                     Venue.anywhere,
                                     [Half.full],
                                     {BettingEvent.elsewhere: 10,
                                      'ZAF1': 6}),

                        BettingEvent(Event.get('draw'),
                                     False,
                                     Venue.anywhere,
                                     [Half.full],
                                     {BettingEvent.elsewhere: 3}),

                        BettingEvent(Event.get('gf_eq_0'),
                                     False,
                                     Venue.anywhere,
                                     [Half.full],
                                     {BettingEvent.elsewhere: 3}),

                        BettingEvent(Event.get('ga_eq_0'),
                                     False,
                                     Venue.anywhere,
                                     [Half.full],
                                     {BettingEvent.elsewhere: 3}),

                        BettingEvent(Event.get('gfa_le_1'),
                                     False,
                                     Venue.anywhere,
                                     [Half.full],
                                     {BettingEvent.elsewhere: 3}),

                        BettingEvent(Event.get('gfa_le_2'),
                                     False,
                                     Venue.anywhere,
                                     [Half.full],
                                     {BettingEvent.elsewhere: 7,
                                      'Argentina': 10,
                                      'Brazil': 8}),

                        BettingEvent(Event.get('win'),
                                     False,
                                     Venue.anywhere,
                                     [Half.full],
                                     {BettingEvent.elsewhere: 8}),

                        BettingEvent(Event.get('bts'),
                                     True,
                                     Venue.anywhere,
                                     [Half.full],
                                     {BettingEvent.elsewhere: 7}),

                        BettingEvent(Event.get('gfa_eq_0'),
                                     False,
                                     Venue.anywhere,
                                     [Half.full],
                                     {BettingEvent.elsewhere: 1}),

                        BettingEvent(Event.get('gfa_eq_0'),
                                     False,
                                     Venue.anywhere,
                                     [Half.first, Half.second],
                                     {BettingEvent.elsewhere: 4}),

                        BettingEvent(Event.get('gfa_eq_0'),
                                     False,
                                     Venue.home,
                                     [Half.first, Half.second],
                                     {BettingEvent.elsewhere: 3}),

                        BettingEvent(Event.get('gfa_eq_0'),
                                     False,
                                     Venue.away,
                                     [Half.first, Half.second],
                                     {BettingEvent.elsewhere: 3}),

                        BettingEvent(Event.get('gfa_eq_0'),
                                     False,
                                     Venue.anywhere,
                                     [Half.first],
                                     {BettingEvent.elsewhere: 4}),

                        BettingEvent(Event.get('gfa_eq_0'),
                                     False,
                                     Venue.home,
                                     [Half.first],
                                     {BettingEvent.elsewhere: 4}),

                        BettingEvent(Event.get('gfa_eq_0'),
                                     False,
                                     Venue.away,
                                     [Half.first],
                                     {BettingEvent.elsewhere: 4}),

                        BettingEvent(Event.get('gfa_eq_0'),
                                     False,
                                     Venue.anywhere,
                                     [Half.second],
                                     {BettingEvent.elsewhere: 3}),

                        BettingEvent(Event.get('gfa_eq_0'),
                                     False,
                                     Venue.home,
                                     [Half.second],
                                     {BettingEvent.elsewhere: 4}),

                        BettingEvent(Event.get('gfa_eq_0'),
                                     False,
                                     Venue.away,
                                     [Half.second],
                                     {BettingEvent.elsewhere: 4}),

                        BettingEvent(Event.get('ga_eq_0'),
                                     False,
                                     Venue.anywhere,
                                     [Half.first],
                                     {BettingEvent.elsewhere: 8}),

                        BettingEvent(Event.get('ga_eq_0'),
                                     False,
                                     Venue.anywhere,
                                     [Half.second],
                                     {BettingEvent.elsewhere: 7}),

                        BettingEvent(Event.get('gf_eq_0'),
                                     False,
                                     Venue.anywhere,
                                     [Half.first],
                                     {BettingEvent.elsewhere: 8}),

                        BettingEvent(Event.get('gf_eq_0'),
                                     False,
                                     Venue.anywhere,
                                     [Half.second],
                                     {BettingEvent.elsewhere: 7}),

                        BettingEvent(Event.get('gf_eq_0'),
                                     False,
                                     Venue.home,
                                     [Half.full],
                                     {BettingEvent.elsewhere: 2}),

                        BettingEvent(Event.get('gf_eq_0'),
                                     False,
                                     Venue.away,
                                     [Half.full],
                                     {BettingEvent.elsewhere: 3}),

                        BettingEvent(Event.get('draw'),
                                     False,
                                     Venue.anywhere,
                                     [Half.first],
                                     {BettingEvent.elsewhere: 5}),

                        BettingEvent(Event.get('draw'),
                                     False,
                                     Venue.anywhere,
                                     [Half.second],
                                     {BettingEvent.elsewhere: 4}),

                        BettingEvent(Event.get('draw'),
                                     False,
                                     Venue.anywhere,
                                     [Half.first, Half.second],
                                     {BettingEvent.elsewhere: 5}),

                        BettingEvent(Event.get('win'),
                                     False,
                                     Venue.anywhere,
                                     [Half.first, Half.second],
                                     {BettingEvent.elsewhere: 6}),

                        BettingEvent(Event.get('loss'),
                                     False,
                                     Venue.anywhere,
                                     [Half.first, Half.second],
                                     {BettingEvent.elsewhere: 5}),

                        BettingEvent(Event.get('draw'),
                                     True,
                                     Venue.anywhere,
                                     [Half.first],
                                     {BettingEvent.elsewhere: 7}),

                        BettingEvent(Event.get('draw'),
                                     True,
                                     Venue.anywhere,
                                     [Half.first, Half.second],
                                     {BettingEvent.elsewhere: 12}),

                        BettingEvent(Event.get('gfa_ne_0'),
                                     False,
                                     Venue.anywhere,
                                     [Half.first],
                                     {BettingEvent.elsewhere: 12}),

                        BettingEvent(Event.get('bts'),
                                     False,
                                     Venue.anywhere,
                                     [Half.full],
                                     {BettingEvent.elsewhere: 6,
                                      'Argentina': 8}),

                        BettingEvent(Event.get('gfa_le_1'),
                                     False,
                                     Venue.anywhere,
                                     [Half.first, Half.second],
                                     {BettingEvent.elsewhere: 12}),

                        BettingEvent(Event.get('gfa_le_1'),
                                     True,
                                     Venue.anywhere,
                                     [Half.first, Half.second],
                                     {BettingEvent.elsewhere: 5}),

                        BettingEvent(Event.get('gfa_gt_2'),
                                     False,
                                     Venue.anywhere,
                                     [Half.full],
                                     {BettingEvent.elsewhere: 7})]
        bets.extend(classic_bets)

    left_window = datetime.now() + timedelta(hours=args.lower)
    if args.upper:
        right_window = datetime.now() + timedelta(hours=args.upper)
    else:
        right_window = None

    league_predictions = {}
    flat_predictions = []
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
                this_season = seasons.pop()
                fixture_predictions = analyse_teams(league, this_season, bets, left_window, right_window)
                if fixture_predictions:
                    if args.time:
                        for predictions in fixture_predictions.values():
                            flat_predictions.extend(predictions)
                    else:
                        league_predictions[league] = fixture_predictions
            else:
                messages.warning_message('The current season for {} has not yet started'.format(league.name))

    home_color = Fore.BLUE
    away_color = Fore.RED
    if args.time:
        flat_predictions.sort(key=lambda prediction: (prediction.fixture.date,
                                                      prediction.league.country,
                                                      prediction.league.code,
                                                      prediction.fixture.id,
                                                      prediction.team == prediction.fixture.away_team))
        last_prediction = None
        for prediction in flat_predictions:
            date_emitted = False
            if last_prediction is None:
                print(create_date_header(prediction.fixture.date))
                date_emitted = True
            elif ((last_prediction.fixture.date.day != prediction.fixture.date.day) or
                  (last_prediction.fixture.date.hour != prediction.fixture.date.hour)):
                print()
                print(create_date_header(prediction.fixture.date))
                date_emitted = True

            league_header_emitted = False
            if last_prediction is None:
                print(create_league_header(prediction.league))
                league_header_emitted = True
            elif last_prediction.league != prediction.league or date_emitted:
                if not date_emitted:
                    print()
                print(create_league_header(prediction.league))
                league_header_emitted = True

            if last_prediction is None:
                print(create_fixture_header(prediction.fixture, home_color, away_color))
            elif last_prediction.fixture != prediction.fixture:
                if not league_header_emitted:
                    print()
                print(create_fixture_header(prediction.fixture, home_color, away_color))

            print(create_prediction(prediction, home_color, away_color))
            last_prediction = prediction
        print()
    else:
        for league, fixture_predictions in league_predictions.items():
            print(create_league_header(league))

            fixtures = list(fixture_predictions.keys())
            fixtures.sort(key=lambda fixture: fixture.date)

            for fixture in fixtures:
                print(create_fixture_header(fixture, home_color, away_color))
                for prediction in fixture_predictions[fixture]:
                    print(create_prediction(prediction, home_color, away_color))
                print()


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)

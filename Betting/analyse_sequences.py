from argparse import ArgumentParser, Namespace
from cli.cli import (add_database_option,
                     add_half_option,
                     add_history_option,
                     add_league_option,
                     add_country_option,
                     add_team_option,
                     add_minimum_option,
                     add_logging_options,
                     add_venue_option,
                     add_events_option,
                     get_multiple_teams,
                     set_logging_options)
from collections import Counter
from datetime import datetime, timedelta
from lib import messages
from model.fixtures import BettingEvent, Event, Half, Fixture, Venue, classic_bets
from model.leagues import League, league_register, prettify
from model.seasons import Season
from model.sequences import count_events, DataUnit
from model.teams import Team
from sql.sql import extract_picked_team, load_league, load_teams, get_unfinished_matches
from typing import Dict, List


def parse_command_line():
    parser = ArgumentParser(description='Analyse sequence data and predict outcomes')
    add_database_option(parser)
    add_half_option(parser)
    add_history_option(parser)
    add_league_option(parser, False)
    add_country_option(parser, False)
    add_minimum_option(parser, False)
    add_venue_option(parser)
    add_events_option(parser, False)
    add_logging_options(parser)

    parser.add_argument('-w',
                        '--window',
                        type=int,
                        metavar='<INT>',
                        help='the time window in hours to consider (zero indicates no limit)',
                        default=12)

    parser.add_argument('-D',
                        '--defaults',
                        action='store_true',
                        help='also analyse the default betting signals')

    parser.add_argument('-T',
                        '--time',
                        action='store_true',
                        help='sort by time rather than by league')

    return parser.parse_args()


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

    def __str__(self):
        return '{} {} in the last {} ({}) ({}): {}'.format('>' * 10,
                                                           Event.name(self.bet.func, self.bet.negate),
                                                           self.run,
                                                           self.bet.venue.name,
                                                           Half.to_string(self.bet.halves),
                                                           self.team.name)


def analyse_teams(league: League,
                  season: Season,
                  bets: List[BettingEvent],
                  window: datetime):
    now = datetime.now().timetuple()
    fixture_predictions = {}
    for team, fixtures in season.fixtures_per_team().items():
        filtered = []
        for fixture in fixtures:
            if not fixture.finished:
                if fixture.date.timetuple().tm_yday > now.tm_yday:
                    filtered.append(fixture)

                if fixture.date.timetuple().tm_yday == now.tm_yday and fixture.date.timetuple().tm_hour >= now.tm_hour - 1:
                    filtered.append(fixture)

        if filtered:
            next_match = filtered[0]
            next_match_datetime = datetime.fromisoformat(str(next_match.date)).replace(tzinfo=None)
            if window is None or next_match_datetime <= window:
                data = [DataUnit(Counter(), [season], team=team) for bet in bets]
                count_events(season,
                             team,
                             fixtures,
                             bets,
                             data)

                for bet, datum in zip(bets, data):
                    if datum.last and datum.last >= bet.minimum and satisfies_venue_constraint(team, next_match, bet):
                        prediction = Prediction(league, team, next_match, bet, datum.last)
                        fixture_predictions.setdefault(next_match, []).append(prediction)
    return fixture_predictions


def create_fixture_header(fixture: Fixture):
    return 'Next match: {} {} {} vs. {}'.format(fixture.date.strftime('%Y-%m-%d'),
                                                fixture.date.strftime('%H.%M'),
                                                fixture.home_team.name,
                                                fixture.away_team.name)


def create_league_header(league: League):
    delimiter = '*' * 80
    return "{}\n{}\n{}".format(delimiter, league, delimiter)


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
            bets.append(BettingEvent(Event.get(event), args.negate, args.minimum, args.venue, args.half))

    if args.defaults:
        bets.extend(classic_bets)

    if args.window:
        window = datetime.now() + timedelta(hours=args.window)
    else:
        window = None

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
                fixture_predictions = analyse_teams(league, this_season, bets, window)
                if fixture_predictions:
                    if args.time:
                        for predictions in fixture_predictions.values():
                            flat_predictions.extend(predictions)
                    else:
                        league_predictions[league] = fixture_predictions
            else:
                messages.warning_message('The current season for {} has not yet started'.format(league.name))

    if args.time:
        flat_predictions.sort(key=lambda prediction: (prediction.fixture.date,
                                                      prediction.league.country,
                                                      prediction.league.name,
                                                      prediction.team.name))
        last_league = None
        last_fixture = None
        for prediction in flat_predictions:
            league_header_emitted = False
            if last_league is None:
                print(create_league_header(prediction.league))
                league_header_emitted = True

            if prediction.league != last_league:
                print()
                print(create_league_header(prediction.league))
                league_header_emitted = True
            last_league = prediction.league

            if last_fixture is None:
                print(create_fixture_header(prediction.fixture))

            if prediction.fixture != last_fixture:
                if not league_header_emitted:
                    print()
                print(create_fixture_header(prediction.fixture))
            last_fixture = prediction.fixture

            print(prediction)
    else:
        for league, fixture_predictions in league_predictions.items():
            print(create_league_header(league))

            for fixture, predictions in fixture_predictions.items():
                print(create_fixture_header(fixture))
                for prediction in predictions:
                    print(prediction)
                print()


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)

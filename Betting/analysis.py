import argparse
import collections
import colorama
import dataclasses
import datetime

import cli.cli
import lib.messages
import model.competitions
import model.fixtures
import model.seasons
import model.sequences
import model.teams


def parse_command_line():
    parser = argparse.ArgumentParser(description='Analyse events')
    cli.cli.add_half_option(parser)
    cli.cli.add_country_option(parser, False)
    cli.cli.add_minimum_option(parser, False)
    cli.cli.add_venue_option(parser)
    cli.cli.add_events_option(parser, False)
    cli.cli.add_logging_options(parser)

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

    return parser.parse_args()


class BettingEvent(model.fixtures.ContextualEvent):
    elsewhere = ''

    def __init__(
            self,
            func: collections.abc.Callable,
            negate: bool,
            venue: model.fixtures.Venue,
            periods: list[model.fixtures.Period],
            minimums: dict[str, int]
    ):
        model.sequences.ContextualEvent.__init__(self, func, negate, venue, periods)
        assert BettingEvent.elsewhere in minimums
        self.minimums = minimums


def satisfies_venue_constraint(team: model.teams.Team, fixture: model.fixtures.Fixture, bet: BettingEvent):
    return ((bet.venue == model.fixtures.Venue.ANYWHERE) or
            (bet.venue == model.fixtures.Venue.HOME and fixture.home_team == team) or
            (bet.venue == model.fixtures.Venue.AWAY and fixture.away_team == team))


@dataclasses.dataclass(slots=True)
class Prediction:
    league: model.competitions.Competition
    team: model.teams.Team
    fixture: model.fixtures.Fixture
    bet: BettingEvent
    run: int


def analyse_teams(
        league: model.competitions.Competition,
        season: model.seasons.Season,
        fixtures: list[model.fixtures.Fixture],
        bets: list[BettingEvent],
        left_window: datetime.date,
        right_window: datetime.date
):
    fixture_predictions = {}
    fixtures_per_team = model.fixtures.fixtures_per_team(fixtures)
    for team, fixtures in fixtures_per_team.items():
        filtered = []
        for fixture in fixtures:
            if not fixture.finished:
                fixture_datetime = datetime.datetime.fromisoformat(str(fixture.date)).replace(tzinfo=None)
                if left_window <= fixture_datetime and (right_window is None or fixture_datetime <= right_window):
                    filtered.append(fixture)

        if filtered:
            next_match = filtered[0]
            data = [model.sequences.DataUnit(collections.Counter(), [season], team=team) for _ in bets]
            model.sequences.count_events(team, fixtures, bets, data)

            for bet, datum in zip(bets, data):
                if league.country in bet.minimums:
                    minimum = bet.minimums[league.country]
                elif league.id in bet.minimums:
                    minimum = bet.minimums[league.id]
                else:
                    minimum = bet.minimums[BettingEvent.elsewhere]

                if datum.last and datum.last >= minimum and satisfies_venue_constraint(team, next_match, bet):
                    prediction = Prediction(league, team, next_match, bet, datum.last)
                    fixture_predictions.setdefault(next_match, []).append(prediction)
    return fixture_predictions


def create_fixture_header(fixture: model.fixtures.Fixture, home_color, away_color):
    return (f'[{fixture.date.strftime('%H.%M: %d %b %Y')}] '
            f'{home_color}{fixture.home_team.name}{colorama.Style.RESET_ALL} vs. '
            f'{away_color}{fixture.away_team.name}{colorama.Style.RESET_ALL}')


def create_league_header(league: model.competitions.Competition):
    delimiter = '*' * (len(str(league)) + 2)
    return f"{delimiter}\n {league} \n{delimiter}"


def create_date_header(date: datetime):
    delimiter = '-' * 80
    return f"{delimiter}\n{date.strftime('%-I %p')} on {date.strftime('%d %B %Y')}\n{delimiter}"


def create_prediction(prediction: Prediction, home_color, away_color):
    if prediction.team == prediction.fixture.home_team:
        color = home_color
    else:
        color = away_color
    event_info = model.fixtures.Event.name(prediction.bet.func, prediction.bet.negate)
    period_info = ', '.join(period.value for period in prediction.bet.periods)
    return (
        f"> {color}{prediction.team.name}{colorama.Style.RESET_ALL}: "
        f"{event_info} in the last {prediction.run} ({prediction.bet.venue}) ({period_info})"
    )


def gather_betting_events(args: argparse.Namespace):
    bets = []
    if args.event:
        for event in args.event:
            bets.append(
                BettingEvent(model.fixtures.Event.get(event),
                             args.negate,
                             args.venue,
                             args.half,
                             {BettingEvent.elsewhere: args.minimum})
            )

    if args.classics:
        classic_bets = [
            BettingEvent(model.fixtures.Event.get('draw'),
                         True,
                         model.fixtures.Venue.ANYWHERE,
                         [model.fixtures.Period.FULL],
                         {BettingEvent.elsewhere: 10,
                          'ZAF1': 6}),

            BettingEvent(model.fixtures.Event.get('draw'),
                         False,
                         model.fixtures.Venue.ANYWHERE,
                         [model.fixtures.Period.FULL],
                         {BettingEvent.elsewhere: 3}),

            BettingEvent(model.fixtures.Event.get('gf_eq_0'),
                         False,
                         model.fixtures.Venue.ANYWHERE,
                         [model.fixtures.Period.FULL],
                         {BettingEvent.elsewhere: 3}),

            BettingEvent(model.fixtures.Event.get('ga_eq_0'),
                         False,
                         model.fixtures.Venue.ANYWHERE,
                         [model.fixtures.Period.FULL],
                         {BettingEvent.elsewhere: 3}),

            BettingEvent(model.fixtures.Event.get('gfa_le_1'),
                         False,
                         model.fixtures.Venue.ANYWHERE,
                         [model.fixtures.Period.FULL],
                         {BettingEvent.elsewhere: 3}),

            BettingEvent(model.fixtures.Event.get('gfa_le_2'),
                         False,
                         model.fixtures.Venue.ANYWHERE,
                         [model.fixtures.Period.FULL],
                         {BettingEvent.elsewhere: 7,
                          'Argentina': 10,
                          'Brazil': 8}),

            BettingEvent(model.fixtures.Event.get('win'),
                         False,
                         model.fixtures.Venue.ANYWHERE,
                         [model.fixtures.Period.FULL],
                         {BettingEvent.elsewhere: 8}),

            BettingEvent(model.fixtures.Event.get('bts'),
                         True,
                         model.fixtures.Venue.ANYWHERE,
                         [model.fixtures.Period.FULL],
                         {BettingEvent.elsewhere: 7}),

            BettingEvent(model.fixtures.Event.get('gfa_eq_0'),
                         False,
                         model.fixtures.Venue.ANYWHERE,
                         [model.fixtures.Period.FULL],
                         {BettingEvent.elsewhere: 1}),

            BettingEvent(model.fixtures.Event.get('gfa_eq_0'),
                         False,
                         model.fixtures.Venue.ANYWHERE,
                         [model.fixtures.Period.FIRST, model.fixtures.Period.SECOND],
                         {BettingEvent.elsewhere: 4}),

            BettingEvent(model.fixtures.Event.get('gfa_eq_0'),
                         False,
                         model.fixtures.Venue.HOME,
                         [model.fixtures.Period.FIRST, model.fixtures.Period.SECOND],
                         {BettingEvent.elsewhere: 4}),

            BettingEvent(model.fixtures.Event.get('gfa_eq_0'),
                         False,
                         model.fixtures.Venue.AWAY,
                         [model.fixtures.Period.FIRST, model.fixtures.Period.SECOND],
                         {BettingEvent.elsewhere: 4}),

            BettingEvent(model.fixtures.Event.get('gfa_eq_0'),
                         False,
                         model.fixtures.Venue.ANYWHERE,
                         [model.fixtures.Period.FIRST],
                         {BettingEvent.elsewhere: 5}),

            BettingEvent(model.fixtures.Event.get('gfa_eq_0'),
                         False,
                         model.fixtures.Venue.HOME,
                         [model.fixtures.Period.FIRST],
                         {BettingEvent.elsewhere: 5}),

            BettingEvent(model.fixtures.Event.get('gfa_eq_0'),
                         False,
                         model.fixtures.Venue.AWAY,
                         [model.fixtures.Period.FIRST],
                         {BettingEvent.elsewhere: 5}),

            BettingEvent(model.fixtures.Event.get('gfa_eq_0'),
                         False,
                         model.fixtures.Venue.ANYWHERE,
                         [model.fixtures.Period.SECOND],
                         {BettingEvent.elsewhere: 3}),

            BettingEvent(model.fixtures.Event.get('gfa_eq_0'),
                         False,
                         model.fixtures.Venue.HOME,
                         [model.fixtures.Period.SECOND],
                         {BettingEvent.elsewhere: 4}),

            BettingEvent(model.fixtures.Event.get('gfa_eq_0'),
                         False,
                         model.fixtures.Venue.AWAY,
                         [model.fixtures.Period.SECOND],
                         {BettingEvent.elsewhere: 4}),

            BettingEvent(model.fixtures.Event.get('ga_eq_0'),
                         False,
                         model.fixtures.Venue.ANYWHERE,
                         [model.fixtures.Period.FIRST],
                         {BettingEvent.elsewhere: 8}),

            BettingEvent(model.fixtures.Event.get('ga_eq_0'),
                         False,
                         model.fixtures.Venue.ANYWHERE,
                         [model.fixtures.Period.SECOND],
                         {BettingEvent.elsewhere: 8}),

            BettingEvent(model.fixtures.Event.get('gf_eq_0'),
                         False,
                         model.fixtures.Venue.ANYWHERE,
                         [model.fixtures.Period.FIRST],
                         {BettingEvent.elsewhere: 10}),

            BettingEvent(model.fixtures.Event.get('gf_eq_0'),
                         False,
                         model.fixtures.Venue.ANYWHERE,
                         [model.fixtures.Period.SECOND],
                         {BettingEvent.elsewhere: 10}),

            BettingEvent(model.fixtures.Event.get('gf_eq_0'),
                         False,
                         model.fixtures.Venue.HOME,
                         [model.fixtures.Period.FULL],
                         {BettingEvent.elsewhere: 3}),

            BettingEvent(model.fixtures.Event.get('gf_eq_0'),
                         False,
                         model.fixtures.Venue.AWAY,
                         [model.fixtures.Period.FULL],
                         {BettingEvent.elsewhere: 4}),

            BettingEvent(model.fixtures.Event.get('draw'),
                         False,
                         model.fixtures.Venue.ANYWHERE,
                         [model.fixtures.Period.FIRST],
                         {BettingEvent.elsewhere: 6}),

            BettingEvent(model.fixtures.Event.get('draw'),
                         False,
                         model.fixtures.Venue.ANYWHERE,
                         [model.fixtures.Period.SECOND],
                         {BettingEvent.elsewhere: 6}),

            BettingEvent(model.fixtures.Event.get('draw'),
                         False,
                         model.fixtures.Venue.ANYWHERE,
                         [model.fixtures.Period.FIRST, model.fixtures.Period.SECOND],
                         {BettingEvent.elsewhere: 5}),

            BettingEvent(model.fixtures.Event.get('draw'),
                         True,
                         model.fixtures.Venue.ANYWHERE,
                         [model.fixtures.Period.FIRST],
                         {BettingEvent.elsewhere: 7}),

            BettingEvent(model.fixtures.Event.get('draw'),
                         True,
                         model.fixtures.Venue.ANYWHERE,
                         [model.fixtures.Period.FIRST, model.fixtures.Period.SECOND],
                         {BettingEvent.elsewhere: 12}),

            BettingEvent(model.fixtures.Event.get('gfa_ne_0'),
                         False,
                         model.fixtures.Venue.ANYWHERE,
                         [model.fixtures.Period.FIRST],
                         {BettingEvent.elsewhere: 12}),

            BettingEvent(model.fixtures.Event.get('bts'),
                         False,
                         model.fixtures.Venue.ANYWHERE,
                         [model.fixtures.Period.FULL],
                         {BettingEvent.elsewhere: 8,
                          'Argentina': 10}),

            BettingEvent(model.fixtures.Event.get('gfa_le_1'),
                         False,
                         model.fixtures.Venue.ANYWHERE,
                         [model.fixtures.Period.FIRST, model.fixtures.Period.SECOND],
                         {BettingEvent.elsewhere: 14}),

            BettingEvent(model.fixtures.Event.get('gfa_le_1'),
                         True,
                         model.fixtures.Venue.ANYWHERE,
                         [model.fixtures.Period.FIRST, model.fixtures.Period.SECOND],
                         {BettingEvent.elsewhere: 6}),

            BettingEvent(model.fixtures.Event.get('gfa_gt_2'),
                         False,
                         model.fixtures.Venue.ANYWHERE,
                         [model.fixtures.Period.FULL],
                         {BettingEvent.elsewhere: 7})
        ]
        bets.extend(classic_bets)

    return bets


def output_predictions(all_predictions: list[Prediction]):
    home_color = colorama.Fore.BLUE
    away_color = colorama.Fore.RED
    all_predictions.sort(
        key=lambda prediction: (
        prediction.fixture.date,
        prediction.league.country,
        prediction.league.id,
        prediction.fixture.id,
        prediction.team == prediction.fixture.away_team
        )
    )

    last_prediction = None
    for prediction in all_predictions:
        date_emitted = False
        if last_prediction is None:
            print(create_date_header(prediction.fixture.date))
            date_emitted = True
        elif (
                (last_prediction.fixture.date.day != prediction.fixture.date.day) or
                (last_prediction.fixture.date.hour != prediction.fixture.date.hour)
        ):
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


def main(args: argparse.Namespace):
    bets = gather_betting_events(args)

    left_window = datetime.datetime.now() + datetime.timedelta(hours=args.lower)
    if args.upper:
        right_window = datetime.datetime.now() + datetime.timedelta(hours=args.upper)
    else:
        right_window = None

    all_predictions = []
    for league in model.competitions.get_competition_whitelist(model.competitions.CompetitionType.LEAGUE):
        this_season = model.seasons.load_current_season(league)
        if not this_season:
            lib.messages.warning_message('No season data found for {}'.format(league))
        else:
            lib.messages.verbose_message(f"Analsying {league}")
            fixtures = model.seasons.load_fixtures(league, this_season)
            fixture_predictions = analyse_teams(league, this_season, fixtures, bets, left_window, right_window)
            if fixture_predictions:
                for predictions in fixture_predictions.values():
                    all_predictions.extend(predictions)

    output_predictions(all_predictions)


if __name__ == '__main__':
    args = parse_command_line()
    cli.cli.set_logging_options(args)
    main(args)

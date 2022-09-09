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
from datetime import date, datetime, timedelta
from lib import messages
from model.fixtures import Half, Fixture, Event, Venue
from model.leagues import League, league_register, prettify
from model.seasons import Season
from model.teams import Team
from model.sequences import count_events, DataUnit
from sql.sql import Database, extract_picked_team, load_league, load_teams, get_team_fixtures
from sql.sql_columns import ColumnNames
from sql.sql_language import Characters, Keywords
from typing import Callable, Dict, List


def parse_command_line():
    parser = ArgumentParser(description='Analyse sequence data and predict outcomes')
    add_database_option(parser)
    add_half_option(parser)
    add_history_option(parser)
    add_league_option(parser, False)
    add_country_option(parser, False)
    add_minimum_option(parser)
    add_team_option(parser)
    add_venue_option(parser)
    add_events_option(parser)
    add_logging_options(parser)

    parser.add_argument('--probability',
                        type=float,
                        help='only show events below or at this threshold',
                        default=0.1)

    parser.add_argument('-S',
                        '--show-match',
                        action='store_true',
                        help='show the next match even if it is beyond the next 24 hours',
                        default=False)

    parser.add_argument('--no-header',
                        action='store_true',
                        help='do not show the league header banner',
                        default=False)

    return parser.parse_args()


def probability(current_run: int, counter: Counter) -> float:
    if counter:
        numerator = 0
        for i in range(current_run + 1, len(counter) + 1):
            numerator += counter[i]
        return numerator / sum(counter.values())
    else:
        return 1


def analyse_team_and_event(seasons: List[Season], this_season: Season, team: Team, event: Callable):
    team_history = DataUnit(Counter(), seasons)
    for season in seasons:
        count_events(season,
                     team,
                     args.venue,
                     args.half,
                     event,
                     args.negate,
                     team_history)

    team_now = DataUnit(Counter(), [this_season], team=team)
    count_events(this_season,
                 team,
                 args.venue,
                 args.half,
                 event,
                 args.negate,
                 team_now)

    return team_now, team_history


def predict(team_now: DataUnit, team_history: DataUnit, aggregated_history: DataUnit, threshold: float, minimum: int):
    aggregated_message = ''
    team_message = ''

    if team_now.last and team_now.last >= minimum:
        aggregated_probability = probability(team_now.last, aggregated_history.counter)
        if aggregated_probability <= threshold:
            aggregated_message = 'Probability of extension is {:.3f} [aggregated]'.format(aggregated_probability)

        team_probability = probability(team_now.last, team_history.counter)
        if team_probability and team_probability <= threshold:
            team_message = 'Probability of extension is {:.3f}'.format(team_probability)
            team_message = '{} [based on {} individual observations]'.format(team_message,
                                                                             sum(team_history.counter.values()))

    return aggregated_message, team_message


def output_prediction(team: Team,
                      next_match: Fixture,
                      half: Half,
                      venue: Venue,
                      event: Callable,
                      negate: bool,
                      length_of_run: int):
    event = Event.name(event, negate)
    header = '{} {} in the last {}'.format('>' * 10, event, length_of_run)

    if venue != Venue.any:
        header += ' {}'.format(venue.name)

    if half == Half.both:
        header += ' {}'.format('games' if length_of_run > 1 else 'game')
    elif half == Half.first:
        header += ' 1st {}'.format('halves' if length_of_run > 1 else 'half')
    elif half == Half.second:
        header += ' 2nd {}'.format('halves' if length_of_run > 1 else 'half')
    elif half == Half.separate:
        header += ' consecutive {}'.format('halves' if length_of_run > 1 else 'half')
    else:
        assert False

    header = '{}: {}'.format(header, team.name)
    next_match_message = 'Next match: {} {} {} vs. {}'.format(next_match.date.strftime('%Y-%m-%d'),
                                                              next_match.date.strftime('%H.%M'),
                                                              next_match.home_team.name,
                                                              next_match.away_team.name)
    message = '{}\n{}\n'.format(next_match_message, header)
    print(message)


def analyse_teams(args: Namespace,
                  league: League,
                  seasons: List[Season],
                  this_season: Season,
                  teams: List[Team],
                  historical_data: Dict[Callable, DataUnit]):
    header = "{}Analysing sequences in {} {} {}".format('*' * 80 + '\n',
                                                        prettify(league.country),
                                                        league.name,
                                                        '\n' + '*' * 80)
    header_emitted = False
    now = datetime.now().timetuple()

    for team in teams:
        for event, history in historical_data.items():
            team_now, team_history = analyse_team_and_event(seasons, this_season, team, event)
            aggregated_message, team_message = predict(team_now, team_history, history, args.probability, args.minimum)

            if aggregated_message or team_message:
                fixtures = get_team_fixtures(args.database, this_season, team)
                fixtures_remaining = len(fixtures)
                fixtures = [fixture for fixture in fixtures if
                            (fixture.date.timetuple().tm_yday == now.tm_yday and
                             fixture.date.timetuple().tm_hour >= now.tm_hour - 1) or
                            fixture.date.timetuple().tm_yday > now.tm_yday]

                if fixtures:
                    next_match = fixtures[0]
                    window = datetime.now() + timedelta(hours=12)
                    satisfies_date_constraint = (datetime.date(next_match.date) <= window.date() or args.show_match)
                    satisfies_venue_constraint = ((args.venue == Venue.any) or
                                                  (args.venue == Venue.home and next_match.home_team == team) or
                                                  (args.venue == Venue.away and next_match.away_team == team))

                    if satisfies_date_constraint and satisfies_venue_constraint:
                        if not args.no_header and not header_emitted:
                            header_emitted = True
                            messages.vanilla_message(header)

                        output_prediction(team,
                                          next_match,
                                          args.half,
                                          args.venue,
                                          event,
                                          args.negate,
                                          team_now.last)


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

    for league_code in leagues:
        league = league_register[league_code]
        load_league(args.database, league)

        seasons = Season.seasons(league)
        if not seasons:
            messages.warning_message("No season data found for {} {}".format(league.country, league.name))
        else:
            if args.history:
                seasons = seasons[-args.history:]

            if seasons[-1].current:
                this_season = seasons.pop()

                if args.team:
                    teams = []
                    for team_name in get_multiple_teams(args):
                        (row,) = extract_picked_team(args.database, team_name, league)
                        team = Team.inventory[row[0]]
                        teams.append(team)
                else:
                    teams = this_season.teams()

                events = [Event.get(event) for event in args.event]
                historical_data = {}
                for event in events:
                    historical_data[event] = DataUnit(Counter(), seasons)
                    for season in seasons:
                        for team in season.teams():
                            count_events(season,
                                         team,
                                         args.venue,
                                         args.half,
                                         event,
                                         args.negate,
                                         historical_data[event])

                analyse_teams(args, league, seasons, this_season, teams, historical_data)
            else:
                messages.error_message("The current season has not yet started")


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)

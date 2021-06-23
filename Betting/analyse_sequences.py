from argparse import ArgumentParser, Namespace
from cli.cli import (add_database_option,
                     add_half_option,
                     add_history_option,
                     add_league_option,
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
from sql.sql import Database, extract_picked_team, load_league, load_teams, get_fixtures
from sql.sql_columns import ColumnNames
from sql.sql_language import Characters, Keywords
from typing import Callable, Dict, List


def parse_command_line():
    parser = ArgumentParser(description='Analyse sequence data and predict outcomes')
    add_database_option(parser)
    add_half_option(parser)
    add_history_option(parser)
    add_league_option(parser)
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


def get_match_information(database: str, season: Season, team: Team):
    with Database(database) as db:
        season_constraint = "{}={}".format(ColumnNames.Season_ID.name, season.id)
        team_constraint = "({}={} {} {}={})".format(ColumnNames.Home_ID.name,
                                                    team.id,
                                                    Keywords.OR.name,
                                                    ColumnNames.Away_ID.name,
                                                    team.id)
        finished_constraint = "{}={}".format(ColumnNames.Finished.name, Characters.FALSE.value)
        constraints = [season_constraint, team_constraint, finished_constraint]
        fixtures = get_fixtures(db, constraints)
        fixtures.sort(key=lambda fixture: fixture.date)
        return fixtures


class Text:
    BLUE = '\033[94m'
    YELLOW = '\033[93m'
    RED = '\033[91m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'
    END = '\033[0m'


def output_prediction(team: Team,
                      next_match: Fixture,
                      remaining_matches: int,
                      half: Half,
                      event: Callable,
                      negate: bool,
                      length_of_run: int,
                      aggregated_message: str,
                      team_message: str):
    event = Event.name(event, negate)
    header = '{} {}{}{} in the last {}{}{}'.format('>' * 10,
                                                   Text.BLUE,
                                                   event,
                                                   Text.END,
                                                   Text.BOLD,
                                                   length_of_run,
                                                   Text.END)

    if half == Half.both:
        header += ' {}{}{}'.format(Text.UNDERLINE, 'games' if length_of_run > 1 else 'game', Text.END)
    elif half == Half.first:
        header += ' {}1st {}{}'.format(Text.UNDERLINE, 'halves' if length_of_run > 1 else 'half', Text.END)
    elif half == Half.second:
        header += ' {}2nd {}{}'.format(Text.UNDERLINE, 'halves' if length_of_run > 1 else 'half', Text.END)
    elif half == Half.separate:
        header += ' {}consecutive {}{}'.format(Text.UNDERLINE,'halves' if length_of_run > 1 else 'half', Text.END)
    else:
        assert False

    header = '{}: {}{}{}'.format(header, Text.RED, team.name, Text.END)
    next_match_message = 'Next match: {} {}{}{} {} vs. {}'.format(next_match.date.strftime('%Y-%m-%d'),
                                                                  Text.BOLD,
                                                                  next_match.date.strftime('%H.%M'),
                                                                  Text.END,
                                                                  next_match.home_team.name,
                                                                  next_match.away_team.name)
    remaining_matches_message = '{} matches remaining'.format(remaining_matches)
    message = '{}\n{}{}{}\n{}\n'.format(header,
                                        aggregated_message + '\n' if aggregated_message else '',
                                        team_message + '\n' if team_message else '',
                                        next_match_message,
                                        remaining_matches_message)
    print(message)


def analyse_teams(args: Namespace,
                  league: League,
                  seasons: List[Season],
                  this_season: Season,
                  teams: List[Team],
                  historical_data: Dict[Callable, DataUnit]):
    header = "{} Analysing sequences in {} {} {}".format('*' * 80 + '\n',
                                                         prettify(league.country),
                                                         league.name,
                                                         '\n' + '*' * 80)
    header_emitted = False

    for team in teams:
        for event, history in historical_data.items():
            team_now, team_history = analyse_team_and_event(seasons, this_season, team, event)
            aggregated_message, team_message = predict(team_now, team_history, history, args.probability, args.minimum)

            if aggregated_message or team_message:
                fixtures = get_match_information(args.database, this_season, team)
                if fixtures:
                    fixtures = [fixture for fixture in fixtures if datetime.date(fixture.date) >= date.today()]
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
                                          len(fixtures),
                                          args.half,
                                          event,
                                          args.negate,
                                          team_now.last,
                                          aggregated_message,
                                          team_message)


def main(args: Namespace):
    load_teams(args.database)

    for league_code in args.league:
        league = league_register[league_code]
        load_league(args.database, league)

        seasons = Season.seasons(league)
        if not seasons:
            messages.error_message("No season data found")

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

from argparse import ArgumentParser, Namespace

from cli.cli import (add_database_option,
                     add_logging_options,
                     set_logging_options)
from model.fixtures import Event, Half
from model.leagues import League, league_register
from model.predictions import Prediction
from sql.sql import Database, load_teams, get_current_season, get_fixtures
from sql.sql_columns import ColumnNames
from sql.sql_language import Characters


def parse_command_line():
    parser = ArgumentParser(description='Update database with predictions made by the Goal Gambling God')
    add_database_option(parser)
    add_logging_options(parser)
    return parser.parse_args()


def invalid_input_message(corrective_action: str):
    print('Invalid input: {}'.format(corrective_action))


def pick_option(lower, upper):
    identifier = None
    while identifier is None:
        try:
            number = int(input('Choose: ').strip())
            if number < lower or number > upper:
                invalid_input_message('Enter an integer in the range [{}, {}]'.format(lower, upper))
            else:
                return number
        except ValueError:
            invalid_input_message('Enter a whole number')


def pick_league():
    league_choices = list(league_register.values())
    lower = 0
    upper = len(league_choices) - 1
    id_width = len(str(len(league_choices)))
    country_width = 0
    for league in league_choices:
        country_width = max(len(league.country), country_width)

    print('League options')
    for identifier, league in enumerate(league_choices, start=lower):
        print('{:>{id_width}}  {:<{country_width}} {}'.format(str(identifier),
                                                              league.country,
                                                              league.name,
                                                              id_width=id_width, country_width=country_width))
    print()
    return league_choices[pick_option(lower, upper)]


def pick_fixture(db: Database, league: League):
    print()
    print('{}  {} {}  {}'.format('*' * 10, league.country, league.name, '*' * 10))
    season = get_current_season(db, league)
    season_constraint = "{}={}".format(ColumnNames.Season_ID.name, season[0])
    finished_constraint = "{}={}".format(ColumnNames.Finished.name, Characters.TRUE.value)
    constraints = [season_constraint, finished_constraint]
    fixtures = get_fixtures(db, constraints)
    if fixtures:
        fixtures.sort(key=lambda fixture: (fixture.date.date(), fixture.date.time()))
        lower = 0
        upper = len(fixtures) - 1
        id_width = len(str(len(fixtures)))

        print('Fixture options')
        for identifier, fixture in enumerate(fixtures, start=lower):
            print('{:>{id_width}}  {}'.format(str(identifier), fixture, id_width=id_width))
        print()

        return fixtures[pick_option(lower, upper)]
    else:
        print('No fixtures found')


def pick_odds():
    odds = 0
    while odds <= 0:
        try:
            odds = float(input('Enter the odds in decimal format: ').strip())
            if odds <= 0:
                invalid_input_message('Odds are always positive real numbers')
        except ValueError:
            invalid_input_message('Enter a real number')
    return odds


def pick_event():
    events = [Event.get('gfa_gt_0'), Event.get('gfa_gt_1')]
    lower = 0
    upper = len(events) - 1
    for identifier, event in enumerate(events, start=lower):
        print('{}  {}'.format(str(identifier), Event.name(event, False)))
    return events[pick_option(lower, upper)]


def pick_half():
    halves = [Half.first, Half.second, Half.full]
    lower = 0
    upper = len(halves) - 1
    for identifier, half in enumerate(halves, start=lower):
        print('{}  {}'.format(str(identifier), half.name.capitalize()))
    return halves[pick_option(lower, upper)]


def affirmative(question: str):
    answer = input('{} '.format(question)).strip().lower()
    return answer in ['y', 'ye', 'yes']


def create_prediction(database: str):
    with Database(database) as db:
        league = pick_league()
        fixture = pick_fixture(db, league)

        if fixture:

            fixture_id_constraint = "{}={}".format(ColumnNames.Fixture_ID.name, fixture.id)
            constraints = [fixture_id_constraint]
            predictions = db.fetch_all_rows(Prediction.sql_table(), constraints)

            if predictions:
                print('There is already a prediction entered for this game')
            else:
                odds = pick_odds()
                event = pick_event()
                half = pick_half()

                reiteration = 'The prediction was {} ({}) at odds of {} for {} vs {}.'.format(Event.name(event, False),
                                                                                              Half.to_string([half]),
                                                                                              odds,
                                                                                              fixture.home_team.name,
                                                                                              fixture.away_team.name)
                if affirmative('{} Correct?'.format(reiteration)):
                    id_ = db.get_number_of_rows(Prediction) + len(Prediction.inventory) + 1
                    prediction = Prediction(id_, league.code, fixture.id, odds, event, half)
                    Prediction.inventory[id_] = prediction


def main(args: Namespace):
    load_teams(args.database)
    with Database(args.database) as db:
        db.create_table(Prediction)

    create_prediction(args.database)
    while True:
        if not affirmative('Continue?'):
            break
        create_prediction(args.database)

    with Database(args.database) as db:
        db.create_rows(Prediction)


if __name__ == '__main__':
    args = parse_command_line()
    set_logging_options(args)
    main(args)

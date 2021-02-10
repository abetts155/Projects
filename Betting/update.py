from argparse import ArgumentParser, Namespace
from cli.cli import add_database_option, add_logging_options, set_logging_options
from lib import messages
from model.leagues import country_register, league_register
from model.seasons import Season, create_season_from_row
from pathlib import Path
from sql.sql_columns import ColumnNames
from sql.sql import Database, check_database_exists
from subprocess import run, DEVNULL
from time import gmtime, sleep, strftime
from typing import List


def parse_command_line():
    parser = ArgumentParser(description='Update database')
    add_database_option(parser)
    add_logging_options(parser)

    parser.add_argument('--teams',
                        action='store_true',
                        help='update teams',
                        default=False)

    parser.add_argument('--fixtures',
                        action='store_true',
                        help='update teams',
                        default=False)

    return parser.parse_args()


def run_command(command: List[str]):
    process = run(command)
    if process.returncode != 0:
        messages.warning_message("Unsuccessful run: '{}'".format(' '.join(command)))
    else:
        messages.vanilla_message("Successful run: '{}'".format(' '.join(command)))


def update_teams(database: str):
    for country in country_register:
        update_teams_script = Path(__file__).parent.absolute().joinpath('update_teams.py')
        command = ['python3', str(update_teams_script), '--database', database, '-C', country]
        run_command(command)


def update_fixtures(database: str):
    codes = []
    with Database(database) as db:
        for code, league in league_register.items():
            constraints = ["{}='{}'".format(ColumnNames.Country.name, league.country),
                           "{}='{}'".format(ColumnNames.Code.name, league.name)]
            season_rows = db.fetch_all_rows(Season.sql_table(), constraints)
            for season_row in season_rows:
                season = create_season_from_row(season_row)
                if season.current:
                    codes.append(code)

    allowed = 20
    suspend = 3700
    update_fixtures_script = Path(__file__).parent.absolute().joinpath('update_fixtures.py')
    for i, code in enumerate(codes):
        if i > 0 and i % allowed == 0:
            sleep(suspend)
            messages.vanilla_message('Awake at {}'.format(strftime("%Y-%m-%d %H:%M:%S", gmtime())))
        else:
            sleep(10)

        command = ['python3', str(update_fixtures_script), '--database', database, '-L', code]
        run_command(command)


def main(arguments: Namespace):
    check_database_exists(arguments.database)

    if arguments.teams:
        update_teams(arguments.database)

    if arguments.fixtures:
        update_fixtures(arguments.database)


if __name__ == '__main__':
    arguments = parse_command_line()
    set_logging_options(arguments)
    main(arguments)

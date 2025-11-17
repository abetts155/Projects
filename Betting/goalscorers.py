import argparse
import collections
import datetime
import os
import plotly.graph_objects as go
import plotly.subplots
import sys
import typing

import cli.cli
import cli.user_input
import lib.messages
import lib.structure
import model.fixtures
import model.competitions
import model.events
import model.players
import model.seasons
import model.tables
import model.teams


def parse_command_line():
    parser = argparse.ArgumentParser(description='View goal times for a particular team')
    cli.cli.add_logging_options(parser)
    cli.cli.add_venue_option(parser)
    cli.cli.add_competition_option(parser)
    cli.cli.add_season_option(parser)
    return parser.parse_args()


def set_season(
        competition: model.competitions.Competition,
        selected_season: typing.Optional[int]
) -> model.seasons.Season:
    database = lib.structure.get_database(competition.country)
    if selected_season is not None:
        return model.seasons.load_season(database, competition, selected_season)
    else:
        return model.seasons.load_current_season(database, competition)


def set_competition(selected_competition: typing.Optional[int]) -> model.competitions.Competition:
    if selected_competition is not None:
        return model.competitions.load_competition(selected_competition)
    else:
        country = cli.user_input.pick_country()
        return cli.user_input.pick_competition(country)


def get_start_date(year: int):
    for day in range(1, 8):
        date = datetime.date(year, 8, day)
        if date.weekday() == 4:
            return date


def next_friday(date: datetime.datetime) -> datetime.datetime:
    days_ahead = (4 - date.weekday()) % 7
    next_friday = date + datetime.timedelta(days=days_ahead)
    return datetime.date(next_friday.year, next_friday.month, next_friday.day)


def main(selected_competition_id: typing.Optional[int], selected_season_id: typing.Optional[int]):
    competition = set_competition(selected_competition_id)
    start_season = set_season(competition, selected_season_id)
    lib.messages.vanilla_message(f"Analysing {competition} from season {start_season.year} (id={competition.id})")

    data = {}
    current = get_start_date(start_season.year)
    today = datetime.date.today()
    while current <= today:
        data[current] = collections.defaultdict(int)
        current += datetime.timedelta(days=7)

    database = lib.structure.get_database(competition.country)
    seasons = model.seasons.load_seasons(database, competition)
    for season in seasons:
        if season.events and season.year >= start_season.year:
            print(season.year)

            fixtures = model.seasons.load_fixtures(database, competition, season)
            for fixture in fixtures:
                if fixture.finished:
                    friday = next_friday(fixture.date)
                    events = model.events.load_events(database, fixture)
                    for event in events:
                        if model.events.is_goal(event.detail):
                            player = model.players.load_player(event.left_player_id)
                            data[friday][player] += 1

    running_totals = collections.defaultdict(int)
    for friday, weekly in data.items():
        for player, goals in weekly.items():
            running_totals[player] += goals
        data[friday] = running_totals.copy()

    last_friday = max(data.keys())
    sorted_totals = sorted(data[last_friday].items(), key=lambda x: x[1], reverse=True)
    top = sorted_totals[:25]
    for player, goals in top:
        print(f"{player.name}: {goals} goals")

if __name__ == '__main__':
    args = parse_command_line()
    cli.cli.set_logging_options(args)
    main(args.competition, args.season)
    sys.exit(os.EX_OK)

import argparse
import matplotlib.pyplot as plt
import os
import sys
import typing

import cli.cli
import cli.user_input
import lib.messages
import lib.structure
import model.fixtures
import model.competitions
import model.events
import model.seasons
import model.tables
import model.teams


def parse_command_line():
    parser = argparse.ArgumentParser(description='View goal probabilities for a particular team')
    cli.cli.add_logging_options(parser)
    cli.cli.add_competition_option(parser)
    return parser.parse_args()


def place(n: int) -> str:
    suffix = ['th', 'st', 'nd', 'rd', 'th'][min(n % 10, 4)] if not 10 <= n % 100 <= 20 else 'th'
    return f"{n}{suffix}"


def gather_data(
        competition: model.competitions.Competition,
):
    database = lib.structure.get_database(competition.country)

    min_minute = 1
    max_minute = 91

    match_counts = {
        0: [0] * (max_minute - min_minute),
        1: [0] * (max_minute - min_minute),
        2: [0] * (max_minute - min_minute)
    }

    over_2_5_counts = {
        0: [0] * (max_minute - min_minute),
        1: [0] * (max_minute - min_minute),
        2: [0] * (max_minute - min_minute)
    }

    seasons = model.seasons.load_seasons(database, competition)
    for season in seasons:

        fixtures = model.seasons.load_fixtures(database, competition, season)
        for fixture in fixtures:
            if fixture.finished:
                goal_times = []
                events = model.events.load_events(database, fixture)
                if events:
                    for event in events:
                        if model.events.is_goal(event.detail):
                            goal_times.append(event.time)

                    for t in range(min_minute, max_minute):
                        goals_by_t = sum(m <= t for m in goal_times)
                        if goals_by_t in [0, 1, 2]:
                            i = t - min_minute

                            match_counts[goals_by_t][i] += 1

                            result = fixture.result(model.fixtures.Period.FULL)
                            if result.left + result.right > 2:
                                over_2_5_counts[goals_by_t][i] += 1

    probabilities = {}
    for g in (0, 1, 2):
        p = []
        for count, over in zip(match_counts[g], over_2_5_counts[g]):
            print(g, over, count)
            if count > 0:
                p.append(over / count)
            else:
                p.append(None)
        probabilities[g] = p

    minutes = list(range(min_minute, max_minute))

    plt.figure(figsize=(12, 6))

    for g in (0, 1, 2):
        plt.plot(minutes, probabilities[g], label=f"{g} goals so far")

    plt.title("Probability of Over 2.5 Goals vs Minute (Based on Goals So Far)")
    plt.xlabel("Minute")
    plt.ylabel("Probability of Over 2.5 Goals")
    plt.legend()
    plt.tight_layout()
    plt.show()


def set_competition(selected_competition: typing.Optional[int]) -> model.competitions.Competition:
    if selected_competition is not None:
        return model.competitions.load_competition(selected_competition)
    else:
        country = cli.user_input.pick_country()
        return cli.user_input.pick_competition(country)


def main(selected_competition_id: typing.Optional[int]):
    competition = set_competition(selected_competition_id)
    lib.messages.vanilla_message(f"Analysing {competition} (id={competition.id})")
    gather_data(competition)


if __name__ == '__main__':
    args = parse_command_line()
    cli.cli.set_logging_options(args)
    main(args.competition)
    sys.exit(os.EX_OK)

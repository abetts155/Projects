import argparse
import enum
import os

import cli.cli
import football_api.structure
import model.competitions


class Mode(enum.Enum):
    SELECT = enum.auto()
    DESELECT = enum.auto()


def parse_command_line():
    parser = argparse.ArgumentParser(description='Select or deselect leagues and cups under analysis')

    parser.add_argument('-S',
                        '--starts-with',
                        type=str,
                        help='pick out countries that start with this sequence')

    parser.add_argument('--mode',
                        choices=[mode.name.lower() for mode in Mode],
                        help='select or deselect?',
                        default=Mode.SELECT.name.lower())

    return parser.parse_args()


def select_competitions(
        candidates: list[model.competitions.Competition],
        mode: Mode
) -> list[model.competitions.Competition]:
    selected = []
    candidates.sort(key=lambda competition: (competition.country, competition.id))
    print("In the following, 'Y' or 'y' means Yes, 'Q' or 'q' means Quit, and Enter means No")
    for competition in candidates:
        answer = input(f"{mode.name.capitalize()} {competition.type.name.capitalize()}: {competition}? ")
        if answer in ['y', 'Y']:
            selected.append(competition)
        elif answer in ['q', 'Q']:
            break
    return selected


def read_competition_ids(filename: str) -> set[int]:
    competition_ids = set()
    with open(filename, 'r') as in_file:
        for line in in_file:
            line = line.strip()
            competition_ids.add(int(line))
    return competition_ids


def write_competition_ids(filename: str, competitions: list[model.competitions.Competition]):
    with open(filename, 'w') as out_file:
        for competition in competitions:
            out_file.write(f'{competition.id}\n')


def get_competitions(competition_type: model.competitions.CompetitionType) -> list[model.competitions.Competition]:
    competitions = model.competitions.load_competitions(competition_type=competition_type)
    return competitions


def filter_competitions_by_selected_countries(
        competitions: list[model.competitions.Competition],
        starts_with: str
) -> list[model.competitions.Competition]:
    if starts_with is not None:
        return [competition for competition in competitions if competition.country.casefold().startswith(starts_with)]
    else:
        return competitions


def split_competitions_into_selected_and_unselected(
        competitions: list[model.competitions.Competition],
        existing_ids: set[int]
):
    selected = []
    unselected = []
    for competition in competitions:
        if competition.id in existing_ids:
            selected.append(competition)
        else:
            unselected.append(competition)
    return selected, unselected


def write_country_whitelist(competitions: list[model.competitions.Competition]):
    country_whitelist = set()
    for competition in competitions:
        country_whitelist.add(competition.country)

    with open(football_api.structure.get_countries_whitelist(), 'w') as out_file:
        for country in country_whitelist:
            out_file.write(f'{country}\n')


def select(
        starts_with: str,
        mode: Mode,
        competition_type: model.competitions.CompetitionType,
        id_filename: str
) -> list[model.competitions.Competition]:
    competitions = get_competitions(competition_type)
    competition_ids = read_competition_ids(id_filename)
    selected, unselected = split_competitions_into_selected_and_unselected(competitions, competition_ids)
    unselected = filter_competitions_by_selected_countries(unselected, starts_with)
    chosen = select_competitions(unselected, mode)
    whitelist = selected + chosen
    write_competition_ids(id_filename, whitelist)
    return whitelist


def deselect(
        starts_with: str,
        mode: Mode,
        competition_type: model.competitions.CompetitionType,
        id_filename: str
) -> list[model.competitions.Competition]:
    competitions = get_competitions(competition_type)
    competition_ids = read_competition_ids(id_filename)
    selected, _ = split_competitions_into_selected_and_unselected(competitions, competition_ids)
    candidates = filter_competitions_by_selected_countries(selected, starts_with)
    deselected = select_competitions(candidates, mode)
    whitelist = [competition for competition in selected if competition not in deselected]
    write_competition_ids(id_filename, whitelist)
    return whitelist


def main(starts_with: str, mode: Mode):
    left, right = '>' * 20, '<' * 20
    if mode == Mode.SELECT:
        league_whitelist = select(
            starts_with,
            mode,
            model.competitions.CompetitionType.LEAGUE,
            football_api.structure.get_leagues_whitelist()
        )

        cup_whitelist = select(
            starts_with,
            mode,
            model.competitions.CompetitionType.CUP,
            football_api.structure.get_cups_whitelist()
        )
    else:
        print(f"{left} League Deselection {right}")
        league_whitelist = deselect(
            starts_with,
            mode,
            model.competitions.CompetitionType.LEAGUE,
            football_api.structure.get_leagues_whitelist()
        )

        print()
        print(f"{left} Cup Deselection {right}")
        cup_whitelist = deselect(
            starts_with,
            mode,
            model.competitions.CompetitionType.CUP,
            football_api.structure.get_cups_whitelist()
        )

    write_country_whitelist(league_whitelist + cup_whitelist)


if __name__ == '__main__':
    args = parse_command_line()
    main(args.starts_with, Mode[args.mode.upper()])
    exit(os.EX_OK)

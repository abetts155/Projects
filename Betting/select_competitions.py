import argparse
import enum
import os

import lib.structure
import model.competitions
import sql.sql


class Mode(enum.Enum):
    SELECTION = enum.auto()
    DESELECTION = enum.auto()


def parse_command_line():
    parser = argparse.ArgumentParser(description='Select or deselect leagues and cups under analysis')

    parser.add_argument('-C',
                        '--countries',
                        type=str,
                        help='pick out countries that start with this sequence')

    parser.add_argument('--mode',
                        choices=[mode.name.lower() for mode in Mode],
                        help='select or deselect?',
                        default=Mode.SELECTION.name.lower())

    return parser.parse_args()


def filter_competitions_by_selected_countries(
        competitions: list[model.competitions.Competition],
        countries_prefix: str
) -> list[model.competitions.Competition]:
    if countries_prefix is not None:
        return [
            competition for competition in competitions
            if competition.country.casefold().startswith(countries_prefix.casefold())
        ]
    else:
        return competitions


def split_into_selected_and_unselected_competitions(
        competitions: list[model.competitions.Competition]
) -> tuple[
    list[model.competitions.Competition],
    list[model.competitions.Competition]
]:
    selected = []
    unselected = []
    for competition in competitions:
        if competition.whitelisted:
            selected.append(competition)
        else:
            unselected.append(competition)
    return selected, unselected


def select_competitions(
        candidates: list[model.competitions.Competition],
        competition_type: model.competitions.CompetitionType,
        mode: Mode
) -> list[model.competitions.Competition]:
    left, right = '>' * 20, '<' * 20
    print(f"{left} {competition_type.name.capitalize()} {mode.name.capitalize()} {right}")
    print("'Y' or 'y' means Yes, 'Q' or 'q' means Quit, and anything else (including just Enter) means No")

    changed = []
    candidates.sort(key=lambda competition: (competition.country, competition.id))
    for competition in candidates:
        answer = input(f"{competition}? ")
        if answer in ['y', 'Y']:
            if mode == Mode.SELECTION:
                competition.whitelisted = True
            else:
                competition.whitelisted = False
            changed.append(competition)
        elif answer in ['q', 'Q']:
            break

    print()
    return changed


def update_database(whitelisted: list[model.competitions.Competition]):
    with sql.sql.Database(lib.structure.get_base_database()) as db:
        db.create_rows(model.competitions.Competition.sql_table(), whitelisted)


def main(countries_prefix: str, mode: Mode):
    updated_competitions = []
    for competition_type in model.competitions.CompetitionType:
        competitions = model.competitions.load_competitions(
            lib.structure.get_base_database(), competition_type=competition_type
        )
        competitions = filter_competitions_by_selected_countries(competitions, countries_prefix)
        selected, unselected = split_into_selected_and_unselected_competitions(competitions)

        if mode == Mode.SELECTION:
            updated_competitions.extend(select_competitions(unselected, competition_type, mode))
        else:
            updated_competitions.extend(select_competitions(selected, competition_type, mode))

    update_database(updated_competitions)


if __name__ == '__main__':
    args = parse_command_line()
    main(args.countries, Mode[args.mode.upper()])
    exit(os.EX_OK)

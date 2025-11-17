import prompt_toolkit
import prompt_toolkit.completion

import lib.structure
import model.competitions
import model.fixtures
import model.seasons
import model.teams
import sql.sql


class AutoSelectCompleter(prompt_toolkit.completion.Completer):
    def __init__(self, choices: list, prompt_text: str):
        self.choices = [str(choice) for choice in choices]
        self.choices.sort()
        self.prompt_text = prompt_text

    def get_completions(self, document, complete_event):
        text = document.text.strip().lower()
        matches = [c for c in self.choices if c.lower().startswith(text)]

        if matches:
            # Prefill the first match (auto-select) and remove it from further completions
            first_match = matches.pop(0)
            yield prompt_toolkit.completion.Completion(first_match, start_position=-len(text))

            # Yield remaining matches (ensuring the first match is not repeated)
            for match in matches:
                yield prompt_toolkit.completion.Completion(match, start_position=-len(text))

    def choose(self):
        while True:
            selection = prompt_toolkit.prompt(f"{self.prompt_text}: ", completer=self)
            if selection in self.choices:
                return selection
            print("Invalid selection. Please select from the given options.")


def pick_country(whitelisted: bool = True) -> str:
    if whitelisted:
        competitions = model.competitions.get_whitelisted_competitions(model.competitions.CompetitionType.LEAGUE)
        countries = {competition.country for competition in competitions}
    else:
        countries = set()
        with sql.sql.Database(lib.structure.get_base_database()) as db:
            competition_rows = db.fetch_all_rows(model.competitions.Competition.sql_table())
            for row in competition_rows:
                competition = model.competitions.create_competition_from_row(row)
                countries.add(competition.country)

    choices = sorted(countries)
    country_completer = AutoSelectCompleter(choices, 'Select a country')
    return country_completer.choose()


def pick_competition(
        selected_country: str,
        competition_type: model.competitions.CompetitionType = None
) -> model.competitions.Competition:
    database = lib.structure.get_database(selected_country)
    competitions = model.competitions.load_competitions(database, competition_type)

    if len(competitions) == 1:
        return competitions[0]
    else:
        competition_choices = {competition.name: competition for competition in competitions}
        names = [name for name in competition_choices.keys()]
        names.sort()
        print("Competitions:")
        print("\n".join(names))
        competition_completer = AutoSelectCompleter(list(competition_choices.keys()), 'Select a competition')
        selected_competition = competition_completer.choose()
        return competition_choices[selected_competition]


def pick_season(competition: model.competitions.Competition) -> model.seasons.Season:
    database = lib.structure.get_database(competition.country)
    seasons = model.seasons.load_seasons(database, competition)
    season_choices = {}
    for season in seasons:
        season_choices[str(season.year)] = season

    season_completer = AutoSelectCompleter(list(season_choices.keys()), 'Select a season')
    selected_season = season_completer.choose()
    return season_choices[selected_season]


def pick_team(competition: model.competitions.Competition, season: model.seasons.Season) -> model.teams.Team:
    team_choices = {}
    database = lib.structure.get_database(competition.country)
    fixtures = model.seasons.load_fixtures(database, competition, season)
    for team in model.fixtures.teams(fixtures):
        team_choices[team.name] = team

    team_completer = AutoSelectCompleter(list(team_choices.keys()), 'Select a team')
    selected_team = team_completer.choose()
    return team_choices[selected_team]

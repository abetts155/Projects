import matplotlib.font_manager
import pathlib
import re


hidden_directory = pathlib.Path.home().joinpath('.football')
database = hidden_directory.joinpath('football.db')
json_directory = hidden_directory.joinpath('json')
json_directory.mkdir(parents=True, exist_ok=True)
json_teams_directory = json_directory.joinpath('teams')
json_teams_directory.mkdir(parents=True, exist_ok=True)
json_fixtures_directory = json_directory.joinpath('fixtures')
json_fixtures_directory.mkdir(parents=True, exist_ok=True)
json_events_directory = json_directory.joinpath('events')
json_events_directory.mkdir(parents=True, exist_ok=True)
json_stats_directory = json_directory.joinpath('stats')
json_stats_directory.mkdir(parents=True, exist_ok=True)
logo_directory = hidden_directory.joinpath('logo')
logo_directory.mkdir(parents=True, exist_ok=True)
json_players_directory = hidden_directory.joinpath('players')
json_players_directory.mkdir(parents=True, exist_ok=True)
png_players_directory = json_players_directory.joinpath('pics')
png_players_directory.mkdir(parents=True, exist_ok=True)

temp_matplotlib_directory =  hidden_directory.joinpath('matplotlib')
temp_matplotlib_directory.mkdir(parents=True, exist_ok=True)

pdf_report_directory = pathlib.Path.home().joinpath('Reports')
pdf_report_directory.mkdir(parents=True, exist_ok=True)


fonts_base_path = pathlib.Path.home().joinpath("Library").joinpath("Fonts")
noto_regular_font = matplotlib.font_manager.FontProperties(fname=fonts_base_path.joinpath("NotoSans-Regular.ttf"))
noto_bold_font = matplotlib.font_manager.FontProperties(fname=fonts_base_path.joinpath("NotoSans-Bold.ttf"))


def get_seasons_json() -> pathlib.Path:
    return json_directory.joinpath('seasons.json')


def get_leagues_json() -> pathlib.Path:
    return json_directory.joinpath('leagues.json')


def get_teams_json(country: str) -> pathlib.Path:
    return json_teams_directory.joinpath('{}.json'.format(country))


def get_fixtures_json(league_id: int, season_year: int) -> pathlib.Path:
    return json_fixtures_directory.joinpath(f'{league_id}.{season_year}.json')


def get_players_json(page_id: int) -> pathlib.Path:
    return json_players_directory.joinpath(f'{page_id}.json')


def get_stats_json(fixture_id: int) -> pathlib.Path:
    return json_stats_directory.joinpath(f'{fixture_id}.json')


def get_events_json(fixture_id: int) -> pathlib.Path:
    return json_events_directory.joinpath(f'{fixture_id}.json')


def get_logo_png(team_id: int) -> pathlib.Path:
    return logo_directory.joinpath(f'{team_id}')


def get_timezone_json() -> pathlib.Path:
    return json_directory.joinpath('timezone.json')


def get_leagues_whitelist() -> pathlib.Path:
    return hidden_directory.joinpath('leagues.txt')


def get_cups_whitelist() -> pathlib.Path:
    return hidden_directory.joinpath('cups.txt')


def get_countries_whitelist() -> pathlib.Path:
    return hidden_directory.joinpath('countries.txt')


def get_matplotlib_dir(country: str) -> pathlib.Path:
    country_directory = temp_matplotlib_directory.joinpath(f"{country}")
    country_directory.mkdir(parents=True, exist_ok=True)
    return country_directory


def get_matplotlib_filename(base_directory: pathlib.Path, team_name: str) -> pathlib.Path:
    pattern = re.compile(rf"^{re.escape(team_name)}\.(\d+)\.png$")
    max_index = 0
    for file in base_directory.iterdir():
        if file.is_file():
            match = pattern.match(file.name)
            if match:
                index = int(match.group(1))
                max_index = max(max_index, index)
    next_index = max_index + 1
    return base_directory.joinpath(f"{team_name}.{next_index}.png")


def get_pdf_report_dir(country: str, competition_name: str) -> pathlib.Path:
    country_directory = pdf_report_directory.joinpath(f"{country}")
    country_directory.mkdir(parents=True, exist_ok=True)
    competition_directory = country_directory.joinpath(f"{competition_name}")
    competition_directory.mkdir(parents=True, exist_ok=True)
    return competition_directory


def store(path: pathlib.Path, response):
    with path.open('w') as out_file:
        out_file.write(response.text)

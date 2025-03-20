import argparse
import datetime
import os
import pathlib
import sys

import prompt_toolkit
import prompt_toolkit.completion
import reportlab.lib
import reportlab.lib.colors
import reportlab.lib.enums
import reportlab.lib.pagesizes
import reportlab.lib.styles
import reportlab.lib.units
import reportlab.pdfbase.pdfmetrics
import reportlab.pdfbase.ttfonts
import reportlab.platypus

import cli.cli
import cli.user_input
import football_api.structure
import lib.messages
import model.competitions
import model.fixtures
import model.seasons
import model.teams
import report.data
import report.events
import report.history
import report.title
import report.table


def parse_command_line():
    parser = argparse.ArgumentParser(description='Generate a report')
    cli.cli.add_logging_options(parser)
    cli.cli.add_venue_option(parser)
    cli.cli.add_competition_option(parser, True)

    parser.add_argument('-D',
                        '--days',
                        type=int,
                        help="generate reports this many days ahead",
                        default=0)

    return parser.parse_args()


def cleanup(output_dir: pathlib.Path):
    for file in output_dir.iterdir():
        if file.is_file() and file.suffix.lower() in {'.png', '.pdf'}:
            file.unlink()


def setup_fonts():
    base_path = pathlib.Path.home().joinpath("Library").joinpath("Fonts")
    regular_font = reportlab.pdfbase.ttfonts.TTFont("NotoSans", base_path.joinpath("NotoSans-Regular.ttf"))
    bold_font = reportlab.pdfbase.ttfonts.TTFont("NotoSans-Bold", base_path.joinpath("NotoSans-Bold.ttf"))
    italic_font = reportlab.pdfbase.ttfonts.TTFont("NotoSans-Italic", base_path.joinpath("NotoSans-Italic.ttf"))
    bold_italic_font = reportlab.pdfbase.ttfonts.TTFont("NotoSans-BoldItalic", base_path.joinpath("NotoSans-BoldItalic.ttf"))
    reportlab.pdfbase.pdfmetrics.registerFont(regular_font)
    reportlab.pdfbase.pdfmetrics.registerFont(bold_font)
    reportlab.pdfbase.pdfmetrics.registerFont(italic_font)
    reportlab.pdfbase.pdfmetrics.registerFont(bold_italic_font)

    reportlab.pdfbase.pdfmetrics.registerFontFamily(
        "NotoSans",
        normal="NotoSans",
        bold="NotoSans-Bold",
        italic="NotoSans-Italic",
        boldItalic="NotoSans-BoldItalic"
    )

    styles = reportlab.lib.styles.getSampleStyleSheet()
    base_style = styles["Normal"]
    base_style.fontName = "NotoSans"
    base_style.fontSize = 10
    base_style.leading = 12
    styles["Title"].fontName = "NotoSans-Bold"
    styles["Heading1"].fontName = "NotoSans-Bold"
    styles["Heading2"].fontName = "NotoSans-Bold"
    styles["Italic"].fontName = "NotoSans-Italic"


def on_title_page(color: reportlab.lib.colors.HexColor):

    def setup_doc(canvas, doc):
        canvas.saveState()
        canvas.setFillColor(color)
        canvas.rect(0, 0, doc.pagesize[0], doc.pagesize[1], fill=True, stroke=False)

        canvas.setFont("NotoSans-Italic", 7)
        canvas.setFillColor(reportlab.lib.colors.HexColor("#777777"))
        disclaimer = (
            "This document is provided for informational purposes only. "
            "It does not constitute betting advice or a recommendation to wager. "
            "Please gamble responsibly."
        )
        text_width = canvas.stringWidth(disclaimer, "Helvetica-Oblique", 7)
        canvas.drawString(
            (doc.pagesize[0] - text_width) / 2,
            0.4 * reportlab.lib.units.inch,
            disclaimer
        )

        canvas.restoreState()

    return setup_doc


def on_report_pages(color: reportlab.lib.colors.HexColor, fixture: model.fixtures.Fixture):

    def setup_doc(canvas, doc):
        canvas.saveState()
        canvas.setFillColor(color)
        canvas.rect(0, 0, doc.pagesize[0], doc.pagesize[1], fill=True, stroke=False)

        canvas.setFillColor(reportlab.lib.colors.HexColor("#888888"))

        canvas.setFont("NotoSans-Bold", 9)
        canvas.drawRightString(
            doc.pagesize[0] - reportlab.lib.units.inch,
            doc.pagesize[1] - 0.5 * reportlab.lib.units.inch,
            f"{fixture.home_team.name} vs. {fixture.away_team.name}"
        )

        canvas.setStrokeColor(reportlab.lib.colors.HexColor("#DDDDDD"))
        canvas.setLineWidth(0.5)
        canvas.line(
            reportlab.lib.units.inch,
            doc.pagesize[1] - 0.55 * reportlab.lib.units.inch,
            doc.pagesize[0] - reportlab.lib.units.inch,
            doc.pagesize[1] - 0.55 * reportlab.lib.units.inch
        )

        canvas.setFont("NotoSans", 8)
        canvas.drawCentredString(
            doc.pagesize[0] / 2.0,
            0.5 * reportlab.lib.units.inch,
            f"Page {doc.page}"
        )

        canvas.restoreState()

    return setup_doc


def create_league_tables_per_period(
        fixtures: list[model.fixtures.Fixture],
        team: model.teams.Team,
        venue: model.fixtures.Venue
):
    table_first_half = report.table.create_league_table(
        fixtures,
        model.fixtures.Period.FIRST,
        venue,
        [team]
    )

    table_second_half = report.table.create_league_table(
        fixtures,
        model.fixtures.Period.SECOND,
        venue,
        [team]
    )

    table_full_time = report.table.create_league_table(
        fixtures,
        model.fixtures.Period.FULL,
        venue,
        [team]
    )

    return table_first_half + table_second_half + table_full_time


def create_this_season_form(
        fixtures: list[model.fixtures.Fixture],
        team: model.teams.Team,
        venue: model.fixtures.Venue
):
    goals_for = {}
    goals_against = {}
    outcomes = {}
    periods = [model.fixtures.Period.FIRST, model.fixtures.Period.SECOND, model.fixtures.Period.FULL]
    for period in periods:
        goals_for[period] = []
        goals_against[period] = []
        outcomes[period] = []

    for fixture in fixtures:
        if fixture.finished:
            if venue == model.fixtures.Venue.HOME and team == fixture.home_team:
                analyse = True
            elif venue == model.fixtures.Venue.AWAY and team == fixture.away_team:
                analyse = True
            elif venue == model.fixtures.Venue.ANYWHERE and team in [fixture.home_team, fixture.away_team]:
                analyse = True
            else:
                analyse = False

            if analyse:
                for period in periods:
                    score = fixture.result(period)
                    if score is not None:
                        score = model.fixtures.canonicalise_scoreline(fixture, team, score)
                        if model.fixtures.win(score):
                            outcomes[period].append(model.fixtures.win)
                        elif model.fixtures.loss(score):
                            outcomes[period].append(model.fixtures.loss)
                        else:
                            outcomes[period].append(model.fixtures.draw)

                        goals_for[period].append(score.left)
                        goals_against[period].append(score.right)

    elements = []
    if sum(len(outcomes[period]) for period in periods):
        title = f"{team.name}'s {venue.value} Form This Season"

        styles = reportlab.lib.styles.getSampleStyleSheet()
        title_style = reportlab.lib.styles.ParagraphStyle(
            name="CenteredTitle",
            parent=styles["Title"],
            alignment=reportlab.lib.enums.TA_CENTER,
            fontName="NotoSans-Bold",
            fontSize=16,
            leading=18
        )

        elements.extend([
            reportlab.platypus.Paragraph(title, title_style),
            reportlab.platypus.Spacer(1, 0.2 * reportlab.lib.units.inch)
        ])

        for period in periods:
            history_in_period = report.history.create_this_season_history(
                competition,
                team,
                period,
                goals_for[period],
                goals_against[period],
                outcomes[period]
            )
            elements.extend(history_in_period)

        elements.append(reportlab.platypus.PageBreak())

    return elements


def create_report(
        competition: model.competitions.Competition,
        fixtures_this_season: list[model.fixtures.Fixture],
        fixture: model.fixtures.Fixture,
        seasons: list[model.seasons.Season],
        historical_data: dict[model.teams.Team, report.data.TeamData]
):
    lib.messages.vanilla_message(f"Creating report for {fixture.home_team.name} vs {fixture.away_team.name}")
    title_page = report.title.create_title_page(competition, fixture)

    home_tables = create_league_tables_per_period(fixtures_this_season, fixture.home_team, model.fixtures.Venue.HOME)
    home_data = historical_data[fixture.home_team]
    home_history = report.history.create_history(
        competition,
        fixture.home_team,
        model.fixtures.Venue.HOME,
        seasons,
        home_data
    )
    home_this_season = create_this_season_form(
        fixtures_this_season,
        fixture.home_team,
        model.fixtures.Venue.HOME
    )
    home_this_season_anywhere = create_this_season_form(
        fixtures_this_season,
        fixture.home_team,
        model.fixtures.Venue.ANYWHERE
    )

    away_tables = create_league_tables_per_period(fixtures_this_season, fixture.away_team, model.fixtures.Venue.AWAY)
    away_data = historical_data[fixture.away_team]
    away_history = report.history.create_history(
        competition,
        fixture.away_team,
        model.fixtures.Venue.AWAY,
        seasons,
        away_data
    )
    away_this_season = create_this_season_form(
        fixtures_this_season,
        fixture.away_team,
        model.fixtures.Venue.AWAY
    )
    away_this_season_anywhere = create_this_season_form(
        fixtures_this_season,
        fixture.away_team,
        model.fixtures.Venue.ANYWHERE
    )

    head_to_head = report.history.create_head_to_head(competition, fixture.home_team, fixture.away_team)

    complete_table = report.table.create_league_table(
        fixtures_this_season,
        model.fixtures.Period.FULL,
        model.fixtures.Venue.ANYWHERE,
        [fixture.home_team, fixture.away_team]
    )

    story = (
            title_page +
            home_tables +
            home_history +
            home_this_season +
            home_this_season_anywhere +
            away_tables +
            away_history +
            away_this_season +
            away_this_season_anywhere +
            head_to_head +
            complete_table
    )

    country_pdf_report_dir = football_api.structure.get_pdf_report_dir(competition.country, competition.name)
    filename = country_pdf_report_dir.joinpath(f"{fixture.home_team.name} versus {fixture.away_team.name}.pdf")
    doc = reportlab.platypus.SimpleDocTemplate(str(filename), pagesize=reportlab.lib.pagesizes.A4)
    color = reportlab.lib.colors.HexColor("#F9F9F9")
    doc.build(story, onFirstPage=on_title_page(color), onLaterPages=on_report_pages(color, fixture))


def main(competition: model.competitions.Competition, fixtures_under_analysis: list[model.fixtures.Fixture]):
    lib.messages.vanilla_message(f"{'*' * 10} Analysing {competition} (id={competition.id}) {'*' * 10}")
    setup_fonts()
    country_pdf_report_dir = football_api.structure.get_pdf_report_dir(competition.country, competition.name)
    cleanup(country_pdf_report_dir)
    matplolib_output_dir = football_api.structure.get_matplotlib_dir(competition.country)
    cleanup(matplolib_output_dir)

    season = model.seasons.load_current_season(competition)
    fixtures_this_season = model.seasons.load_fixtures(competition, season)

    team_venues = dict()
    for fixture in fixtures_under_analysis:
        team_venues[fixture.home_team] = model.fixtures.Venue.HOME
        team_venues[fixture.away_team] = model.fixtures.Venue.AWAY

    seasons = model.seasons.load_seasons(competition)
    historical_data = report.history.collect_historical_data(competition, seasons, team_venues)

    for fixture in fixtures_under_analysis:
        create_report(competition, fixtures_this_season, fixture, seasons, historical_data)


def pick_fixtures(
        fixtures: list[model.fixtures.Fixture],
        teams: set[model.teams.Team]
) -> list[model.fixtures.Fixture]:
    unpicked_teams = set(teams)
    candidate_fixtures = set()
    for fixture in fixtures:
        if not fixture.finished:
            if fixture.home_team in unpicked_teams:
                candidate_fixtures.add(fixture)
                unpicked_teams.remove(fixture.home_team)

            if fixture.away_team in unpicked_teams:
                candidate_fixtures.add(fixture)
                unpicked_teams.remove(fixture.away_team)

        if not unpicked_teams:
            break

    print("Upcoming Fixtures")
    sorted_fixtures = sorted(candidate_fixtures, key=lambda fixture: fixture.date)
    choices = []
    for i, fixture in enumerate(sorted_fixtures, start=1):
        print(f"({i}) {fixture.date.strftime('%d-%m-%y')} - {fixture.home_team.name} vs {fixture.away_team.name}")
        choices.append(str(i))

    sentinel = 'q'
    choices.append(sentinel)

    completer = prompt_toolkit.completion.WordCompleter(choices, ignore_case=True)
    done = False
    selected_fixtures = []
    while not done and len(selected_fixtures) < len(choices):
        selection = prompt_toolkit.prompt(f"Select a fixture or {sentinel} to quit: ", completer=completer)
        if selection in choices:
            if selection == sentinel:
                done = True
            else:
                index = int(selection) - 1
                selected_fixtures.append(sorted_fixtures[index])
        else:
            print("Invalid selection. Please choose a number from the list.")

    return selected_fixtures


def gather_competition_fixtures(
        selected_competition_ids: list[int],
        days: int
) -> dict[model.competitions.Competition, list[model.fixtures.Fixture]]:
    competition_to_fixtures = {}
    if days > 0:
        whitelisted = []
        if selected_competition_ids is not None:
            for competition_id in selected_competition_ids:
                competition = model.competitions.load_competition(competition_id)
                if competition not in whitelisted:
                    whitelisted.append(competition)
        else:
            with open(football_api.structure.get_leagues_whitelist(), 'r') as in_file:
                for line in in_file:
                    competition_id = int(line.strip())
                    competition = model.competitions.load_competition(competition_id)
                    whitelisted.append(competition)

        whitelisted.sort(key=lambda competition: (competition.country, competition.id))
        for competition in whitelisted:
            fixtures = model.fixtures.load_fixtures_within_window(competition, days)
            if fixtures:
                analyse = True
                if selected_competition_ids is None:
                    answer = input(f"Analyse {competition}? (Enter = Yes, 'N' or 'n' = No) ")
                    answer = answer.strip()
                    if answer in ['n', 'N']:
                        analyse = False

                if analyse:
                    competition_to_fixtures[competition] = fixtures

    if not competition_to_fixtures:
        competitions = []
        if selected_competition_ids is None:
            country = cli.user_input.pick_country()
            competition = cli.user_input.pick_competition(country, model.competitions.CompetitionType.LEAGUE)
            competitions.append(competition)
        else:
            for competition_id in selected_competition_ids:
                competition = model.competitions.load_competition(competition_id)
                competitions.append(competition)

        for competition in competitions:
            season = model.seasons.load_current_season(competition)
            fixtures = model.seasons.load_fixtures(competition, season)
            teams = model.fixtures.teams(fixtures)
            competition_to_fixtures[competition] = pick_fixtures(fixtures, teams)

    return competition_to_fixtures


if __name__ == '__main__':
    args = parse_command_line()
    cli.cli.set_logging_options(args)
    competition_to_fixtures = gather_competition_fixtures(args.competition, args.days)
    for competition, fixtures in competition_to_fixtures.items():
        main(competition, fixtures)
    sys.exit(os.EX_OK)

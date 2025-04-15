import argparse
import concurrent.futures
import os
import pathlib
import re
import sys

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
import lib.structure
import lib.messages
import model.competitions
import model.fixtures
import model.seasons
import model.teams
import report.data
import report.events
import report.frontmatter
import report.history
import report.table


def parse_command_line():
    parser = argparse.ArgumentParser(description='Generate a report')
    cli.cli.add_logging_options(parser)
    cli.cli.add_venue_option(parser)

    parser.add_argument('-C',
                        '--countries',
                        nargs='+',
                        type=str,
                        help='pick out countries that start with this sequence')

    parser.add_argument('-H',
                        '--hours',
                        type=int,
                        help='the number of hours to look forward',
                        default=3)

    parser.add_argument('-A',
                        '--all-competitions',
                        action='store_true',
                        help='analyse all competitions that satisfy the given constraints',
                        default=False)

    return parser.parse_args()


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
        competition: model.competitions.Competition,
        fixtures: list[model.fixtures.Fixture],
        team: model.teams.Team,
        venue: model.fixtures.Venue
):
    table_first_half = report.table.create_league_table(
        competition,
        fixtures,
        model.fixtures.Period.FIRST,
        venue,
        [team]
    )

    table_second_half = report.table.create_league_table(
        competition,
        fixtures,
        model.fixtures.Period.SECOND,
        venue,
        [team]
    )

    table_full_time = report.table.create_league_table(
        competition,
        fixtures,
        model.fixtures.Period.FULL,
        venue,
        [team]
    )

    return table_first_half + table_second_half + table_full_time


def create_this_season_form(
        competition: model.competitions.Competition,
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


def sanitize_name(name: str) -> str:
    return re.sub(r'[\\/:"*?<>|]+', '-', name)


def create_report(
        competition: model.competitions.Competition,
        fixtures_this_season: list[model.fixtures.Fixture],
        seasons: list[model.seasons.Season],
        historical_data: dict[model.teams.Team, report.data.TeamData],
        fixture: model.fixtures.Fixture
):
    country_pdf_report_dir = lib.structure.get_pdf_report_dir(competition.country, competition.name)
    fixture_name = f"{sanitize_name(fixture.home_team.name)} versus {sanitize_name(fixture.away_team.name)}"
    filename = country_pdf_report_dir.joinpath(f"{fixture_name}.pdf")
    if not filename.exists():
        lib.messages.vanilla_message(f"Creating report for {fixture.home_team.name} vs {fixture.away_team.name}")
        setup_fonts()

        title_page = report.frontmatter.create_title_page(competition, fixture)

        home_tables = create_league_tables_per_period(
            competition,
            fixtures_this_season,
            fixture.home_team,
            model.fixtures.Venue.HOME
        )
        home_data = historical_data[fixture.home_team]
        home_history = report.history.create_history(
            competition,
            fixture.home_team,
            model.fixtures.Venue.HOME,
            seasons,
            home_data
        )
        home_this_season = create_this_season_form(
            competition,
            fixtures_this_season,
            fixture.home_team,
            model.fixtures.Venue.HOME
        )
        home_this_season_anywhere = create_this_season_form(
            competition,
            fixtures_this_season,
            fixture.home_team,
            model.fixtures.Venue.ANYWHERE
        )

        home_player_summary = report.history.create_player_summary(competition, home_data, model.fixtures.Venue.HOME)

        away_tables = create_league_tables_per_period(
            competition,
            fixtures_this_season,
            fixture.away_team,
            model.fixtures.Venue.AWAY
        )
        away_data = historical_data[fixture.away_team]
        away_history = report.history.create_history(
            competition,
            fixture.away_team,
            model.fixtures.Venue.AWAY,
            seasons,
            away_data
        )
        away_this_season = create_this_season_form(
            competition,
            fixtures_this_season,
            fixture.away_team,
            model.fixtures.Venue.AWAY
        )
        away_this_season_anywhere = create_this_season_form(
            competition,
            fixtures_this_season,
            fixture.away_team,
            model.fixtures.Venue.ANYWHERE
        )

        away_player_summary = report.history.create_player_summary(competition, away_data, model.fixtures.Venue.AWAY)

        head_to_head = report.history.create_head_to_head(competition, fixture.home_team, fixture.away_team)

        complete_table = report.table.create_league_table(
            competition,
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
                home_player_summary +
                away_tables +
                away_history +
                away_this_season +
                away_this_season_anywhere +
                away_player_summary +
                head_to_head +
                complete_table
        )

        doc = reportlab.platypus.SimpleDocTemplate(str(filename), pagesize=reportlab.lib.pagesizes.A4)
        color = reportlab.lib.colors.HexColor("#F9F9F9")
        doc.build(story, onFirstPage=on_title_page(color), onLaterPages=on_report_pages(color, fixture))


def gather_competition_fixtures(
        hours: int,
        countries_prefixes: list[str],
        analyse_all_competitions: bool
) -> dict[model.competitions.Competition, list[model.fixtures.Fixture]]:
    competition_to_fixtures = {}
    whitelisted = model.competitions.get_whitelisted_competitions(model.competitions.CompetitionType.LEAGUE)

    if countries_prefixes:
        whitelisted = [
            c for c in whitelisted for prefix in countries_prefixes if c.country.casefold().startswith(prefix.casefold())
        ]

    if not analyse_all_competitions:
        print("Pick competitions to analyse. Only 'N' or 'n' means No.")

    for competition in whitelisted:
        database = lib.structure.get_database(competition.country)
        fixtures = model.fixtures.load_fixtures_within_window(database, competition, hours)
        if fixtures:
            if analyse_all_competitions:
                competition_to_fixtures[competition] = fixtures
            else:
                answer = input(f"{competition}? ")
                answer = answer.strip()
                if answer not in ['n', 'N']:
                    competition_to_fixtures[competition] = fixtures

    return competition_to_fixtures


def main(hours: int, country_prefixes: list[str], analyse_all_competitions: bool):
    competition_to_fixtures = gather_competition_fixtures(hours, country_prefixes, analyse_all_competitions)
    for competition, fixtures in competition_to_fixtures.items():
        lib.messages.vanilla_message(f"{'*' * 10} Analysing {competition} (id={competition.id}) {'*' * 10}")
        path = lib.structure.get_matplotlib_dir(competition.country)
        lib.structure.purge_png_files(path)

        database = lib.structure.get_database(competition.country)
        season = model.seasons.load_current_season(database, competition)
        fixtures_this_season = model.seasons.load_fixtures(database, competition, season)

        team_venues = dict()
        for fixture in fixtures:
            team_venues[fixture.home_team] = model.fixtures.Venue.HOME
            team_venues[fixture.away_team] = model.fixtures.Venue.AWAY

        seasons = model.seasons.load_seasons(database, competition)
        historical_data = report.history.collect_historical_data(competition, seasons, team_venues)

        with concurrent.futures.ProcessPoolExecutor(max_workers=2) as executor:
            futures = [
                executor.submit(create_report, competition, fixtures_this_season, seasons, historical_data, fixture)
                for fixture in fixtures
            ]

            for future in futures:
                future.result()


if __name__ == '__main__':
    args = parse_command_line()
    cli.cli.set_logging_options(args)
    main(args.hours, args.countries, args.all_competitions)
    sys.exit(os.EX_OK)

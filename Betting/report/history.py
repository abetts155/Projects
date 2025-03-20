import dataclasses
import enum
import typing

import matplotlib
import matplotlib.font_manager
import matplotlib.patches
import matplotlib.pyplot as plt
import matplotlib.ticker
import pathlib

import reportlab.platypus
import reportlab.lib.colors
import reportlab.lib.enums
import reportlab.lib.styles
import reportlab.lib.units

import football_api.structure
import model.competitions
import model.fixtures
import model.seasons
import model.teams
import report.data


def collect_historical_data(
        competition: model.competitions.Competition,
        seasons: list[model.seasons.Season],
        team_venues: dict[model.teams.Team, model.fixtures.Venue]
) -> dict[model.teams.Team, report.data.TeamData]:
    historical_data = {}
    for team in team_venues.keys():
        historical_data[team] = report.data.TeamData(team)

    for season in seasons:
        fixtures = model.seasons.load_fixtures(competition, season)
        for fixture in fixtures:
            if fixture.finished:
                if fixture.home_team in team_venues and team_venues[fixture.home_team] == model.fixtures.Venue.HOME:
                    home_data = historical_data[fixture.home_team]
                    home_data.collect(fixture, model.fixtures.Period.FULL, model.fixtures.Venue.HOME)

                if fixture.away_team in team_venues and team_venues[fixture.away_team] == model.fixtures.Venue.AWAY:
                    away_data = historical_data[fixture.away_team]
                    away_data.collect(fixture, model.fixtures.Period.FULL, model.fixtures.Venue.AWAY)

    return historical_data


@dataclasses.dataclass(slots=True)
class PieChunk:
    value: int | float
    label: str
    color: str


def create_pie(
        chunks: list[PieChunk],
        donut: str,
        title: str,
        filename: pathlib.Path
):
    alive_chunks = [chunk for chunk in chunks if chunk.value != 0]
    values = [chunk.value for chunk in alive_chunks]
    labels = [chunk.label for chunk in alive_chunks]
    colors = [chunk.color for chunk in alive_chunks]

    fig, ax = plt.subplots(figsize=(8, 8))

    _, texts, autotexts = ax.pie(
        values,
        labels=labels,
        startangle=90,
        colors=colors,
        autopct='%1.0f%%',
        pctdistance=0.85,
        wedgeprops=dict(width=0.3, edgecolor='white')
    )

    for text in texts:
        text.set_fontproperties(football_api.structure.noto_regular_font)
        text.set_fontsize(20)

    for autotext in autotexts:
        autotext.set_fontproperties(football_api.structure.noto_regular_font)
        autotext.set_color('white')
        autotext.set_fontsize(20)

    donut_text = f"{sum(values)}\n{donut}"
    ax.text(0, 0, donut_text, ha='center', va='center', fontsize=20, color='grey',
            fontproperties=football_api.structure.noto_bold_font)
    ax.axis('equal')

    plt.figtext(0.5, 0.05, title, ha='center', fontsize=20, fontproperties=football_api.structure.noto_bold_font)
    plt.draw()
    plt.savefig(filename, dpi=300)
    plt.close()


class ChartLabel(enum.StrEnum):
    WON = 'W'
    DREW = 'D'
    LOST = 'L'
    FOR = 'F'
    AGAINST = 'A'
    GOALS_0 = '0'
    GOALS_1 = '1'
    GOALS_2 = '2'
    GOALS_3_PLUS = '3+'
    BTS = 'BTS'
    NTS = 'NTS'
    ONLY_ONE = 'OO'


class ChartColor(enum.StrEnum):
    WON = '#6A8DFF'
    DREW = '#FFB347'
    LOST = '#000000'
    FOR = '#3F51B5'
    AGAINST = '#E53935'
    GOALS_0 = '#003F5C'
    GOALS_1 = '#7A5195'
    GOALS_2 = '#EF5675'
    GOALS_3_PLUS = '#FFA600'
    BTS = '#3F51B5'
    NTS = '#000000'
    ONLY_ONE = '#E53935'


def create_history_page_1(
        competition: model.competitions.Competition,
        team: model.teams.Team,
        team_data: report.data.TeamData
):
    country_specific_output_dir = football_api.structure.get_matplotlib_dir(competition.country)

    filename1 = football_api.structure.get_matplotlib_filename(country_specific_output_dir, team.name)
    chunks1 = [
        PieChunk(len(team_data.won), ChartLabel.WON, ChartColor.WON),
        PieChunk(len(team_data.drawn), ChartLabel.DREW, ChartColor.DREW),
        PieChunk(len(team_data.lost), ChartLabel.LOST, ChartColor.LOST)
    ]
    create_pie(
        chunks1,
        'Games',
        "Results Breakdown",
        filename1
    )
    img1 = reportlab.platypus.Image(
        filename1, width=2.5 * reportlab.lib.units.inch, height=2.5 * reportlab.lib.units.inch
    )

    filename2 = football_api.structure.get_matplotlib_filename(country_specific_output_dir, team.name)
    chunks2 = [
        PieChunk(team_data.goals_for.total, ChartLabel.FOR, ChartColor.FOR),
        PieChunk(team_data.goals_against.total, ChartLabel.AGAINST, ChartColor.AGAINST)
    ]
    create_pie(
        chunks2,
        'Goals',
        "Total Goals Breakdown",
        filename2
    )
    img2 = reportlab.platypus.Image(
        filename2, width=2.5 * reportlab.lib.units.inch, height=2.5 * reportlab.lib.units.inch
    )

    filename3 = football_api.structure.get_matplotlib_filename(country_specific_output_dir, team.name)
    chunks3 = [
        PieChunk(len(team_data.goals_for.goals_0), ChartLabel.GOALS_0, ChartColor.GOALS_0),
        PieChunk(len(team_data.goals_for.goals_1), ChartLabel.GOALS_1, ChartColor.GOALS_1),
        PieChunk(len(team_data.goals_for.goals_2), ChartLabel.GOALS_2, ChartColor.GOALS_2),
        PieChunk(len(team_data.goals_for.goals_3_or_more), ChartLabel.GOALS_3_PLUS, ChartColor.GOALS_3_PLUS)
    ]
    create_pie(
        chunks3,
        'Games',
        "Goals For in a Game",
        filename3
    )
    img3 = reportlab.platypus.Image(
        filename3, width=2.5 * reportlab.lib.units.inch, height=2.5 * reportlab.lib.units.inch
    )

    filename4 = football_api.structure.get_matplotlib_filename(country_specific_output_dir, team.name)
    chunks4 = [
        PieChunk(len(team_data.goals_against.goals_0), ChartLabel.GOALS_0, ChartColor.GOALS_0),
        PieChunk(len(team_data.goals_against.goals_1), ChartLabel.GOALS_1, ChartColor.GOALS_1),
        PieChunk(len(team_data.goals_against.goals_2), ChartLabel.GOALS_2, ChartColor.GOALS_2),
        PieChunk(len(team_data.goals_against.goals_3_or_more), ChartLabel.GOALS_3_PLUS, ChartColor.GOALS_3_PLUS)
    ]
    create_pie(
        chunks4,
        'Games',
        "Goals Against in a Game",
        filename4
    )
    img4 = reportlab.platypus.Image(
        filename4, width=2.5 * reportlab.lib.units.inch, height=2.5 * reportlab.lib.units.inch
    )

    filename5 = football_api.structure.get_matplotlib_filename(country_specific_output_dir, team.name)
    chunks5 = [
        PieChunk(len(team_data.total_goals.goals_0), ChartLabel.GOALS_0, ChartColor.GOALS_0),
        PieChunk(len(team_data.total_goals.goals_1), ChartLabel.GOALS_1, ChartColor.GOALS_1),
        PieChunk(len(team_data.total_goals.goals_2), ChartLabel.GOALS_2, ChartColor.GOALS_2),
        PieChunk(len(team_data.total_goals.goals_3_or_more), ChartLabel.GOALS_3_PLUS, ChartColor.GOALS_3_PLUS)
    ]
    create_pie(
        chunks5,
        'Games',
        "Total Goals in a Game",
        filename5
    )
    img5 = reportlab.platypus.Image(
        filename5, width=2.5 * reportlab.lib.units.inch, height=2.5 * reportlab.lib.units.inch
    )

    filename6 = football_api.structure.get_matplotlib_filename(country_specific_output_dir, team.name)
    chunks6 = [
        PieChunk(len(team_data.bts), ChartLabel.BTS, ChartColor.BTS),
        PieChunk(len(team_data.nts), ChartLabel.NTS, ChartColor.NTS),
        PieChunk(team_data.played() - len(team_data.bts) - len(team_data.nts), ChartLabel.ONLY_ONE, ChartColor.ONLY_ONE)
    ]
    create_pie(
        chunks6,
        'Games',
        "Who Scored: Both (B), None (N), or Only One (OO)",
        filename6
    )
    img6 = reportlab.platypus.Image(
        filename6, width=2.5 * reportlab.lib.units.inch, height=2.5 * reportlab.lib.units.inch
    )

    table = reportlab.platypus.Table(
        [
            [img1, img2],
            [img3, img4],
            [img5, img6]
        ],
        colWidths=[3 * reportlab.lib.units.inch, 3 * reportlab.lib.units.inch],
        hAlign="CENTER"
    )

    return table


def create_history_page_2(
        competition: model.competitions.Competition,
        team: model.teams.Team,
        team_data: report.data.TeamData
):
    country_specific_output_dir = football_api.structure.get_matplotlib_dir(competition.country)

    filename1 = football_api.structure.get_matplotlib_filename(country_specific_output_dir, team.name)
    chunks1 = [
        PieChunk(len(team_data.half_time_winning.won), ChartLabel.WON, ChartColor.WON),
        PieChunk(len(team_data.half_time_winning.drew), ChartLabel.DREW, ChartColor.DREW),
        PieChunk(len(team_data.half_time_winning.lost), ChartLabel.LOST, ChartColor.LOST)
    ]
    create_pie(
        chunks1,
        'Games',
        "FT Result when Winning at HT",
        filename1
    )
    img1 = reportlab.platypus.Image(
        filename1, width=2.5 * reportlab.lib.units.inch, height=2.5 * reportlab.lib.units.inch
    )

    filename2 = football_api.structure.get_matplotlib_filename(country_specific_output_dir, team.name)
    chunks2 = [
        PieChunk(len(team_data.half_time_drawing.won), ChartLabel.WON, ChartColor.WON),
        PieChunk(len(team_data.half_time_drawing.drew), ChartLabel.DREW, ChartColor.DREW),
        PieChunk(len(team_data.half_time_drawing.lost), ChartLabel.LOST, ChartColor.LOST)
    ]
    create_pie(
        chunks2,
        'Games',
        "FT Result when Drawing at HT",
        filename2
    )
    img2 = reportlab.platypus.Image(
        filename2, width=2.5 * reportlab.lib.units.inch, height=2.5 * reportlab.lib.units.inch
    )

    filename3 = football_api.structure.get_matplotlib_filename(country_specific_output_dir, team.name)
    chunks3 = [
        PieChunk(len(team_data.half_time_losing.won), ChartLabel.WON, ChartColor.WON),
        PieChunk(len(team_data.half_time_losing.drew), ChartLabel.DREW, ChartColor.DREW),
        PieChunk(len(team_data.half_time_losing.lost), ChartLabel.LOST, ChartColor.LOST)
    ]
    create_pie(
        chunks3,
        'Games',
        "FT Result when Losing at HT",
        filename3
    )
    img3 = reportlab.platypus.Image(
        filename3, width=2.5 * reportlab.lib.units.inch, height=2.5 * reportlab.lib.units.inch
    )

    filename4 = football_api.structure.get_matplotlib_filename(country_specific_output_dir, team.name)
    chunks4 = [
        PieChunk(len(team_data.half_time_0_goals.goals_0), ChartLabel.GOALS_0, ChartColor.GOALS_0),
        PieChunk(len(team_data.half_time_0_goals.goals_1), ChartLabel.GOALS_1, ChartColor.GOALS_1),
        PieChunk(len(team_data.half_time_0_goals.goals_2), ChartLabel.GOALS_2, ChartColor.GOALS_2),
        PieChunk(len(team_data.half_time_0_goals.goals_3_or_more), ChartLabel.GOALS_3_PLUS, ChartColor.GOALS_3_PLUS)
    ]
    create_pie(
        chunks4,
        'Games',
        "Goals in 2nd Half when 0 goals in 1st Half",
        filename4
    )
    img4 = reportlab.platypus.Image(
        filename4, width=2.5 * reportlab.lib.units.inch, height=2.5 * reportlab.lib.units.inch
    )

    filename5 = football_api.structure.get_matplotlib_filename(country_specific_output_dir, team.name)
    chunks5 = [
        PieChunk(len(team_data.half_time_1_goal.goals_0), ChartLabel.GOALS_0, ChartColor.GOALS_0),
        PieChunk(len(team_data.half_time_1_goal.goals_1), ChartLabel.GOALS_1, ChartColor.GOALS_1),
        PieChunk(len(team_data.half_time_1_goal.goals_2), ChartLabel.GOALS_2, ChartColor.GOALS_2),
        PieChunk(len(team_data.half_time_1_goal.goals_3_or_more), ChartLabel.GOALS_3_PLUS, ChartColor.GOALS_3_PLUS)
    ]
    create_pie(
        chunks5,
        'Games',
        "Goals in 2nd Half when 1 goal in 1st Half",
        filename5
    )
    img5 = reportlab.platypus.Image(
        filename5, width=2.5 * reportlab.lib.units.inch, height=2.5 * reportlab.lib.units.inch
    )

    filename6 = football_api.structure.get_matplotlib_filename(country_specific_output_dir, team.name)
    chunks6 = [
        PieChunk(len(team_data.half_time_2_goals.goals_0), ChartLabel.GOALS_0, ChartColor.GOALS_0),
        PieChunk(len(team_data.half_time_2_goals.goals_1), ChartLabel.GOALS_1, ChartColor.GOALS_1),
        PieChunk(len(team_data.half_time_2_goals.goals_2), ChartLabel.GOALS_2, ChartColor.GOALS_2),
        PieChunk(len(team_data.half_time_2_goals.goals_3_or_more), ChartLabel.GOALS_3_PLUS, ChartColor.GOALS_3_PLUS)
    ]
    create_pie(
        chunks6,
        'Games',
        "Goals in 2nd Half when 2+ goals in 1st Half",
        filename6
    )
    img6 = reportlab.platypus.Image(
        filename6, width=2.5 * reportlab.lib.units.inch, height=2.5 * reportlab.lib.units.inch
    )

    table = reportlab.platypus.Table(
        [
            [img1, img2],
            [img3, img4],
            [img5, img6],
        ],
        colWidths=[3 * reportlab.lib.units.inch, 3 * reportlab.lib.units.inch],
        hAlign="CENTER"
    )

    return table


def create_history(
        competition: model.competitions.Competition,
        team: model.teams.Team,
        venue: model.fixtures.Venue,
        seasons: list[model.seasons.Season],
        team_data: report.data.TeamData
):
    table1 = create_history_page_1(competition, team, team_data)
    table2 = create_history_page_2(competition, team, team_data)

    styles = reportlab.lib.styles.getSampleStyleSheet()
    title_style = reportlab.lib.styles.ParagraphStyle(
        name="CenteredTitle",
        parent=styles["Title"],
        alignment=reportlab.lib.enums.TA_CENTER,
        fontName="NotoSans-Bold",
        fontSize=16,
        leading=18
    )

    subtitle_style = reportlab.lib.styles.ParagraphStyle(
        name="CenteredSubtitle",
        parent=styles["Heading2"],
        alignment=reportlab.lib.enums.TA_CENTER,
        textColor=reportlab.lib.colors.HexColor("#555555"),
        fontName="NotoSans",
        fontSize=10,
        leading=11,
        italic=True
    )

    title = f"{team.name}'s {venue.value} Record in the {competition.name} "
    subtitle = f"Data Available from the Last {len(seasons)} Seasons"

    return [
        reportlab.platypus.Paragraph(title, title_style),
        reportlab.platypus.Paragraph(subtitle, subtitle_style),
        reportlab.platypus.Spacer(1, 0.2 * reportlab.lib.units.inch),
        table1,
        reportlab.platypus.PageBreak(),
        table2,
        reportlab.platypus.PageBreak()
    ]


def create_outcome_history(
        period: model.fixtures.Period,
        outcomes: list[typing.Callable],
        filename: pathlib.Path
):
    games = list(range(1, len(outcomes) + 1))
    if len(games) <= 10:
        marker_size = 200
        font_size = 10
    else:
        marker_size = max(2000 // len(games), 50)
        font_size = max(150 // len(games), 6)

    fig, ax = plt.subplots(figsize=(6, 6))
    for x, outcome in zip(games, outcomes):
        if outcome == model.fixtures.win:
            text = ChartLabel.WON
            color = ChartColor.WON
        elif outcome == model.fixtures.draw:
            text = ChartLabel.DREW
            color = ChartColor.DREW
        else:
            text = ChartLabel.LOST
            color = ChartColor.LOST

        ax.scatter(x=x, y=0, c=color, s=marker_size, marker='s')
        ax.text(
            x, 0, text,
            ha='center', va='center',
            fontsize=font_size, fontweight='bold', color='white', fontproperties=football_api.structure.noto_bold_font
        )

    ax.set_xlabel("Game", fontproperties=football_api.structure.noto_bold_font, fontsize=12)
    ax.set_xticks(games)
    ax.xaxis.set_major_locator(matplotlib.ticker.MaxNLocator(15, integer=True))
    ax.tick_params(axis='x', labelsize=12)

    ax.set_yticks([])

    for spine in ax.spines.values():
        spine.set_visible(False)

    ax.text(
        0, 1,
        f"{period.value}",
        transform=ax.transAxes,
        ha='left',
        va='bottom',
        fontproperties=football_api.structure.noto_bold_font,
        fontsize=12
    )

    plt.savefig(filename, dpi=300)
    plt.close()


def create_goals_history(
        period: model.fixtures.Period,
        goals_for: list[int],
        goals_against: list[int],
        filename: pathlib.Path
):
    games = list(range(1, len(goals_for) + 1))
    fig, ax = plt.subplots(figsize=(6, 6))

    bar_width = max(0.2, min(0.6, 10 / len(games)))
    goals_against =  [-g for g in goals_against]
    ax.bar(games, goals_for, color=ChartColor.FOR, width=bar_width, label="Goals For")
    ax.bar(games, goals_against, color=ChartColor.AGAINST, width=bar_width, label="Goals Against")

    ax.axhline(0, color='black', linewidth=1, linestyle='--')
    ax.set_xlabel("Game", fontproperties=football_api.structure.noto_bold_font, fontsize=12)
    ax.set_xticks(games)
    ax.xaxis.set_major_locator(matplotlib.ticker.MaxNLocator(15, integer=True))

    y_value_min = min(goals_against)
    y_value_max = max(goals_for)
    ax.set_yticks(list(range(y_value_min, y_value_max + 1)))
    ax.tick_params(labelsize=12)

    for spine in ax.spines.values():
        spine.set_visible(False)

    ax.text(
        0, 1,
        f"{period.value}",
        transform=ax.transAxes,
        ha='left',
        va='center',
        fontproperties=football_api.structure.noto_bold_font,
        fontsize=12
    )

    ax.legend(
        loc='upper right',
        bbox_to_anchor=(1, 1.05),
        ncol=2,
        frameon=False,
        prop=football_api.structure.noto_regular_font
    )

    plt.savefig(filename, dpi=300)
    plt.close()


def create_this_season_history(
        competition: model.competitions.Competition,
        team: model.teams.Team,
        period: model.fixtures.Period,
        goals_for: list[int],
        goals_against: list[int],
        outcomes: list[typing.Callable]
):
    country_specific_output_dir = football_api.structure.get_matplotlib_dir(competition.country)

    filename1 = football_api.structure.get_matplotlib_filename(country_specific_output_dir, team.name)
    create_outcome_history(period, outcomes, filename1)
    img1 = reportlab.platypus.Image(
        filename1, width=2.5 * reportlab.lib.units.inch, height=2.5 * reportlab.lib.units.inch
    )

    filename2 = football_api.structure.get_matplotlib_filename(country_specific_output_dir, team.name)
    create_goals_history(period, goals_for, goals_against, filename2)
    img2 = reportlab.platypus.Image(
        filename2, width=2.5 * reportlab.lib.units.inch, height=2.5 * reportlab.lib.units.inch
    )

    return [
        reportlab.platypus.Table(
            [[img1, img2]],
            colWidths=[3 * reportlab.lib.units.inch, 3 * reportlab.lib.units.inch],
            hAlign="CENTER"
        )
    ]


def create_head_to_head(
        competition: model.competitions.Competition,
        home_team: model.teams.Team,
        away_team: model.teams.Team
):
    headings = [
        "Date",
        "Home",
        "Away",
        model.fixtures.Period.FIRST,
        model.fixtures.Period.SECOND,
        model.fixtures.Period.FULL,
    ]

    displayed_rows = [
        headings
    ]

    highlight_styles = []
    home_color = "#D4AF37"
    away_color = "#C0C0C0"
    row_index = 0
    home_col_index = 1
    away_col_index = 2

    unknown_score = '?-?'
    previous_fixtures = model.fixtures.load_head_to_head_fixtures(competition, home_team, away_team)
    for fixture in reversed(previous_fixtures):
        if fixture.finished:
            row_index += 1
            first_half_score = fixture.result(model.fixtures.Period.FIRST)
            second_half_score = fixture.result(model.fixtures.Period.SECOND)
            full_time_score = fixture.result(model.fixtures.Period.FULL)
            row = [
                fixture.date.strftime("%d-%m-%y"),
                fixture.home_team.name,
                fixture.away_team.name,
                str(first_half_score) if first_half_score else unknown_score,
                str(second_half_score) if second_half_score else unknown_score,
                str(full_time_score) if full_time_score else unknown_score
            ]
            displayed_rows.append(row)

            if full_time_score is not None:
                if full_time_score.left > full_time_score.right:
                    if fixture.home_team == home_team:
                        color = home_color
                    else:
                        color = away_color

                    coordinates = (home_col_index, row_index)
                    highlight_styles.append(("BACKGROUND", coordinates, coordinates, color))
                elif full_time_score.left < full_time_score.right:
                    if fixture.away_team == home_team:
                        color = home_color
                    else:
                        color = away_color

                    coordinates = (away_col_index, row_index)
                    highlight_styles.append(("BACKGROUND", coordinates, coordinates, color))

    col_widths = [
        1 * reportlab.lib.units.inch,
        2 * reportlab.lib.units.inch,
        2 * reportlab.lib.units.inch,
        0.6 * reportlab.lib.units.inch,
        0.6 * reportlab.lib.units.inch,
        0.6 * reportlab.lib.units.inch,
    ]

    table = reportlab.platypus.Table(displayed_rows, colWidths=col_widths)
    table.setStyle(reportlab.platypus.TableStyle([
        ("BACKGROUND", (0, 0), (-1, 0), reportlab.lib.colors.HexColor("#2C3E50")),
        ("TEXTCOLOR", (0, 0), (-1, 0), reportlab.lib.colors.HexColor("#FDFEFE")),
        ("FONTNAME", (0, 0), (-1, -1), "NotoSans"),
        ("FONTNAME", (0, 0), (-1, 0), "NotoSans-Bold"),
        ("FONTSIZE", (0, 0), (-1, 0), 9),
        ("BOTTOMPADDING", (0, 0), (-1, 0), 8),
        ("ALIGN", (3, 0), (-1, -1), "CENTER")
    ]))
    table.setStyle(reportlab.platypus.TableStyle(highlight_styles))

    styles = reportlab.lib.styles.getSampleStyleSheet()
    title_style = reportlab.lib.styles.ParagraphStyle(
        name="CenteredTitle",
        parent=styles["Title"],
        alignment=reportlab.lib.enums.TA_CENTER,
        fontName="NotoSans-Bold",
        fontSize=16,
        leading=18
    )

    title = f"Head to Head in the {competition.name} "

    elements = [
        reportlab.platypus.Paragraph(title, title_style),
        reportlab.platypus.Spacer(1, 0.2 * reportlab.lib.units.inch),
        table,
        reportlab.platypus.PageBreak()
    ]

    return elements

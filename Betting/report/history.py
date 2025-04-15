import concurrent.futures
import dataclasses
import enum
import pathlib
import typing

import matplotlib
import matplotlib.cm
import matplotlib.font_manager
import matplotlib.image
import matplotlib.offsetbox
import matplotlib.patches
import matplotlib.pyplot as plt
import matplotlib.ticker
import matplotlib.transforms
import reportlab.lib.colors
import reportlab.lib.enums
import reportlab.lib.styles
import reportlab.lib.units
import reportlab.platypus

import lib.structure
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

    database = lib.structure.get_database(competition.country)
    for season in seasons:
        fixtures = model.seasons.load_fixtures(database, competition, season)
        for fixture in fixtures:
            if fixture.finished:
                if fixture.home_team in team_venues and team_venues[fixture.home_team] == model.fixtures.Venue.HOME:
                    home_data = historical_data[fixture.home_team]
                    home_data.collect(database, fixture, model.fixtures.Period.FULL, model.fixtures.Venue.HOME)

                if fixture.away_team in team_venues and team_venues[fixture.away_team] == model.fixtures.Venue.AWAY:
                    away_data = historical_data[fixture.away_team]
                    away_data.collect(database, fixture, model.fixtures.Period.FULL, model.fixtures.Venue.AWAY)

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
        text.set_fontproperties(lib.structure.noto_regular_font)
        text.set_fontsize(20)

    for autotext in autotexts:
        autotext.set_fontproperties(lib.structure.noto_regular_font)
        autotext.set_color('white')
        autotext.set_fontsize(20)

    donut_text = f"{sum(values)}\n{donut}"
    ax.text(0, 0, donut_text, ha='center', va='center', fontsize=20, color='grey',
            fontproperties=lib.structure.noto_bold_font)
    ax.axis('equal')

    plt.figtext(0.5, 0.05, title, ha='center', fontsize=20, fontproperties=lib.structure.noto_bold_font)
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
    country_specific_output_dir = lib.structure.get_matplotlib_dir(competition.country)

    filename1 = lib.structure.get_matplotlib_file(country_specific_output_dir, team.id)
    filename1.touch()
    file1_arguments = (
        [
            PieChunk(len(team_data.won), ChartLabel.WON, ChartColor.WON),
            PieChunk(len(team_data.drawn), ChartLabel.DREW, ChartColor.DREW),
            PieChunk(len(team_data.lost), ChartLabel.LOST, ChartColor.LOST)
        ],
        'Games',
        "Results Breakdown",
        filename1
    )

    filename2 = lib.structure.get_matplotlib_file(country_specific_output_dir, team.id)
    filename2.touch()
    file2_arguments = (
        [
            PieChunk(team_data.goals_for.total, ChartLabel.FOR, ChartColor.FOR),
            PieChunk(team_data.goals_against.total, ChartLabel.AGAINST, ChartColor.AGAINST)
        ],
        'Goals',
        "Total Goals Breakdown",
        filename2
    )

    filename3 = lib.structure.get_matplotlib_file(country_specific_output_dir, team.id)
    filename3.touch()
    file3_arguments = (
        [
            PieChunk(len(team_data.goals_for.goals_0), ChartLabel.GOALS_0, ChartColor.GOALS_0),
            PieChunk(len(team_data.goals_for.goals_1), ChartLabel.GOALS_1, ChartColor.GOALS_1),
            PieChunk(len(team_data.goals_for.goals_2), ChartLabel.GOALS_2, ChartColor.GOALS_2),
            PieChunk(len(team_data.goals_for.goals_3_or_more), ChartLabel.GOALS_3_PLUS, ChartColor.GOALS_3_PLUS)
        ],
        'Games',
        "Goals For in a Game",
        filename3
    )

    filename4 = lib.structure.get_matplotlib_file(country_specific_output_dir, team.id)
    filename4.touch()
    file4_arguments = (
        [
            PieChunk(len(team_data.goals_against.goals_0), ChartLabel.GOALS_0, ChartColor.GOALS_0),
            PieChunk(len(team_data.goals_against.goals_1), ChartLabel.GOALS_1, ChartColor.GOALS_1),
            PieChunk(len(team_data.goals_against.goals_2), ChartLabel.GOALS_2, ChartColor.GOALS_2),
            PieChunk(len(team_data.goals_against.goals_3_or_more), ChartLabel.GOALS_3_PLUS, ChartColor.GOALS_3_PLUS)
        ],
        'Games',
        "Goals Against in a Game",
        filename4
    )

    filename5 = lib.structure.get_matplotlib_file(country_specific_output_dir, team.id)
    filename5.touch()
    file5_arguments = (
        [
            PieChunk(len(team_data.total_goals.goals_0), ChartLabel.GOALS_0, ChartColor.GOALS_0),
            PieChunk(len(team_data.total_goals.goals_1), ChartLabel.GOALS_1, ChartColor.GOALS_1),
            PieChunk(len(team_data.total_goals.goals_2), ChartLabel.GOALS_2, ChartColor.GOALS_2),
            PieChunk(len(team_data.total_goals.goals_3_or_more), ChartLabel.GOALS_3_PLUS, ChartColor.GOALS_3_PLUS)
        ],
        'Games',
        "Total Goals in a Game",
        filename5
    )

    filename6 = lib.structure.get_matplotlib_file(country_specific_output_dir, team.id)
    filename6.touch()
    file6_arguments = (
        [
            PieChunk(len(team_data.bts), ChartLabel.BTS, ChartColor.BTS),
            PieChunk(len(team_data.nts), ChartLabel.NTS, ChartColor.NTS),
            PieChunk(team_data.played() - len(team_data.bts) - len(team_data.nts), ChartLabel.ONLY_ONE,
                     ChartColor.ONLY_ONE)
        ],
        'Games',
        "Who Scored: Both (B), None (N), or Only One (OO)",
        filename6
    )

    work = [file1_arguments, file2_arguments, file3_arguments, file4_arguments, file5_arguments, file6_arguments]
    with concurrent.futures.ProcessPoolExecutor(max_workers=6) as executor:
        futures = [executor.submit(create_pie, *args) for args in work]
        for future in futures:
            future.result()

    img_width = 2.5 * reportlab.lib.units.inch
    img_height = 2.5 * reportlab.lib.units.inch
    img1 = reportlab.platypus.Image(filename1, width=img_width, height=img_height)
    img2 = reportlab.platypus.Image(filename2, width=img_width, height=img_height)
    img3 = reportlab.platypus.Image(filename3, width=img_width, height=img_height)
    img4 = reportlab.platypus.Image(filename4, width=img_width, height=img_height)
    img5 = reportlab.platypus.Image(filename5, width=img_width, height=img_height)
    img6 = reportlab.platypus.Image(filename6, width=img_width, height=img_height)

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
    country_specific_output_dir = lib.structure.get_matplotlib_dir(competition.country)

    filename1 = lib.structure.get_matplotlib_file(country_specific_output_dir, team.id)
    filename1.touch()
    file1_arguments = (
        [
            PieChunk(len(team_data.half_time_winning.won), ChartLabel.WON, ChartColor.WON),
            PieChunk(len(team_data.half_time_winning.drew), ChartLabel.DREW, ChartColor.DREW),
            PieChunk(len(team_data.half_time_winning.lost), ChartLabel.LOST, ChartColor.LOST)
        ],
        'Games',
        "FT Result when Winning at HT",
        filename1
    )

    filename2 = lib.structure.get_matplotlib_file(country_specific_output_dir, team.id)
    filename2.touch()
    file2_arguments = (
        [
            PieChunk(len(team_data.half_time_drawing.won), ChartLabel.WON, ChartColor.WON),
            PieChunk(len(team_data.half_time_drawing.drew), ChartLabel.DREW, ChartColor.DREW),
            PieChunk(len(team_data.half_time_drawing.lost), ChartLabel.LOST, ChartColor.LOST)
        ],
        'Games',
        "FT Result when Drawing at HT",
        filename2
    )

    filename3 = lib.structure.get_matplotlib_file(country_specific_output_dir, team.id)
    filename3.touch()
    file3_arguments = (
        [
            PieChunk(len(team_data.half_time_losing.won), ChartLabel.WON, ChartColor.WON),
            PieChunk(len(team_data.half_time_losing.drew), ChartLabel.DREW, ChartColor.DREW),
            PieChunk(len(team_data.half_time_losing.lost), ChartLabel.LOST, ChartColor.LOST)
        ],
        'Games',
        "FT Result when Losing at HT",
        filename3
    )

    filename4 = lib.structure.get_matplotlib_file(country_specific_output_dir, team.id)
    filename4.touch()
    file4_arguments = (
        [
            PieChunk(len(team_data.half_time_0_goals.goals_0), ChartLabel.GOALS_0, ChartColor.GOALS_0),
            PieChunk(len(team_data.half_time_0_goals.goals_1), ChartLabel.GOALS_1, ChartColor.GOALS_1),
            PieChunk(len(team_data.half_time_0_goals.goals_2), ChartLabel.GOALS_2, ChartColor.GOALS_2),
            PieChunk(len(team_data.half_time_0_goals.goals_3_or_more), ChartLabel.GOALS_3_PLUS, ChartColor.GOALS_3_PLUS)
        ],
        'Games',
        "Goals in 2nd Half when 0 goals in 1st Half",
        filename4
    )

    filename5 = lib.structure.get_matplotlib_file(country_specific_output_dir, team.id)
    filename5.touch()
    file5_arguments = (
        [
            PieChunk(len(team_data.half_time_1_goal.goals_0), ChartLabel.GOALS_0, ChartColor.GOALS_0),
            PieChunk(len(team_data.half_time_1_goal.goals_1), ChartLabel.GOALS_1, ChartColor.GOALS_1),
            PieChunk(len(team_data.half_time_1_goal.goals_2), ChartLabel.GOALS_2, ChartColor.GOALS_2),
            PieChunk(len(team_data.half_time_1_goal.goals_3_or_more), ChartLabel.GOALS_3_PLUS, ChartColor.GOALS_3_PLUS)
        ],
        'Games',
        "Goals in 2nd Half when 1 goal in 1st Half",
        filename5
    )

    filename6 = lib.structure.get_matplotlib_file(country_specific_output_dir, team.id)
    filename6.touch()
    file6_arguments = (
        [
            PieChunk(len(team_data.half_time_2_goals.goals_0), ChartLabel.GOALS_0, ChartColor.GOALS_0),
            PieChunk(len(team_data.half_time_2_goals.goals_1), ChartLabel.GOALS_1, ChartColor.GOALS_1),
            PieChunk(len(team_data.half_time_2_goals.goals_2), ChartLabel.GOALS_2, ChartColor.GOALS_2),
            PieChunk(len(team_data.half_time_2_goals.goals_3_or_more), ChartLabel.GOALS_3_PLUS, ChartColor.GOALS_3_PLUS)
        ],
        'Games',
        "Goals in 2nd Half when 2+ goals in 1st Half",
        filename6
    )

    work = [file1_arguments, file2_arguments, file3_arguments, file4_arguments, file5_arguments, file6_arguments]
    with concurrent.futures.ProcessPoolExecutor(max_workers=6) as executor:
        futures = [executor.submit(create_pie, *args) for args in work]
        for future in futures:
            future.result()

    img_width = 2.5 * reportlab.lib.units.inch
    img_height = 2.5 * reportlab.lib.units.inch
    img1 = reportlab.platypus.Image(filename1, width=img_width, height=img_height)
    img2 = reportlab.platypus.Image(filename2, width=img_width, height=img_height)
    img3 = reportlab.platypus.Image(filename3, width=img_width, height=img_height)
    img4 = reportlab.platypus.Image(filename4, width=img_width, height=img_height)
    img5 = reportlab.platypus.Image(filename5, width=img_width, height=img_height)
    img6 = reportlab.platypus.Image(filename6, width=img_width, height=img_height)

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
            fontsize=font_size, fontweight='bold', color='white', fontproperties=lib.structure.noto_bold_font
        )

    ax.set_xlabel("Game", fontproperties=lib.structure.noto_bold_font, fontsize=12)
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
        fontproperties=lib.structure.noto_bold_font,
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
    goals_against = [-g for g in goals_against]
    ax.bar(games, goals_for, color=ChartColor.FOR, width=bar_width, label="Goals For")
    ax.bar(games, goals_against, color=ChartColor.AGAINST, width=bar_width, label="Goals Against")

    ax.axhline(0, color='black', linewidth=1, linestyle='--')
    ax.set_xlabel("Game", fontproperties=lib.structure.noto_bold_font, fontsize=12)
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
        fontproperties=lib.structure.noto_bold_font,
        fontsize=12
    )

    ax.legend(
        loc='upper right',
        bbox_to_anchor=(1, 1.05),
        ncol=2,
        frameon=False,
        prop=lib.structure.noto_regular_font
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
    country_specific_output_dir = lib.structure.get_matplotlib_dir(competition.country)

    filename1 = lib.structure.get_matplotlib_file(country_specific_output_dir, team.id)
    filename1.touch()
    filename2 = lib.structure.get_matplotlib_file(country_specific_output_dir, team.id)
    filename2.touch()

    with concurrent.futures.ProcessPoolExecutor() as executor:
        futures = [
            executor.submit(create_outcome_history, period, outcomes, filename1),
            executor.submit(create_goals_history, period, goals_for, goals_against, filename2),
        ]
        for future in futures:
            future.result()

    img_width = 2.5 * reportlab.lib.units.inch
    img_height = 2.5 * reportlab.lib.units.inch
    img1 = reportlab.platypus.Image(filename1, width=img_width, height=img_height)
    img2 = reportlab.platypus.Image(filename2, width=img_width, height=img_height)

    return [
        reportlab.platypus.Table(
            [
                [img1, img2]
            ],
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
    database = lib.structure.get_database(competition.country)
    previous_fixtures = model.fixtures.load_head_to_head_fixtures(database, competition, home_team, away_team)
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


def create_player_summary(
        competition: model.competitions.Competition,
        team_data: report.data.TeamData,
        venue: model.fixtures.Venue
):
    elements = []
    if team_data.player_data:
        sorted_contributors = sorted(
            team_data.player_data.contributions.items(),
            key=lambda item: (item[1].goals + item[1].assists, item[1].goals),
            reverse=True
        )

        x_ticks = []
        x_labels = []
        players = []
        goals_bar = []
        assists_bar = []

        for x_value, (player, contributions) in enumerate(sorted_contributors):
            if contributions.goals + contributions.assists > 0:
                x_ticks.append(x_value)
                x_labels.append(player.lastname)
                players.append(player)
                goals_bar.append(contributions.goals)
                assists_bar.append(contributions.assists)

        fig, ax = plt.subplots(figsize=(14, 6))

        bar_width = 0.35
        ax.bar(
            [i - bar_width / 2 for i in x_ticks],
            goals_bar,
            width=bar_width,
            label="Goals",
            color="#e15759",
            edgecolor="black",
            linewidth=0.5
        )
        ax.bar(
            [i + bar_width / 2 for i in x_ticks],
            assists_bar,
            width=bar_width,
            label="Assists",
            color="#f28e2b",
            edgecolor="black",
            linewidth=0.5
        )

        for i, (goals, assists) in enumerate(zip(goals_bar, assists_bar)):
            if goals > 0:
                x = i - bar_width / 2
                y = goals + 0.2
                ax.text(x, y, f"{goals} G", ha='center', fontsize=10, fontproperties=lib.structure.noto_regular_font)
            if assists > 0:
                x = i + bar_width / 2
                y = assists + 0.2
                ax.text(x, y, f"{assists} A", ha='center', fontsize=10, fontproperties=lib.structure.noto_regular_font)

        ax.set_xticks(x_ticks)
        ax.set_xticklabels(["" for _ in x_labels])
        ax.set_yticks([])
        ax.set_ylim(bottom=-2)
        ax.tick_params(bottom=False)
        ax.tick_params(axis='x', length=0)

        ax.legend(
            loc='upper right',
            ncol=2,
            frameon=False,
            prop=lib.structure.noto_regular_font
        )

        for spine in ax.spines.values():
            spine.set_visible(False)

        for i, player in enumerate(players):
            image_path = pathlib.Path(player.get_picture_png())
            imagebox = matplotlib.offsetbox.OffsetImage(matplotlib.image.imread(image_path), zoom=0.25)
            imagebox.image.axes = ax

            text = matplotlib.offsetbox.TextArea(
                player.name,
                textprops=dict(
                    ha='center',
                    va='top',
                    fontsize=8,
                    fontproperties=lib.structure.noto_regular_font
                )
            )

            if len(players) > 10:
                if i % 2 == 0:
                    separation = 1
                else:
                    separation = 10
            else:
                separation = 1

            packed = matplotlib.offsetbox.VPacker(
                children=[imagebox, text],
                align="center",
                pad=1,
                sep=separation
            )

            ab = matplotlib.offsetbox.AnnotationBbox(
                packed,
                (i, -0.6),
                frameon=False,
                box_alignment=(0.5, 1.0)
            )
            ax.add_artist(ab)

        country_specific_output_dir = lib.structure.get_matplotlib_dir(competition.country)
        filename = lib.structure.get_matplotlib_file(country_specific_output_dir, team_data.team.id)
        filename.touch()
        plt.tight_layout()
        plt.savefig(filename, dpi=300)
        plt.close()

        styles = reportlab.lib.styles.getSampleStyleSheet()
        title_style = reportlab.lib.styles.ParagraphStyle(
            name="CenteredTitle",
            parent=styles["Title"],
            alignment=reportlab.lib.enums.TA_CENTER,
            fontName="NotoSans-Bold",
            fontSize=16,
            leading=18
        )

        title = f"{team_data.team.name}'s {venue.value} Goal Involvements"

        img = reportlab.platypus.Image(
            filename,
            width=6.5 * reportlab.lib.units.inch,
            height=3.2 * reportlab.lib.units.inch
        )

        elements.extend(
            [
            reportlab.platypus.Paragraph(title, title_style),
            reportlab.platypus.Spacer(1, 0.2 * reportlab.lib.units.inch),
            img,
            reportlab.platypus.PageBreak()
            ]
        )

    return elements

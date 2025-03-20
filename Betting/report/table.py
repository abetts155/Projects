import reportlab.lib
import reportlab.lib.colors
import reportlab.lib.enums
import reportlab.lib.styles
import reportlab.lib.units
import reportlab.platypus

import model.fixtures
import model.teams
import report.data


def create_legend_caption(
        styles,
        period_description: str,
        goal_description: str,
        min_color: reportlab.lib.colors.Color,
        max_color: reportlab.lib.colors.Color
):
    return reportlab.platypus.Paragraph(
        f"<b>F 0</b>: the % of {period_description} scoring 0.  "
        f"<b>A 0</b>: the % of {period_description} conceding 0.  "
        f"<b>F {goal_description}+</b>: the % of {period_description} scoring {goal_description} or more.  "
        f"<b>A {goal_description}+</b>: the % of {period_description} conceding {goal_description} or more.  "
        f"<b>BTS</b>: the % of {period_description} where both teams scored.  "
        f"<b>NTS</b>: the % of {period_description} where no team scored.  "
        "<b>AGR</b>: the average goal rate.  "
        f"<font color='{min_color}'>■</font> is a <b>minimum</b> column value.  "
        f"<font color='{max_color}'>■</font> is a <b>maximum</b> column value",
        reportlab.lib.styles.ParagraphStyle(
            name="LegendCaption",
            parent=styles["Normal"],
            fontSize=7,
            textColor=reportlab.lib.colors.grey,
            leading=9,
            spaceAfter=6,
            italic=True,
            alignment=reportlab.lib.enums.TA_LEFT
        )
    )


def create_team_summary(
        league_table: report.data.LeagueTable,
        data_rows: list[list],
        team_row: list,
        period_description: str,
        goal_description: str
):
    bullet_items = []
    for j, col in enumerate(league_table.columns()):
        if col not in [league_table.col_team, league_table.col_played, league_table.col_points]:
            if col == league_table.col_won:
                description = f"# of {period_description} won"
            elif col == league_table.col_drawn:
                description = f"# of {period_description} drawn"
            elif col == league_table.col_lost:
                description = f"# of {period_description} lost"
            elif col == league_table.col_goals_for:
                description = "# of goals for"
            elif col == league_table.col_goals_against:
                description = "# of goals against"
            elif col == league_table.col_goals_for_0:
                description = f"# of {period_description} without scoring"
            elif col == league_table.col_goals_against_0:
                description = f"# of {period_description} without conceding"
            elif col == league_table.col_goals_for_not_0:
                description = f"# of {period_description} scoring {goal_description}+"
            elif col == league_table.col_goals_against_not_0:
                description = f"# of {period_description} conceding {goal_description}+"
            elif col == league_table.col_bts:
                description = f"# of {period_description} where both teams scored"
            elif col == league_table.col_nts:
                description = f"# of {period_description} ending 0-0"
            elif col == league_table.col_goal_rate:
                description = "goal rate"

            values = sorted({row[j] for row in data_rows})
            if len(values) > 1:
                if team_row[j] == values[0]:
                    bullet_items.append(f"the <b>lowest</b> {description}")
                elif team_row[j] == values[-1]:
                    bullet_items.append(f"the <b>highest</b> {description}")
                elif team_row[j] == values[1]:
                    bullet_items.append(f"the <b>2nd</b> lowest {description}")
                elif team_row[j] == values[-2]:
                    bullet_items.append(f"the <b>2nd</b> highest {description}")

    if bullet_items:
        intro_style = reportlab.lib.styles.ParagraphStyle(
            name="NotoSansIntro",
            fontName="NotoSans",
            fontSize=12,
            leading=14
        )

        bullet_style = reportlab.lib.styles.ParagraphStyle(
            name="NotoSansBullet",
            fontName="NotoSans",
            fontSize=10,
            leading=12
        )

        return reportlab.platypus.KeepTogether([
            reportlab.platypus.Paragraph(f"{team_row[0]} has", intro_style),
            reportlab.platypus.Spacer(1, 0.1 * reportlab.lib.units.inch),
            reportlab.platypus.ListFlowable(
                [reportlab.platypus.ListItem(reportlab.platypus.Paragraph(item, bullet_style)) for item in bullet_items],
                bulletType='bullet',
                start='→',
                leftIndent=20)
            ]
        )


def get_percentage(left: int, right: int) -> float:
    return 100 * left / right


def compute_teams_data(
        fixtures: list[model.fixtures.Fixture],
        period: model.fixtures.Period,
        venue: model.fixtures.Venue
) -> list[report.data.TeamData]:
    teams = model.fixtures.teams(fixtures)
    teams_data = {team: report.data.TeamData(team) for team in teams}
    for fixture in fixtures:
        if fixture.finished:
            home_data = teams_data[fixture.home_team]
            home_data.collect(fixture, period, venue)
            away_data = teams_data[fixture.away_team]
            away_data.collect(fixture, period, venue)

    teams_data_list = list(teams_data.values())
    teams_data_list.sort(
        key=lambda team_data: (
        team_data.points(), team_data.goals_for.total - team_data.goals_against.total, team_data.goals_for.total),
        reverse=True
    )
    return teams_data_list


def create_league_table_rows(
        fixtures: list[model.fixtures.Fixture],
        period: model.fixtures.Period,
        venue: model.fixtures.Venue
) -> tuple[list[list], int]:
    if period == model.fixtures.Period.FULL:
        goal_cutoff = 2
    else:
        goal_cutoff = 1

    unknown_scores = 0
    rows = []
    teams_data: list[report.data.TeamData] = compute_teams_data(fixtures, period, venue)
    for i, team_data in enumerate(teams_data):
        row = [
            team_data.team.name,
            team_data.played(),
            len(team_data.won),
            len(team_data.drawn),
            len(team_data.lost),
            team_data.goals_for.total,
            team_data.goals_against.total,
            team_data.points()
        ]

        if team_data.played() == 0:
            row.extend([0] * 7)
        else:
            row.extend([
                get_percentage(team_data.goals_for.at_most(0), team_data.played()),
                get_percentage(team_data.goals_against.at_most(0), team_data.played()),
                get_percentage(team_data.goals_for.at_least(goal_cutoff), team_data.played()),
                get_percentage(team_data.goals_against.at_least(goal_cutoff), team_data.played()),
                get_percentage(len(team_data.bts), team_data.played()),
                get_percentage(len(team_data.nts), team_data.played()),
                round(team_data.goal_rate(), 1)
            ])

        rows.append(row)
        unknown_scores += team_data.unknown_scores

    return rows, unknown_scores


def create_league_table(
        fixtures: list[model.fixtures.Fixture],
        period: model.fixtures.Period,
        venue: model.fixtures.Venue,
        teams: list[model.teams.Team]
):
    if period == model.fixtures.Period.FULL:
        league_table = report.data.LeagueTable('F 2+', 'A 2+')
    else:
        league_table = report.data.LeagueTable('F 1+', 'A 1+')

    rows, unknown_scores = create_league_table_rows(fixtures, period, venue)
    min_columns = {}
    max_columns = {}
    for j, col in enumerate(league_table.columns()):
        if col not in [league_table.col_team, league_table.col_played]:
            values = [row[j] for row in rows]
            min_columns[j] = min(values)
            max_columns[j] = max(values)

    min_color = reportlab.lib.colors.HexColor("#FFCDD2")
    max_color = reportlab.lib.colors.HexColor("#FFD700")
    highlighted_rows = []
    highlight_styles = []
    for i, row in enumerate(rows):
        for team in teams:
            if row[0] == team.name:
                highlighted_rows.append((i, row))

        for j, col in enumerate(league_table.columns()):
            if col not in [league_table.col_team, league_table.col_played]:
                if row[j] == min_columns[j]:
                    highlight_styles.append(("BACKGROUND", (j, i + 1), (j, i + 1), min_color))

                if row[j] == max_columns[j]:
                    highlight_styles.append(("BACKGROUND", (j, i + 1), (j, i + 1), max_color))

    col_widths = [
        2 * reportlab.lib.units.inch,
        0.25 * reportlab.lib.units.inch,
        0.25 * reportlab.lib.units.inch,
        0.25 * reportlab.lib.units.inch,
        0.3 * reportlab.lib.units.inch,
        0.3 * reportlab.lib.units.inch,
        0.3 * reportlab.lib.units.inch,
        0.4 * reportlab.lib.units.inch,
        0.4 * reportlab.lib.units.inch,
        0.4 * reportlab.lib.units.inch,
        0.4 * reportlab.lib.units.inch,
        0.4 * reportlab.lib.units.inch,
        0.4 * reportlab.lib.units.inch,
        0.4 * reportlab.lib.units.inch,
        0.4 * reportlab.lib.units.inch
    ]

    displayed_rows = [[col for col in league_table.columns()]]
    for row in rows:
        displayed_row = []
        for j, col in enumerate(league_table.columns()):
            if col in [league_table.col_goals_for_0,
                       league_table.col_goals_against_0,
                       league_table.col_goals_for_not_0,
                       league_table.col_goals_against_not_0,
                       league_table.col_bts,
                       league_table.col_nts]:
                displayed_row.append(f"{round(row[j])}%")
            elif col == league_table.col_goal_rate:
                displayed_row.append(f"{row[j]:.1f}")
            else:
                displayed_row.append(f"{row[j]}")

        displayed_rows.append(displayed_row)

    table = reportlab.platypus.Table(displayed_rows, colWidths=col_widths)
    table.setStyle(reportlab.platypus.TableStyle([
        ("BACKGROUND", (0, 0), (-1, 0), reportlab.lib.colors.HexColor("#2C3E50")),
        ("TEXTCOLOR", (0, 0), (-1, 0), reportlab.lib.colors.HexColor("#FDFEFE")),
        ("FONTNAME", (0, 0), (-1, -1), "NotoSans"),
        ("FONTNAME", (0, 0), (-1, 0), "NotoSans-Bold"),
        ("FONTSIZE", (0, 0), (-1, 0), 9),
        ("BOTTOMPADDING", (0, 0), (-1, 0), 8),
        ("ALIGN", (1, 1), (-1, -1), "RIGHT"),
        ("ALIGN", (0, 0), (0, -1), "LEFT"),
        ("ALIGN", (0, 0), (0, 0), "LEFT"),
        ("ALIGN", (1, 0), (-1, 0), "RIGHT")
    ]))

    for row_index, _ in highlighted_rows:
        table.setStyle(reportlab.platypus.TableStyle([
            ("BOX", (0, row_index + 1), (-1, row_index + 1), 1, reportlab.lib.colors.black),
            ("LINEBEFORE", (0, row_index + 1), (0, row_index + 1), 2, reportlab.lib.colors.black),
            ("LINEAFTER", (-1, row_index + 1), (-1, row_index + 1), 2, reportlab.lib.colors.black),
        ]))

    table.setStyle(reportlab.platypus.TableStyle(highlight_styles))
    styles = reportlab.lib.styles.getSampleStyleSheet()

    if period == model.fixtures.Period.FULL:
        goal_description = '2'
        period_description = 'games'
    else:
        goal_description = '1'
        period_description = 'halves'

    title_style = reportlab.lib.styles.ParagraphStyle(
        name="CenteredTitle",
        parent=styles["Title"],
        alignment=reportlab.lib.enums.TA_CENTER,
        fontSize=16,
        leading=18
    )

    footnote_style = reportlab.lib.styles.ParagraphStyle(
        name="Footnote",
        parent=styles["Normal"],
        fontSize=7,
        textColor=reportlab.lib.colors.HexColor("#666666"),
        leading=8,
        alignment=reportlab.lib.enums.TA_CENTER
    )

    if unknown_scores:
        dagger = "&#8224;"
        title_text = f"{venue.value} League Table ({period.value}) {dagger}"
        footnote_para = reportlab.platypus.Paragraph(
            f"{dagger} This table is missing {unknown_scores} scores",
            footnote_style,
        )
        elements = [
            reportlab.platypus.Paragraph(title_text, title_style),
            reportlab.platypus.Spacer(1, 0.1 * reportlab.lib.units.inch),
            footnote_para,
            reportlab.platypus.Spacer(1, 0.2 * reportlab.lib.units.inch)
        ]
    else:
        title_text = f"{venue.value} League Table ({period.value})"
        elements = [
            reportlab.platypus.Paragraph(title_text, title_style),
            reportlab.platypus.Spacer(1, 0.2 * reportlab.lib.units.inch)
        ]

    elements.extend([
        table,
        reportlab.platypus.Spacer(1, 0.2 * reportlab.lib.units.inch),
        create_legend_caption(styles, period_description, goal_description, min_color, max_color),
        reportlab.platypus.Spacer(1, 0.2 * reportlab.lib.units.inch)
    ])

    for _, team_row in highlighted_rows:
        elements.extend([
            create_team_summary(league_table, rows, team_row, period_description, goal_description),
            reportlab.platypus.Spacer(1, 0.2 * reportlab.lib.units.inch)
        ])

    elements.append(reportlab.platypus.PageBreak())
    return elements

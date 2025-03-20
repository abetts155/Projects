import reportlab.lib.enums
import reportlab.lib.styles
import reportlab.lib.units
import reportlab.platypus

import model.competitions
import model.fixtures


def create_title_page(competition: model.competitions.Competition, fixture: model.fixtures.Fixture):
    styles = reportlab.lib.styles.getSampleStyleSheet()
    styles["Normal"].fontName = "NotoSans"
    styles["Heading1"].fontName = "NotoSans-Bold"
    styles["Heading2"].fontName = "NotoSans-Bold"
    styles["Title"].fontName = "NotoSans-Bold"

    title_style = reportlab.lib.styles.ParagraphStyle(
        name='TitleStyle',
        parent=styles['Title'],
        fontSize=22,
        alignment=reportlab.lib.enums.TA_CENTER
    )
    subtitle_style = reportlab.lib.styles.ParagraphStyle(
        name='SubtitleStyle',
        parent=styles['Normal'],
        fontSize=14,
        alignment=reportlab.lib.enums.TA_CENTER
    )

    logo_side_length = 1.2 * reportlab.lib.units.inch

    competition_logo = reportlab.platypus.Image(
        competition.get_competition_logo(),
        width=logo_side_length,
        height=logo_side_length
    )

    home_logo = reportlab.platypus.Image(
        fixture.home_team.get_team_logo(),
        width=logo_side_length,
        height=logo_side_length
    )

    away_logo = reportlab.platypus.Image(
        fixture.away_team.get_team_logo(),
        width=logo_side_length,
        height=logo_side_length
    )

    elements = [
        reportlab.platypus.Spacer(1, 1.0 * reportlab.lib.units.inch),
        competition_logo,
        reportlab.platypus.Spacer(1, 0.2 * reportlab.lib.units.inch),
        reportlab.platypus.Paragraph(f"{competition.get_demonym()} {competition.name}", title_style),
        reportlab.platypus.Spacer(1, 0.7 * reportlab.lib.units.inch),
        reportlab.platypus.Table(
            data=[
                [
                    home_logo,
                    reportlab.platypus.Paragraph("<b>versus</b>", subtitle_style),
                    away_logo
                ],
                [
                    reportlab.platypus.Paragraph(fixture.home_team.name, subtitle_style),
                    reportlab.platypus.Paragraph(" ", subtitle_style),
                    reportlab.platypus.Paragraph(fixture.away_team.name, subtitle_style)
                ]
            ],
            colWidths=[
                2.5 * reportlab.lib.units.inch,
                1.5 * reportlab.lib.units.inch,
                2.5 * reportlab.lib.units.inch
            ],
            style=reportlab.platypus.TableStyle([
                ('ALIGN', (0, 0), (-1, -1), 'CENTER'),
                ('VALIGN', (0, 0), (-1, 0), 'BOTTOM')
            ])
        ),
        reportlab.platypus.Spacer(1, 0.5 * reportlab.lib.units.inch),
        reportlab.platypus.Paragraph(
            f"<b>{fixture.date.strftime('%A, %d %B %Y')}</b>",
            subtitle_style
        ),
        reportlab.platypus.Spacer(1, reportlab.lib.units.inch),
        reportlab.platypus.Paragraph(
            f"Match Analysis",
            title_style
        ),
        reportlab.platypus.PageBreak()
    ]

    return elements

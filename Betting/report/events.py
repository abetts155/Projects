import matplotlib.pyplot as plt
import matplotlib.patches as patches
import reportlab.platypus
import reportlab.lib.styles
import reportlab.lib.units

import model.events
import model.fixtures
import model.teams


def gather_timeline_segments(
    fixtures: list[model.fixtures.Fixture],
    venue: model.fixtures.Venue,
    team: model.teams.Team
):
    match_segments_list = []
    match_labels = []
    start_time = 0
    end_time = 90

    for fixture in fixtures:
        if fixture.finished and (
            (fixture.home_team == team and venue == model.fixtures.Venue.HOME) or
            (fixture.away_team == team and venue == model.fixtures.Venue.AWAY)
        ):
            scoreline = model.fixtures.Scoreline(0, 0)
            events = model.events.load_events(fixture)
            segments = []
            last_event_time = start_time

            for event in events:
                if model.events.is_goal(event.detail):
                    # Determine current state before goal
                    if scoreline.left > scoreline.right:
                        state = 'winning'
                    elif scoreline.left < scoreline.right:
                        state = 'losing'
                    else:
                        state = 'drawing'

                    # Record time segment up to this goal
                    segments.append((last_event_time, event.time, state))
                    last_event_time = event.time

                    # Update scoreline after goal
                    if event.team == team:
                        scoreline.left += 1
                    else:
                        scoreline.right += 1

            # Add final segment to the end of the match
            if scoreline.left > scoreline.right:
                state = 'winning'
            elif scoreline.left < scoreline.right:
                state = 'losing'
            else:
                state = 'drawing'
            segments.append((last_event_time, end_time, state))

            match_segments_list.append(segments)
            opponent = fixture.away_team if fixture.home_team == team else fixture.home_team
            match_labels.append(f"{opponent.name}")

    return match_segments_list, match_labels


def draw_multi_match_timeline(match_segments_list, match_labels, output_path):
    """
    match_segments_list: list of lists of (start_min, end_min, state)
    match_labels: list of strings, one per match (e.g., "Cardiff vs Luton")
    """
    color_map = {
        'winning': '#4CAF50',
        'drawing': '#FFC107',
        'losing': '#F44336'
    }

    num_matches = len(match_segments_list)
    fig_height = 0.6 * num_matches
    fig, ax = plt.subplots(figsize=(10, fig_height))

    for i, segments in enumerate(match_segments_list):
        y = num_matches - i - 1
        for start, end, state in segments:
            ax.add_patch(
                patches.Rectangle(
                    (start, y), end - start, 1,
                    color=color_map[state], linewidth=0
                )
            )

    # Match labels
    ax.set_yticks([num_matches - i - 0.5 for i in range(num_matches)])
    ax.set_yticklabels(match_labels, fontsize=8)
    ax.set_xticks([0, 45, 90])
    ax.set_xlim(0, 90)
    ax.set_ylim(0, num_matches)
    ax.invert_yaxis()

    # Clean visuals
    ax.tick_params(axis='x', labelsize=8)
    ax.spines[['top', 'left', 'right']].set_visible(False)
    ax.spines['bottom'].set_position(('outward', 5))

    # Legend
    legend_patches = [
        patches.Patch(color=color_map['winning'], label="Winning"),
        patches.Patch(color=color_map['drawing'], label="Drawing"),
        patches.Patch(color=color_map['losing'], label="Losing"),
    ]
    ax.legend(handles=legend_patches, loc='lower center', bbox_to_anchor=(0.5, -0.15), ncol=3, fontsize=9, frameon=False)

    plt.tight_layout()
    plt.savefig(output_path, dpi=150, bbox_inches='tight')
    plt.close()


def create_event_timeline(fixtures: list[model.fixtures.Fixture], venue: model.fixtures.Venue, team: model.teams.Team):
    segments, labels = gather_timeline_segments(fixtures, venue, team)
    draw_multi_match_timeline(segments, labels, "timeline.png")

    timeline_image = reportlab.platypus.Image("timeline.png", width=2.5 * reportlab.lib.units.inch, height=None)
    timeline_image.hAlign = "CENTER"  # or "LEFT" / "RIGHT"

    styles = reportlab.lib.styles.getSampleStyleSheet()
    caption_style = reportlab.lib.styles.ParagraphStyle(
        name="Caption",
        parent=styles["Normal"],
        fontSize=8,
        alignment=1  # Center
    )

    caption = reportlab.platypus.Paragraph(
        "Winning (🟩), Drawing (🟨), and Losing (🟥) over 90 minutes per match.",
        caption_style
    )

    return [timeline_image, caption]

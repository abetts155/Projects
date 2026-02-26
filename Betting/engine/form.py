"""
engine/form.py

Team form construction from raw fixtures.
This module is the single source of truth for TeamStats and form building.
It has no dependencies on other engine modules, breaking all circular imports.

Optional enrichment: if a `team_stats` dict is supplied (from engine.team_stats),
shot-based fields are merged into each entry when available. Entries without
shot data simply omit those keys — feature extraction falls back to priors.
"""

from __future__ import annotations

import collections
import datetime
from dataclasses import dataclass, field
from typing import Any

import model.fixtures

Entry = dict[str, Any]


@dataclass
class TeamStats:
    home: list[Entry] = field(default_factory=list)
    away: list[Entry] = field(default_factory=list)
    any: list[Entry] = field(default_factory=list)


def _merge_shot_stats(entry: Entry, raw: dict | None) -> None:
    """
    Merge shot/pass stats from a TeamStats row into a form entry.
    All derived fields added only when underlying values are non-None.
    """
    if raw is None:
        return

    sog = raw.get("shots_on_goal")
    tot = raw.get("total_shots")
    sib = raw.get("shots_inside_box")
    cor = raw.get("corner_kicks")
    sav = raw.get("saves")
    pas = raw.get("passes")
    acc = raw.get("accurate_passes")
    xg  = raw.get("expected_goals")

    if sog is not None and tot is not None and tot > 0:
        entry["shot_accuracy"] = sog / tot
    if sog is not None:
        entry["shots_on_target"] = sog
    if sib is not None:
        entry["shots_inside_box"] = sib
    if cor is not None:
        entry["corners"] = cor
    if sav is not None:
        entry["saves"] = sav
    if pas is not None and acc is not None and pas > 0:
        entry["pass_accuracy"] = acc / pas
    if xg is not None:
        entry["xg"] = xg


def build_team_form(
    fixtures:   list,
    cutoff:     datetime.datetime | None = None,
    team_stats: dict[tuple[int, int], dict] | None = None,
) -> dict[str, TeamStats]:
    """
    Build per-team match history from a fixture list.

    Args:
        fixtures:   All fixtures to consider (finished or not).
        cutoff:     If given, only fixtures strictly before this datetime are included.
        team_stats: Optional {(fixture_id, team_id): raw_stats} from
                    engine.team_stats.load_team_stats(). Shot-based fields are
                    merged into entries where available; missing data falls back
                    to league priors at feature-extraction time.

    Returns:
        Dict mapping team name -> TeamStats, entries sorted chronologically.
    """
    form: dict[str, TeamStats] = collections.defaultdict(TeamStats)

    for fixture in fixtures:
        if not fixture.finished:
            continue
        if cutoff is not None and fixture.date >= cutoff:
            continue

        score = fixture.result(model.fixtures.Period.FULL)
        if score is None:
            continue

        total   = score.left + score.right
        over_25 = total > 2
        over_35 = total > 3
        btts    = score.left > 0 and score.right > 0

        ht      = fixture.result(model.fixtures.Period.FIRST)
        ht_home = ht.left  if ht else None
        ht_away = ht.right if ht else None
        h2_home = (score.left  - ht_home) if ht else None
        h2_away = (score.right - ht_away) if ht else None

        fid      = fixture.id
        home_raw = team_stats.get((fid, fixture.home_team.id)) if team_stats else None
        away_raw = team_stats.get((fid, fixture.away_team.id)) if team_stats else None

        def _make_entry(gf, ga, h1_gf, h1_ga, h2_gf, h2_ga) -> Entry:
            e: Entry = {
                "date":       fixture.date,
                "gf":         gf,
                "ga":         ga,
                "over25":     over_25,
                "over35":     over_35,
                "btts":       btts,
                "scored":     gf > 0,
                "conceded":   ga > 0,
                "under15_ft": total <= 1,
                "draw":       score.left == score.right,
                "home_win":   score.left > score.right,
                "away_win":   score.left < score.right,
                "gd":         float(gf - ga),
            }
            if h1_gf is not None:
                h1_total = h1_gf + h1_ga
                e["1h_over05"]  = h1_total >= 1
                e["under05_1h"] = h1_total == 0
            if h2_gf is not None:
                e["2h_over15"] = (h2_gf + h2_ga) > 1
            return e

        home_entry = _make_entry(score.left,  score.right, ht_home, ht_away, h2_home, h2_away)
        away_entry = _make_entry(score.right, score.left,  ht_away, ht_home, h2_away, h2_home)

        _merge_shot_stats(home_entry, home_raw)
        _merge_shot_stats(away_entry, away_raw)

        home_key = fixture.home_team.name
        away_key = fixture.away_team.name

        form[home_key].home.append(home_entry)
        form[home_key].any.append(home_entry)
        form[away_key].away.append(away_entry)
        form[away_key].any.append(away_entry)

    for stats in form.values():
        for lst in (stats.home, stats.away, stats.any):
            lst.sort(key=lambda e: e["date"])

    return form

"""
engine/dc_training.py

Bridges raw fixtures → Dixon-Coles fit → TrainingSamples.

Dependency graph (no cycles):
    engine.form          ← base, no engine deps
    engine.dixon_coles   ← no engine deps
    engine.features      ← depends on engine.form, engine.dixon_coles
    engine.model         ← depends on engine.features
    engine.dc_training   ← depends on all of the above
"""

from __future__ import annotations

import collections
import datetime
import math
import sys
import time

import model.fixtures

from engine.dixon_coles import DCParams, fit as dc_fit, score_probabilities
from engine.features import extract
from engine.form import TeamStats, build_team_form
from engine.model import TrainingSample

DC_HALFLIFE_DAYS = 90
DC_MAX_DAYS      = 730   # ~2 seasons max lookback for DC fitting


def _to_naive(dt: datetime.datetime) -> datetime.datetime:
    if dt.tzinfo is not None:
        return dt.astimezone(datetime.timezone.utc).replace(tzinfo=None)
    return dt


def _time_weight(fixture_date: datetime.datetime, reference: datetime.datetime) -> float:
    days_ago = (_to_naive(reference) - _to_naive(fixture_date)).total_seconds() / 86400
    if days_ago < 0:
        return 0.0
    return math.exp(-math.log(2) / DC_HALFLIFE_DAYS * days_ago)


def _progress(current: int, total: int, label: str, start_time: float, width: int = 30) -> None:
    frac    = current / total if total else 0
    filled  = int(width * frac)
    bar     = "█" * filled + "░" * (width - filled)
    elapsed = time.monotonic() - start_time
    eta     = (elapsed / frac - elapsed) if frac > 0 else 0
    print(
        f"\r  {label}  [{bar}]  {current}/{total}"
        f"  {elapsed:.0f}s elapsed  ETA {eta:.0f}s   ",
        end="", flush=True, file=sys.stderr,
    )


def fit_dc(
    fixtures:  list,
    reference: datetime.datetime | None = None,
    verbose:   bool = True,
) -> DCParams | None:
    """
    Fit Dixon-Coles on completed fixtures within DC_MAX_DAYS of reference,
    weighted by recency.
    """
    if reference is None:
        reference = datetime.datetime.now()

    ref_naive = _to_naive(reference)
    cutoff    = ref_naive - datetime.timedelta(days=DC_MAX_DAYS)

    home_teams, away_teams, home_goals, away_goals, weights = [], [], [], [], []

    for f in fixtures:
        if not f.finished:
            continue
        score = f.result(model.fixtures.Period.FULL)
        if score is None:
            continue
        if _to_naive(f.date) < cutoff:
            continue
        w = _time_weight(f.date, reference)
        if w < 0.01:
            continue
        home_teams.append(f.home_team.name)
        away_teams.append(f.away_team.name)
        home_goals.append(score.left)
        away_goals.append(score.right)
        weights.append(w)

    if len(home_teams) < 10:
        return None

    if verbose:
        print(
            f"  Dixon-Coles: fitting on {len(home_teams)} fixtures "
            f"({len(set(home_teams) | set(away_teams))} teams)...",
            flush=True, file=sys.stderr,
        )

    t      = time.monotonic()
    result = dc_fit(home_teams, away_teams, home_goals, away_goals, weights)
    if verbose:
        print(f"  Dixon-Coles: done ({time.monotonic() - t:.1f}s)", flush=True, file=sys.stderr)

    return result


def fit_dc_ht(
    fixtures:  list,
    reference: datetime.datetime | None = None,
) -> tuple[DCParams | None, DCParams | None]:
    """
    Fit two separate Dixon-Coles models on half-time scorelines:
      dc_1h  — first-half goals  (home_1h vs away_1h)
      dc_2h  — second-half goals (home_2h vs away_2h, computed as FT minus 1H)

    Returns (dc_1h, dc_2h). Either may be None if insufficient data.
    Same recency weighting and cutoff as fit_dc().
    """
    if reference is None:
        reference = datetime.datetime.now()

    ref_naive = _to_naive(reference)
    cutoff    = ref_naive - datetime.timedelta(days=DC_MAX_DAYS)

    h1_home, h1_away, h1_hg, h1_ag, h1_w = [], [], [], [], []
    h2_home, h2_away, h2_hg, h2_ag, h2_w = [], [], [], [], []

    for f in fixtures:
        if not f.finished:
            continue
        score_ft = f.result(model.fixtures.Period.FULL)
        score_1h = f.result(model.fixtures.Period.FIRST)
        if score_ft is None or score_1h is None:
            continue
        if _to_naive(f.date) < cutoff:
            continue
        w = _time_weight(f.date, reference)
        if w < 0.01:
            continue

        h1_home.append(f.home_team.name);  h1_away.append(f.away_team.name)
        h1_hg.append(score_1h.left);       h1_ag.append(score_1h.right)
        h1_w.append(w)

        h2_hg_val = score_ft.left  - score_1h.left
        h2_ag_val = score_ft.right - score_1h.right
        # Guard against data anomalies
        if h2_hg_val < 0 or h2_ag_val < 0:
            continue
        h2_home.append(f.home_team.name);  h2_away.append(f.away_team.name)
        h2_hg.append(h2_hg_val);           h2_ag.append(h2_ag_val)
        h2_w.append(w)

    dc_1h = dc_fit(h1_home, h1_away, h1_hg, h1_ag, h1_w) if len(h1_home) >= 10 else None
    dc_2h = dc_fit(h2_home, h2_away, h2_hg, h2_ag, h2_w) if len(h2_home) >= 10 else None
    return dc_1h, dc_2h


def _score_entry(fixture, score, team_stats: dict | None = None) -> tuple[dict, dict]:
    """Build home and away entry dicts from a fixture and its score."""
    # Half-time score via Period.FIRST (confirmed present in data)
    ht = fixture.result(model.fixtures.Period.FIRST)

    total   = score.left + score.right
    over_25 = total > 2
    over_35 = total > 3
    btts    = score.left > 0 and score.right > 0

    # Half-time derived values — None when HT data absent
    ht_home   = ht.left  if ht else None
    ht_away   = ht.right if ht else None
    ht_total  = (ht_home + ht_away) if ht else None
    h2_home   = (score.left  - ht_home) if ht else None
    h2_away   = (score.right - ht_away) if ht else None
    h2_total  = (h2_home + h2_away)     if ht else None

    def _entry(gf, ga, h1_gf, h1_ga, h2_gf, h2_ga):
        e = {
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
        # Only add HT keys when data is present — avoids poisoning priors
        if h1_gf is not None:
            h1_total = h1_gf + h1_ga
            e["1h_over05"]  = h1_total >= 1
            e["under05_1h"] = h1_total == 0
        if h2_gf is not None:
            h2_total_e = h2_gf + h2_ga
            e["2h_over15"]  = h2_total_e > 1
        return e

    home_entry = _entry(score.left,  score.right, ht_home, ht_away, h2_home, h2_away)
    away_entry = _entry(score.right, score.left,  ht_away, ht_home, h2_away, h2_home)

    if team_stats is not None:
        from engine.form import _merge_shot_stats
        fid = fixture.id
        _merge_shot_stats(home_entry, team_stats.get((fid, fixture.home_team.id)))
        _merge_shot_stats(away_entry, team_stats.get((fid, fixture.away_team.id)))

    return home_entry, away_entry


def build_training_samples(
    fixtures:       list,
    dc:             DCParams | None,
    min_team_games: int = 5,
    reference:      datetime.datetime | None = None,
    verbose:        bool = True,
    dc_1h:          DCParams | None = None,
    dc_2h:          DCParams | None = None,
    team_stats:     dict | None = None,
) -> list[TrainingSample]:
    """
    Build look-ahead-safe training samples from completed fixtures in O(N).

    Half-time markets are included only when half_time_result is available on
    the fixture object — missing data is silently omitted rather than defaulted
    to False, so the model doesn't learn that games without HT data had 0-0 halves.
    """
    if reference is None:
        reference = datetime.datetime.now()

    completed = sorted(
        [f for f in fixtures if f.finished],
        key=lambda f: f.date,
    )

    total      = len(completed)
    team_form: dict[str, TeamStats] = collections.defaultdict(TeamStats)
    games_seen = 0
    samples:   list[TrainingSample] = []
    start      = time.monotonic()

    for n, fixture in enumerate(completed, 1):
        if verbose and n % 50 == 0:
            _progress(n, total, "Samples", start)

        score = fixture.result(model.fixtures.Period.FULL)
        if score is None:
            continue

        home_key = fixture.home_team.name
        away_key = fixture.away_team.name

        # --- Read form BEFORE updating (look-ahead safe) ---
        if games_seen >= min_team_games * 2:
            home_stats = team_form.get(home_key)
            away_stats = team_form.get(away_key)
            if (
                home_stats and away_stats
                and len(home_stats.any) >= min_team_games
                and len(away_stats.any) >= min_team_games
            ):
                dc_probs = score_probabilities(dc, home_key, away_key) if dc else None
                dc_probs_1h = score_probabilities(dc_1h, home_key, away_key) if dc_1h else None
                dc_probs_2h = score_probabilities(dc_2h, home_key, away_key) if dc_2h else None
                feats       = extract(home_stats, away_stats, dc_probs, dc_probs_1h, dc_probs_2h)

                total_g  = score.left + score.right

                ht       = fixture.result(model.fixtures.Period.FIRST)
                ht_total = (ht.left + ht.right) if ht else None
                h2_total = (total_g - ht_total) if ht_total is not None else None

                samples.append(TrainingSample(
                    features   = feats,
                    btts       = score.left > 0 and score.right > 0,
                    over25     = total_g > 2,
                    over35     = total_g > 3,
                    over05_1h  = (ht_total >= 1)  if ht_total is not None else False,
                    over15_2h  = (h2_total > 1)   if h2_total is not None else False,
                    under15_ft = total_g <= 1,
                    under05_1h = (ht_total == 0)  if ht_total is not None else False,
                    draw       = score.left == score.right,
                    ah_home    = score.left > score.right,
                    ah_away    = score.right > score.left,
                    asian_over = total_g > 2,
                    weight     = max(_time_weight(fixture.date, reference), 0.01),
                ))

        # --- Update form ---
        home_entry, away_entry = _score_entry(fixture, score, team_stats)
        team_form[home_key].home.append(home_entry)
        team_form[home_key].any.append(home_entry)
        team_form[away_key].away.append(away_entry)
        team_form[away_key].any.append(away_entry)
        games_seen += 1

    if verbose:
        _progress(total, total, "Samples", start)
        print(file=sys.stderr)

    return samples

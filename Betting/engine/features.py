"""
engine/features.py

Feature extraction for the logistic regression model.
Only depends on engine.form — no circular imports.

Feature vector (per team ×2, then shared):
  Per team:
    Bayesian-shrunk venue rates : btts, over25, over35, scored, conceded, gf, ga,
                                   draw, home_win, away_win
    Exponentially decayed rates : same keys
    Half-time rates             : 1h_over05, 2h_over15, under15_ft, under05_1h
    Goal-difference signal      : avg_gd (normalised)
    Sample size                 : n_games / 38
  Shared DC signals:
    dc_p_btts, dc_p_over25, dc_p_over35, dc_p_under15_ft,
    dc_p_draw, dc_p_home_win, dc_p_away_win,
    dc_p_ah_home_{-2,-1.5,-1,-0.5,0,+0.5,+1,+1.5,+2},
    dc_p_asian_over_{2.25,2.5,2.75,3.0,3.25},
    xg_total_norm, xg_diff_norm
"""

from __future__ import annotations

import math

from engine.form import Entry, TeamStats

# ---------------------------------------------------------------------------
# Hyperparameters
# ---------------------------------------------------------------------------

PRIOR_GAMES: float = 8.0

LEAGUE_PRIOR: dict[str, float] = {
    "btts":       0.50,
    "over25":     0.52,
    "over35":     0.28,
    "scored":     0.73,
    "conceded":   0.73,
    "gf":         1.35,
    "ga":         1.35,
    "draw":       0.26,
    "home_win":   0.46,
    "away_win":   0.28,
    "gd":         0.0,    # avg goal difference, centred at 0
    "1h_over05":  0.72,
    "2h_over15":  0.42,
    "under15_ft": 0.25,
    "under05_1h": 0.28,
    # Shot-based priors (league averages)
    "shots_on_target":  4.5,   # per game
    "shots_inside_box": 8.0,
    "corners":          5.0,
    "saves":            3.5,
    "shot_accuracy":    0.38,  # shots on target / total shots
    "pass_accuracy":    0.78,
    "xg":               1.35,  # same scale as goals
}

DECAY_ALPHA: float = 0.85
RECENT_N:    int   = 8
GOAL_SCALE:  float = 3.5
GD_SCALE:    float = 2.0   # normalise goal difference to roughly [-1, 1]

# Asian handicap lines to include as DC features
AH_LINES:    list[float] = [-2.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 2.0]

# Asian total lines
AT_LINES:    list[float] = [2.25, 2.5, 2.75, 3.0, 3.25]


# ---------------------------------------------------------------------------
# Rate helpers
# ---------------------------------------------------------------------------

def _shrunk_rate(entries: list[Entry], key: str, prior: float) -> float:
    """Bayesian shrinkage toward league prior. Skips entries missing the key."""
    relevant = [e for e in entries if key in e]
    n = len(relevant)
    if n == 0:
        return prior
    observed = sum(
        (1.0 if e[key] else 0.0) if isinstance(e[key], bool) else float(e[key])
        for e in relevant
    )
    return (observed + PRIOR_GAMES * prior) / (n + PRIOR_GAMES)


def _decayed_rate(entries: list[Entry], key: str) -> float | None:
    """Exponentially weighted mean. Skips entries missing the key."""
    relevant = [e for e in entries if key in e]
    if not relevant:
        return None
    total_w = total_v = 0.0
    for k, entry in enumerate(reversed(relevant)):
        w   = DECAY_ALPHA ** k
        val = entry[key]
        total_v += w * (1.0 if val else 0.0) if isinstance(val, bool) else w * float(val)
        total_w += w
    return total_v / total_w if total_w else None


def _venue_entries(stats: TeamStats, use_home: bool) -> list[Entry]:
    venue = stats.home if use_home else stats.away
    return venue if venue else stats.any


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def extract(
    home_stats:  TeamStats,
    away_stats:  TeamStats,
    dc_probs:    dict[tuple[int, int], float] | None = None,
    dc_probs_1h: dict[tuple[int, int], float] | None = None,
    dc_probs_2h: dict[tuple[int, int], float] | None = None,
) -> dict[str, float]:
    """
    Build the feature dict for one fixture.

    Parameters
    ----------
    home_stats, away_stats : TeamStats from engine.form.build_team_form()
    dc_probs               : Full-time DC scoreline distribution
    dc_probs_1h            : First-half DC distribution (optional)
    dc_probs_2h            : Second-half DC distribution (optional)
    """
    feats: dict[str, float] = {}

    for prefix, stats, use_home in [("h", home_stats, True), ("a", away_stats, False)]:
        venue  = _venue_entries(stats, use_home)
        recent = venue[-RECENT_N:]

        # Full-match boolean/rate features
        for key in ("btts", "over25", "over35", "scored", "conceded", "draw", "home_win", "away_win"):
            prior = LEAGUE_PRIOR[key]
            feats[f"{prefix}_shrunk_{key}"] = _shrunk_rate(venue, key, prior)
            feats[f"{prefix}_decay_{key}"]  = _decayed_rate(recent, key) or prior

        # Goal-rate features normalised to [0, 1]
        for key in ("gf", "ga"):
            prior = LEAGUE_PRIOR[key]
            feats[f"{prefix}_shrunk_{key}"] = min(1.0, _shrunk_rate(venue, key, prior) / GOAL_SCALE)
            feats[f"{prefix}_decay_{key}"]  = min(1.0, (_decayed_rate(recent, key) or prior) / GOAL_SCALE)

        # Goal difference — normalised, centred at 0
        gd_shrunk = _shrunk_rate(venue, "gd", LEAGUE_PRIOR["gd"])
        gd_decay  = _decayed_rate(recent, "gd") or LEAGUE_PRIOR["gd"]
        feats[f"{prefix}_shrunk_gd"] = max(-1.0, min(1.0, gd_shrunk / GD_SCALE))
        feats[f"{prefix}_decay_gd"]  = max(-1.0, min(1.0, gd_decay  / GD_SCALE))

        # Half-time features
        for key in ("1h_over05", "2h_over15", "under15_ft", "under05_1h"):
            prior = LEAGUE_PRIOR[key]
            feats[f"{prefix}_shrunk_{key}"] = _shrunk_rate(venue, key, prior)
            feats[f"{prefix}_decay_{key}"]  = _decayed_rate(recent, key) or prior

        # Shot-based features — only computed when data is present in entries
        # Shrinkage pulls toward prior so sparse teams are handled gracefully.
        # xG is computed separately with a higher coverage threshold.
        _SHOT_KEYS = ("shots_on_target", "shots_inside_box", "corners",
                      "saves", "shot_accuracy", "pass_accuracy")
        _SHOT_SCALES = {
            "shots_on_target":  10.0,
            "shots_inside_box": 15.0,
            "corners":          10.0,
            "saves":             8.0,
            "shot_accuracy":     1.0,   # already 0-1
            "pass_accuracy":     1.0,
        }
        for key in _SHOT_KEYS:
            prior = LEAGUE_PRIOR[key]
            scale = _SHOT_SCALES[key]
            shrunk  = _shrunk_rate(venue, key, prior)
            decayed = _decayed_rate(recent, key)
            # Normalise continuous values to [0, 1]
            feats[f"{prefix}_shrunk_{key}"] = min(1.0, shrunk  / scale)
            feats[f"{prefix}_decay_{key}"]  = min(1.0, (decayed if decayed is not None else prior) / scale)

        # xG — only use when the team has enough xG-covered matches
        xg_entries  = [e for e in venue if "xg" in e]
        xg_coverage = len(xg_entries) / max(len(venue), 1)
        if xg_coverage >= 0.3:   # at least 30% of games have xG
            prior_xg = LEAGUE_PRIOR["xg"]
            feats[f"{prefix}_xg_shrunk"] = min(1.0, _shrunk_rate(venue, "xg", prior_xg) / GOAL_SCALE)
            feats[f"{prefix}_xg_decay"]  = min(1.0, (_decayed_rate(xg_entries[-RECENT_N:], "xg") or prior_xg) / GOAL_SCALE)
        else:
            # Fall back to goal-rate proxy when xG is too sparse
            feats[f"{prefix}_xg_shrunk"] = feats[f"{prefix}_shrunk_gf"]
            feats[f"{prefix}_xg_decay"]  = feats[f"{prefix}_decay_gf"]

        feats[f"{prefix}_n_games"] = min(1.0, len(stats.any) / 38)

    # Dixon-Coles signals
    if dc_probs is not None:
        from engine.dixon_coles import (
            p_btts, p_over, p_draw, p_home_win, p_away_win, p_ah, p_asian_total
        )
        feats["dc_p_btts"]       = p_btts(dc_probs)
        feats["dc_p_over25"]     = p_over(dc_probs, 2.5)
        feats["dc_p_over35"]     = p_over(dc_probs, 3.5)
        feats["dc_p_under15_ft"] = 1.0 - p_over(dc_probs, 1.5)
        feats["dc_p_draw"]       = p_draw(dc_probs)
        feats["dc_p_home_win"]   = p_home_win(dc_probs)
        feats["dc_p_away_win"]   = p_away_win(dc_probs)

        for line in AH_LINES:
            key = f"dc_ah_home_{line:+.1f}".replace("+", "p").replace("-", "m").replace(".", "")
            feats[key] = p_ah(dc_probs, line, home=True)

        for line in AT_LINES:
            key = f"dc_at_over_{line}".replace(".", "")
            feats[key] = p_asian_total(dc_probs, line, over=True)

    else:
        # Geometric-mean fallbacks when DC unavailable
        feats["dc_p_btts"]       = math.sqrt(feats["h_shrunk_btts"]   * feats["a_shrunk_btts"])
        feats["dc_p_over25"]     = math.sqrt(feats["h_shrunk_over25"]  * feats["a_shrunk_over25"])
        feats["dc_p_over35"]     = math.sqrt(feats["h_shrunk_over35"]  * feats["a_shrunk_over35"])
        feats["dc_p_under15_ft"] = math.sqrt(feats["h_shrunk_under15_ft"] * feats["a_shrunk_under15_ft"])
        feats["dc_p_draw"]       = math.sqrt(feats["h_shrunk_draw"]    * feats["a_shrunk_draw"])
        feats["dc_p_home_win"]   = feats["h_shrunk_home_win"]
        feats["dc_p_away_win"]   = feats["a_shrunk_away_win"]

        # Fallback AH/AT features from goal-rate estimates
        h_xg_raw = (feats["h_shrunk_gf"] + feats["a_shrunk_ga"]) / 2 * GOAL_SCALE
        a_xg_raw = (feats["a_shrunk_gf"] + feats["h_shrunk_ga"]) / 2 * GOAL_SCALE
        exp_margin = h_xg_raw - a_xg_raw
        for line in AH_LINES:
            key = f"dc_ah_home_{line:+.1f}".replace("+", "p").replace("-", "m").replace(".", "")
            # Rough approximation: sigmoid of (margin - handicap)
            feats[key] = 1 / (1 + math.exp(-(exp_margin + line) * 0.8))
        for line in AT_LINES:
            key = f"dc_at_over_{line}".replace(".", "")
            feats[key] = feats["dc_p_over25"] * (2.5 / line) if line > 0 else 0.5

    # Half-time DC features — independent models fit on 1H/2H scorelines
    from engine.dixon_coles import p_over as _p_over
    feats["dc_p_over05_1h"] = (
        _p_over(dc_probs_1h, 0.5) if dc_probs_1h is not None
        else feats.get("h_shrunk_1h_over05", LEAGUE_PRIOR["1h_over05"])
    )
    feats["dc_p_over15_2h"] = (
        _p_over(dc_probs_2h, 1.5) if dc_probs_2h is not None
        else feats.get("h_shrunk_2h_over15", LEAGUE_PRIOR["2h_over15"])
    )

    h_xg = (feats["h_shrunk_gf"] + feats["a_shrunk_ga"]) / 2
    a_xg = (feats["a_shrunk_gf"] + feats["h_shrunk_ga"]) / 2
    feats["xg_total_norm"] = min(1.0, h_xg + a_xg)
    feats["xg_diff_norm"]  = max(-1.0, min(1.0, (h_xg - a_xg) / 0.5))

    return feats


# Stable feature order — must not change between training and inference.
# New features are appended at the end; existing indices never move.
FEATURE_NAMES: list[str] = [
    # ---- Home team ----
    "h_shrunk_btts",       "h_shrunk_over25",      "h_shrunk_over35",
    "h_shrunk_scored",     "h_shrunk_conceded",
    "h_shrunk_gf",         "h_shrunk_ga",
    "h_shrunk_under15_ft",
    "h_decay_btts",        "h_decay_over25",        "h_decay_over35",
    "h_decay_scored",      "h_decay_conceded",
    "h_decay_gf",          "h_decay_ga",
    "h_decay_under15_ft",
    "h_n_games",
    # new
    "h_shrunk_draw",       "h_shrunk_home_win",     "h_shrunk_away_win",
    "h_decay_draw",        "h_decay_home_win",      "h_decay_away_win",
    "h_shrunk_gd",         "h_decay_gd",
    # ---- Away team ----
    "a_shrunk_btts",       "a_shrunk_over25",       "a_shrunk_over35",
    "a_shrunk_scored",     "a_shrunk_conceded",
    "a_shrunk_gf",         "a_shrunk_ga",
    "a_shrunk_under15_ft",
    "a_decay_btts",        "a_decay_over25",        "a_decay_over35",
    "a_decay_scored",      "a_decay_conceded",
    "a_decay_gf",          "a_decay_ga",
    "a_decay_under15_ft",
    "a_n_games",
    # new
    "a_shrunk_draw",       "a_shrunk_home_win",     "a_shrunk_away_win",
    "a_decay_draw",        "a_decay_home_win",      "a_decay_away_win",
    "a_shrunk_gd",         "a_decay_gd",
    # ---- Shared DC signals ----
    "dc_p_btts",           "dc_p_over25",           "dc_p_over35",
    "dc_p_under15_ft",
    "xg_total_norm",
    # new
    "dc_p_draw",           "dc_p_home_win",         "dc_p_away_win",
    "dc_p_over05_1h",      "dc_p_over15_2h",
    # AH lines
    "dc_ah_home_m20",  "dc_ah_home_m15",  "dc_ah_home_m10",
    "dc_ah_home_m05",  "dc_ah_home_p00",
    "dc_ah_home_p05",  "dc_ah_home_p10",  "dc_ah_home_p15",  "dc_ah_home_p20",
    # Asian totals
    "dc_at_over_225",  "dc_at_over_25",  "dc_at_over_275",
    "dc_at_over_30",   "dc_at_over_325",
    # xG diff
    "xg_diff_norm",
    # ---- Shot-based features (home) ----
    "h_shrunk_shots_on_target",  "h_decay_shots_on_target",
    "h_shrunk_shots_inside_box", "h_decay_shots_inside_box",
    "h_shrunk_corners",          "h_decay_corners",
    "h_shrunk_saves",            "h_decay_saves",
    "h_shrunk_shot_accuracy",    "h_decay_shot_accuracy",
    "h_shrunk_pass_accuracy",    "h_decay_pass_accuracy",
    "h_xg_shrunk",               "h_xg_decay",
    # ---- Shot-based features (away) ----
    "a_shrunk_shots_on_target",  "a_decay_shots_on_target",
    "a_shrunk_shots_inside_box", "a_decay_shots_inside_box",
    "a_shrunk_corners",          "a_decay_corners",
    "a_shrunk_saves",            "a_decay_saves",
    "a_shrunk_shot_accuracy",    "a_decay_shot_accuracy",
    "a_shrunk_pass_accuracy",    "a_decay_pass_accuracy",
    "a_xg_shrunk",               "a_xg_decay",
]


def to_vector(feats: dict[str, float]) -> list[float]:
    return [feats.get(name, 0.0) for name in FEATURE_NAMES]

"""
engine/dixon_coles.py

Dixon-Coles attack/defence parameter estimation.

For each fixture we model:
    λ_home = attack[home] * defence[away] * home_adv
    λ_away = attack[away] * defence[home]

P(score = h:a) = τ(h,a,λ_h,λ_a) * Poisson(h|λ_h) * Poisson(a|λ_a)

where τ is the low-score correction (Dixon & Coles 1997) controlled by ρ,
which adjusts probability mass of 0-0, 1-0, 0-1 and 1-1 scorelines.

Marginal probability functions:
    p_btts()          — both teams to score
    p_over()          — total goals over threshold (whole or half lines)
    p_draw()          — draw
    p_home_win()      — home win
    p_away_win()      — away win
    p_ah()            — Asian handicap (quarter-ball lines split stake)
    p_asian_total()   — Asian total (quarter-ball lines split stake)
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field

import numpy as np
from scipy.optimize import minimize


@dataclass
class DCParams:
    attack:   dict[str, float] = field(default_factory=dict)
    defence:  dict[str, float] = field(default_factory=dict)
    home_adv: float            = 1.0
    rho:      float            = 0.0
    teams:    list[str]        = field(default_factory=list)

    def lambda_home(self, home: str, away: str) -> float:
        return self.attack[home] * self.defence[away] * self.home_adv

    def lambda_away(self, home: str, away: str) -> float:
        return self.attack[away] * self.defence[home]

    def expected_goals(self, home: str, away: str) -> float:
        return self.lambda_home(home, away) + self.lambda_away(home, away)


def _tau(h: int, a: int, lh: float, la: float, rho: float) -> float:
    """Dixon-Coles correction factor for low-scoring outcomes."""
    if h == 0 and a == 0: return 1 - lh * la * rho
    if h == 0 and a == 1: return 1 + lh * rho
    if h == 1 and a == 0: return 1 + la * rho
    if h == 1 and a == 1: return 1 - rho
    return 1.0


def _poisson_pmf(k: int, lam: float) -> float:
    return math.exp(-lam) * (lam ** k) / math.factorial(k)


_PARAM_CLIP = 3.0


def _neg_log_likelihood(
    params_vec: np.ndarray,
    teams:      list[str],
    home_teams: list[str],
    away_teams: list[str],
    home_goals: list[int],
    away_goals: list[int],
    weights:    list[float],
) -> float:
    n  = len(teams)
    pv = np.clip(params_vec, -_PARAM_CLIP, _PARAM_CLIP)
    attack   = {t: math.exp(pv[i])     for i, t in enumerate(teams)}
    defence  = {t: math.exp(pv[n + i]) for i, t in enumerate(teams)}
    home_adv = math.exp(pv[2 * n])
    rho      = float(params_vec[2 * n + 1])

    total = 0.0
    for ht, at, hg, ag, w in zip(home_teams, away_teams, home_goals, away_goals, weights):
        lh = attack[ht] * defence[at] * home_adv
        la = attack[at] * defence[ht]
        hg_c, ag_c = min(hg, 10), min(ag, 10)
        t  = max(_tau(hg_c, ag_c, lh, la, rho), 1e-10)
        ph = _poisson_pmf(hg_c, lh)
        pa = _poisson_pmf(ag_c, la)
        if ph <= 0 or pa <= 0:
            continue
        total += w * (math.log(t) + math.log(ph) + math.log(pa))

    return -total


def fit(
    home_teams: list[str],
    away_teams: list[str],
    home_goals: list[int],
    away_goals: list[int],
    weights:    list[float] | None = None,
    max_iter:   int = 200,
) -> DCParams | None:
    """Fit Dixon-Coles via L-BFGS-B. Returns None if insufficient data."""
    teams = sorted(set(home_teams) | set(away_teams))
    if len(teams) < 2 or len(home_teams) < len(teams):
        return None

    if weights is None:
        weights = [1.0] * len(home_teams)

    n           = len(teams)
    x0          = np.zeros(2 * n + 2)
    param_bounds = [(-_PARAM_CLIP, _PARAM_CLIP)] * (2 * n + 1) + [(-1.0, 1.0)]

    result = minimize(
        _neg_log_likelihood,
        x0,
        args=(teams, home_teams, away_teams, home_goals, away_goals, weights),
        method="L-BFGS-B",
        bounds=param_bounds,
        options={"maxiter": max_iter, "ftol": 1e-6},
    )

    pv = result.x
    return DCParams(
        attack  ={t: math.exp(pv[i])     for i, t in enumerate(teams)},
        defence ={t: math.exp(pv[n + i]) for i, t in enumerate(teams)},
        home_adv=math.exp(pv[2 * n]),
        rho     =float(pv[2 * n + 1]),
        teams   =teams,
    )


def score_probabilities(
    dc:        DCParams,
    home:      str,
    away:      str,
    max_goals: int = 8,
) -> dict[tuple[int, int], float] | None:
    """
    Full scoreline probability distribution, or None if either team is unknown.
    Keys are (home_goals, away_goals) tuples.
    """
    if home not in dc.teams or away not in dc.teams:
        return None
    lh = dc.lambda_home(home, away)
    la = dc.lambda_away(home, away)
    return {
        (h, a): max(0.0, _tau(h, a, lh, la, dc.rho) * _poisson_pmf(h, lh) * _poisson_pmf(a, la))
        for h in range(max_goals + 1)
        for a in range(max_goals + 1)
    }


# ---------------------------------------------------------------------------
# Marginal probability functions — all take the scoreline dict from
# score_probabilities() so the distribution is only computed once per fixture.
# ---------------------------------------------------------------------------

def p_btts(probs: dict[tuple[int, int], float]) -> float:
    """P(both teams score)."""
    return sum(p for (h, a), p in probs.items() if h > 0 and a > 0)


def p_over(probs: dict[tuple[int, int], float], threshold: float = 2.5) -> float:
    """P(total goals > threshold). Works for whole and half lines (e.g. 2.5, 3.5)."""
    return sum(p for (h, a), p in probs.items() if h + a > threshold)


def p_draw(probs: dict[tuple[int, int], float]) -> float:
    """P(draw)."""
    return sum(p for (h, a), p in probs.items() if h == a)


def p_home_win(probs: dict[tuple[int, int], float]) -> float:
    """P(home win)."""
    return sum(p for (h, a), p in probs.items() if h > a)


def p_away_win(probs: dict[tuple[int, int], float]) -> float:
    """P(away win)."""
    return sum(p for (h, a), p in probs.items() if a > h)


def p_ah(
    probs:    dict[tuple[int, int], float],
    handicap: float,
    home:     bool = True,
) -> float:
    """
    Asian handicap probability for a given line.

    Quarter-ball lines (±0.25, ±0.75, ±1.25 ...) split the stake evenly
    between the two adjacent half-ball lines — we return the average of those
    two so the caller can compare directly against the bookmaker's implied
    probability for the quarter-ball price.

    handicap > 0  →  home team gives goals (e.g. -1.5 means home must win by 2+)
    handicap < 0  →  home team receives goals (e.g. +1.5 means home can lose by 1)

    home=True  →  price the home side
    home=False →  price the away side (applies handicap in reverse)
    """
    # Quarter-ball: average adjacent half-ball lines
    frac = handicap % 0.5
    if abs(frac - 0.25) < 1e-9 or abs(frac - 0.75) < 1e-9:
        lo = handicap - 0.25
        hi = handicap + 0.25
        return (p_ah(probs, lo, home) + p_ah(probs, hi, home)) / 2

    def _p_half_ball(h_cap: float, for_home: bool) -> float:
        total = 0.0
        for (h, a), p in probs.items():
            if for_home:
                # Home wins if (home_goals - away_goals) + handicap > 0
                # handicap is negative when home gives goals (e.g. -1.5)
                adjusted = (h - a) + h_cap
            else:
                # Away side: mirror the handicap — away "gives" the negative
                # of the home handicap. e.g. home -1.5 → away +1.5
                adjusted = (a - h) + (-h_cap)
            if abs(adjusted) < 1e-9:
                pass        # push — stake returned, contributes 0 to win prob
            elif adjusted > 0:
                total += p
        return total

    return _p_half_ball(handicap, home)


def p_asian_total(
    probs:     dict[tuple[int, int], float],
    line:      float,
    over:      bool = True,
) -> float:
    """
    Asian total probability for a given line.

    Quarter-ball lines (2.25, 2.75, 3.25 ...) are averaged across adjacent
    half-ball lines, identical logic to p_ah().

    over=True  →  price the over side
    over=False →  price the under side
    """
    frac = line % 0.5
    if abs(frac - 0.25) < 1e-9 or abs(frac - 0.75) < 1e-9:
        lo = line - 0.25
        hi = line + 0.25
        return (p_asian_total(probs, lo, over) + p_asian_total(probs, hi, over)) / 2

    # Half-ball line — no push possible
    if over:
        return p_over(probs, line)
    else:
        return 1.0 - p_over(probs, line)

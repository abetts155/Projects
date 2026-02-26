"""
engine/corners.py

Poisson models for corners and yellow cards.

Both use the same architecture: fit independent home/away Poisson rates
per team (attack × defence × home_advantage), then price over/under lines
from the resulting distribution.

No Dixon-Coles low-score correction — that adjustment is specific to goals
at 0-0/1-0/0-1/1-1 and doesn't apply to corners or cards.

Fitting
───────
    from engine.corners import fit_corners, fit_cards, price_corners, price_cards

    corner_model = fit_corners(fixtures, corner_data, reference)
    if corner_model:
        c_probs = price_corners(corner_model, home_team, away_team)
        # c_probs: dict[int, float] mapping total corners → probability

"""

from __future__ import annotations

import math
import datetime
from dataclasses import dataclass, field

import numpy as np
from scipy.optimize import minimize


# ---------------------------------------------------------------------------
# Shared types
# ---------------------------------------------------------------------------

@dataclass
class PoissonModel:
    """Fitted attack/defence rates for a Poisson count model."""
    attack:   dict[str, float] = field(default_factory=dict)
    defence:  dict[str, float] = field(default_factory=dict)
    home_adv: float = 1.0
    teams:    list[str] = field(default_factory=list)

    def lambda_home(self, home: str, away: str) -> float:
        return self.attack[home] * self.defence[away] * self.home_adv

    def lambda_away(self, home: str, away: str) -> float:
        return self.attack[away] * self.defence[home]


# ---------------------------------------------------------------------------
# Fitting helpers
# ---------------------------------------------------------------------------

_HALFLIFE_DAYS = 90
_MAX_DAYS      = 730


def _time_weight(fixture_date: datetime.datetime, reference: datetime.datetime) -> float:
    def _naive(dt):
        if dt.tzinfo is not None:
            return dt.astimezone(datetime.timezone.utc).replace(tzinfo=None)
        return dt
    days_ago = (_naive(reference) - _naive(fixture_date)).total_seconds() / 86400
    if days_ago < 0:
        return 0.0
    return math.exp(-math.log(2) / _HALFLIFE_DAYS * days_ago)


def _poisson_pmf(k: int, lam: float) -> float:
    lam = min(max(lam, 0.0), 100.0)   # guard against optimiser extremes
    if lam == 0.0:
        return 1.0 if k == 0 else 0.0
    return math.exp(-lam) * (lam ** k) / math.factorial(k)


def _neg_ll(
    params_vec: np.ndarray,
    teams:       list[str],
    home_teams:  list[str],
    away_teams:  list[str],
    home_counts: list[int],
    away_counts: list[int],
    weights:     list[float],
) -> float:
    n = len(teams)
    attack   = {t: math.exp(params_vec[i])     for i, t in enumerate(teams)}
    defence  = {t: math.exp(params_vec[n + i]) for i, t in enumerate(teams)}
    home_adv = math.exp(params_vec[2 * n])

    total = 0.0
    for ht, at, hc, ac, w in zip(home_teams, away_teams, home_counts, away_counts, weights):
        lh = attack[ht] * defence[at] * home_adv
        la = attack[at] * defence[ht]
        ph = _poisson_pmf(min(hc, 20), lh)
        pa = _poisson_pmf(min(ac, 20), la)
        if ph <= 0 or pa <= 0:
            continue
        total += w * (math.log(ph) + math.log(pa))
    return -total


def _fit(
    fixtures:    list,
    count_data:  dict[int, tuple[int, int]],   # fixture_id -> (home_count, away_count)
    reference:   datetime.datetime,
    min_samples: int = 10,
) -> PoissonModel | None:
    """Generic Poisson fitter used by both corners and cards."""
    cutoff = reference - datetime.timedelta(days=_MAX_DAYS)

    home_teams, away_teams, home_counts, away_counts, weights = [], [], [], [], []

    def _naive(dt):
        if dt.tzinfo is not None:
            return dt.astimezone(datetime.timezone.utc).replace(tzinfo=None)
        return dt

    for f in fixtures:
        if not f.finished:
            continue
        if f.id not in count_data:
            continue
        if _naive(f.date) < cutoff:
            continue
        w = _time_weight(f.date, reference)
        if w < 0.01:
            continue
        hc, ac = count_data[f.id]
        home_teams.append(f.home_team.name)
        away_teams.append(f.away_team.name)
        home_counts.append(hc)
        away_counts.append(ac)
        weights.append(w)

    if len(home_teams) < min_samples:
        return None

    teams = sorted(set(home_teams) | set(away_teams))
    n     = len(teams)
    x0    = np.zeros(2 * n + 1)

    bounds = [(-4.0, 4.0)] * (2 * n) + [(-1.0, 2.0)]  # attack/defence + home_adv
    result = minimize(
        _neg_ll, x0,
        args=(teams, home_teams, away_teams, home_counts, away_counts, weights),
        method="L-BFGS-B",
        bounds=bounds,
        options={"maxiter": 200, "ftol": 1e-9},
    )

    pv = result.x
    return PoissonModel(
        attack   = {t: math.exp(pv[i])     for i, t in enumerate(teams)},
        defence  = {t: math.exp(pv[n + i]) for i, t in enumerate(teams)},
        home_adv = math.exp(pv[2 * n]),
        teams    = teams,
    )


# ---------------------------------------------------------------------------
# Public fitting functions
# ---------------------------------------------------------------------------

def fit_corners(
    fixtures:    list,
    corner_data: dict[int, tuple[int, int]],
    reference:   datetime.datetime | None = None,
) -> PoissonModel | None:
    if reference is None:
        reference = datetime.datetime.now()
    return _fit(fixtures, corner_data, reference)




# ---------------------------------------------------------------------------
# Pricing functions
# ---------------------------------------------------------------------------

def _total_distribution(
    model:    PoissonModel,
    home:     str,
    away:     str,
    max_each: int = 25,
) -> dict[int, float] | None:
    """
    Return P(total = k) for k in 0..2*max_each.
    Convolves independent home and away Poisson distributions.
    Returns None if either team is unknown.
    """
    if home not in model.teams or away not in model.teams:
        return None

    lh = model.lambda_home(home, away)
    la = model.lambda_away(home, away)

    # P(home = i) and P(away = j), then convolve for total
    ph = [_poisson_pmf(i, lh) for i in range(max_each + 1)]
    pa = [_poisson_pmf(j, la) for j in range(max_each + 1)]

    total_dist: dict[int, float] = {}
    for i, pi in enumerate(ph):
        for j, pj in enumerate(pa):
            t = i + j
            total_dist[t] = total_dist.get(t, 0.0) + pi * pj

    return total_dist


def _p_over_line(dist: dict[int, float], line: float) -> float:
    return sum(p for k, p in dist.items() if k > line)


def price_corners(
    model: PoissonModel,
    home:  str,
    away:  str,
) -> dict[str, float] | None:
    """
    Returns over probabilities for standard corner lines.
    Keys: 'over_85', 'over_95', 'over_105', 'over_115'
    Returns None if either team unknown to model.
    """
    dist = _total_distribution(model, home, away)
    if dist is None:
        return None
    return {
        "over_85":  _p_over_line(dist, 8.5),
        "over_95":  _p_over_line(dist, 9.5),
        "over_105": _p_over_line(dist, 10.5),
        "over_115": _p_over_line(dist, 11.5),
        "expected": sum(k * p for k, p in dist.items()),
    }


def fit_shots(
    fixtures:   list,
    shot_data:  dict[int, tuple[int, int]],
    reference:  datetime.datetime | None = None,
) -> PoissonModel | None:
    if reference is None:
        reference = datetime.datetime.now()
    return _fit(fixtures, shot_data, reference)


def price_shots(
    model: PoissonModel,
    home:  str,
    away:  str,
) -> dict[str, float] | None:
    """
    Over probabilities for standard total-shots lines.
    Mean ~28 combined, so lines at 22.5, 24.5, 26.5, 28.5, 30.5.
    Returns None if either team unknown to model.
    """
    dist = _total_distribution(model, home, away, max_each=40)
    if dist is None:
        return None
    return {
        "over_225": _p_over_line(dist, 22.5),
        "over_245": _p_over_line(dist, 24.5),
        "over_265": _p_over_line(dist, 26.5),
        "over_285": _p_over_line(dist, 28.5),
        "over_305": _p_over_line(dist, 30.5),
        "expected": sum(k * p for k, p in dist.items()),
    }

def fit_cards(
    fixtures:  list,
    card_data: dict[int, tuple[int, int]],
    reference: datetime.datetime | None = None,
) -> PoissonModel | None:
    if reference is None:
        reference = datetime.datetime.now()
    return _fit(fixtures, card_data, reference)


def price_cards(
    model: PoissonModel,
    home:  str,
    away:  str,
) -> dict[str, float] | None:
    """
    Over probabilities for standard card lines.
    Mean ~4.7 combined, so lines at 3.5, 4.5, 5.5.
    Returns None if either team unknown to model.
    """
    dist = _total_distribution(model, home, away, max_each=20)
    if dist is None:
        return None
    return {
        "over_35": _p_over_line(dist, 3.5),
        "over_45": _p_over_line(dist, 4.5),
        "over_55": _p_over_line(dist, 5.5),
        "expected": sum(k * p for k, p in dist.items()),
    }

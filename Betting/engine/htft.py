"""
engine/htft.py

Half-time / full-time (HT/FT) result probabilities.

Uses the DC half-time and full-time scoreline distributions jointly.
In a Poisson model, second-half goals are independent of first-half goals,
so P(FT score | HT score) can be computed by convolving the HT distribution
with the second-half distribution.

Nine outcomes: HH HD HA DH DD DA AH AD AA
  First letter = half-time result (H=home win, D=draw, A=away win)
  Second letter = full-time result

Usage:
    from engine.htft import price_htft

    probs = price_htft(dc_probs_1h, dc_probs, home, away)
    # returns dict[str, float] e.g. {"HH": 0.31, "HD": 0.08, ...}
    # or None if DC models unavailable
"""

from __future__ import annotations


# HT/FT outcome labels
HTFT_OUTCOMES = ["HH", "HD", "HA", "DH", "DD", "DA", "AH", "AD", "AA"]


def _ht_result(h: int, a: int) -> str:
    if h > a:  return "H"
    if h == a: return "D"
    return "A"


def _ft_result(h: int, a: int) -> str:
    if h > a:  return "H"
    if h == a: return "D"
    return "A"


def price_htft(
    dc_probs_1h: dict[tuple[int, int], float] | None,
    dc_probs_ft: dict[tuple[int, int], float] | None,
    max_goals:   int = 8,
) -> dict[str, float] | None:
    """
    Compute HT/FT probabilities from DC scoreline distributions.

    dc_probs_1h: P(HT score = h:a) from the half-time DC model
    dc_probs_ft: P(FT score = h:a) from the full-time DC model

    The key insight: in a Poisson model second-half goals are independent
    of first-half goals.  So for each HT score (hh, ha) with probability
    p_ht, and each possible second-half addition (sh, sa) with probability
    p_2h, the FT score is (hh+sh, ha+sa) with probability p_ht * p_2h.

    We derive the second-half distribution implicitly: since
    P(FT = H:A) = sum over all HT scores of P(HT=h:a) * P(2H = H-h : A-a),
    we can work forwards by iterating over (HT, 2H addition) pairs.

    In practice we iterate over all HT scorelines, then for each one
    iterate over all possible second-half additions (sh, sa >= 0) and
    accumulate into the nine HT/FT buckets.

    The second-half Poisson rates are derived from the FT rates minus the
    HT rates — but since we don't have a separate 2H model, we approximate:
    the marginal FT distribution already encodes expected second-half scoring.
    We use a simpler and equally valid approach: iterate jointly over
    (ht_score, ft_score) pairs where ft >= ht componentwise, and accumulate
    P(ht) * P(ft | ht).

    P(ft | ht) is proportional to P(ft) summed over all ht that are
    consistent — but since goals are independent across halves:
    P(FT = H:A, HT = h:a) = P(HT = h:a) * P(2H = H-h : A-a)

    We don't have dc_probs_2h here, but we can derive the 2H marginal:
    P(2H = s:t) = sum_{h,a} P(HT=h:a) * P(FT=h+s : a+t) / P(HT=h:a)
    which simplifies to P(FT = h+s : a+t) summed is not right either.

    Cleanest correct approach: since HT and 2H are independent Poisson,
    we use the fact that the FT distribution IS the convolution of HT and 2H.
    We iterate over all (ht_h, ht_a) and (ft_h, ft_a) where ft >= ht,
    and the joint probability is:

        P(HT=ht_h:ht_a) * P(2H = ft_h-ht_h : ft_a-ht_a)

    where P(2H = s:t) comes from the residual Poisson with lambda_2h =
    lambda_ft - lambda_ht.  But we don't have lambdas here, only probs.

    Practical solution: use dc_probs_ft as a proxy for the joint by noting
    that for independent Poisson half-times:

        P(HT=h:a, FT=H:A) ≈ P(HT=h:a) * P(FT=H:A) / P(FT margins consistent)

    This is only exact for Poisson if we have the lambdas. Since we do have
    them encoded in the distributions, we use the ratio estimator:

        P(2H = s:t) = sum_{h,a} [P(FT=h+s:a+t) normalised per HT]

    Actually the simplest correct thing given what we have:
    we know lambda_1h and lambda_ft for each team from the distributions.
    E[HT goals] = sum h * P(HT=h:a), E[FT goals] = sum H * P(FT=H:A).
    lambda_2h = lambda_ft - lambda_1h, and we generate Poisson(lambda_2h).

    Returns None if either distribution is None.
    """
    if dc_probs_1h is None or dc_probs_ft is None:
        return None

    # Derive lambda_1h and lambda_ft for home and away from distributions
    lh_1h = sum(h * p for (h, a), p in dc_probs_1h.items())
    la_1h = sum(a * p for (h, a), p in dc_probs_1h.items())
    lh_ft = sum(h * p for (h, a), p in dc_probs_ft.items())
    la_ft = sum(a * p for (h, a), p in dc_probs_ft.items())

    # Second-half lambdas (floor at small positive to avoid degenerate Poisson)
    lh_2h = max(lh_ft - lh_1h, 0.05)
    la_2h = max(la_ft - la_1h, 0.05)

    import math

    def _pmf(k: int, lam: float) -> float:
        return math.exp(-lam) * (lam ** k) / math.factorial(k)

    # Precompute 2H Poisson PMFs
    max_2h = max_goals
    pmf_h2h = [_pmf(s, lh_2h) for s in range(max_2h + 1)]
    pmf_a2h = [_pmf(t, la_2h) for t in range(max_2h + 1)]

    result: dict[str, float] = {o: 0.0 for o in HTFT_OUTCOMES}

    for (ht_h, ht_a), p_ht in dc_probs_1h.items():
        if p_ht < 1e-9:
            continue
        ht_res = _ht_result(ht_h, ht_a)

        for sh in range(max_2h + 1):
            p_sh = pmf_h2h[sh]
            if p_sh < 1e-9:
                continue
            for sa in range(max_2h + 1):
                p_sa = pmf_a2h[sa]
                if p_sa < 1e-9:
                    continue
                ft_h = ht_h + sh
                ft_a = ht_a + sa
                ft_res = _ft_result(ft_h, ft_a)
                outcome = ht_res + ft_res
                result[outcome] = result.get(outcome, 0.0) + p_ht * p_sh * p_sa

    # Normalise (should already sum to ~1 but floating point)
    total = sum(result.values())
    if total > 0:
        result = {k: v / total for k, v in result.items()}

    return result


# Readable labels for display
HTFT_LABELS: dict[str, str] = {
    "HH": "Home / Home",
    "HD": "Home / Draw",
    "HA": "Home / Away",  # comeback
    "DH": "Draw / Home",
    "DD": "Draw / Draw",
    "DA": "Draw / Away",
    "AH": "Away / Home",  # comeback
    "AD": "Away / Draw",
    "AA": "Away / Away",
}

# Which outcomes are "comebacks" — inherently higher odds, often mispriced
COMEBACKS = {"HA", "AH"}

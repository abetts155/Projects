"""
engine/backtest.py

Walk-forward backtesting harness.

For each matchday d in chronological order:
  1. Train on all completed fixtures strictly before d
  2. Predict fixtures on day d using the model
  3. Compare predicted probabilities to actual outcomes

Metrics reported:
  - Brier score vs naive baseline (lower is better; 0.25 = random)
  - Brier skill score (fraction of baseline variance explained)
  - Log-loss
  - Calibration by prediction decile
  - Theoretical ROI at a fixed threshold (no bookmaker margin — measures edge)

Retrains every N days to keep wall-clock time manageable.
"""

from __future__ import annotations

import collections
import datetime
import math
from dataclasses import dataclass, field

import numpy as np
import model.fixtures

from engine.dc_training import build_training_samples, fit_dc
from engine.dixon_coles import score_probabilities
from engine.features import extract
from engine.form import build_team_form
from engine.model import ModelPair


# ---------------------------------------------------------------------------
# Result types
# ---------------------------------------------------------------------------

@dataclass
class PredictionRecord:
    date:      datetime.date
    home:      str
    away:      str
    market:    str    # "btts", "over25", "over35", "over05_1h", "over15_2h", "under05_ft", "under05_1h"
    predicted: float  # model probability ∈ [0, 1]
    actual:    bool   # ground truth


@dataclass
class BacktestResults:
    records: list[PredictionRecord] = field(default_factory=list)

    def _market(self, market: str) -> list[PredictionRecord]:
        return [r for r in self.records if r.market == market]

    # --- Scoring metrics ---

    def brier_score(self, market: str) -> float:
        recs = self._market(market)
        if not recs:
            return float("nan")
        return sum((r.predicted - int(r.actual)) ** 2 for r in recs) / len(recs)

    def baseline_brier(self, market: str) -> float:
        """Brier score of a model that always predicts the historical base rate."""
        recs = self._market(market)
        if not recs:
            return float("nan")
        base = sum(r.actual for r in recs) / len(recs)
        return sum((base - int(r.actual)) ** 2 for r in recs) / len(recs)

    def brier_skill(self, market: str) -> float:
        bs, bsb = self.brier_score(market), self.baseline_brier(market)
        return 1 - bs / bsb if bsb > 0 else float("nan")

    def log_loss(self, market: str) -> float:
        recs = self._market(market)
        if not recs:
            return float("nan")
        eps = 1e-12
        return -sum(
            math.log(r.predicted + eps) if r.actual else math.log(1 - r.predicted + eps)
            for r in recs
        ) / len(recs)

    # --- Calibration ---

    def calibration(self, market: str, n_bins: int = 10) -> list[dict]:
        """Decile calibration: mean predicted probability vs actual hit rate per bucket."""
        recs = sorted(self._market(market), key=lambda r: r.predicted)
        if not recs:
            return []
        result = []
        for bucket in np.array_split(recs, n_bins):
            if not len(bucket):
                continue
            mean_pred   = sum(r.predicted for r in bucket) / len(bucket)
            actual_rate = sum(r.actual    for r in bucket) / len(bucket)
            result.append({
                "n":              len(bucket),
                "mean_predicted": mean_pred,
                "actual_rate":    actual_rate,
                "error":          mean_pred - actual_rate,
            })
        return result

    # --- ROI simulation ---

    def roi_simulation(self, market: str, threshold: float = 0.65) -> dict:
        """
        Theoretical flat-stake ROI when backing the market above `threshold`.
        Uses fair odds (no margin) — measures model edge, not bookmaker-adjusted profit.
        """
        recs = self._market(market)
        if not recs:
            return {"bets": 0, "roi": float("nan"), "hit_rate": float("nan")}

        base_rate  = sum(r.actual for r in recs) / len(recs)
        fair_odds  = 1 / base_rate if base_rate > 0 else 2.0
        bets       = [r for r in recs if r.predicted >= threshold]

        if not bets:
            return {"bets": 0, "roi": float("nan"), "hit_rate": float("nan")}

        profit = sum((fair_odds - 1) if r.actual else -1.0 for r in bets)
        return {
            "bets":     len(bets),
            "hit_rate": sum(r.actual for r in bets) / len(bets),
            "roi":      profit / len(bets),
        }

    # --- Summary ---

    def summary(self) -> str:
        lines = []
        for market in ("btts", "over25", "over35", "over05_1h", "over15_2h", "under05_ft", "under05_1h"):
            recs = self._market(market)
            if not recs:
                continue
            roi = self.roi_simulation(market)
            lines.append(
                f"\n{market.upper()}  ({len(recs)} predictions)\n"
                f"  Brier score : {self.brier_score(market):.4f}"
                f"  (baseline {self.baseline_brier(market):.4f},"
                f"  skill {self.brier_skill(market):+.3f})\n"
                f"  Log-loss    : {self.log_loss(market):.4f}\n"
                f"  ROI @65%    : {roi['roi']:.2%}"
                f"  ({roi['bets']} bets, hit rate {roi.get('hit_rate', 0):.2%})"
            )
        return "\n".join(lines) if lines else "No backtest results."


# ---------------------------------------------------------------------------
# Walk-forward runner
# ---------------------------------------------------------------------------

def run_walkforward(
    fixtures: list,
    min_train_fixtures: int = 60,
    min_team_games:     int = 5,
    retrain_every_days: int = 7,
) -> BacktestResults:
    """
    Chronological walk-forward backtest.

    Parameters
    ----------
    fixtures            : All fixtures across all seasons, any order.
    min_train_fixtures  : Minimum completed fixtures before first prediction.
    min_team_games      : Minimum games a team must have to be included.
    retrain_every_days  : Retrain interval (daily retraining is too expensive).
    """
    results = BacktestResults()

    by_date: dict[datetime.date, list] = collections.defaultdict(list)
    for f in fixtures:
        by_date[f.date.date()].append(f)

    sorted_dates = sorted(by_date.keys())
    if len(sorted_dates) < 2:
        return results

    pair:            ModelPair | None       = None
    dc:              object | None          = None
    last_train_date: datetime.date | None   = None

    for date in sorted_dates:
        cutoff       = datetime.datetime.combine(date, datetime.time.min)
        past         = [f for f in fixtures if f.finished and f.date < cutoff]

        if len(past) < min_train_fixtures:
            continue

        needs_retrain = (
            pair is None
            or last_train_date is None
            or (date - last_train_date).days >= retrain_every_days
        )

        if needs_retrain:
            dc      = fit_dc(past, reference=cutoff)
            samples = build_training_samples(past, dc, min_team_games, reference=cutoff)
            if len(samples) < 20:
                continue
            pair = ModelPair()
            try:
                pair.fit(samples)
            except Exception:
                pair = None
                continue
            last_train_date = date

        if pair is None:
            continue

        # Predict finished fixtures on this date (ground truth available)
        team_form    = build_team_form(fixtures, cutoff=cutoff)
        today_done   = [f for f in by_date[date] if f.finished]

        for fixture in today_done:
            home_key = fixture.home_team.name
            away_key = fixture.away_team.name

            home_stats = team_form.get(home_key)
            away_stats = team_form.get(away_key)
            if not home_stats or not away_stats:
                continue
            if len(home_stats.any) < min_team_games or len(away_stats.any) < min_team_games:
                continue

            score = fixture.result(model.fixtures.Period.FULL)
            if score is None:
                continue

            dc_probs = score_probabilities(dc, home_key, away_key) if dc else None
            feats    = extract(home_stats, away_stats, dc_probs)
            p_b, p_o, p_o35, p_o05_1h, p_o15_2h, p_u05_ft, p_u05_1h = pair.predict(feats)

            results.records.append(PredictionRecord(
                date=date, home=home_key, away=away_key,
                market="btts", predicted=p_b,
                actual=score.left > 0 and score.right > 0,
            ))
            results.records.append(PredictionRecord(
                date=date, home=home_key, away=away_key,
                market="over25", predicted=p_o,
                actual=score.left + score.right > 2,
            ))
            results.records.append(PredictionRecord(
                date=date, home=home_key, away=away_key,
                market="over35", predicted=p_o35,
                actual=score.left + score.right > 3,
            ))
            first_half = fixture.result(model.fixtures.Period.FIRST)
            if first_half is not None:
                results.records.append(PredictionRecord(
                    date=date, home=home_key, away=away_key,
                    market="over05_1h", predicted=p_o05_1h,
                    actual=(first_half.left + first_half.right) > 0,
                ))
                results.records.append(PredictionRecord(
                    date=date, home=home_key, away=away_key,
                    market="under05_1h", predicted=p_u05_1h,
                    actual=(first_half.left + first_half.right) == 0,
                ))
            results.records.append(PredictionRecord(
                date=date, home=home_key, away=away_key,
                market="under05_ft", predicted=p_u05_ft,
                actual=(score.left + score.right) == 0,
            ))
            second_half = fixture.result(model.fixtures.Period.SECOND)
            if second_half is not None:
                results.records.append(PredictionRecord(
                    date=date, home=home_key, away=away_key,
                    market="over15_2h", predicted=p_o15_2h,
                    actual=(second_half.left + second_half.right) > 1,
                ))

    return results

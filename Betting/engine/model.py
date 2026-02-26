"""
engine/model.py

Logistic regression wrapper for all betting markets.

Markets: btts, over25, over35, over05_1h, over15_2h, under15_ft, under05_1h, draw, ah_home, ah_away, asian_over

Trains a separate calibrated logistic regression per market.
Uses CalibratedClassifierCV (Platt scaling, 5-fold CV) for well-calibrated
output probabilities.
"""

from __future__ import annotations

import pickle
from dataclasses import dataclass, field
from pathlib import Path
from typing import Literal

import numpy as np
from sklearn.calibration import CalibratedClassifierCV
from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import StandardScaler

from engine.features import FEATURE_NAMES, to_vector

Market = Literal["btts", "over25", "over35", "over05_1h", "over15_2h", "under15_ft", "under05_1h", "draw", "ah_home", "ah_away", "asian_over"]

ALL_MARKETS: list[Market] = ["btts", "over25", "over35", "over05_1h", "over15_2h", "under15_ft", "under05_1h", "draw", "ah_home", "ah_away", "asian_over"]


@dataclass
class TrainingSample:
    features:    dict[str, float]
    btts:        bool
    over25:      bool
    over35:      bool
    over05_1h:   bool
    over15_2h:   bool
    under15_ft:  bool
    under05_1h:  bool
    draw:        bool
    ah_home:     bool   # home wins on AH -0.5 (home win by 1+)
    ah_away:     bool   # away wins on AH -0.5 (away win by 1+)
    asian_over:  bool   # over 2.75 Asian total (proxy label)
    weight:      float = 1.0

    def label(self, market: Market) -> bool:
        return getattr(self, market)


@dataclass
class PredictionModel:
    """Fitted scaler + calibrated logistic regression for one market."""
    market:             Market
    scaler:             StandardScaler         = field(default_factory=StandardScaler)
    clf:                CalibratedClassifierCV | None = None
    n_samples:          int                    = 0
    feature_importance: dict[str, float]       = field(default_factory=dict)

    def fit(self, samples: list[TrainingSample]) -> None:
        if len(samples) < 20:
            raise ValueError(f"Need ≥20 samples to train, got {len(samples)}")

        X = np.array([to_vector(s.features) for s in samples])
        y = np.array([int(s.label(self.market)) for s in samples])
        w = np.array([s.weight for s in samples])

        # Drop samples where the label is missing (all False due to absent half-time data)
        # by checking if the market has any positive labels at all
        if y.sum() == 0 or y.sum() == len(y):
            raise ValueError(
                f"Market {self.market} has no label variance — "
                "half-time data may be missing for this competition"
            )

        self.scaler  = StandardScaler()
        X_scaled     = self.scaler.fit_transform(X)

        base = LogisticRegression(
            C=0.5,
            max_iter=1000,
            solver="lbfgs",
            class_weight="balanced",
        )
        self.clf = CalibratedClassifierCV(base, cv=5, method="sigmoid")
        self.clf.fit(X_scaled, y, sample_weight=w)
        self.n_samples = len(samples)

        try:
            coefs = np.mean(
                [est.estimator.coef_[0] for est in self.clf.calibrated_classifiers_],
                axis=0,
            )
            self.feature_importance = {
                name: float(abs(c)) for name, c in zip(FEATURE_NAMES, coefs)
            }
        except Exception:
            self.feature_importance = {}

    def predict_proba(self, features: dict[str, float]) -> float:
        if self.clf is None:
            raise RuntimeError("Model not fitted — call fit() first")
        X = self.scaler.transform(np.array([to_vector(features)]))
        return float(self.clf.predict_proba(X)[0, 1])

    def save(self, path: Path) -> None:
        path.parent.mkdir(parents=True, exist_ok=True)
        with open(path, "wb") as f:
            pickle.dump(self, f)

    @classmethod
    def load(cls, path: Path) -> PredictionModel:
        with open(path, "rb") as f:
            return pickle.load(f)


@dataclass
class ModelPair:
    """Container for all market models."""
    btts:        PredictionModel = field(default_factory=lambda: PredictionModel("btts"))
    over25:      PredictionModel = field(default_factory=lambda: PredictionModel("over25"))
    over35:      PredictionModel = field(default_factory=lambda: PredictionModel("over35"))
    over05_1h:   PredictionModel = field(default_factory=lambda: PredictionModel("over05_1h"))
    over15_2h:   PredictionModel = field(default_factory=lambda: PredictionModel("over15_2h"))
    under15_ft:  PredictionModel = field(default_factory=lambda: PredictionModel("under15_ft"))
    under05_1h:  PredictionModel = field(default_factory=lambda: PredictionModel("under05_1h"))
    draw:        PredictionModel = field(default_factory=lambda: PredictionModel("draw"))
    ah_home:     PredictionModel = field(default_factory=lambda: PredictionModel("ah_home"))
    ah_away:     PredictionModel = field(default_factory=lambda: PredictionModel("ah_away"))
    asian_over:  PredictionModel = field(default_factory=lambda: PredictionModel("asian_over"))

    def fit(self, samples: list[TrainingSample]) -> None:
        for market in ALL_MARKETS:
            m = getattr(self, market)
            try:
                m.fit(samples)
            except ValueError as e:
                # Half-time markets may lack data in some datasets — skip gracefully
                import sys
                print(f"  Warning: skipping {market} — {e}", file=sys.stderr)

    def predict(self, features: dict[str, float]) -> tuple[float, float, float, float, float, float, float]:
        """Return one probability per market in ALL_MARKETS order."""
        return tuple(
            getattr(self, market).predict_proba(features)
            if getattr(self, market).clf is not None
            else 0.0
            for market in ALL_MARKETS
        )

    def save(self, directory: Path) -> None:
        directory.mkdir(parents=True, exist_ok=True)
        for market in ALL_MARKETS:
            getattr(self, market).save(directory / f"{market}.pkl")

    @classmethod
    def load(cls, directory: Path) -> ModelPair:
        pair = cls()
        for market in ALL_MARKETS:
            path = directory / f"{market}.pkl"
            if path.exists():
                setattr(pair, market, PredictionModel.load(path))
        return pair

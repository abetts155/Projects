"""
engine/team_stats.py

Loads per-fixture, per-team detailed stats from the TeamStats SQLite table.

Schema (read-only):
    TeamStats(Fixture_ID, Team_ID, Shots_On_Goal, Shots_Off_Goal, Total_Shots,
              Blocked_Shots, Shots_Inside_Box, Shots_Outside_Box, Fouls,
              Corner_Kicks, Offsides, Yellow_Cards, Red_Cards, Saves,
              Passes, Accurate_Passes, Expected_Goals)
"""

from __future__ import annotations

import sqlite3
from pathlib import Path
from typing import TypeAlias

StatsEntry: TypeAlias = dict[str, float | None]

_COLUMNS = (
    "Shots_On_Goal",
    "Shots_Off_Goal",
    "Total_Shots",
    "Shots_Inside_Box",
    "Corner_Kicks",
    "Saves",
    "Passes",
    "Accurate_Passes",
    "Expected_Goals",
)

_COL_KEYS = {col: col.lower() for col in _COLUMNS}


def load_team_stats(db_path: Path) -> dict[tuple[int, int], StatsEntry]:
    """
    Load all TeamStats rows keyed by (fixture_id, team_id).
    Returns empty dict if table absent or db unreadable.
    """
    result: dict[tuple[int, int], StatsEntry] = {}
    try:
        con = sqlite3.connect(db_path)
        con.row_factory = sqlite3.Row
        cur = con.cursor()
        cur.execute(
            "SELECT name FROM sqlite_master WHERE type='table' AND name='TeamStats'"
        )
        if not cur.fetchone():
            con.close()
            return result
        cols = ", ".join(_COLUMNS)
        cur.execute(f"SELECT Fixture_ID, Team_ID, {cols} FROM TeamStats")
        for row in cur.fetchall():
            entry: StatsEntry = {}
            for i, col in enumerate(_COLUMNS):
                val = row[i + 2]
                entry[_COL_KEYS[col]] = float(val) if val is not None else None
            result[(row[0], row[1])] = entry
        con.close()
    except Exception:
        pass
    return result


def load_corner_data(
    db_path: Path,
    fixtures: list,
) -> dict[int, tuple[int, int]]:
    """
    Load per-fixture corner totals split by home/away team.
    Returns {fixture_id: (home_corners, away_corners)}.
    Fixtures with missing data are omitted.
    """
    fid_to_teams: dict[int, tuple[int, int]] = {
        f.id: (f.home_team.id, f.away_team.id)
        for f in fixtures if f.finished
    }

    corners: dict[int, tuple[int, int]] = {}

    try:
        con = sqlite3.connect(db_path)
        cur = con.cursor()
        cur.execute(
            "SELECT name FROM sqlite_master WHERE type='table' AND name='TeamStats'"
        )
        if not cur.fetchone():
            con.close()
            return corners

        cur.execute("SELECT Fixture_ID, Team_ID, Corner_Kicks FROM TeamStats")
        raw: dict[int, dict[int, int | None]] = {}
        for fid, tid, corn in cur.fetchall():
            raw.setdefault(fid, {})[tid] = corn
        con.close()

        for fid, (home_id, away_id) in fid_to_teams.items():
            if fid not in raw:
                continue
            hc = raw[fid].get(home_id)
            ac = raw[fid].get(away_id)
            if hc is not None and ac is not None:
                corners[fid] = (int(hc), int(ac))

    except Exception:
        pass

    return corners

def load_shot_data(
    db_path: Path,
    fixtures: list,
) -> dict[int, tuple[int, int]]:
    """
    Load per-fixture total shot counts split by home/away team.
    Returns {fixture_id: (home_shots, away_shots)}.
    Fixtures with missing data are omitted.
    """
    fid_to_teams: dict[int, tuple[int, int]] = {
        f.id: (f.home_team.id, f.away_team.id)
        for f in fixtures if f.finished
    }

    shots: dict[int, tuple[int, int]] = {}

    try:
        con = sqlite3.connect(db_path)
        cur = con.cursor()
        cur.execute(
            "SELECT name FROM sqlite_master WHERE type='table' AND name='TeamStats'"
        )
        if not cur.fetchone():
            con.close()
            return shots

        cur.execute("SELECT Fixture_ID, Team_ID, Total_Shots FROM TeamStats")
        raw: dict[int, dict[int, int | None]] = {}
        for fid, tid, sh in cur.fetchall():
            raw.setdefault(fid, {})[tid] = sh
        con.close()

        for fid, (home_id, away_id) in fid_to_teams.items():
            if fid not in raw:
                continue
            hs = raw[fid].get(home_id)
            as_ = raw[fid].get(away_id)
            if hs is not None and as_ is not None:
                shots[fid] = (int(hs), int(as_))

    except Exception:
        pass

    return shots

def load_card_data(
    db_path: Path,
    fixtures: list,
) -> dict[int, tuple[int, int]]:
    """
    Load per-fixture yellow card counts split by home/away team.
    Returns {fixture_id: (home_cards, away_cards)}.
    Fixtures with missing data are omitted.
    """
    fid_to_teams: dict[int, tuple[int, int]] = {
        f.id: (f.home_team.id, f.away_team.id)
        for f in fixtures if f.finished
    }

    cards: dict[int, tuple[int, int]] = {}

    try:
        con = sqlite3.connect(db_path)
        cur = con.cursor()
        cur.execute(
            "SELECT name FROM sqlite_master WHERE type='table' AND name='TeamStats'"
        )
        if not cur.fetchone():
            con.close()
            return cards

        cur.execute("SELECT Fixture_ID, Team_ID, Yellow_Cards FROM TeamStats")
        raw: dict[int, dict[int, int | None]] = {}
        for fid, tid, yel in cur.fetchall():
            raw.setdefault(fid, {})[tid] = yel
        con.close()

        for fid, (home_id, away_id) in fid_to_teams.items():
            if fid not in raw:
                continue
            hk = raw[fid].get(home_id)
            ak = raw[fid].get(away_id)
            if hk is not None and ak is not None:
                cards[fid] = (int(hk), int(ak))

    except Exception:
        pass

    return cards

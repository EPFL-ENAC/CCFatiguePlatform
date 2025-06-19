"""fatigue_utils.py
====================
Utilities for *fatigue tests without fracture propagation*.

This module centralises all helpers that the main ``fatigue.py`` workflow
needs when the current test **is not** a fracture‑propagation test
(``is_fracture == False``).

Keeping them here avoids cluttering the core workflow and prevents
circular imports by hosting shared types such as ``HysteresisLoop`` in a
single, importable place.
"""

from __future__ import annotations

import os
from statistics import median
from typing import Dict, List, Optional, Sequence, Tuple

import math
import numpy as np
import pandas as pd
from pandas.core.frame import DataFrame
from pydantic import BaseModel

from ccfatigue.experiment.common import DATA_DIRECTORY

__all__ = [
    "HysteresisLoop",
    "get_loops_dataframe",
    "extract_hysteresis_loops_from_file",
    "compute_sub_indexes",
    "fatigue_processing",
    "smooth_spikes",
    "apply_spike_filter",
    "smooth_iqr",
    "apply_iqr_filter",
]

# ---------------------------------------------------------------------------
# Pydantic model shared with the main workflow
# ---------------------------------------------------------------------------

class HysteresisLoop(BaseModel):
    n_cycles: List[float]
    strain: List[float]
    stress: List[float]


# ---------------------------------------------------------------------------
# Data‑frame helpers
# ---------------------------------------------------------------------------

def get_loops_dataframe(experiment: Dict[str, str], specimen_id: int) -> Optional[DataFrame]:
    """Return the *Loops_measure_XXX.csv* file if it exists.

    The filename convention matches that used inside the raw data folder,
    constructed from the *researcher* and *experiment_type* fields.
    """
    researcher_name = experiment["researcher"].split(" ")[-1]
    folder_name = f"TST_{researcher_name}_{experiment['date']}_{experiment['experiment_type']}"
    file_path = os.path.join(
        DATA_DIRECTORY, folder_name, f"Loops_measure_{specimen_id:03d}.csv"
    )
    return pd.read_csv(file_path) if os.path.exists(file_path) else None


def extract_hysteresis_loops_from_file(df: pd.DataFrame) -> List[HysteresisLoop]:
    """Convert the *Loops_measure*.csv format to a list[HysteresisLoop]."""
    loops: List[HysteresisLoop] = []
    for n_cycle, group in df.groupby("n_cycles"):
        sorted_group = group.sort_values("point_index")
        loops.append(
            HysteresisLoop(
                n_cycles=[n_cycle] * len(sorted_group),
                strain=sorted_group["fit_x"].tolist(),
                stress=sorted_group["fit_y"].tolist(),
            )
        )
    return loops


def compute_sub_indexes(df: DataFrame) -> List[int]:
    """Pick ten roughly‑uniform indices from *df["n_cycles"]*."""
    unique_n_cycles = np.unique(df["n_cycles"])
    indexes = np.linspace(0, len(unique_n_cycles) - 1, 10).astype(int)
    return unique_n_cycles[indexes]


# ---------------------------------------------------------------------------
# Fatigue‑specific preprocessing
# ---------------------------------------------------------------------------

def fatigue_processing(
    df: DataFrame, sub_indexes: List[int], test_meta: Dict
) -> Dict[str, object]:
    """Build sub‑sampled hysteresis loops and derive *n_fail*.

    Parameters
    ----------
    df : DataFrame
        The *TST_*.csv standard dataframe with at least ``Load``, ``exx``
        and ``N_cycles`` columns.
    sub_indexes : list[int]
        Indices (cycle counts) at which to extract closed hysteresis
        loops.
    test_meta : dict
        Width/thickness etc. obtained via ``get_test_fields``.
    """
    sub_hystloops: List[HysteresisLoop] = []

    # Normalise stress / strain columns for convenience
    df = df.assign(
        stress=df["Load"] / (test_meta["width"] * test_meta["thickness"]),
        strain=df["exx"],
    )

    n_fail = int(df["N_cycles"].max())

    for sub_index in sub_indexes:
        mask = df["N_cycles"] == sub_index
        if not mask.any():
            continue

        sub_df = df[mask]
        # Close the loop by appending the first point at the end
        stress = np.append(sub_df["stress"].to_numpy(), sub_df["stress"].iloc[0])
        strain = np.append(sub_df["strain"].to_numpy(), sub_df["strain"].iloc[0])
        cycles = np.append(sub_df["N_cycles"].to_numpy(), sub_df["N_cycles"].iloc[0])

        sub_hystloops.append(
            HysteresisLoop(
                n_cycles=cycles.tolist(),
                stress=stress.tolist(),
                strain=strain.tolist(),
            )
        )

    return {"sub_hystloops": sub_hystloops, "n_fail": n_fail}


# ---------------------------------------------------------------------------
# Global & local outlier filters
# ---------------------------------------------------------------------------

def smooth_spikes(data: List[float], threshold: float = 0.05) -> List[float]:
    """Suppress single‑point spikes using a 5‑sample sliding window.

    The logic is identical to the original implementation: if the
    *middle* point of a 5‑sample window deviates more than *threshold*
    from the average of the two immediate neighbours on either side, it
    gets replaced by that average.

    Parameters
    ----------
    data : list[float]
        Input numeric sequence.
    threshold : float, optional
        Relative deviation tolerated before considering a sample a spike.
    """
    smoothed = data.copy()
    for i in range(2, len(data) - 2):
        v0, v1, v2, v3, v4 = data[i - 2 : i + 3]
        m1, m2 = (v0 + v1) / 2, (v3 + v4) / 2
        avg = (m1 + m2) / 2

        if avg == 0:  # avoid div‑by‑zero
            continue
        if abs(m1 - m2) / avg < threshold and abs(v2 - avg) / avg > threshold:
            smoothed[i] = avg
    return smoothed


def apply_spike_filter(df: DataFrame, field: str, specimen_name: str) -> Tuple[List[float], bool]:
    """Apply ``smooth_spikes`` to *df[field]* and report changes."""
    original = df[field].tolist()
    filtered = smooth_spikes(original)
    changed = original != filtered
    if changed:
        print(
            f"[Spike Filter] '{specimen_name}' – '{field}' modified in "
            f"{sum(o != f for o, f in zip(original, filtered))} points"
        )
    return filtered, changed


# ---------------------------------------------------------------------------
# IQR‑based global outlier filter
# ---------------------------------------------------------------------------

def smooth_iqr(data: Sequence[float], k: float = 1.5) -> List[float]:
    """Replace global outliers with the median of nearby non‑outliers."""

    if not data:
        return list(data)

    q1, q3 = np.percentile(data, [25, 75])
    iqr = q3 - q1
    lower, upper = q1 - k * iqr, q3 + k * iqr

    filtered = list(data)
    for i, value in enumerate(data):
        if not (lower <= value <= upper) and math.isfinite(value):
            # up to 2 neighbours either side within the fence
            neighbours = [
                data[j]
                for j in range(max(0, i - 2), min(len(data), i + 3))
                if j != i and lower <= data[j] <= upper and math.isfinite(data[j])
            ]
            if neighbours:
                filtered[i] = median(neighbours)
    return filtered


def apply_iqr_filter(
    df: pd.DataFrame, field: str, specimen_name: str, k: float = 1.5
) -> Tuple[List[float], bool]:
    """Apply IQR filtering and log how many points changed."""
    original = df[field].tolist()
    filtered = smooth_iqr(original, k=k)
    changed = original != filtered
    if changed:
        print(
            f"[IQR Filter] '{specimen_name}' – '{field}' modified in "
            f"{sum(o != f for o, f in zip(original, filtered))} points"
        )
    return filtered, changed

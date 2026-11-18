"""Palmgren-Miner damage summation from CYC and CLD data.

This module deliberately does not know how cycle spectra, S-N curves, or
constant-life diagrams are produced.  Those are independent, tested stages in
CCFatigue.  It consumes their standard tabular outputs and performs only the
last step from the paper's workflow:

1. scale one complete load spectrum;
2. obtain the constant-amplitude life of every counted cycle by inverting the
   CLD at its mean stress and stress amplitude;
3. sum ``n_i / N_i``;
4. report both damage per spectrum pass and spectrum passes to failure.
"""

from __future__ import annotations

import json
import math
from dataclasses import dataclass
from typing import Any

import numpy as np
import pandas as pd
from pandas._typing import FilePath, ReadCsvBuffer, WriteBuffer


_CYC_COLUMNS = {"stress_range", "stress_mean", "n_cycles"}
_CLD_COLUMNS = {"cycles_to_failure", "stress_amplitude", "stress_mean"}
_MAX_PREDICTED_LIFE = 1e30


def _read_numeric_csv(
    source: FilePath | ReadCsvBuffer,
    required_columns: set[str],
    label: str,
) -> pd.DataFrame:
    try:
        frame = pd.read_csv(source)
    except (pd.errors.EmptyDataError, pd.errors.ParserError) as exc:
        raise ValueError(f"{label} CSV is invalid: {exc}") from exc

    missing = required_columns - set(frame.columns)
    if missing:
        raise ValueError(
            f"{label} CSV is missing required column(s): "
            + ", ".join(sorted(missing))
        )

    frame = frame.copy()
    for column in required_columns:
        frame[column] = pd.to_numeric(frame[column], errors="coerce")
    if frame[list(required_columns)].isna().any().any():
        raise ValueError(f"{label} CSV contains non-numeric or missing values.")
    if not np.isfinite(frame[list(required_columns)].to_numpy(dtype=float)).all():
        raise ValueError(f"{label} CSV contains non-finite values.")
    return frame


@dataclass(frozen=True)
class IsolifeCurve:
    cycles_to_failure: float
    stress_mean: np.ndarray
    stress_amplitude: np.ndarray

    def amplitude_on_load_ray(
        self, mean_stress: float, stress_amplitude: float
    ) -> float | None:
        """Intersect this isoline with the constant-stress-ratio load ray.

        Scaling a load cycle scales its mean and alternating components by the
        same amount, so every loading pattern lies on a ray from the origin in
        the CLD plane.  The first positive intersection is the allowable
        amplitude for this life level and stress ratio.
        """

        tolerance = 1e-12
        candidates: list[tuple[float, float]] = []

        if abs(mean_stress) <= tolerance:
            # R = -1: the load ray is the vertical sigma_m = 0 line.
            for index in range(len(self.stress_mean) - 1):
                m1, m2 = self.stress_mean[index : index + 2]
                a1, a2 = self.stress_amplitude[index : index + 2]
                if min(m1, m2) - tolerance <= 0 <= max(m1, m2) + tolerance:
                    if abs(m2 - m1) <= tolerance:
                        amplitude = min(a1, a2)
                    else:
                        fraction = -m1 / (m2 - m1)
                        amplitude = a1 + fraction * (a2 - a1)
                    if amplitude > tolerance:
                        candidates.append((amplitude / stress_amplitude, amplitude))
        else:
            slope = stress_amplitude / mean_stress
            for index in range(len(self.stress_mean) - 1):
                m1, m2 = self.stress_mean[index : index + 2]
                a1, a2 = self.stress_amplitude[index : index + 2]
                f1 = a1 - slope * m1
                f2 = a2 - slope * m2

                if abs(f1) <= tolerance:
                    mean_at_intersection = m1
                    amplitude = a1
                elif f1 * f2 > 0 or abs(f1 - f2) <= tolerance:
                    continue
                else:
                    fraction = f1 / (f1 - f2)
                    if fraction < -tolerance or fraction > 1 + tolerance:
                        continue
                    mean_at_intersection = m1 + fraction * (m2 - m1)
                    amplitude = a1 + fraction * (a2 - a1)

                scale = mean_at_intersection / mean_stress
                if scale > tolerance and amplitude > tolerance:
                    candidates.append((scale, amplitude))

        if not candidates:
            return None
        return float(min(candidates, key=lambda item: item[0])[1])


class ConstantLifeDiagram:
    """A set of CLD isolines that can be inverted to estimate fatigue life."""

    def __init__(self, frame: pd.DataFrame):
        curves: list[IsolifeCurve] = []
        for cycles, group in frame.groupby("cycles_to_failure", sort=True):
            if cycles <= 0:
                raise ValueError("CLD cycles_to_failure values must be positive.")

            # Some formulations can emit the same mean-stress location more than
            # once at a domain junction.  Their amplitudes should coincide; using
            # the minimum is conservative and makes interpolation unambiguous.
            points = (
                group.groupby("stress_mean", as_index=False)["stress_amplitude"]
                .min()
                .sort_values("stress_mean")
            )
            points = points[points["stress_amplitude"] >= 0]
            if len(points) < 2:
                continue
            curves.append(
                IsolifeCurve(
                    cycles_to_failure=float(cycles),
                    stress_mean=points["stress_mean"].to_numpy(dtype=float),
                    stress_amplitude=points["stress_amplitude"].to_numpy(
                        dtype=float
                    ),
                )
            )

        if len(curves) < 2:
            raise ValueError(
                "CLD data must contain at least two usable constant-life isolines."
            )

        self.curves = sorted(curves, key=lambda curve: curve.cycles_to_failure)
        # CLD generators terminate every isoline at (-UCS, 0) and (UTS, 0).
        # Use the common supported interval so an invalid static load is never
        # silently extrapolated outside the diagram.
        self.minimum_mean_stress = max(curve.stress_mean[0] for curve in self.curves)
        self.maximum_mean_stress = min(curve.stress_mean[-1] for curve in self.curves)
        if self.minimum_mean_stress >= self.maximum_mean_stress:
            raise ValueError("CLD isolines do not share a valid mean-stress range.")

    @classmethod
    def from_csv(cls, source: FilePath | ReadCsvBuffer) -> "ConstantLifeDiagram":
        frame = _read_numeric_csv(source, _CLD_COLUMNS, "CLD")
        return cls(frame)

    def cycles_to_failure(
        self, mean_stress: float, stress_amplitude: float
    ) -> tuple[float, bool]:
        """Return ``(N, static_overload)`` for one constant-amplitude cycle.

        Interpolation is linear in ``log10(stress amplitude)`` and
        ``log10(cycles)``.  The nearest two isolines are used for extrapolation
        beyond the generated life grid, matching the power-law behavior of an
        S-N curve without imposing an artificial 1e3/1e9 life clamp.
        """

        if stress_amplitude <= 0:
            return math.inf, False

        lower = self.minimum_mean_stress
        upper = self.maximum_mean_stress
        minimum_stress = mean_stress - stress_amplitude
        maximum_stress = mean_stress + stress_amplitude
        if minimum_stress <= lower or maximum_stress >= upper:
            return 1.0, True

        lives: list[float] = []
        allowable: list[float] = []
        for curve in self.curves:
            amplitude = curve.amplitude_on_load_ray(mean_stress, stress_amplitude)
            if amplitude is not None and amplitude > 0:
                lives.append(curve.cycles_to_failure)
                allowable.append(amplitude)

        if len(lives) < 2:
            # This occurs only extremely close to a static-strength endpoint.
            # Treat it as immediate failure rather than inventing unsupported
            # long-life behavior.
            return 1.0, True

        # A physical CLD must not gain amplitude capacity as life increases.
        # Small numerical crossings are made conservative before inversion.
        allowable_array = np.minimum.accumulate(np.asarray(allowable, dtype=float))
        life_array = np.asarray(lives, dtype=float)

        log_amplitude = np.log10(allowable_array)
        log_life = np.log10(life_array)

        # Reverse to make amplitude ascending, then collapse plateaus introduced
        # by conservative monotonicization.  At a plateau, retain the shortest
        # supported life.
        unique: dict[float, float] = {}
        for x_value, y_value in zip(log_amplitude[::-1], log_life[::-1]):
            unique[x_value] = min(unique.get(x_value, y_value), y_value)
        ordered = sorted(unique.items())
        if len(ordered) < 2:
            raise ValueError(
                "CLD isolines have no usable amplitude separation at "
                f"mean stress {mean_stress:g} MPa."
            )

        x_values = np.asarray([item[0] for item in ordered], dtype=float)
        y_values = np.asarray([item[1] for item in ordered], dtype=float)
        target = math.log10(stress_amplitude)

        if target <= x_values[0]:
            left, right = 0, 1
        elif target >= x_values[-1]:
            left, right = len(x_values) - 2, len(x_values) - 1
        else:
            right = int(np.searchsorted(x_values, target, side="right"))
            left = right - 1

        dx = x_values[right] - x_values[left]
        if abs(dx) < 1e-15:
            log_n = y_values[left]
        else:
            fraction = (target - x_values[left]) / dx
            log_n = y_values[left] + fraction * (
                y_values[right] - y_values[left]
            )

        life = 10.0 ** float(np.clip(log_n, 0.0, math.log10(_MAX_PREDICTED_LIFE)))
        return max(1.0, life), False


def _validate_factors(
    factor_min: float, factor_max: float, factor_count: int
) -> np.ndarray:
    if not math.isfinite(factor_min) or factor_min <= 0:
        raise ValueError("Minimum spectrum factor must be greater than zero.")
    if not math.isfinite(factor_max) or factor_max <= 0:
        raise ValueError("Maximum spectrum factor must be greater than zero.")
    if factor_max < factor_min:
        raise ValueError("Maximum spectrum factor must not be below the minimum.")
    if factor_count < 1 or factor_count > 100:
        raise ValueError("Spectrum factor count must be between 1 and 100.")
    if factor_count == 1:
        return np.asarray([factor_min], dtype=float)
    return np.linspace(factor_min, factor_max, factor_count)


def execute(
    cyc_csv_input_file: FilePath | ReadCsvBuffer,
    cld_csv_input_file: FilePath | ReadCsvBuffer,
    das_csv_output_file: FilePath | WriteBuffer,
    das_json_output_file: FilePath | WriteBuffer,
    factor_min: float = 0.4,
    factor_max: float = 1.4,
    factor_count: int = 11,
    cycle_counting_method: str | None = None,
    sn_curve_method: str | None = None,
    cld_method: str | None = None,
) -> None:
    """Calculate damage per pass and passes to failure over a stress sweep."""

    cyc = _read_numeric_csv(cyc_csv_input_file, _CYC_COLUMNS, "CYC")
    if cyc.empty:
        raise ValueError("CYC data contains no counted cycles.")
    if (cyc["stress_range"] < 0).any():
        raise ValueError("CYC stress_range values must be non-negative.")
    if (cyc["n_cycles"] < 0).any():
        raise ValueError("CYC n_cycles values must be non-negative.")
    if float(cyc["n_cycles"].sum()) <= 0:
        raise ValueError("CYC data must contain at least one cycle.")

    diagram = ConstantLifeDiagram.from_csv(cld_csv_input_file)
    factors = _validate_factors(factor_min, factor_max, int(factor_count))

    base_amplitudes = cyc["stress_range"].to_numpy(dtype=float) / 2.0
    base_means = cyc["stress_mean"].to_numpy(dtype=float)
    cycle_counts = cyc["n_cycles"].to_numpy(dtype=float)

    rows: list[dict[str, Any]] = []
    details_by_factor: list[list[dict[str, Any]]] = []
    for factor in factors:
        amplitudes = base_amplitudes * factor
        means = base_means * factor
        maximum_stresses = means + amplitudes
        minimum_stresses = means - amplitudes

        damage_contributions: list[float] = []
        static_overload_bins = 0
        factor_details: list[dict[str, Any]] = []
        for index, (mean, amplitude, count) in enumerate(
            zip(means, amplitudes, cycle_counts)
        ):
            if count <= 0 or amplitude <= 0:
                contribution = 0.0
                life = math.inf
                static_overload = False
            else:
                life, static_overload = diagram.cycles_to_failure(mean, amplitude)
                contribution = count / life
            if static_overload:
                static_overload_bins += 1
            damage_contributions.append(contribution)
            factor_details.append(
                {
                    "bin": index,
                    "stress_mean": float(mean),
                    "stress_amplitude": float(amplitude),
                    "n_cycles": float(count),
                    "cycles_to_failure": (
                        None if not math.isfinite(life) else float(life)
                    ),
                    "damage": float(contribution),
                }
            )

        damage_per_pass = float(math.fsum(damage_contributions))
        passes_to_failure = (
            _MAX_PREDICTED_LIFE
            if damage_per_pass <= 0
            else min(1.0 / damage_per_pass, _MAX_PREDICTED_LIFE)
        )
        rows.append(
            {
                "scale_factor": float(factor),
                "stress_max": float(np.max(maximum_stresses)),
                "stress_min": float(np.min(minimum_stresses)),
                "damage_per_pass": damage_per_pass,
                "spectrum_passes_to_failure": passes_to_failure,
                "static_overload_bins": static_overload_bins,
                "cycle_counting_method": cycle_counting_method or "",
                "sn_curve_method": sn_curve_method or "supplied SNC",
                "cld_method": cld_method or "",
            }
        )
        details_by_factor.append(factor_details)

    result = pd.DataFrame(rows).sort_values("scale_factor").reset_index(drop=True)
    result.to_csv(das_csv_output_file, index=False)

    reference_index = int(np.argmin(np.abs(factors - 1.0)))
    reference_row = rows[reference_index]
    contributors = sorted(
        details_by_factor[reference_index],
        key=lambda item: item["damage"],
        reverse=True,
    )[:10]
    warnings: list[str] = []
    if reference_row["scale_factor"] != 1.0:
        warnings.append(
            "The requested factor grid does not contain 1.0; the summary uses "
            f"the nearest factor ({reference_row['scale_factor']:g})."
        )
    if any(row["static_overload_bins"] for row in rows):
        warnings.append(
            "One or more scaled cycle bins reached the CLD static-strength "
            "boundary and were assigned a life of one cycle."
        )

    metadata = {
        "damage_rule": "Palmgren-Miner linear summation",
        "life_interpolation": (
            "constant-stress-ratio CLD intersections in "
            "log(stress amplitude) versus log(cycles)"
        ),
        "cycle_bins": int(len(cyc)),
        "cycles_per_spectrum_pass": float(cyc["n_cycles"].sum()),
        "cld_life_levels": [curve.cycles_to_failure for curve in diagram.curves],
        "summary": reference_row,
        "top_damage_contributors": contributors,
        "warnings": warnings,
    }
    json_bytes = json.dumps(metadata, allow_nan=False).encode("utf-8")
    das_json_output_file.write(json_bytes)

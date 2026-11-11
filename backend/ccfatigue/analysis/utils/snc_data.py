"""Canonical S-N curve documents and CLD input conversion.

The tabular SNC CSV remains useful for plotting and interchange of sampled
points.  The JSON representation, however, describes the fitted curves and
uses the same ``SN_curves`` objects as the material document consumed by the
rest of the application.
"""

from __future__ import annotations

import io
import json
import math
from enum import Enum
from typing import Any, IO, Iterable

import pandas as pd

from ccfatigue.model import SnCurveMethod


CLD_CYCLES_TO_FAILURE = [10**exponent for exponent in range(3, 10)]

_MODEL_BY_METHOD = {
    SnCurveMethod.LIN_LOG: "Lin-Log",
    SnCurveMethod.LOG_LOG: "Log-Log",
    SnCurveMethod.SENDECKYJ: "Sendeckyj",
    SnCurveMethod.WHITNEY: "Whitney",
}

SNC_METADATA_COLUMNS = ("type", "angle", "model", "confidence_interval")
SNC_SAMPLE_COLUMNS = ("stress_ratio", "cycles_to_failure", "stress_max")


def _load_type_value(load_type: str | Enum) -> str:
    value = load_type.value if isinstance(load_type, Enum) else load_type
    normalized = str(value).strip().lower()
    if normalized not in {"tensile", "compressive", "shear"}:
        raise ValueError("S-N curve load type must be tensile, compressive, or shear.")
    return normalized


def build_sn_curve_document(
    json_data: bytes,
    method: SnCurveMethod,
    load_type: str | Enum,
    angle: float | None,
) -> bytes:
    """Add canonical curve metadata to a method's raw fit records."""

    normalized_type = _load_type_value(load_type)
    if normalized_type != "shear":
        if angle is None or not math.isfinite(angle):
            raise ValueError("A finite angle is required for an S-N curve.")
        normalized_angle: float | None = float(angle)
    else:
        normalized_angle = None

    try:
        raw = json.loads(json_data)
    except (json.JSONDecodeError, UnicodeDecodeError) as exc:
        raise ValueError(f"S-N method returned invalid JSON: {exc}") from exc

    if isinstance(raw, dict) and isinstance(raw.get("SN_curves"), list):
        records = raw["SN_curves"]
    elif isinstance(raw, list):
        records = raw
    else:
        raise ValueError("S-N method did not return a list of fitted curves.")

    curves: list[dict[str, Any]] = []
    for index, record in enumerate(records):
        if not isinstance(record, dict):
            raise ValueError(f"S-N fit record {index} must be an object.")

        curve: dict[str, Any] = dict(record)
        curve["type"] = normalized_type
        if normalized_angle is not None:
            curve["angle"] = normalized_angle
        else:
            curve.pop("angle", None)
        curve["model"] = _MODEL_BY_METHOD[method]
        curves.append(curve)

    return json.dumps({"SN_curves": curves}, allow_nan=False).encode("utf-8")


def _number(record: dict[str, Any], field: str, index: int) -> float:
    value = record.get(field)
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"SN_curves[{index}].{field} must be a number.")
    result = float(value)
    if not math.isfinite(result):
        raise ValueError(f"SN_curves[{index}].{field} must be finite.")
    return result


def _normalized_model(record: dict[str, Any], index: int) -> str:
    value = record.get("model")
    if not isinstance(value, str):
        raise ValueError(f"SN_curves[{index}].model must be a string.")
    normalized = value.strip().lower().replace("_", "").replace("-", "")
    aliases = {
        "linlog": "Lin-Log",
        "loglog": "Log-Log",
        "sendeckyj": "Sendeckyj",
        "whitney": "Whitney",
    }
    try:
        return aliases[normalized]
    except KeyError as exc:
        raise ValueError(
            f"SN_curves[{index}].model must be Lin-Log, Log-Log, "
            "Sendeckyj, or Whitney."
        ) from exc


def _curve_identity(record: dict[str, Any], index: int) -> tuple[str, float | None]:
    load_type = _load_type_value(record.get("type", "tensile"))
    if load_type == "shear":
        return load_type, None
    if "angle" not in record:
        # Legacy SNC JSON did not carry direction metadata. Treat it as the
        # former implicit longitudinal tensile curve when reading only.
        return load_type, 0.0
    return load_type, _number(record, "angle", index)


def _probability(record: dict[str, Any], index: int) -> float:
    probability = _number(record, "confidence_interval", index)
    if probability > 1:
        probability /= 100
    if not 0 < probability < 1:
        raise ValueError(
            f"SN_curves[{index}].confidence_interval must represent a "
            "probability strictly between 0 and 1."
        )
    return probability


def evaluate_sn_curve(
    record: dict[str, Any], cycles_to_failure: Iterable[float], index: int = 0
) -> list[float]:
    """Evaluate one canonical curve on a cycle grid."""

    model = _normalized_model(record, index)
    cycles = [float(value) for value in cycles_to_failure]
    if any(not math.isfinite(value) or value <= 0 for value in cycles):
        raise ValueError("Cycles to failure must be finite and greater than zero.")

    if model == "Lin-Log":
        a = _number(record, "a", index)
        b = _number(record, "b", index)
        stresses = [a + b * math.log10(n) for n in cycles]
    elif model == "Log-Log":
        a = _number(record, "a", index)
        b = _number(record, "b", index)
        if a <= 0:
            raise ValueError(
                f"SN_curves[{index}].a must be greater than zero for Log-Log."
            )
        stresses = [a * n ** (-b) for n in cycles]
    elif model == "Sendeckyj":
        alpha = _number(record, "a", index)
        beta = _number(record, "b", index)
        c_star = _number(record, "cstar", index)
        s_star = _number(record, "sstar", index)
        if alpha <= 0 or beta <= 0 or c_star <= 0 or s_star <= 0:
            raise ValueError(
                f"SN_curves[{index}] has non-positive Sendeckyj parameters."
            )
        sigma_e = beta * (-math.log(_probability(record, index))) ** (1 / alpha)
        stresses = [
            sigma_e / ((1 - c_star + c_star * max(n, 2)) ** s_star) for n in cycles
        ]
    else:
        alpha_f = _number(record, "alpha_f", index)
        sigma0 = _number(record, "sigma0", index)
        power = _number(record, "power", index)
        if alpha_f <= 0 or sigma0 <= 0:
            raise ValueError(f"SN_curves[{index}] has non-positive Whitney parameters.")
        probability = _probability(record, index)
        factor = (-math.log(probability)) ** (power / alpha_f)
        stresses = [sigma0 * factor * n ** (-power) for n in cycles]

    if any(not math.isfinite(value) or value <= 0 for value in stresses):
        raise ValueError(
            f"SN_curves[{index}] produces a non-positive or non-finite stress "
            "on the CLD life grid."
        )
    return stresses


def read_sn_curve_document(source: IO[bytes] | IO[str]) -> list[dict[str, Any]]:
    source.seek(0)
    try:
        document = json.load(source)
    except (json.JSONDecodeError, UnicodeDecodeError) as exc:
        raise ValueError(f"S-N curve file is not valid JSON: {exc}") from exc

    if isinstance(document, dict):
        curves = document.get("SN_curves")
    elif isinstance(document, list):
        # Read-only compatibility with the former SNC JSON representation.
        curves = document
    else:
        curves = None

    if not isinstance(curves, list) or not curves:
        raise ValueError("S-N curve JSON must contain a non-empty SN_curves array.")
    if not all(isinstance(curve, dict) for curve in curves):
        raise ValueError("Every SN_curves entry must be an object.")
    return curves


def build_sn_curve_csv(csv_data: bytes, json_data: bytes) -> bytes:
    """Add canonical curve metadata to an S-N method's sampled CSV output."""

    curves = read_sn_curve_document(io.BytesIO(json_data))
    identities = {_curve_identity(curve, index) for index, curve in enumerate(curves)}
    if len(identities) != 1:
        raise ValueError(
            "An SNC CSV output must contain one load type and one material angle."
        )

    curve_records: list[tuple[float, dict[str, Any]]] = []
    for index, curve in enumerate(curves):
        stress_ratio = _number(curve, "stress_ratio", index)
        if any(
            math.isclose(stress_ratio, existing, rel_tol=0, abs_tol=1e-12)
            for existing, _ in curve_records
        ):
            raise ValueError(
                f"SN_curves contains more than one curve at R={stress_ratio:g}."
            )

        load_type, angle = _curve_identity(curve, index)
        curve_records.append(
            (
                stress_ratio,
                {
                    "type": load_type,
                    "angle": angle,
                    "model": _normalized_model(curve, index),
                    "confidence_interval": _number(curve, "confidence_interval", index),
                },
            )
        )

    try:
        samples = pd.read_csv(io.BytesIO(csv_data))
    except (pd.errors.EmptyDataError, pd.errors.ParserError) as exc:
        raise ValueError(f"S-N method returned invalid CSV: {exc}") from exc

    missing = set(SNC_SAMPLE_COLUMNS) - set(samples.columns)
    if missing:
        raise ValueError(
            "S-N method CSV is missing required column(s): "
            + ", ".join(sorted(missing))
        )

    ratios = pd.to_numeric(samples["stress_ratio"], errors="coerce")
    if ratios.isna().any():
        raise ValueError("S-N method CSV contains a non-numeric stress_ratio.")

    used_curves: set[int] = set()
    metadata_rows: list[dict[str, Any]] = []
    for row_index, stress_ratio in enumerate(ratios):
        matches = [
            (index, metadata)
            for index, (curve_ratio, metadata) in enumerate(curve_records)
            if math.isclose(float(stress_ratio), curve_ratio, rel_tol=0, abs_tol=1e-12)
        ]
        if len(matches) != 1:
            raise ValueError(
                f"SNC CSV row {row_index + 2} at R={stress_ratio:g} does not "
                "match exactly one SN_curves entry."
            )
        curve_index, metadata = matches[0]
        used_curves.add(curve_index)
        metadata_rows.append(metadata)

    if used_curves != set(range(len(curve_records))):
        raise ValueError("SNC CSV does not contain samples for every SN_curves entry.")

    for column in SNC_METADATA_COLUMNS:
        samples[column] = [metadata[column] for metadata in metadata_rows]

    ordered = [
        *SNC_METADATA_COLUMNS[:2],
        "stress_ratio",
        *SNC_METADATA_COLUMNS[2:],
        "cycles_to_failure",
        "stress_max",
    ]
    ordered.extend(column for column in samples.columns if column not in ordered)

    buffer = io.BytesIO()
    samples[ordered].to_csv(buffer, index=False)
    return buffer.getvalue()


def _validate_enriched_snc_csv(source: IO[bytes] | IO[str]) -> None:
    """Validate new metadata when present while accepting legacy SNC CSV files."""

    source.seek(0)
    try:
        samples = pd.read_csv(source)
    except (pd.errors.EmptyDataError, pd.errors.ParserError) as exc:
        raise ValueError(f"SNC CSV is invalid: {exc}") from exc
    finally:
        source.seek(0)

    present = set(SNC_METADATA_COLUMNS) & set(samples.columns)
    if not present:
        return

    missing = set(SNC_METADATA_COLUMNS) - set(samples.columns)
    if missing:
        raise ValueError(
            "An enriched SNC CSV must contain all metadata columns; missing: "
            + ", ".join(sorted(missing))
        )

    identities: set[tuple[str, float | None]] = set()
    curves_by_ratio: dict[float, tuple[str, float | None, str, float]] = {}
    for row_index, row in samples.iterrows():
        csv_line = row_index + 2
        try:
            load_type = _load_type_value(row["type"])
        except ValueError as exc:
            raise ValueError(f"SNC CSV line {csv_line}: {exc}") from exc

        angle_value = row["angle"]
        if load_type == "shear":
            if not pd.isna(angle_value) and str(angle_value).strip():
                raise ValueError(
                    f"SNC CSV line {csv_line}: angle must be empty for shear curves."
                )
            angle = None
        else:
            try:
                angle = float(angle_value)
            except (TypeError, ValueError) as exc:
                raise ValueError(
                    f"SNC CSV line {csv_line}: angle must be a finite number."
                ) from exc
            if not math.isfinite(angle):
                raise ValueError(
                    f"SNC CSV line {csv_line}: angle must be a finite number."
                )

        try:
            model = _normalized_model({"model": row["model"]}, row_index)
            confidence = float(row["confidence_interval"])
        except (TypeError, ValueError) as exc:
            raise ValueError(f"SNC CSV line {csv_line}: {exc}") from exc
        if not math.isfinite(confidence):
            raise ValueError(
                f"SNC CSV line {csv_line}: confidence_interval must be finite."
            )
        probability = confidence / 100 if confidence > 1 else confidence
        if not 0 < probability < 1:
            raise ValueError(
                f"SNC CSV line {csv_line}: confidence_interval must represent "
                "a probability strictly between 0 and 1."
            )

        identities.add((load_type, angle))

        try:
            stress_ratio = float(row["stress_ratio"])
        except (KeyError, TypeError, ValueError) as exc:
            raise ValueError(
                f"SNC CSV line {csv_line}: stress_ratio must be a finite number."
            ) from exc
        if not math.isfinite(stress_ratio):
            raise ValueError(
                f"SNC CSV line {csv_line}: stress_ratio must be a finite number."
            )

        metadata = (load_type, angle, model, confidence)
        existing = curves_by_ratio.get(stress_ratio)
        if existing is not None and existing != metadata:
            raise ValueError(
                f"SNC CSV contains conflicting metadata for R={stress_ratio:g}."
            )
        curves_by_ratio[stress_ratio] = metadata

    if len(identities) != 1:
        raise ValueError(
            "CLD requires S-N curves for one load type and one angle; the "
            "uploaded SNC CSV contains multiple material directions."
        )


def sn_curve_document_to_csv(
    source: IO[bytes] | IO[str],
    cycles_to_failure: Iterable[float] = CLD_CYCLES_TO_FAILURE,
) -> io.BytesIO:
    """Convert canonical fitted curves to the sampled SNC input used by CLD."""

    curves = read_sn_curve_document(source)
    identities = {_curve_identity(curve, index) for index, curve in enumerate(curves)}
    if len(identities) != 1:
        raise ValueError(
            "CLD requires S-N curves for one load type and one angle; the "
            "uploaded SN_curves document contains multiple material directions."
        )

    cycles = list(cycles_to_failure)
    rows: list[dict[str, Any]] = []
    ratios: set[float] = set()
    for index, curve in enumerate(curves):
        stress_ratio = _number(curve, "stress_ratio", index)
        if stress_ratio in ratios:
            raise ValueError(
                f"SN_curves contains more than one curve at R={stress_ratio:g}."
            )
        ratios.add(stress_ratio)
        stresses = evaluate_sn_curve(curve, cycles, index)
        load_type, angle = _curve_identity(curve, index)
        model = _normalized_model(curve, index)
        confidence_interval = _number(curve, "confidence_interval", index)
        rows.extend(
            {
                "type": load_type,
                "angle": angle,
                "stress_ratio": stress_ratio,
                "model": model,
                "confidence_interval": confidence_interval,
                "cycles_to_failure": cycles_value,
                "stress_max": stress,
            }
            for cycles_value, stress in zip(cycles, stresses)
        )

    buffer = io.BytesIO()
    pd.DataFrame(rows).to_csv(buffer, index=False)
    buffer.seek(0)
    return buffer


def prepare_cld_snc_input(source: IO[bytes]) -> IO[bytes]:
    """Return a CSV-like stream for either legacy SNC CSV or canonical JSON."""

    source.seek(0)
    prefix = source.read(256)
    source.seek(0)
    if isinstance(prefix, str):
        stripped = prefix.lstrip()
        is_json = stripped.startswith("{") or stripped.startswith("[")
    else:
        stripped = prefix.lstrip()
        is_json = stripped.startswith(b"{") or stripped.startswith(b"[")
    if is_json:
        return sn_curve_document_to_csv(source)
    _validate_enriched_snc_csv(source)
    return source

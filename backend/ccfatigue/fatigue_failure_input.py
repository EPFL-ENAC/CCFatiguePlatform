"""Parsing and selection helpers for fatigue-failure material files.

The fatigue-failure UI uploads one JSON document containing all available
static strengths and fitted S-N curves.  This module keeps knowledge of that
document format out of the individual fatigue-failure implementations.
"""

from __future__ import annotations

import json
import math
from dataclasses import dataclass
from io import BytesIO
from typing import IO, Any, Iterable

from ccfatigue.analysis.utils.faf import FatigueModel


ANGLE_TOLERANCE = 1e-9


def _number(value: Any, path: str) -> float:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{path} must be a number.")
    result = float(value)
    if not math.isfinite(result):
        raise ValueError(f"{path} must be finite.")
    return result


def _positive_number(value: Any, path: str) -> float:
    result = _number(value, path)
    if result <= 0:
        raise ValueError(f"{path} must be greater than zero.")
    return result


def _load_type(value: Any, path: str, *, default: str = "tensile") -> str:
    if value is None:
        return default
    if not isinstance(value, str):
        raise ValueError(f"{path} must be a string.")
    normalized = value.strip().lower().replace("_", "-")
    aliases = {
        "normal": "tensile",
        "tension": "tensile",
        "tensile": "tensile",
        "compression": "compressive",
        "compressive": "compressive",
        "shear": "shear",
    }
    try:
        return aliases[normalized]
    except KeyError as exc:
        raise ValueError(
            f"{path} must be one of tensile, compressive, or shear."
        ) from exc


def _fatigue_model(value: Any, path: str) -> FatigueModel:
    if not isinstance(value, str):
        raise ValueError(f"{path} must be a string.")
    normalized = value.strip().lower().replace("_", "").replace("-", "")
    aliases = {
        "linlog": FatigueModel.LIN_LOG,
        "loglog": FatigueModel.LOG_LOG,
    }
    try:
        return aliases[normalized]
    except KeyError as exc:
        raise ValueError(f"{path} must be either Lin-Log or Log-Log.") from exc


@dataclass(frozen=True)
class StaticStrength:
    load_type: str
    value: float
    angle: float | None


@dataclass(frozen=True)
class SNCurve:
    load_type: str
    angle: float | None
    stress_ratio: float
    model: FatigueModel
    a: float
    b: float
    confidence_interval: float
    lrsq: float | None = None
    fp: float | None = None

    def as_file(self) -> BytesIO:
        """Return the legacy one-record SNC file expected by FAF modules."""
        record: dict[str, float] = {
            "stress_ratio": self.stress_ratio,
            "confidence_interval": self.confidence_interval,
            "a": self.a,
            "b": self.b,
        }
        if self.lrsq is not None:
            record["lrsq"] = self.lrsq
        if self.fp is not None:
            record["fp"] = self.fp
        return BytesIO(json.dumps([record]).encode("utf-8"))


@dataclass(frozen=True)
class FatigueFailureMaterial:
    static_strengths: tuple[StaticStrength, ...]
    sn_curves: tuple[SNCurve, ...]

    @classmethod
    def from_file(cls, source: IO[bytes] | IO[str]) -> "FatigueFailureMaterial":
        source.seek(0)
        try:
            raw = json.load(source)
        except (json.JSONDecodeError, UnicodeDecodeError) as exc:
            raise ValueError(f"Material file is not valid JSON: {exc}") from exc

        if not isinstance(raw, dict):
            raise ValueError("Material file must contain a JSON object.")

        strengths_raw = raw.get("static_strengths")
        if not isinstance(strengths_raw, list) or not strengths_raw:
            raise ValueError(
                "Material file must contain a non-empty static_strengths array."
            )

        curves_raw = raw.get("SN_curves")
        if not isinstance(curves_raw, list) or not curves_raw:
            raise ValueError("Material file must contain a non-empty SN_curves array.")

        strengths = tuple(
            cls._parse_strength(item, index)
            for index, item in enumerate(strengths_raw)
        )
        curves = tuple(
            cls._parse_curve(item, index) for index, item in enumerate(curves_raw)
        )
        return cls(strengths, curves)

    @staticmethod
    def _parse_strength(item: Any, index: int) -> StaticStrength:
        path = f"static_strengths[{index}]"
        if not isinstance(item, dict):
            raise ValueError(f"{path} must be an object.")
        load_type = _load_type(item.get("type"), f"{path}.type")
        if "value" not in item:
            raise ValueError(f"{path}.value is required.")
        value = _positive_number(item["value"], f"{path}.value")
        angle = None
        if load_type != "shear":
            if "angle" not in item:
                raise ValueError(f"{path}.angle is required for {load_type} strength.")
            angle = _number(item["angle"], f"{path}.angle")
        elif "angle" in item and item["angle"] is not None:
            angle = _number(item["angle"], f"{path}.angle")
        return StaticStrength(load_type, value, angle)

    @staticmethod
    def _parse_curve(item: Any, index: int) -> SNCurve:
        path = f"SN_curves[{index}]"
        if not isinstance(item, dict):
            raise ValueError(f"{path} must be an object.")
        load_type = _load_type(item.get("type"), f"{path}.type")
        angle = None
        if load_type != "shear":
            if "angle" not in item:
                raise ValueError(f"{path}.angle is required for {load_type} curve.")
            angle = _number(item["angle"], f"{path}.angle")
        elif "angle" in item and item["angle"] is not None:
            angle = _number(item["angle"], f"{path}.angle")

        required = ("stress_ratio", "model", "a", "b")
        for field in required:
            if field not in item:
                raise ValueError(f"{path}.{field} is required.")

        model = _fatigue_model(item["model"], f"{path}.model")
        a = _number(item["a"], f"{path}.a")
        if model == FatigueModel.LOG_LOG and a <= 0:
            raise ValueError(f"{path}.a must be greater than zero for Log-Log.")

        return SNCurve(
            load_type=load_type,
            angle=angle,
            stress_ratio=_number(item["stress_ratio"], f"{path}.stress_ratio"),
            model=model,
            a=a,
            b=_number(item["b"], f"{path}.b"),
            confidence_interval=_number(
                item.get("confidence_interval", 50),
                f"{path}.confidence_interval",
            ),
            lrsq=(
                _number(item["lrsq"], f"{path}.lrsq")
                if item.get("lrsq") is not None
                else None
            ),
            fp=(
                _number(item["fp"], f"{path}.fp")
                if item.get("fp") is not None
                else None
            ),
        )

    def strength(self, load_type: str, angle: float | None = None) -> float:
        candidates = [
            strength
            for strength in self.static_strengths
            if strength.load_type == load_type
            and (
                angle is None
                or (
                    strength.angle is not None
                    and math.isclose(
                        strength.angle,
                        angle,
                        rel_tol=0,
                        abs_tol=ANGLE_TOLERANCE,
                    )
                )
            )
        ]
        if len(candidates) == 1:
            return candidates[0].value
        description = (
            load_type if angle is None else f"{load_type} at {angle:g} degrees"
        )
        if not candidates:
            raise ValueError(
                f"Material file is missing static strength: {description}."
            )
        raise ValueError(
            f"Material file has more than one static strength for {description}."
        )

    def curves(
        self,
        load_type: str,
        *,
        angle: float | None = None,
        stress_ratio: float | None = None,
    ) -> list[SNCurve]:
        return [
            curve
            for curve in self.sn_curves
            if curve.load_type == load_type
            and (
                angle is None
                or (
                    curve.angle is not None
                    and math.isclose(
                        curve.angle, angle, rel_tol=0, abs_tol=ANGLE_TOLERANCE
                    )
                )
            )
            and (
                stress_ratio is None
                or math.isclose(
                    curve.stress_ratio,
                    stress_ratio,
                    rel_tol=0,
                    abs_tol=ANGLE_TOLERANCE,
                )
            )
        ]

    def curve(
        self,
        load_type: str,
        *,
        angle: float | None = None,
        stress_ratio: float | None = None,
        allow_ratio_fallback: bool = False,
    ) -> SNCurve:
        candidates = self.curves(
            load_type, angle=angle, stress_ratio=stress_ratio
        )
        if not candidates and allow_ratio_fallback and stress_ratio is not None:
            candidates = self.curves(load_type, angle=angle)
        description = (
            load_type if angle is None else f"{load_type} at {angle:g} degrees"
        )
        if not candidates:
            ratio = "" if stress_ratio is None else f" and R={stress_ratio:g}"
            raise ValueError(
                f"Material file is missing S-N curve: {description}{ratio}."
            )
        if len(candidates) > 1:
            ratio = "" if stress_ratio is None else f" at R={stress_ratio:g}"
            raise ValueError(
                f"Material file has ambiguous S-N curves for {description}{ratio}."
            )
        return candidates[0]

    def transverse_and_shear_curves(
        self, stress_ratio: float
    ) -> tuple[SNCurve, SNCurve, str, str, float, float]:
        """Resolve Hashin-Rotem panels, preferring direct transverse/shear data."""
        transverse = self.curves("tensile", angle=90, stress_ratio=stress_ratio)
        shear = self.curves("shear", stress_ratio=stress_ratio)
        off_axis = sorted(
            (
                curve
                for curve in self.curves("tensile", stress_ratio=stress_ratio)
                if curve.angle is not None
                and not math.isclose(curve.angle, 0, abs_tol=ANGLE_TOLERANCE)
                and not math.isclose(curve.angle, 90, abs_tol=ANGLE_TOLERANCE)
            ),
            key=lambda curve: curve.angle or 0,
        )

        if transverse and shear:
            return transverse[0], shear[0], "Transverse", "Shear", 0, 0
        if transverse and off_axis:
            curve = off_axis[0]
            return (
                transverse[0],
                curve,
                "Transverse",
                "Off-axis",
                0,
                self.strength("tensile", curve.angle),
            )
        if off_axis and shear:
            curve = off_axis[0]
            return (
                curve,
                shear[0],
                "Off-axis",
                "Shear",
                self.strength("tensile", curve.angle),
                0,
            )
        if len(off_axis) >= 2:
            first, second = off_axis[:2]
            return (
                first,
                second,
                "Off-axis",
                "Off-axis",
                self.strength("tensile", first.angle),
                self.strength("tensile", second.angle),
            )
        raise ValueError(
            "HashinRotem requires transverse and shear S-N curves, or enough "
            "off-axis curves to reconstruct them, at the target stress ratio."
        )

    def shear_or_off_axis_curve(self, stress_ratio: float) -> tuple[SNCurve, float]:
        shear = self.curves("shear", stress_ratio=stress_ratio)
        if len(shear) == 1:
            return shear[0], 0
        if len(shear) > 1:
            raise ValueError(
                f"Material file has ambiguous shear S-N curves at R={stress_ratio:g}."
            )

        candidates = [
            curve
            for curve in self.curves("tensile", stress_ratio=stress_ratio)
            if curve.angle is not None
            and not math.isclose(curve.angle, 0, abs_tol=ANGLE_TOLERANCE)
            and not math.isclose(curve.angle, 90, abs_tol=ANGLE_TOLERANCE)
        ]
        if not candidates:
            raise ValueError(
                "Material file needs a shear or off-axis S-N curve at "
                f"R={stress_ratio:g}."
            )
        curve = min(candidates, key=lambda item: abs((item.angle or 0) - 45))
        return curve, float(curve.angle)

    def reference_curve(
        self, target_angle: float, target_stress_ratio: float
    ) -> SNCurve:
        candidates = [
            curve
            for curve in self.sn_curves
            if curve.load_type != "compressive"
            and curve.angle is not None
            and self._has_strength(curve.load_type, curve.angle)
        ]
        if not candidates:
            raise ValueError(
                "FawazEllyin requires an S-N curve with a matching static strength."
            )
        matching_ratio = [
            curve
            for curve in candidates
            if math.isclose(
                curve.stress_ratio,
                target_stress_ratio,
                rel_tol=0,
                abs_tol=ANGLE_TOLERANCE,
            )
        ]
        if matching_ratio:
            candidates = matching_ratio
        return min(
            candidates,
            key=lambda curve: (
                abs((curve.angle or 0) - target_angle),
                curve.angle or 0,
            ),
        )

    def reference_curve_group(self, target_angle: float) -> list[SNCurve]:
        """Pick the normal-load angle having the richest stress-ratio dataset."""
        groups: dict[float, list[SNCurve]] = {}
        for curve in self.sn_curves:
            if curve.load_type == "tensile" and curve.angle is not None:
                groups.setdefault(curve.angle, []).append(curve)
        if not groups:
            raise ValueError("Material file contains no tensile S-N curves.")

        angle, curves = min(
            groups.items(),
            key=lambda item: (
                -len({curve.stress_ratio for curve in item[1]}),
                1 if math.isclose(item[0], 0) or math.isclose(item[0], 90) else 0,
                abs(item[0] - target_angle),
                item[0],
            ),
        )
        del angle
        return sorted(curves, key=lambda curve: curve.stress_ratio)

    def primary_stress_ratio(self) -> float:
        """Use the first longitudinal tensile curve as the source campaign."""
        curves = self.curves("tensile", angle=0)
        if not curves:
            raise ValueError(
                "Material file is missing a longitudinal tensile S-N curve "
                "at 0 degrees."
            )
        return curves[0].stress_ratio

    def common_model(self, curves: Iterable[SNCurve]) -> FatigueModel:
        models = {curve.model for curve in curves}
        if len(models) != 1:
            values = ", ".join(sorted(model.value for model in models))
            raise ValueError(f"Selected S-N curves use different models: {values}.")
        return next(iter(models))

    def _has_strength(self, load_type: str, angle: float | None) -> bool:
        return any(
            strength.load_type == load_type
            and strength.angle is not None
            and angle is not None
            and math.isclose(
                strength.angle, angle, rel_tol=0, abs_tol=ANGLE_TOLERANCE
            )
            for strength in self.static_strengths
        )

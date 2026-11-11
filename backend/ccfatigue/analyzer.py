import io
import math
import os
import subprocess
from tempfile import NamedTemporaryFile, SpooledTemporaryFile
from typing import IO, Callable

import pandas as pd
from pandas._typing import ReadCsvBuffer, WriteBuffer
from pandas.core.frame import DataFrame

import ccfatigue.analysis.cld_boerstra as cld_boerstra
import ccfatigue.analysis.cld_harris as cld_harris
import ccfatigue.analysis.cld_kawai as cld_kawai
import ccfatigue.analysis.cld_piecewiselinear as cld_piecewiselinear
import ccfatigue.analysis.cld_piecewisenonlinear as cld_piecewisenonlinear
import ccfatigue.analysis.cld_simplified_harris as cld_simplified_harris
import ccfatigue.analysis.cyc_rainflow as cyc_rainflow
import ccfatigue.analysis.cyc_rangemean as cyc_rangemean
import ccfatigue.analysis.cyc_rangepair as cyc_rangepair
import ccfatigue.analysis.cyc_simplifiedrainflow as cyc_simplifiedrainflow
import ccfatigue.analysis.damage_summation as damage_summation
import ccfatigue.analysis.faf_fawazellyin as faf_fawazellyin
import ccfatigue.analysis.faf_ftpf as faf_ftpf
import ccfatigue.analysis.faf_hashinrotem as faf_hashinrotem
import ccfatigue.analysis.faf_kawai as faf_kawai
import ccfatigue.analysis.faf_shokriehtaheri as faf_shokriehtaheri
import ccfatigue.analysis.faf_simsbrogdon as faf_simsbrogdon
import ccfatigue.analysis.snc_linlog as snc_linlog
import ccfatigue.analysis.snc_loglog as snc_loglog
import ccfatigue.analysis.snc_sendeckyj as snc_sendeckyj
import ccfatigue.analysis.snc_whitney as snc_whitney
from ccfatigue.analysis.utils.snc_data import (
    build_sn_curve_csv,
    build_sn_curve_document,
    prepare_cld_snc_input,
)
from ccfatigue.utils.fatigue_failure_input import FatigueFailureMaterial
from ccfatigue.model import (
    AnalysisResult,
    CldMethod,
    CycleCountingMethod,
    DamageSummationInputType,
    FatigueFailureMethod,
    HashinRotemPanelType,
    SnCurveLoadType,
    SnCurveMethod,
)

ROUND_DECIMAL = 8


def run_fortran(exec_path: str, input_file: SpooledTemporaryFile[bytes] | IO) -> bytes:
    with NamedTemporaryFile() as tmp_file:
        input_file.seek(0)
        tmp_file.write(input_file.read())
        tmp_file.flush()
        split_path = os.path.split(exec_path)
        directory = os.path.abspath(split_path[0])
        print(f"executing {os.path.abspath(exec_path)} {tmp_file.name}")
        ouput = subprocess.check_output(
            [
                f"./{split_path[1]}",
                tmp_file.name,
            ],
            cwd=directory,
        )
        return ouput


def run_python(
    execute: Callable[[ReadCsvBuffer, WriteBuffer, WriteBuffer], None],
    input_file: SpooledTemporaryFile[bytes] | IO,
) -> AnalysisResult:
    with (
        NamedTemporaryFile() as output_csv_file,
        NamedTemporaryFile() as output_json_file,
    ):
        print(f"executing -> {output_csv_file.name} + {output_json_file.name}")
        input_file.seek(0)
        execute(input_file, output_csv_file, output_json_file)
        output_csv_file.seek(0)
        output_json_file.seek(0)
        return AnalysisResult(
            csv_data=output_csv_file.read(),
            json_data=output_json_file.read(),
        )


def run_python_2(
    execute: Callable[[ReadCsvBuffer, ReadCsvBuffer, WriteBuffer, WriteBuffer], None],
    input_file_1: SpooledTemporaryFile[bytes] | IO,
    input_file_2: SpooledTemporaryFile[bytes] | IO,
) -> AnalysisResult:
    with (
        NamedTemporaryFile() as output_csv_file,
        NamedTemporaryFile() as output_json_file,
    ):
        print(f"executing -> {output_csv_file.name} + {output_json_file.name}")
        input_file_1.seek(0)
        input_file_2.seek(0)
        execute(input_file_1, input_file_2, output_csv_file, output_json_file)
        output_csv_file.seek(0)
        output_json_file.seek(0)
        return AnalysisResult(
            csv_data=output_csv_file.read(),
            json_data=output_json_file.read(),
        )


def run_python_3(
    execute: Callable[
        [ReadCsvBuffer, ReadCsvBuffer, ReadCsvBuffer, WriteBuffer, WriteBuffer], None
    ],
    input_file_1: SpooledTemporaryFile[bytes] | IO,
    input_file_2: SpooledTemporaryFile[bytes] | IO,
    input_file_3: SpooledTemporaryFile[bytes] | IO,
) -> AnalysisResult:
    with (
        NamedTemporaryFile() as output_csv_file,
        NamedTemporaryFile() as output_json_file,
    ):
        print(f"executing -> {output_csv_file.name} + {output_json_file.name}")
        input_file_1.seek(0)
        input_file_2.seek(0)
        input_file_3.seek(0)
        execute(
            input_file_1, input_file_2, input_file_3, output_csv_file, output_json_file
        )
        output_csv_file.seek(0)
        output_json_file.seek(0)
        return AnalysisResult(
            csv_data=output_csv_file.read(),
            json_data=output_json_file.read(),
        )


def create_dataframe(output: bytes) -> DataFrame:
    df: DataFrame = pd.read_csv(io.BytesIO(output))
    return df.fillna("")


def run_sn_curve(
    file: SpooledTemporaryFile[bytes] | IO,
    method: SnCurveMethod,
    confidence_interval: float | None = None,
    load_type: SnCurveLoadType = SnCurveLoadType.TENSILE,
    angle: float | None = 0,
) -> AnalysisResult:
    match method:
        case SnCurveMethod.LIN_LOG:
            output = run_python(
                lambda input, csv_output, json_output: snc_linlog.execute(
                    input, json_output, csv_output
                ),
                file,
            )
        case SnCurveMethod.LOG_LOG:
            output = run_python(
                lambda input, csv_output, json_output: snc_loglog.execute(
                    input, json_output, csv_output
                ),
                file,
            )
        case SnCurveMethod.SENDECKYJ:
            output = run_python(
                lambda input, csv_output, json_output: snc_sendeckyj.execute(
                    input,
                    json_output,
                    csv_output,
                    confidence_interval=(
                        confidence_interval if confidence_interval is not None else 50
                    ),
                ),
                file,
            )
        case SnCurveMethod.WHITNEY:
            output = run_python(
                lambda input, csv_output, json_output: snc_whitney.execute(
                    input,
                    json_output,
                    csv_output,
                    confidence_interval=(
                        confidence_interval if confidence_interval is not None else 50
                    ),
                ),
                file,
            )
        case _:
            raise Exception(f"unknown method {method}")
    json_data = build_sn_curve_document(
        output.json_data,
        method,
        load_type,
        angle,
    )
    return AnalysisResult(
        csv_data=build_sn_curve_csv(output.csv_data, json_data),
        json_data=json_data,
    )


def run_cld_piecewiselinear_from_sn(
    file: SpooledTemporaryFile[bytes] | IO,
    sn_method: SnCurveMethod,
    ucs: float,
    uts: float,
    confidence_interval: float | None = None,
) -> AnalysisResult:
    snc_result = run_sn_curve(file, sn_method, confidence_interval)
    snc_buffer = io.BytesIO(snc_result.csv_data)
    return run_python(
        lambda inp, csv_out, _: cld_piecewiselinear.execute(inp, csv_out, ucs, uts),
        snc_buffer,
    )


def run_cld_piecewisenonlinear_from_sn(
    file: SpooledTemporaryFile[bytes] | IO,
    sn_method: SnCurveMethod,
    ucs: float,
    uts: float,
    confidence_interval: float | None = None,
) -> AnalysisResult:
    snc_result = run_sn_curve(file, sn_method, confidence_interval)
    snc_buffer = io.BytesIO(snc_result.csv_data)
    return run_python(
        lambda inp, csv_out, _: cld_piecewisenonlinear.execute(inp, csv_out, ucs, uts),
        snc_buffer,
    )


def run_cycle_counting(
    file: SpooledTemporaryFile[bytes] | IO,
    method: CycleCountingMethod,
) -> bytes:
    match method:
        case CycleCountingMethod.RANGE_MEAN:
            output = run_python(
                lambda input, csv_output, _: cyc_rangemean.execute(input, csv_output),
                file,
            )
        case CycleCountingMethod.RANGE_PAIR:
            output = run_python(
                lambda input, csv_output, _: cyc_rangepair.execute(input, csv_output),
                file,
            )
        case CycleCountingMethod.SIMPLIFIED_RAINFLOW:
            output = run_python(
                lambda input, csv_output, _: cyc_simplifiedrainflow.execute(
                    input, csv_output
                ),
                file,
            )
        case CycleCountingMethod.RAINFLOW:
            output = run_python(
                lambda input, csv_output, _: cyc_rainflow.execute(input, csv_output),
                file,
            )
        case _:
            raise Exception(f"unknown method {method}")
    return output.csv_data


def run_cld(
    file: SpooledTemporaryFile[bytes] | IO,
    method: CldMethod,
    ucs: float,
    uts: float,
    np_reference: float | None = None,
    m0_init: float | None = None,
    d_init: float | None = None,
    alpha_t_init: float | None = None,
    alpha_c_init: float | None = None,
) -> AnalysisResult:
    snc_input = prepare_cld_snc_input(file)
    match method:
        case CldMethod.HARRIS:
            output = run_python(
                lambda input, csv_output, json_output: cld_harris.execute(
                    input,
                    csv_output,
                    json_output,
                    ucs,
                    uts,
                ),
                snc_input,
            )
        case CldMethod.SIMPLIFIED_HARRIS:
            output = run_python(
                lambda input, csv_output, json_output: cld_simplified_harris.execute(
                    input, csv_output, json_output, ucs, uts
                ),
                snc_input,
            )
        case CldMethod.PIECEWISELINEAR:
            output = run_python(
                lambda input, csv_output, _: cld_piecewiselinear.execute(
                    input, csv_output, ucs, uts
                ),
                snc_input,
            )
        case CldMethod.PIECEWISENONLINEAR:
            output = run_python(
                lambda input, csv_output, _: cld_piecewisenonlinear.execute(
                    input, csv_output, ucs, uts
                ),
                snc_input,
            )
        case CldMethod.KAWAI:
            output = run_python(
                lambda input, csv_output, _: cld_kawai.execute(
                    input, csv_output, ucs, uts
                ),
                snc_input,
            )
        case CldMethod.BOERSTRA:
            output = run_python(
                lambda input, csv_output, json_output: cld_boerstra.execute(
                    input,
                    csv_output,
                    json_output,
                    ucs,
                    uts,
                    np_reference,
                    m0_init,
                    d_init,
                    alpha_t_init,
                    alpha_c_init,
                ),
                snc_input,
            )
        case _:
            raise Exception(f"unknown method {method}")
    return output


def _validate_target_angle(target_angle: float) -> None:
    if not math.isfinite(target_angle):
        raise ValueError("Target angle must be finite.")


def _require_target_stress_ratio(
    method: FatigueFailureMethod, target_stress_ratio: float | None
) -> float:
    if target_stress_ratio is None or not math.isfinite(target_stress_ratio):
        raise ValueError(f"{method.value} requires a finite target stress ratio.")
    return target_stress_ratio


def _require_target_static_strength(
    method: FatigueFailureMethod, target_static_strength: float | None
) -> float:
    if (
        target_static_strength is None
        or not math.isfinite(target_static_strength)
        or target_static_strength <= 0
    ):
        raise ValueError(
            f"{method.value} requires a target static strength greater than zero."
        )
    return target_static_strength


def _require_agg_file(
    method: FatigueFailureMethod,
    agg_file: SpooledTemporaryFile[bytes] | IO | None,
) -> SpooledTemporaryFile[bytes] | IO:
    if agg_file is None:
        raise ValueError(f"{method.value} requires an AGG CSV file.")
    return agg_file


def run_fatigue_failure(
    material_file: SpooledTemporaryFile[bytes] | IO,
    method: FatigueFailureMethod,
    target_angle: float,
    target_stress_ratio: float | None = None,
    target_static_strength: float | None = None,
    agg_file: SpooledTemporaryFile[bytes] | IO | None = None,
) -> AnalysisResult:
    """Dispatch a fatigue-failure analysis to its method-specific runner."""

    runners = {
        FatigueFailureMethod.FTPF: run_fatigue_failure_ftpf,
        FatigueFailureMethod.HASHIN_ROTEM: run_fatigue_failure_hashinrotem,
        FatigueFailureMethod.SIMS_BROGDON: run_fatigue_failure_simsbrogdon,
        FatigueFailureMethod.SHOKRIEH_TAHERI: run_fatigue_failure_shokriehtaheri,
        FatigueFailureMethod.FAWAZ_ELLYIN: run_fatigue_failure_fawazellyin,
        FatigueFailureMethod.KAWAI: run_fatigue_failure_kawai,
    }
    try:
        runner = runners[method]
    except KeyError as exc:
        raise ValueError(f"unknown method {method}") from exc
    return runner(
        material_file,
        target_angle,
        target_static_strength,
        target_stress_ratio,
        agg_file,
    )


def run_fatigue_failure_ftpf(
    material: SpooledTemporaryFile[bytes] | IO,
    target_angle: float,
    target_static_strength: float | None = None,
    target_stress_ratio: float | None = None,
    agg_file: SpooledTemporaryFile[bytes] | IO | None = None,
) -> AnalysisResult:
    _validate_target_angle(target_angle)
    material_data = FatigueFailureMaterial.from_file(material)

    source_stress_ratio = material_data.primary_stress_ratio()
    x_curve = material_data.curve(
        "tensile", angle=0, stress_ratio=source_stress_ratio
    )
    y_curve = material_data.curve(
        "tensile", angle=90, stress_ratio=source_stress_ratio
    )
    f_curve, off_axis_angle = material_data.shear_or_off_axis_curve(
        source_stress_ratio
    )
    xc_curve = material_data.curve(
        "compressive",
        angle=0,
        stress_ratio=source_stress_ratio,
        allow_ratio_fallback=True,
    )
    yc_curve = material_data.curve(
        "compressive",
        angle=90,
        stress_ratio=source_stress_ratio,
        allow_ratio_fallback=True,
    )
    sn_model = material_data.common_model(
        (x_curve, y_curve, f_curve, xc_curve, yc_curve)
    )

    with (
        NamedTemporaryFile() as output_csv_file,
        NamedTemporaryFile() as output_json_file,
    ):
        faf_ftpf.execute(
            x_curve.as_file(),
            y_curve.as_file(),
            f_curve.as_file(),
            output_csv_file,
            output_json_file,
            sn_model,
            target_angle,
            off_axis_angle,
            xc_curve.as_file(),
            yc_curve.as_file(),
        )
        output_csv_file.seek(0)
        output_json_file.seek(0)
        return AnalysisResult(
            csv_data=output_csv_file.read(),
            json_data=output_json_file.read(),
        )


def run_fatigue_failure_hashinrotem(
    material: SpooledTemporaryFile[bytes] | IO,
    target_angle: float,
    target_static_strength: float | None = None,
    target_stress_ratio: float | None = None,
    agg_file: SpooledTemporaryFile[bytes] | IO | None = None,
) -> AnalysisResult:
    _validate_target_angle(target_angle)
    target_static_strength = _require_target_static_strength(
        FatigueFailureMethod.HASHIN_ROTEM, target_static_strength
    )
    material_data = FatigueFailureMaterial.from_file(material)

    source_stress_ratio = material_data.primary_stress_ratio()
    x_curve = material_data.curve(
        "tensile", angle=0, stress_ratio=source_stress_ratio
    )
    (
        panel2_curve,
        panel3_curve,
        panel2_type,
        panel3_type,
        tensile_strength1,
        tensile_strength2,
    ) = material_data.transverse_and_shear_curves(source_stress_ratio)
    sn_model = material_data.common_model((x_curve, panel2_curve, panel3_curve))

    with (
        NamedTemporaryFile() as output_csv_file,
        NamedTemporaryFile() as output_json_file,
    ):
        faf_hashinrotem.execute(
            x_curve.as_file(),
            panel2_curve.as_file(),
            panel3_curve.as_file(),
            output_csv_file,
            output_json_file,
            sn_model,
            target_angle,
            panel2_curve.angle or 0,
            panel3_curve.angle or 0,
            material_data.strength("tensile", 90),
            material_data.strength("shear"),
            tensile_strength1,
            tensile_strength2,
            target_static_strength,
            material_data.strength("tensile", 0),
            HashinRotemPanelType(panel2_type),
            HashinRotemPanelType(panel3_type),
        )
        output_csv_file.seek(0)
        output_json_file.seek(0)
        return AnalysisResult(
            csv_data=output_csv_file.read(),
            json_data=output_json_file.read(),
        )


def run_fatigue_failure_simsbrogdon(
    material: SpooledTemporaryFile[bytes] | IO,
    target_angle: float,
    target_static_strength: float | None = None,
    target_stress_ratio: float | None = None,
    agg_file: SpooledTemporaryFile[bytes] | IO | None = None,
) -> AnalysisResult:
    _validate_target_angle(target_angle)
    material_data = FatigueFailureMaterial.from_file(material)

    source_stress_ratio = material_data.primary_stress_ratio()
    x_curve = material_data.curve(
        "tensile", angle=0, stress_ratio=source_stress_ratio
    )
    y_curve = material_data.curve(
        "tensile", angle=90, stress_ratio=source_stress_ratio
    )
    f_curve, off_axis_angle = material_data.shear_or_off_axis_curve(
        source_stress_ratio
    )
    sn_model = material_data.common_model((x_curve, y_curve, f_curve))
    return run_python_3(
        lambda x_input, y_input, f_input, csv, json: faf_simsbrogdon.execute(
            x_input,
            y_input,
            f_input,
            csv,
            json,
            sn_model,
            target_angle,
            off_axis_angle,
        ),
        x_curve.as_file(),
        y_curve.as_file(),
        f_curve.as_file(),
    )


def run_fatigue_failure_shokriehtaheri(
    material: SpooledTemporaryFile[bytes] | IO,
    target_angle: float,
    target_static_strength: float | None = None,
    target_stress_ratio: float | None = None,
    agg_file: SpooledTemporaryFile[bytes] | IO | None = None,
) -> AnalysisResult:
    _validate_target_angle(target_angle)
    target_stress_ratio = _require_target_stress_ratio(
        FatigueFailureMethod.SHOKRIEH_TAHERI, target_stress_ratio
    )
    agg_file = _require_agg_file(FatigueFailureMethod.SHOKRIEH_TAHERI, agg_file)
    material_data = FatigueFailureMaterial.from_file(material)

    curves = material_data.reference_curve_group(target_angle)
    reference_curve = next(
        (
            curve
            for curve in curves
            if math.isclose(
                curve.stress_ratio,
                target_stress_ratio,
                rel_tol=0,
                abs_tol=1e-9,
            )
        ),
        curves[0],
    )
    return run_python(
        lambda input, csv_output, json_output: faf_shokriehtaheri.execute(
            input,
            csv_output,
            json_output,
            float(reference_curve.angle),
            reference_curve.stress_ratio,
            target_angle,
            target_stress_ratio,
            material_data.strength("tensile", 0),
            material_data.strength("compressive", 0),
            material_data.strength("tensile", 90),
            material_data.strength("compressive", 90),
            material_data.strength("shear"),
        ),
        agg_file,
    )


def run_fatigue_failure_fawazellyin(
    material: SpooledTemporaryFile[bytes] | IO,
    target_angle: float,
    target_static_strength: float | None = None,
    target_stress_ratio: float | None = None,
    agg_file: SpooledTemporaryFile[bytes] | IO | None = None,
) -> AnalysisResult:
    _validate_target_angle(target_angle)
    target_static_strength = _require_target_static_strength(
        FatigueFailureMethod.FAWAZ_ELLYIN, target_static_strength
    )
    target_stress_ratio = _require_target_stress_ratio(
        FatigueFailureMethod.FAWAZ_ELLYIN, target_stress_ratio
    )
    material_data = FatigueFailureMaterial.from_file(material)
    curve = material_data.reference_curve(target_angle, target_stress_ratio)

    return run_python(
        lambda input, csv_output, json_output: faf_fawazellyin.execute(
            input,
            csv_output,
            json_output,
            curve.model,
            float(curve.angle),
            material_data.strength(curve.load_type, curve.angle),
            target_angle,
            target_stress_ratio,
            target_static_strength,
        ),
        curve.as_file(),
    )


def run_fatigue_failure_kawai(
    material: SpooledTemporaryFile[bytes] | IO,
    target_angle: float,
    target_static_strength: float | None = None,
    target_stress_ratio: float | None = None,
    agg_file: SpooledTemporaryFile[bytes] | IO | None = None,
) -> AnalysisResult:
    _validate_target_angle(target_angle)
    target_stress_ratio = _require_target_stress_ratio(
        FatigueFailureMethod.KAWAI, target_stress_ratio
    )
    agg_file = _require_agg_file(FatigueFailureMethod.KAWAI, agg_file)
    material_data = FatigueFailureMaterial.from_file(material)

    curves = material_data.reference_curve_group(target_angle)
    reference_curve = next(
        (
            curve
            for curve in curves
            if math.isclose(
                curve.stress_ratio,
                target_stress_ratio,
                rel_tol=0,
                abs_tol=1e-9,
            )
        ),
        curves[0],
    )
    reference_angle = float(reference_curve.angle)
    return run_python(
        lambda input, csv_output, json_output: faf_kawai.execute(
            input,
            csv_output,
            json_output,
            reference_angle,
            reference_curve.stress_ratio,
            material_data.strength("tensile", reference_angle),
            target_angle,
            target_stress_ratio,
            material_data.strength("tensile", 0),
            material_data.strength("tensile", 90),
            material_data.strength("shear"),
        ),
        agg_file,
    )


def run_damage_summation(
    load_history_file: SpooledTemporaryFile[bytes] | IO,
    fatigue_data_file: SpooledTemporaryFile[bytes] | IO,
    fatigue_data_type: DamageSummationInputType,
    cycle_counting_method: CycleCountingMethod,
    cld_method: CldMethod,
    ucs: float,
    uts: float,
    sn_curve_method: SnCurveMethod | None = None,
    confidence_interval: float | None = None,
    load_type: SnCurveLoadType = SnCurveLoadType.TENSILE,
    angle: float | None = 0,
    factor_min: float = 0.4,
    factor_max: float = 1.4,
    factor_count: int = 11,
) -> AnalysisResult:
    """Run the paper's uniaxial damage-summation workflow end to end.

    Existing module entry points produce the CYC, SNC, and CLD intermediate
    representations.  Only the final CLD inversion and Miner summation live in
    :mod:`ccfatigue.analysis.damage_summation`.
    """

    cycle_csv = run_cycle_counting(load_history_file, cycle_counting_method)

    if fatigue_data_type == DamageSummationInputType.AGG:
        if sn_curve_method is None:
            raise ValueError("An S-N curve method is required for AGG input.")
        snc_result = run_sn_curve(
            fatigue_data_file,
            sn_curve_method,
            confidence_interval,
            load_type,
            angle,
        )
        snc_input: IO[bytes] = io.BytesIO(snc_result.csv_data)
        sn_method_label = sn_curve_method.value
    elif fatigue_data_type == DamageSummationInputType.SNC:
        fatigue_data_file.seek(0)
        snc_input = io.BytesIO(fatigue_data_file.read())
        sn_method_label = "supplied SNC"
    else:
        raise ValueError(f"Unknown fatigue data type: {fatigue_data_type}")

    cld_result = run_cld(snc_input, cld_method, ucs, uts)
    return run_python_2(
        lambda cyc_input, cld_input, csv_output, json_output: damage_summation.execute(
            cyc_input,
            cld_input,
            csv_output,
            json_output,
            factor_min,
            factor_max,
            factor_count,
            cycle_counting_method.value,
            sn_method_label,
            cld_method.value,
        ),
        io.BytesIO(cycle_csv),
        io.BytesIO(cld_result.csv_data),
    )

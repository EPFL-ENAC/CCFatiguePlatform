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
import ccfatigue.analysis.das_harris as das_harris
import ccfatigue.analysis.das_piecewiselinear as das_piecewiselinear
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
from ccfatigue.analysis.utils.faf import FatigueModel
from ccfatigue.fatigue_failure_input import FatigueFailureMaterial
from ccfatigue.model import (
    AnalysisResult,
    CldMethod,
    CycleCountingMethod,
    DamageSummationMethod,
    FatigueFailureMethod,
    HashinRotemPanelType,
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
    return output


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
                file,
            )
        case CldMethod.SIMPLIFIED_HARRIS:
            output = run_python(
                lambda input, csv_output, json_output: cld_simplified_harris.execute(
                    input, csv_output, json_output, ucs, uts
                ),
                file,
            )
        case CldMethod.PIECEWISELINEAR:
            output = run_python(
                lambda input, csv_output, _: cld_piecewiselinear.execute(
                    input, csv_output, ucs, uts
                ),
                file,
            )
        case CldMethod.PIECEWISENONLINEAR:
            output = run_python(
                lambda input, csv_output, _: cld_piecewisenonlinear.execute(
                    input, csv_output, ucs, uts
                ),
                file,
            )
        case CldMethod.KAWAI:
            output = run_python(
                lambda input, csv_output, _: cld_kawai.execute(
                    input, csv_output, ucs, uts
                ),
                file,
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
                file,
            )
        case _:
            raise Exception(f"unknown method {method}")
    return output

"""

"""
def run_fatigue_failure(
    material_file: SpooledTemporaryFile[bytes] | IO,
    method: FatigueFailureMethod,
    target_angle: float,
    target_stress_ratio: float | None = None,
    target_static_strength: float | None = None,
    agg_file: SpooledTemporaryFile[bytes] | IO | None = None,
) -> AnalysisResult:
    """Run a fatigue-failure method from material JSON and optional AGG data."""

    # check1 : checks that all the needed files are present
    if not math.isfinite(target_angle):
        raise ValueError("Target angle must be finite.")

    methods_needing_target_ratio = {
        FatigueFailureMethod.SHOKRIEH_TAHERI,
        FatigueFailureMethod.FAWAZ_ELLYIN,
        FatigueFailureMethod.KAWAI,
    }
    if method in methods_needing_target_ratio and (
        target_stress_ratio is None or not math.isfinite(target_stress_ratio)
    ):
        raise ValueError(f"{method.value} requires a finite target stress ratio.")

    methods_needing_target_strength = {
        FatigueFailureMethod.HASHIN_ROTEM,
        FatigueFailureMethod.FAWAZ_ELLYIN,
    }
    if method in methods_needing_target_strength and (
        target_static_strength is None
        or not math.isfinite(target_static_strength)
        or target_static_strength <= 0
    ):
        raise ValueError(
            f"{method.value} requires a target static strength greater than zero."
        )

    methods_needing_agg_file = {
        FatigueFailureMethod.SHOKRIEH_TAHERI,
        FatigueFailureMethod.KAWAI,
    }

    if method in methods_needing_agg_file and agg_file is None:
        raise ValueError(f"{method.value} requires an AGG CSV file.")
    # end check 1

    #imports the material
    material = FatigueFailureMaterial.from_file(material_file)

    match method:
        case FatigueFailureMethod.FTPF:
            source_stress_ratio = material.primary_stress_ratio()
            x_curve = material.curve(
                "tensile", angle=0, stress_ratio=source_stress_ratio
            )
            y_curve = material.curve(
                "tensile", angle=90, stress_ratio=source_stress_ratio
            )
            f_curve, off_axis_angle = material.shear_or_off_axis_curve(
                source_stress_ratio
            )
            xc_curve = material.curve(
                "compressive",
                angle=0,
                stress_ratio=source_stress_ratio,
                allow_ratio_fallback=True,
            )
            yc_curve = material.curve(
                "compressive",
                angle=90,
                stress_ratio=source_stress_ratio,
                allow_ratio_fallback=True,
            )
            sn_model = material.common_model(
                (x_curve, y_curve, f_curve, xc_curve, yc_curve)
            )
            return run_fatigue_failure_ftpf(
                x_curve.as_file(),
                y_curve.as_file(),
                f_curve.as_file(),
                sn_model,
                target_angle,
                off_axis_angle,
                xc_curve.as_file(),
                yc_curve.as_file(),
            )

        case FatigueFailureMethod.HASHIN_ROTEM:
            source_stress_ratio = material.primary_stress_ratio()
            x_curve = material.curve(
                "tensile", angle=0, stress_ratio=source_stress_ratio
            )
            (
                panel2_curve,
                panel3_curve,
                panel2_type,
                panel3_type,
                tensile_strength1,
                tensile_strength2,
            ) = material.transverse_and_shear_curves(source_stress_ratio)
            sn_model = material.common_model(
                (x_curve, panel2_curve, panel3_curve)
            )
            return run_fatigue_failure_hashinrotem(
                x_curve.as_file(),
                panel2_curve.as_file(),
                panel3_curve.as_file(),
                sn_model,
                target_angle,
                panel2_curve.angle or 0,
                panel3_curve.angle or 0,
                material.strength("tensile", 90),
                material.strength("shear"),
                tensile_strength1,
                tensile_strength2,
                float(target_static_strength),
                material.strength("tensile", 0),
                HashinRotemPanelType(panel2_type),
                HashinRotemPanelType(panel3_type),
            )

        case FatigueFailureMethod.SIMS_BROGDON:
            source_stress_ratio = material.primary_stress_ratio()
            x_curve = material.curve(
                "tensile", angle=0, stress_ratio=source_stress_ratio
            )
            y_curve = material.curve(
                "tensile", angle=90, stress_ratio=source_stress_ratio
            )
            f_curve, off_axis_angle = material.shear_or_off_axis_curve(
                source_stress_ratio
            )
            sn_model = material.common_model((x_curve, y_curve, f_curve))
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

        case FatigueFailureMethod.SHOKRIEH_TAHERI:
            curves = material.reference_curve_group(target_angle)
            reference_curve = next(
                (
                    curve
                    for curve in curves
                    if abs(curve.stress_ratio - float(target_stress_ratio)) < 1e-9
                ),
                curves[0],
            )
            return run_fatigue_failure_shokriehtaheri(
                agg_file,
                float(reference_curve.angle),
                reference_curve.stress_ratio,
                target_angle,
                float(target_stress_ratio),
                material.strength("tensile", 0),
                material.strength("compressive", 0),
                material.strength("tensile", 90),
                material.strength("compressive", 90),
                material.strength("shear"),
            )

        case FatigueFailureMethod.FAWAZ_ELLYIN:
            curve = material.reference_curve(
                target_angle, float(target_stress_ratio)
            )
            return run_fatigue_failure_fawazellyin(
                curve.as_file(),
                curve.model,
                float(curve.angle),
                material.strength(curve.load_type, curve.angle),
                target_angle,
                float(target_stress_ratio),
                float(target_static_strength),
            )

        case FatigueFailureMethod.KAWAI:
            curves = material.reference_curve_group(target_angle)
            reference_curve = next(
                (
                    curve
                    for curve in curves
                    if abs(curve.stress_ratio - float(target_stress_ratio)) < 1e-9
                ),
                curves[0],
            )
            reference_angle = float(reference_curve.angle)
            return run_fatigue_failure_kawai(
                agg_file,
                reference_angle,
                reference_curve.stress_ratio,
                material.strength("tensile", reference_angle),
                target_angle,
                float(target_stress_ratio),
                material.strength("tensile", 0),
                material.strength("tensile", 90),
                material.strength("shear"),
            )

        case _:
            raise Exception(f"unknown method {method}")


def run_fatigue_failure_ftpf(
    x_file: SpooledTemporaryFile[bytes] | IO,
    y_file: SpooledTemporaryFile[bytes] | IO,
    f_file: SpooledTemporaryFile[bytes] | IO,
    snModel: FatigueModel,
    desirable_angle: float,
    off_axis_angle: float,
    xc_file: SpooledTemporaryFile[bytes] | IO | None = None,
    yc_file: SpooledTemporaryFile[bytes] | IO | None = None,
) -> AnalysisResult:
    with (
        NamedTemporaryFile() as output_csv_file,
        NamedTemporaryFile() as output_json_file,
    ):
        x_file.seek(0)
        y_file.seek(0)
        f_file.seek(0)
        if xc_file is not None:
            xc_file.seek(0)
        if yc_file is not None:
            yc_file.seek(0)
        faf_ftpf.execute(
            x_file,
            y_file,
            f_file,
            output_csv_file,
            output_json_file,
            snModel,
            desirable_angle,
            off_axis_angle,
            xc_file,
            yc_file,
        )
        output_csv_file.seek(0)
        output_json_file.seek(0)
        return AnalysisResult(
            csv_data=output_csv_file.read(),
            json_data=output_json_file.read(),
        )


def run_fatigue_failure_hashinrotem(
    x_file: SpooledTemporaryFile[bytes] | IO,
    panel2_file: SpooledTemporaryFile[bytes] | IO,
    panel3_file: SpooledTemporaryFile[bytes] | IO,
    snModel: FatigueModel,
    desirable_angle: float,
    off_axis_angle1: float,
    off_axis_angle2: float,
    tensile_transverse_strength: float,
    shear_strength: float,
    tensile_strength1: float,
    tensile_strength2: float,
    tensile_strength_at_desirable_angle: float,
    tensile_axial_strength: float,
    panel2_type: HashinRotemPanelType,
    panel3_type: HashinRotemPanelType,
) -> AnalysisResult:
    with (
        NamedTemporaryFile() as output_csv_file,
        NamedTemporaryFile() as output_json_file,
    ):
        x_file.seek(0)
        panel2_file.seek(0)
        panel3_file.seek(0)
        faf_hashinrotem.execute(
            x_file,
            panel2_file,
            panel3_file,
            output_csv_file,
            output_json_file,
            snModel,
            desirable_angle,
            off_axis_angle1,
            off_axis_angle2,
            tensile_transverse_strength,
            shear_strength,
            tensile_strength1,
            tensile_strength2,
            tensile_strength_at_desirable_angle,
            tensile_axial_strength,
            panel2_type,
            panel3_type,
        )
        output_csv_file.seek(0)
        output_json_file.seek(0)
        return AnalysisResult(
            csv_data=output_csv_file.read(),
            json_data=output_json_file.read(),
        )


def run_fatigue_failure_shokriehtaheri(
    agg_file: SpooledTemporaryFile[bytes] | IO,
    reference_angle: float,
    reference_stress_ratio: float,
    desirable_angle: float,
    target_stress_ratio: float,
    tensile_axial_strength: float,
    compressive_axial_strength: float,
    tensile_transverse_strength: float,
    compressive_transverse_strength: float,
    shear_strength: float,
) -> AnalysisResult:
    return run_python(
        lambda input, csv_output, json_output: faf_shokriehtaheri.execute(
            input,
            csv_output,
            json_output,
            reference_angle,
            reference_stress_ratio,
            desirable_angle,
            target_stress_ratio,
            tensile_axial_strength,
            compressive_axial_strength,
            tensile_transverse_strength,
            compressive_transverse_strength,
            shear_strength,
        ),
        agg_file,
    )


def run_fatigue_failure_fawazellyin(
    snc_file: SpooledTemporaryFile[bytes] | IO,
    sn_model: FatigueModel,
    reference_angle: float,
    reference_static_strength: float,
    desirable_angle: float,
    target_stress_ratio: float,
    target_static_strength: float,
) -> AnalysisResult:
    return run_python(
        lambda input, csv_output, json_output: faf_fawazellyin.execute(
            input,
            csv_output,
            json_output,
            sn_model,
            reference_angle,
            reference_static_strength,
            desirable_angle,
            target_stress_ratio,
            target_static_strength,
        ),
        snc_file,
    )


def run_fatigue_failure_kawai(
    agg_file: SpooledTemporaryFile[bytes] | IO,
    reference_angle: float,
    reference_stress_ratio: float,
    reference_static_strength: float,
    desirable_angle: float,
    target_stress_ratio: float,
    tensile_axial_strength: float,
    tensile_transverse_strength: float,
    shear_strength: float,
) -> AnalysisResult:
    return run_python(
        lambda input, csv_output, json_output: faf_kawai.execute(
            input,
            csv_output,
            json_output,
            reference_angle,
            reference_stress_ratio,
            reference_static_strength,
            desirable_angle,
            target_stress_ratio,
            tensile_axial_strength,
            tensile_transverse_strength,
            shear_strength,
        ),
        agg_file,
    )


def run_damage_summation(
    snc_file: SpooledTemporaryFile[bytes] | IO,
    cyc_file: SpooledTemporaryFile[bytes] | IO,
    method: DamageSummationMethod,
) -> bytes:
    match method:
        case DamageSummationMethod.HARRIS:
            output = run_python_2(
                lambda snc_input, cyc_input, csv_output, _: das_harris.execute(
                    snc_input,
                    cyc_input,
                    csv_output,
                ),
                snc_file,
                cyc_file,
            )
        case DamageSummationMethod.PIECEWISELINEAR:
            output = run_python_2(
                lambda snc_input, cyc_input, csv_output, _: das_piecewiselinear.execute(
                    snc_input,
                    cyc_input,
                    csv_output,
                ),
                snc_file,
                cyc_file,
            )
        case _:
            raise Exception(f"unknown method {method}")
    return output.csv_data

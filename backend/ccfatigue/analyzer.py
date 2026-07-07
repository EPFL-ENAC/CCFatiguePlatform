import io
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
import ccfatigue.analysis.faf_ftpf as faf_ftpf
import ccfatigue.analysis.snc_linlog as snc_linlog
import ccfatigue.analysis.snc_loglog as snc_loglog
import ccfatigue.analysis.snc_sendeckyj as snc_sendeckyj
import ccfatigue.analysis.snc_whitney as snc_whitney
from ccfatigue.analysis.utils.faf import FatigueModel
from ccfatigue.model import (
    AnalysisResult,
    CldMethod,
    CycleCountingMethod,
    DamageSummationMethod,
    FatigueFailureMethod,
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


def run_fatigue_failure(
    x_file: SpooledTemporaryFile[bytes] | IO,
    y_file: SpooledTemporaryFile[bytes] | IO,
    f_file: SpooledTemporaryFile[bytes] | IO,
    method: FatigueFailureMethod,
    snModel: FatigueModel,
    desirable_angle: float,
    off_axis_angle: float,
) -> AnalysisResult:
    match method:
        case FatigueFailureMethod.FTPT:
            output = run_python_3(
                lambda x_input, y_input, f_input, csv, json: faf_ftpf.execute(
                    x_input,
                    y_input,
                    f_input,
                    csv,
                    json,
                    snModel,
                    desirable_angle,
                    off_axis_angle,
                ),
                x_file,
                y_file,
                f_file,
            )
        case _:
            raise Exception(f"unknown method {method}")
    return output


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

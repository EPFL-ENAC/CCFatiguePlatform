import io
import os
import subprocess
from tempfile import NamedTemporaryFile, SpooledTemporaryFile
from typing import IO, Callable

import pandas as pd
from pandas._typing import ReadCsvBuffer, WriteBuffer
from pandas.core.frame import DataFrame

import ccfatigue.analysis.cld_harris as cld_harris
import ccfatigue.analysis.cyc_rangemean as cyc_rangemean
import ccfatigue.analysis.das_harris as das_harris
import ccfatigue.analysis.faf_ftpf as faf_ftpf
import ccfatigue.analysis.snc_linlog as snc_linlog
import ccfatigue.analysis.snc_loglog as snc_loglog
import ccfatigue.analysis.snc_sendeckyj as snc_sendeckyj
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
    execute: Callable[[ReadCsvBuffer, ReadCsvBuffer, WriteBuffer], None],
    input_file_1: SpooledTemporaryFile[bytes] | IO,
    input_file_2: SpooledTemporaryFile[bytes] | IO,
) -> bytes:
    with NamedTemporaryFile() as output_tmp_file:
        print(f"executing -> {output_tmp_file.name}")
        input_file_1.seek(0)
        input_file_2.seek(0)
        execute(input_file_1, input_file_2, output_tmp_file)
        output_tmp_file.seek(0)
        return output_tmp_file.read()


def run_python_3(
    execute: Callable[[ReadCsvBuffer, ReadCsvBuffer, ReadCsvBuffer, WriteBuffer], None],
    input_file_1: SpooledTemporaryFile[bytes] | IO,
    input_file_2: SpooledTemporaryFile[bytes] | IO,
    input_file_3: SpooledTemporaryFile[bytes] | IO,
) -> bytes:
    with NamedTemporaryFile() as output_tmp_file:
        print(f"executing -> {output_tmp_file.name}")
        input_file_1.seek(0)
        input_file_2.seek(0)
        input_file_3.seek(0)
        execute(input_file_1, input_file_2, input_file_3, output_tmp_file)
        output_tmp_file.seek(0)
        return output_tmp_file.read()


def create_dataframe(output: bytes) -> DataFrame:
    df: DataFrame = pd.read_csv(io.BytesIO(output))
    return df.fillna("")


def run_sn_curve(
    file: SpooledTemporaryFile[bytes] | IO,
    method: SnCurveMethod,
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
                    input, json_output, csv_output
                ),
                file,
            )
        case _:
            raise Exception(f"unknown method {method}")
    return output


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
        case _:
            raise Exception(f"unknown method {method}")
    return output.csv_data


def run_cld(
    file: SpooledTemporaryFile[bytes] | IO,
    method: CldMethod,
) -> bytes:
    match method:
        case CldMethod.HARRIS:
            output = run_python(
                lambda input, csv_output, _: cld_harris.execute(input, csv_output),
                file,
            )
        case _:
            raise Exception(f"unknown method {method}")
    return output.csv_data


def run_fatigue_failure(
    x_file: SpooledTemporaryFile[bytes] | IO,
    y_file: SpooledTemporaryFile[bytes] | IO,
    f_file: SpooledTemporaryFile[bytes] | IO,
    method: FatigueFailureMethod,
    snModel: FatigueModel,
    desirable_angle: float,
    off_axis_angle: float,
) -> bytes:
    match method:
        case FatigueFailureMethod.FTPT:
            output = run_python_3(
                lambda x_input, y_input, f_input, csv_output: faf_ftpf.execute(
                    x_input,
                    y_input,
                    f_input,
                    csv_output,
                    None,
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
                lambda snc_input, cyc_input, csv_output: das_harris.execute(
                    snc_input,
                    cyc_input,
                    csv_output,
                ),
                snc_file,
                cyc_file,
            )
        case _:
            raise Exception(f"unknown method {method}")
    return output

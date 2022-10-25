import io
import os
import subprocess
from tempfile import NamedTemporaryFile, SpooledTemporaryFile
from typing import IO, Callable, Dict, List

import pandas as pd
from deprecation import deprecated
from pandas._typing import ReadCsvBuffer, WriteBuffer
from pandas.core.frame import DataFrame

import ccfatigue.modules.cyc_range_mean as cyc_range_mean
import ccfatigue.modules.sn_curve_linlog as sn_curve_linlog
import ccfatigue.modules.sn_curve_loglog as sn_curve_loglog
import ccfatigue.modules.sn_curve_sendeckyj as sn_curve_sendeckyj
from ccfatigue.model import (
    CycleCountingMethod,
    EchartLine,
    SnCurveMethod,
    SnCurveResult,
)
from ccfatigue.plotter import DataKey, Line

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
    execute: Callable[[ReadCsvBuffer, WriteBuffer], None],
    input_file: SpooledTemporaryFile[bytes] | IO,
) -> bytes:
    with NamedTemporaryFile() as output_tmp_file:
        print(f"executing -> {output_tmp_file.name}")
        input_file.seek(0)
        execute(input_file, output_tmp_file)
        output_tmp_file.seek(0)
        return output_tmp_file.read()


def create_dataframe(output: bytes) -> DataFrame:
    df: DataFrame = pd.read_csv(io.BytesIO(output))
    return df.fillna("")


@deprecated()
def create_line(output: bytes, method: SnCurveMethod, r_ratio: float) -> Line:
    df = create_dataframe(output)
    round_df = df.round({DataKey.R_RATIO.key: ROUND_DECIMAL})
    selected_df = round_df[round_df[DataKey.R_RATIO.key] == r_ratio]
    n_cycles = selected_df[DataKey.N_CYCLES.key].to_list()
    stress_param = selected_df[DataKey.STRESS.key].to_list()
    return Line(
        data={
            DataKey.N_CYCLES: n_cycles,
            DataKey.STRESS: stress_param,
        },
        legend_label=f"{method.value} {r_ratio}",
        color=None,
    )


def get_echarts_lines(
    output: bytes, method: SnCurveMethod, r_ratios: List[float]
) -> List[EchartLine]:
    df = create_dataframe(output)
    round_df = df.round({DataKey.R_RATIO.key: ROUND_DECIMAL})
    lines: List[EchartLine] = []
    for r_ratio in r_ratios:
        selected_df = round_df[round_df[DataKey.R_RATIO.key] == r_ratio]
        n_cycles = selected_df[DataKey.N_CYCLES.key].to_list()
        stress_param = selected_df[DataKey.STRESS.key].to_list()
        lines.append(
            EchartLine(
                xData=n_cycles, yData=stress_param, name=f"{method.value} {r_ratio}"
            )
        )
    return lines


def run_sn_curve(
    file: SpooledTemporaryFile[bytes] | IO,
    methods: List[SnCurveMethod],
    r_ratios: List[float],
) -> SnCurveResult:
    outputs: Dict[SnCurveMethod, bytes] = {}
    lines: List[EchartLine] = []
    for method in methods:
        match method:
            case SnCurveMethod.LIN_LOG:
                output = run_python(
                    lambda input, csv_output: sn_curve_linlog.execute(
                        input, None, csv_output
                    ),
                    file,
                )
            case SnCurveMethod.LOG_LOG:
                output = run_python(
                    lambda input, csv_output: sn_curve_loglog.execute(
                        input, None, csv_output
                    ),
                    file,
                )
            case SnCurveMethod.SENDECKYJ:
                output = run_python(
                    lambda input, csv_output: sn_curve_sendeckyj.execute(
                        input, None, csv_output
                    ),
                    file,
                )
            case _:
                raise Exception(f"unknown method {method}")
        outputs[method] = output
        lines.extend(get_echarts_lines(output, method, r_ratios))
    return SnCurveResult(outputs=outputs, lines=lines)


def run_cycle_counting(
    file: SpooledTemporaryFile[bytes] | IO,
    method: CycleCountingMethod,
) -> bytes:
    match method:
        case CycleCountingMethod.RANGE_MEAN:
            output = run_python(
                lambda input, csv_output: cyc_range_mean.execute(input, csv_output),
                file,
            )
        case _:
            raise Exception(f"unknown method {method}")
    return output

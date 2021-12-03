import io
import os
import subprocess
from tempfile import NamedTemporaryFile, SpooledTemporaryFile
from typing import Any, Dict, List

import pandas as pd
from pandas.core.frame import DataFrame

from ccfatigue import plotter
from ccfatigue.model import SnCurveMethod, SnCurveResult
from ccfatigue.plotter import DataKey, Line, Plot

ROUND_DECIMAL = 8


def run_fortran(exec_path: str, input_file: SpooledTemporaryFile) -> bytes:
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


def create_dataframe(output: bytes, method: SnCurveMethod) -> DataFrame:
    if method in [SnCurveMethod.LIN_LOG, SnCurveMethod.LOG_LOG]:
        widths = [17, 12, 12, 12, 12]
        columns = [
            DataKey.R_RATIO,
            DataKey.N_CYCLES,
            DataKey.STRESS_PARAM,
            DataKey.LOW,
            DataKey.HIGH,
        ]
    elif method in [SnCurveMethod.SENDECKYJ, SnCurveMethod.WHITNEY]:
        widths = [17, 12, 12]
        columns = [DataKey.R_RATIO, DataKey.N_CYCLES, DataKey.STRESS_PARAM]
    else:
        raise ValueError(f"unknown {method.name}")
    df: DataFrame = pd.read_fwf(io.BytesIO(output), widths=widths, header=None)
    df.columns = [column.key for column in columns]
    return df.fillna("")


def create_line(output: bytes, method: SnCurveMethod, r_ratio: float) -> Any:
    df = create_dataframe(output, method)
    round_df = df.round({DataKey.R_RATIO.key: ROUND_DECIMAL})
    selected_df = round_df[round_df[DataKey.R_RATIO.key] == r_ratio]
    n_cycles = selected_df[DataKey.N_CYCLES.key].to_list()
    stress_param = selected_df[DataKey.STRESS_PARAM.key].to_list()
    return Line(
        data={
            DataKey.N_CYCLES: n_cycles,
            DataKey.STRESS_PARAM: stress_param,
        },
        legend_label=f"{method.value} {r_ratio}",
    )


def run_sn_curve(
    file: SpooledTemporaryFile,
    methods: List[SnCurveMethod],
    r_ratios: List[float],
) -> SnCurveResult:
    plot = Plot(
        title="S-N Curves",
        x_axis=DataKey.N_CYCLES,
        y_axis=DataKey.STRESS_PARAM,
        tooltips=[DataKey.N_CYCLES, DataKey.STRESS_PARAM],
        x_axis_type="log",
    )
    outputs: Dict[SnCurveMethod, bytes] = {}
    for method in methods:
        output = run_fortran(
            f"../CCFatigue_modules/2_S-NCurves/S-N-Curve-{method.value}", file
        )
        outputs[method] = output
        for r_ratio in r_ratios:
            plot.lines.append(create_line(output, method, r_ratio))
    return SnCurveResult(outputs=outputs, plot=plotter.export_plot(plot))

import io
import os
import subprocess
from tempfile import NamedTemporaryFile, SpooledTemporaryFile
from typing import IO, Any, Callable, Dict, List, Optional

import pandas as pd
from pandas.core.frame import DataFrame
from pandas._typing import ReadBuffer, WriteBuffer

from ccfatigue import plotter
from ccfatigue.model import SnCurveMethod, SnCurveResult
from ccfatigue.plotter import DataKey, Line, Plot
import ccfatigue.modules.sn_curve_loglog as sn_curve_loglog
import ccfatigue.modules.sn_curve_sendeckyj as sn_curve_sendeckyj

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
    execute: Callable[
        [Optional[ReadBuffer], Optional[WriteBuffer], Optional[WriteBuffer]], None
    ],
    input_file: SpooledTemporaryFile[bytes] | IO,
) -> bytes:
    with NamedTemporaryFile() as output_tmp_file:
        print(f"executing -> {output_tmp_file.name}")
        execute(input_file, None, output_tmp_file)
        output_tmp_file.seek(0)
        return output_tmp_file.read()


def create_dataframe(output: bytes) -> DataFrame:
    df: DataFrame = pd.read_csv(io.BytesIO(output))
    return df.fillna("")


def create_line(output: bytes, method: SnCurveMethod, r_ratio: float) -> Any:
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


def run_sn_curve(
    file: SpooledTemporaryFile[bytes] | IO,
    methods: List[SnCurveMethod],
    r_ratios: List[float],
) -> SnCurveResult:
    plot = Plot(
        title="S-N Curves",
        x_axis=DataKey.N_CYCLES,
        y_axis=DataKey.STRESS,
        tooltips=[DataKey.N_CYCLES, DataKey.STRESS],
        x_axis_type="log",
    )
    outputs: Dict[SnCurveMethod, bytes] = {}
    for method in methods:
        match method:
            case SnCurveMethod.LOG_LOG:
                output = run_python(sn_curve_loglog.execute, file)
            case SnCurveMethod.SENDECKYJ:
                output = run_python(sn_curve_sendeckyj.execute, file)
            case _:
                # FIXME
                output = run_python(sn_curve_loglog.execute, file)

        outputs[method] = output
        for r_ratio in r_ratios:
            plot.lines.append(create_line(output, method, r_ratio))
    return SnCurveResult(outputs=outputs, plot=plotter.export_plot(plot))

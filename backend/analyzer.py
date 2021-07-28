import io
import os
import subprocess
from tempfile import NamedTemporaryFile, SpooledTemporaryFile
from typing import Any

import pandas as pd
from pandas.core.frame import DataFrame

import dashboarder
from dashboarder import DataKey, Line, LinePlot
from model import SnCurveMethod, SnCurveResult


def run_fortran(exec_path: str, input_file: SpooledTemporaryFile) -> bytes:
    with NamedTemporaryFile() as tmp_file:
        tmp_file.write(input_file.read())
        tmp_file.flush()
        split_path = os.path.split(exec_path)
        directory = os.path.abspath(split_path[0])
        ouput = subprocess.check_output([
            f'./{split_path[1]}',
            tmp_file.name,
        ], cwd=directory)
        return ouput


def create_dataframe(method: SnCurveMethod, output: bytes) -> DataFrame:
    if method in [SnCurveMethod.LIN_LOG, SnCurveMethod.LOG_LOG]:
        widths = [17, 12, 12, 12, 12]
        columns = [DataKey.R_RATIO,
                   DataKey.N_CYCLES,
                   DataKey.STRESS_PARAM,
                   DataKey.LOW,
                   DataKey.HIGH]
    elif method in [SnCurveMethod.SENDECKYJ, SnCurveMethod.WHITNEY]:
        widths = [17, 12, 12]
        columns = [DataKey.R_RATIO, DataKey.N_CYCLES, DataKey.STRESS_PARAM]
    else:
        raise ValueError(f'unknown {method.name}')
    df: DataFrame = pd.read_fwf(io.BytesIO(output), widths=widths, header=None)
    df.columns = [column.key for column in columns]
    return df.fillna('')


def create_plot(method: SnCurveMethod, output: bytes, r_ratio: float) -> Any:
    df = create_dataframe(method, output)
    selected_df = df[df[DataKey.R_RATIO.key] == r_ratio]
    plot = LinePlot(
        title='S-N Curves',
        x_axis=DataKey.N_CYCLES,
        y_axis=DataKey.STRESS_PARAM,
        tooltips=[DataKey.N_CYCLES, DataKey.STRESS_PARAM],
        lines=[Line(
            data={
                DataKey.N_CYCLES: selected_df[DataKey.N_CYCLES.key]
                .to_list(),
                DataKey.STRESS_PARAM: selected_df[DataKey.STRESS_PARAM.key]
                .to_list()
            },
        )],
        x_axis_type='log',
    )
    return dashboarder.export_plot(plot)


def run_sn_curve(file: SpooledTemporaryFile,
                 method: SnCurveMethod,
                 r_ratio: float,
                 ) -> SnCurveResult:
    output = run_fortran(
        f'../CCFatigue_modules/2_S-NCurves/S-N-Curve-{method.value}',
        file)
    plot = create_plot(method, output, r_ratio)
    return SnCurveResult(
        output=output,
        plot=plot
    )

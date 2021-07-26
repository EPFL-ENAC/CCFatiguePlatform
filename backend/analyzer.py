import os
import subprocess
from tempfile import NamedTemporaryFile, SpooledTemporaryFile

from model import SnCurveResult


def run_fortran(exec_path: str, input_file: SpooledTemporaryFile) -> bytes:
    with NamedTemporaryFile() as tmp_file:
        tmp_file.write(input_file.read())
        tmp_file.flush()
        split_path = os.path.split(exec_path)
        directory = os.path.abspath(split_path[0])
        ouput = subprocess.check_output([
            f"./{split_path[1]}",
            tmp_file.name,
        ], cwd=directory)
        return ouput


def run_sn_curve(file: SpooledTemporaryFile) -> SnCurveResult:
    content = run_fortran(
        "../CCFatigue_modules/2_S-NCurves/LinLog/S-N-Curve-Lin-Log",
        file)
    return SnCurveResult(
        content=content
    )

import os

import ccfatigue.analysis.cld_harris as cld_harris
import ccfatigue.analysis.cld_piecewiselinear as cld_piecewiselinear  # noqa: E501
import ccfatigue.analysis.cyc_range_mean as cyc_range_mean
import ccfatigue.analysis.das_piecewiselinear as das_piecewiselinear  # noqa: E501
import ccfatigue.analysis.cyc_simplified_rainflow as cyc_simplified_rainflow
import ccfatigue.analysis.faf_ftpf as faf_ftpf
import ccfatigue.analysis.faf_hashin_rotem as faf_hashin_rotem
import ccfatigue.analysis.miner_harris as das_harris
import ccfatigue.analysis.sn_curve_linlog as sn_curve_linlog
import ccfatigue.analysis.sn_curve_loglog as sn_curve_loglog
import ccfatigue.analysis.sn_curve_sendeckyj as sn_curve_sendeckyj
import ccfatigue.analysis.utils.faf as faf

SRC_DIR = os.path.dirname(os.path.realpath(__file__))
DATA_DIR = os.path.join(SRC_DIR, "..", "..", "Data")
OUTPUT_DIR = os.path.join(DATA_DIR, "output")

# Input files
AGG_CSV_INPUT_FILE = os.path.join(DATA_DIR, "AGG_input.csv")
SNC_CSV_1_INPUT_FILE = os.path.join(DATA_DIR, "SNC_input.csv")
SNC_CSV_2_INPUT_FILE = os.path.join(DATA_DIR, "SNC_input2.csv")
SNC_JSON_3_INPUT_FILE = os.path.join(DATA_DIR, "SNC_input3.json")
SNC_JSON_4_INPUT_FILE = os.path.join(DATA_DIR, "SNC_input4.json")
SNC_JSON_A_INPUT_FILE = os.path.join(DATA_DIR, "SNC_inputA.json")
SNC_JSON_F_INPUT_FILE = os.path.join(DATA_DIR, "SNC_inputF.json")
SNC_JSON_X_INPUT_FILE = os.path.join(DATA_DIR, "SNC_inputX.json")
SNC_JSON_Y_INPUT_FILE = os.path.join(DATA_DIR, "SNC_inputY.json")
CYC_CSV_CCINPUT_1_INPUT_FILE = os.path.join(DATA_DIR, "CYC_ccinput.csv")
CYC_CSV_CCINPUT_2_INPUT_FILE = os.path.join(DATA_DIR, "CYC_ccinput2.csv")
LDS_CSV_1_INPUT_FILE = os.path.join(DATA_DIR, "LDS_input.csv")

# Mod 1 - Cycle counting - Range mean
CYC_RANGE_MEAN_CSV_OUTPUT_FILE = os.path.join(OUTPUT_DIR, "CYC_range_mean.csv")


# Mod 1 - Cycle counting - Simplified Rainflow
CYC_SIMPLIFIEDRAINFLOW_CSV_OUTPUT_FILE = os.path.join(
    OUTPUT_DIR, "CYC_simplifiedrainflow.csv"
)

# Mod 2 - S-N Curve - LogLog
SNC_LOGLOG_JSON_OUTPUT_FILE = os.path.join(OUTPUT_DIR, "SNC_LogLog.json")
SNC_LOGLOG_CSV_OUTPUT_FILE = os.path.join(OUTPUT_DIR, "SNC_LogLog.csv")

# Mod 2 - S-N Curve - LinLog
SNC_LINLOG_JSON_OUTPUT_FILE = os.path.join(OUTPUT_DIR, "SNC_LinLog.json")
SNC_LINLOG_CSV_OUTPUT_FILE = os.path.join(OUTPUT_DIR, "SNC_LinLog.csv")

# Mod 2 - S-N Curve - Sendeckyj
SNC_SENDECKYJ_JSON_OUTPUT_FILE = os.path.join(OUTPUT_DIR, "SNC_Sendeckyj.json")
SNC_SENDECKYJ_CSV_OUTPUT_FILE = os.path.join(OUTPUT_DIR, "SNC_Sendeckyj.csv")

# Mod 3 - Constant life diagram - Harris
CLD_HARRIS_OUTPUT_CLD_CSV_FILE = os.path.join(OUTPUT_DIR, "CLD_Harris.csv")
CLD_HARRIS_UCS = 27.1
CLD_HARRIS_UTS = 27.7

# Mod 3 - Constant life diagram - Piecewise Linear
CLD_PIECEWISELINEAR_OUTPUT_CSV_FILE = os.path.join(
    OUTPUT_DIR, "CLD_PiecewiseLinear.csv"
)
CLD_PIECEWISELINEAR_UCS = 27.1
CLD_PIECEWISELINEAR_UTS = 27.7

# Mod 4 - Fatigue failture - FTPF
FAF_FTPF_CSV_OUTPUT_FILE = os.path.join(OUTPUT_DIR, "FAF_ftpf.csv")
FAF_FTPF_JSON_OUTPUT_FILE = os.path.join(OUTPUT_DIR, "FAF_ftpf.json")
FAF_FTPF_DESIRABLE_ANGLE = 30
FAF_FTPF_OFF_AXIS_ANGLE = 160

# Mod 4 - Fatigue failture - Hashin Rotem
FAF_HASHINROTEM_CSV_OUTPUT_FILE = os.path.join(OUTPUT_DIR, "FAF_hashinrotem.csv")
FAF_HASHINROTEM_JSON_OUTPUT_FILE = os.path.join(OUTPUT_DIR, "FAF_hashinrotem.json")
FAF_HASHINROTEM_DESIRABLE_ANGLE = 75
FAF_HASHINROTEM_OFF_AXIS_ANGLE1 = 90
FAF_HASHINROTEM_OFF_AXIS_ANGLE2 = 45
# "tensile_axial_strength": 244.84
# "compressive_axial_strength": 216.68,
FAF_HASHINROTEM_TENSILE_TRANSVERSE_STRENGTH = 84.94
FAF_HASHINROTEM_COMPRESSIVE_TRANSVERSE_STRENGTH = 83.64
FAF_HASHINROTEM_SHEAR_STRENGTH = 61.38
FAF_HASHINROTEM_TENSILE_STRENGTH1 = 84.94
FAF_HASHINROTEM_COMPRESSIVE_STRENGTH1 = 83.64
FAF_HASHINROTEM_TENSILE_STRENGTH2 = 139.12
FAF_HASHINROTEM_COMPRESSIVE_STRENGTH2 = 106.4
FAF_HASHINROTEM_TENSILE_STRENGTH_AT_DESIRABLE_ANGLE = 89.47
FAF_HASHINROTEM_COMPRESSIVE_STRENGTH_AT_DESIRABLE_ANGLE = 145.52

# Mod 5 - Damage summation - Harris
DAS_HARRIS_OUTPUT_DAS_CSV_FILE = os.path.join(OUTPUT_DIR, "DAS_harris.csv")
DAS_HARRIS_UCS = 367.2
DAS_HARRIS_UTS = 416.5
DAS_HARRIS_FACTOR_START = 1.4
DAS_HARRIS_FACTOR_STOP = 0.4
DAS_HARRIS_FACTOR_STEP = -0.1

# Mod 5 - Damage summation - Piecewise Linear
DAS_PIECEWISELINEAR_OUTPUT_DAS_CSV_FILE = os.path.join(
    OUTPUT_DIR, "DAS_piecewiselinear.csv"
)
DAS_PIECEWISELINEAR_UCS = 367.2
DAS_PIECEWISELINEAR_UTS = 416.5
DAS_PIECEWISELINEAR_FACTOR_START = 1.4
DAS_PIECEWISELINEAR_FACTOR_STOP = 0.35
DAS_PIECEWISELINEAR_FACTOR_STEP = -0.1

os.makedirs(OUTPUT_DIR, exist_ok=True)


# Module 1 - Cycle counting


def test_cyc_range_mean():
    cyc_range_mean.execute(LDS_CSV_1_INPUT_FILE, CYC_RANGE_MEAN_CSV_OUTPUT_FILE)


def test_cyc_simplified_rainflow():
    cyc_simplified_rainflow.execute(
        LDS_CSV_1_INPUT_FILE, CYC_SIMPLIFIEDRAINFLOW_CSV_OUTPUT_FILE
    )


# Module 2 - S-N curve


def test_sn_curve_loglog():
    sn_curve_loglog.execute(
        AGG_CSV_INPUT_FILE, SNC_LOGLOG_JSON_OUTPUT_FILE, SNC_LOGLOG_CSV_OUTPUT_FILE
    )


def test_sn_curve_linlog():
    sn_curve_linlog.execute(
        AGG_CSV_INPUT_FILE, SNC_LINLOG_JSON_OUTPUT_FILE, SNC_LINLOG_CSV_OUTPUT_FILE
    )


def test_sn_curve_sendeckyj():
    sn_curve_sendeckyj.execute(
        AGG_CSV_INPUT_FILE,
        SNC_SENDECKYJ_JSON_OUTPUT_FILE,
        SNC_SENDECKYJ_CSV_OUTPUT_FILE,
    )


# Module 3 - Constant life diagram


def test_cld_harris() -> None:
    cld_harris.execute(
        SNC_CSV_1_INPUT_FILE,
        CLD_HARRIS_OUTPUT_CLD_CSV_FILE,
        CLD_HARRIS_UCS,
        CLD_HARRIS_UTS,
    )


def test_cld_piecewiselinear() -> None:
    cld_piecewiselinear.execute(
        SNC_CSV_1_INPUT_FILE,
        CLD_PIECEWISELINEAR_OUTPUT_CSV_FILE,
        CLD_PIECEWISELINEAR_UCS,
        CLD_PIECEWISELINEAR_UTS,
    )


# Module 4 - Fatigue failture


def test_faf_ftpf():
    faf_ftpf.execute(
        SNC_JSON_X_INPUT_FILE,
        SNC_JSON_Y_INPUT_FILE,
        SNC_JSON_F_INPUT_FILE,
        FAF_FTPF_CSV_OUTPUT_FILE,
        FAF_FTPF_JSON_OUTPUT_FILE,
        faf.FatigueModel.LOG_LOG,
        FAF_FTPF_DESIRABLE_ANGLE,
        FAF_FTPF_OFF_AXIS_ANGLE,
    )


def test_faf_hashin_rotem():
    faf_hashin_rotem.execute(
        SNC_JSON_3_INPUT_FILE,
        SNC_JSON_4_INPUT_FILE,
        SNC_JSON_A_INPUT_FILE,
        FAF_HASHINROTEM_CSV_OUTPUT_FILE,
        FAF_HASHINROTEM_JSON_OUTPUT_FILE,
        faf.FatigueModel.LIN_LOG,
        FAF_HASHINROTEM_DESIRABLE_ANGLE,
        FAF_HASHINROTEM_OFF_AXIS_ANGLE1,
        FAF_HASHINROTEM_OFF_AXIS_ANGLE2,
        FAF_HASHINROTEM_TENSILE_TRANSVERSE_STRENGTH,
        FAF_HASHINROTEM_COMPRESSIVE_TRANSVERSE_STRENGTH,
        FAF_HASHINROTEM_SHEAR_STRENGTH,
        FAF_HASHINROTEM_TENSILE_STRENGTH1,
        FAF_HASHINROTEM_COMPRESSIVE_STRENGTH1,
        FAF_HASHINROTEM_TENSILE_STRENGTH2,
        FAF_HASHINROTEM_COMPRESSIVE_STRENGTH2,
        FAF_HASHINROTEM_TENSILE_STRENGTH_AT_DESIRABLE_ANGLE,
        FAF_HASHINROTEM_COMPRESSIVE_STRENGTH_AT_DESIRABLE_ANGLE,
    )


# Module 5 - Damage summation


def test_das_harris() -> None:
    das_harris.execute(
        SNC_CSV_2_INPUT_FILE,
        CYC_CSV_CCINPUT_2_INPUT_FILE,
        DAS_HARRIS_OUTPUT_DAS_CSV_FILE,
        DAS_HARRIS_UCS,
        DAS_HARRIS_UTS,
        DAS_HARRIS_FACTOR_START,
        DAS_HARRIS_FACTOR_STOP,
        DAS_HARRIS_FACTOR_STEP,
    )


def test_das_piecewiselinear() -> None:
    das_piecewiselinear.execute(
        SNC_CSV_2_INPUT_FILE,
        CYC_CSV_CCINPUT_1_INPUT_FILE,
        DAS_PIECEWISELINEAR_OUTPUT_DAS_CSV_FILE,
        DAS_PIECEWISELINEAR_UCS,
        DAS_PIECEWISELINEAR_UTS,
        DAS_PIECEWISELINEAR_FACTOR_START,
        DAS_PIECEWISELINEAR_FACTOR_STOP,
        DAS_PIECEWISELINEAR_FACTOR_STEP,
    )

import os

import ccfatigue.modules.cld_harris as cld_harris
import ccfatigue.modules.cyc_range_mean as cyc_range_mean
import ccfatigue.modules.faf_ftpf as faf_ftpf
import ccfatigue.modules.miner_harris as miner_harris
import ccfatigue.modules.sn_curve_linlog as sn_curve_linlog
import ccfatigue.modules.sn_curve_loglog as sn_curve_loglog
import ccfatigue.modules.sn_curve_sendeckyj as sn_curve_sendeckyj

SRC_DIR = os.path.dirname(os.path.realpath(__file__))
DATA_DIR = os.path.join(SRC_DIR, "..", "..", "Data")
OUTPUT_DIR = os.path.join(DATA_DIR, "output")

# Input files
AGG_CSV_INPUT_FILE = os.path.join(DATA_DIR, "AGG_input.csv")
SNC_CSV_1_INPUT_FILE = os.path.join(DATA_DIR, "SNC_input.csv")
SNC_CSV_2_INPUT_FILE = os.path.join(DATA_DIR, "SNC_input2.csv")
SNC_JSON_X_INPUT_FILE = os.path.join(DATA_DIR, "SNC_inputX.json")
SNC_JSON_Y_INPUT_FILE = os.path.join(DATA_DIR, "SNC_inputY.json")
SNC_JSON_F_INPUT_FILE = os.path.join(DATA_DIR, "SNC_inputF.json")
CYC_CSV_CCINPUT_2_INPUT_FILE = os.path.join(DATA_DIR, "CYC_ccinput2.csv")
LDS_CSV_1_INPUT_FILE = os.path.join(DATA_DIR, "LDS_input.csv")

# Mod 1 - Cycle counting - Range mean
RANGE_MEAN_CYC_CSV_OUTPUT_FILE = os.path.join(OUTPUT_DIR, "CYC_range_mean.csv")

# Mod 2 - S-N Curve - LogLog
LOGLOG_SNC_JSON_OUTPUT_FILE = os.path.join(OUTPUT_DIR, "SNC_LogLog.json")
LOGLOG_SNC_CSV_OUTPUT_FILE = os.path.join(OUTPUT_DIR, "SNC_LogLog.csv")

# Mod 2 - S-N Curve - LinLog
LINLOG_SNC_JSON_OUTPUT_FILE = os.path.join(OUTPUT_DIR, "SNC_LinLog.json")
LINLOG_SNC_CSV_OUTPUT_FILE = os.path.join(OUTPUT_DIR, "SNC_LinLog.csv")

# Mod 2 - S-N Curve - Sendeckyj
SENDECKYJ_SNC_JSON_OUTPUT_FILE = os.path.join(OUTPUT_DIR, "SNC_Sendeckyj.json")
SENDECKYJ_SNC_CSV_OUTPUT_FILE = os.path.join(OUTPUT_DIR, "SNC_Sendeckyj.csv")

# Mod 3 - Constant life diagram - Harris
CLD_HARRIS_OUTPUT_CLD_CSV_FILE = os.path.join(OUTPUT_DIR, "CLD_Harris.csv")
CLD_HARRIS_UCS = 27.1
CLD_HARRIS_UTS = 27.7

# Mod 4 - Fatigue failture - FTPF
FAF_FTPF_CSV_OUTPUT_FILE = os.path.join(OUTPUT_DIR, "FAF_ftpf.csv")
FAF_FTPF_JSON_OUTPUT_FILE = os.path.join(OUTPUT_DIR, "FAF_ftpf.json")
FAF_FTPF_DESIRABLE_ANGLE = 30
FAF_FTPF_OFF_AXIS_ANGLE = 160

# Mod 5 - Damage summation - Harris
MINER_HARRIS_OUTPUT_DAS_CSV_FILE = os.path.join(OUTPUT_DIR, "DAS_harris.csv")
MINER_HARRIS_UCS = 367.2
MINER_HARRIS_UTS = 416.5
MINER_HARRIS_FACTOR_START = 1.4
MINER_HARRIS_FACTOR_STOP = 0.4
MINER_HARRIS_FACTOR_STEP = -0.1

os.makedirs(OUTPUT_DIR, exist_ok=True)


# Module 1 - Cycle counting


def test_cyc_range_mean():
    cyc_range_mean.execute(LDS_CSV_1_INPUT_FILE, RANGE_MEAN_CYC_CSV_OUTPUT_FILE)


# Module 2 - S-N curve


def test_sn_curve_loglog():
    sn_curve_loglog.execute(
        AGG_CSV_INPUT_FILE, LOGLOG_SNC_JSON_OUTPUT_FILE, LOGLOG_SNC_CSV_OUTPUT_FILE
    )


def test_sn_curve_linlog():
    sn_curve_linlog.execute(
        AGG_CSV_INPUT_FILE, LINLOG_SNC_JSON_OUTPUT_FILE, LINLOG_SNC_CSV_OUTPUT_FILE
    )


def test_sn_curve_sendeckyj():
    sn_curve_sendeckyj.execute(
        AGG_CSV_INPUT_FILE,
        SENDECKYJ_SNC_JSON_OUTPUT_FILE,
        SENDECKYJ_SNC_CSV_OUTPUT_FILE,
    )


# Module 3 - Constant life diagram


def test_cld_harris() -> None:
    cld_harris.execute(
        SNC_CSV_1_INPUT_FILE,
        CLD_HARRIS_OUTPUT_CLD_CSV_FILE,
        CLD_HARRIS_UCS,
        CLD_HARRIS_UTS,
    )


# Module 4 - Fatigue failture


def test_faf_ftpf():
    faf_ftpf.execute(
        SNC_JSON_X_INPUT_FILE,
        SNC_JSON_Y_INPUT_FILE,
        SNC_JSON_F_INPUT_FILE,
        FAF_FTPF_CSV_OUTPUT_FILE,
        FAF_FTPF_JSON_OUTPUT_FILE,
        faf_ftpf.SnModel.LOG_LOG,
        FAF_FTPF_DESIRABLE_ANGLE,
        FAF_FTPF_OFF_AXIS_ANGLE,
    )


# Module 5 - Damage summation


def test_miner_harris() -> None:
    miner_harris.execute(
        SNC_CSV_2_INPUT_FILE,
        CYC_CSV_CCINPUT_2_INPUT_FILE,
        MINER_HARRIS_OUTPUT_DAS_CSV_FILE,
        MINER_HARRIS_UCS,
        MINER_HARRIS_UTS,
        MINER_HARRIS_FACTOR_START,
        MINER_HARRIS_FACTOR_STOP,
        MINER_HARRIS_FACTOR_STEP,
    )

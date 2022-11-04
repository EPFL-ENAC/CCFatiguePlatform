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
os.makedirs(OUTPUT_DIR, exist_ok=True)


def test_cyc_range_mean():
    LDS_INPUT_CSV_FILENAME = "LDS_input.csv"
    CYC_OUTPUT_CSV_FILENAME = "CYC_range_mean.csv"
    LDS_INPUT_CSV_FILE = os.path.join(DATA_DIR, LDS_INPUT_CSV_FILENAME)
    CYC_OUTPUT_CSV_FILE = os.path.join(OUTPUT_DIR, CYC_OUTPUT_CSV_FILENAME)
    print(f"LDS_INPUT_CSV_FILE={os.path.abspath(LDS_INPUT_CSV_FILE)}")
    print(f"CYC_OUTPUT_CSV_FILE={os.path.abspath(CYC_OUTPUT_CSV_FILE)}")
    cyc_range_mean.execute(LDS_INPUT_CSV_FILE, CYC_OUTPUT_CSV_FILE)


# Module 1 - Cycle counting


def test_sn_curve_loglog():
    INPUT_FILENAME = "AGG_input.csv"
    OUTPUT_JSON_FILENAME = "SNC_LogLog.json"
    OUTPUT_CSV_FILENAME = "SNC_LogLog.csv"
    INPUT_FILE = os.path.join(DATA_DIR, INPUT_FILENAME)
    OUTPUT_JSON_FILE = os.path.join(OUTPUT_DIR, OUTPUT_JSON_FILENAME)
    OUTPUT_CSV_FILE = os.path.join(OUTPUT_DIR, OUTPUT_CSV_FILENAME)
    print(f"INPUT_FILE={os.path.abspath(INPUT_FILE)}")
    print(f"OUTPUT_JSON_FILE={os.path.abspath(OUTPUT_JSON_FILE)}")
    print(f"OUTPUT_CSV_FILE={os.path.abspath(OUTPUT_CSV_FILE)}")
    sn_curve_loglog.execute(INPUT_FILE, OUTPUT_JSON_FILE, OUTPUT_CSV_FILE)


# Module 2 - S-N curve


def test_sn_curve_linlog():
    INPUT_FILENAME = "AGG_input.csv"
    INPUT_FILE = os.path.join(DATA_DIR, INPUT_FILENAME)
    OUTPUT_JSON_FILENAME = "SNC_LinLog.json"
    OUTPUT_JSON_FILE = os.path.join(OUTPUT_DIR, OUTPUT_JSON_FILENAME)
    OUTPUT_CSV_FILENAME = "SNC_LinLog.csv"
    OUTPUT_CSV_FILE = os.path.join(OUTPUT_DIR, OUTPUT_CSV_FILENAME)
    print(f"INPUT_FILE={os.path.abspath(INPUT_FILE)}")
    print(f"OUTPUT_JSON_FILE={os.path.abspath(OUTPUT_JSON_FILE)}")
    print(f"OUTPUT_CSV_FILE={os.path.abspath(OUTPUT_CSV_FILE)}")
    sn_curve_linlog.execute(INPUT_FILE, OUTPUT_JSON_FILE, OUTPUT_CSV_FILE)


def test_sn_curve_sendeckyj():
    INPUT_FILENAME = "AGG_input.csv"
    INPUT_FILE = os.path.join(DATA_DIR, INPUT_FILENAME)
    OUTPUT_JSON_FILENAME = "SNC_Sendeckyj.json"
    OUTPUT_JSON_FILE = os.path.join(OUTPUT_DIR, OUTPUT_JSON_FILENAME)
    OUTPUT_CSV_FILENAME = "SNC_Sendeckyj.csv"
    OUTPUT_CSV_FILE = os.path.join(OUTPUT_DIR, OUTPUT_CSV_FILENAME)
    print(f"INPUT_FILE={os.path.abspath(INPUT_FILE)}")
    print(f"OUTPUT_JSON_FILE={os.path.abspath(OUTPUT_JSON_FILE)}")
    print(f"OUTPUT_CSV_FILE={os.path.abspath(OUTPUT_CSV_FILE)}")
    sn_curve_sendeckyj.execute(INPUT_FILE, OUTPUT_JSON_FILE, OUTPUT_CSV_FILE)


# Module 3 - Constant life diagram


def test_cld_harris() -> None:
    INPUT_FILENAME = "SNC_input.csv"
    OUTPUT_CSV_FILENAME = "CLD_Harris.csv"
    INPUT_FILE = os.path.join(DATA_DIR, INPUT_FILENAME)
    OUTPUT_CSV_FILE = os.path.join(OUTPUT_DIR, OUTPUT_CSV_FILENAME)
    print(f"INPUT_FILE={os.path.abspath(INPUT_FILE)}")
    print(f"OUTPUT_CSV_FILE={os.path.abspath(OUTPUT_CSV_FILE)}")
    cld_harris.execute(INPUT_FILE, OUTPUT_CSV_FILE, 27.1, 27.7)


# Module 4 - Fatigue failture


def test_faf_ftpf():
    SNC_INPUT_X_JSON_FILENAME = "SNC_inputX.json"
    SNC_INPUT_Y_JSON_FILENAME = "SNC_inputY.json"
    SNC_INPUT_F_JSON_FILENAME = "SNC_inputF.json"
    FAF_OUTPUT_CSV_FILENAME = "FAF_ftpf.csv"
    FAF_OUTPUT_JSON_FILENAME = "FAF_ftpf.json"
    SNC_INPUT_X_JSON_FILE = os.path.join(DATA_DIR, SNC_INPUT_X_JSON_FILENAME)
    SNC_INPUT_Y_JSON_FILE = os.path.join(DATA_DIR, SNC_INPUT_Y_JSON_FILENAME)
    SNC_INPUT_F_JSON_FILE = os.path.join(DATA_DIR, SNC_INPUT_F_JSON_FILENAME)
    FAF_OUTPUT_CSV_FILE = os.path.join(OUTPUT_DIR, FAF_OUTPUT_CSV_FILENAME)
    FAF_OUTPUT_JSON_FILE = os.path.join(OUTPUT_DIR, FAF_OUTPUT_JSON_FILENAME)
    faf_ftpf.execute(
        snc_input_x_json_file=SNC_INPUT_X_JSON_FILE,
        snc_input_y_json_file=SNC_INPUT_Y_JSON_FILE,
        snc_input_f_json_file=SNC_INPUT_F_JSON_FILE,
        faf_output_csv_file=FAF_OUTPUT_CSV_FILE,
        faf_output_json_file=FAF_OUTPUT_JSON_FILE,
        sn_model=faf_ftpf.SnModel.LOG_LOG,
        desirable_angle=30,
        off_axis_angle=160,
    )


# Module 5 - Damage summation


def test_miner_harris() -> None:
    INPUT_SNC_FILENAME = "SNC_input2.csv"
    INPUT_CYC_FILENAME = "CYC_ccinput2.csv"
    OUTPUT_CSV_FILENAME = "DAS_harris.csv"
    INPUT_SNC_FILE = os.path.join(DATA_DIR, INPUT_SNC_FILENAME)
    INPUT_CYC_FILE = os.path.join(DATA_DIR, INPUT_CYC_FILENAME)
    OUTPUT_CSV_FILE = os.path.join(OUTPUT_DIR, OUTPUT_CSV_FILENAME)
    print(f"INPUT_SNC_FILE={os.path.abspath(INPUT_SNC_FILE)}")
    print(f"INPUT_CYC_FILE={os.path.abspath(INPUT_CYC_FILE)}")
    print(f"OUTPUT_CSV_FILE={os.path.abspath(OUTPUT_CSV_FILE)}")
    miner_harris.execute(
        INPUT_SNC_FILE, INPUT_CYC_FILE, OUTPUT_CSV_FILE, 367.2, 416.5, 1.4, 0.4, -0.1
    )

import os

SRC_DIR = os.path.dirname(os.path.realpath(__file__))
DATA_DIR = os.path.join(SRC_DIR, "..", "..", "Data")
OUTPUT_DIR = os.path.join(DATA_DIR, "output")


def test_sn_curve_loglog():
    import ccfatigue.modules.sn_curve_loglog as sn_curve_loglog

    INPUT_FILENAME = "AGG_input.csv"
    OUTPUT_JSON_FILENAME = "SNC_LogLog.json"
    OUTPUT_CSV_FILENAME = "SNC_LogLog.csv"
    INPUT_FILE = os.path.join(DATA_DIR, INPUT_FILENAME)
    OUTPUT_JSON_FILE = os.path.join(OUTPUT_DIR, OUTPUT_JSON_FILENAME)
    OUTPUT_CSV_FILE = os.path.join(OUTPUT_DIR, OUTPUT_CSV_FILENAME)
    print(f"{INPUT_FILE=}")
    print(f"{OUTPUT_JSON_FILE=}")
    print(f"{OUTPUT_CSV_FILE=}")
    sn_curve_loglog.execute(INPUT_FILE, OUTPUT_JSON_FILE, OUTPUT_CSV_FILE)


def test_sn_curve_linlog():
    import ccfatigue.modules.sn_curve_linlog as sn_curve_linlog

    INPUT_FILENAME = "AGG_input.csv"
    INPUT_FILE = os.path.join(DATA_DIR, INPUT_FILENAME)
    OUTPUT_JSON_FILENAME = "SNC_LinLog.json"
    OUTPUT_JSON_FILE = os.path.join(OUTPUT_DIR, OUTPUT_JSON_FILENAME)
    OUTPUT_CSV_FILENAME = "SNC_LinLog.csv"
    OUTPUT_CSV_FILE = os.path.join(OUTPUT_DIR, OUTPUT_CSV_FILENAME)
    print(f"{INPUT_FILE=}")
    print(f"{OUTPUT_JSON_FILE=}")
    print(f"{OUTPUT_CSV_FILE=}")
    sn_curve_linlog.execute(INPUT_FILE, OUTPUT_JSON_FILE, OUTPUT_CSV_FILE)

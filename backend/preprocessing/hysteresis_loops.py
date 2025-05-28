import os
import re
import numpy as np
import pandas as pd
import json

BASE_FOLDER = "../../Data/preprocessed"

def is_fawithoutfracture(folder_path):
    experiment_fp = os.path.join(folder_path, "experiment.json")
    if not os.path.exists(experiment_fp):
        return True  # skip if the file is missing
    with open(experiment_fp, "r") as f:
        data = json.load(f)
    fa_type = data.get("general", {}).get("fa experiment type")
    if fa_type is None:
        return True  # skip if the field is missing
    return fa_type.lower() == "fracture"

def poly_area(x, y):
    return 0.5 * np.abs(np.dot(x, np.roll(y, 1)) - np.dot(y, np.roll(x, 1)))

def fit_ellipse_matlab(x: np.ndarray,
                       y: np.ndarray,
                       num_points: int = 50):
    mean_x = np.round(x.mean(), 6)
    mean_y = np.round(y.mean(), 4)

    x0, y0 = x - mean_x, y - mean_y
    X      = np.column_stack((x0**2, x0 * y0, y0**2, x0, y0))
    a_row  = X.sum(axis=0) @ np.linalg.inv(X.T @ X)
    A, B, C, D, E = a_row

    phi = 0.5 * np.arctan(B / (C - A)) if abs(B) > 1e-19 else 0.0
    cos_phi, sin_phi = np.cos(phi), np.sin(phi)
    Ar = A * cos_phi**2 - B * cos_phi * sin_phi + C * sin_phi**2
    Cr = A * sin_phi**2 + B * cos_phi * sin_phi + C * cos_phi**2
    Dr = D * cos_phi - E * sin_phi
    Er = D * sin_phi + E * cos_phi

    X0r = -Dr / (2 * Ar)
    Y0r = -Er / (2 * Cr)
    F = 1 + Dr**2 / (4 * Ar) + Er**2 / (4 * Cr)
    a_len, b_len = np.sqrt(abs(F / Ar)), np.sqrt(abs(F / Cr))

    R = np.array([[cos_phi,  sin_phi],
                  [-sin_phi, cos_phi]])
    center = R @ np.array([X0r, Y0r]) + np.array([mean_x, mean_y])
    X0_in, Y0_in = center

    theta = np.linspace(0, 2 * np.pi, num_points)
    ex    = a_len * np.cos(theta)
    ey    = b_len * np.sin(theta)
    rot   = (R @ np.vstack((ex, ey))).T + center

    return {
        "fit_x": rot[:, 0],
        "fit_y": rot[:, 1],
        "X0_in": X0_in,
    }

def process_hysteresis(df, test_meta):
    cycle_field = "N_cycles"
    load_field = "Load"
    strain_field = "exx"

    df = df.assign(
        stress=df[load_field] / (test_meta["width"] * test_meta["thickness"]),
        strain=df[strain_field],
    )

    n_cycles = np.sort(df[cycle_field].unique())
    hys_records = []
    loop_points_records = []
    max_load_nominal = test_meta.get("maximum load")

    # Choose a subset of cycles to process
    if len(n_cycles) <= 10:
        selected_cycles = set(n_cycles)
    else:
        selected_cycles = set(n_cycles[np.linspace(0, len(n_cycles) - 1, 10, dtype=int)])

    for n in n_cycles:
        cycle_df = df[df[cycle_field] == n]
        load_max = cycle_df[load_field].max()
        if not (0.9 * max_load_nominal <= load_max <= 1.1 * max_load_nominal):
            continue

        stress = cycle_df["stress"].values
        strain = cycle_df["strain"].values

        if stress.size < 2:
            continue

        params = fit_ellipse_matlab(strain, stress, num_points=50)
        fit_x = params["fit_x"]
        fit_y = params["fit_y"]

        if len(fit_x) >= 2:
            coeffs = np.polyfit(fit_x, fit_y, 1)
            stiffness = coeffs[0]
        else:
            stiffness = np.nan
        hyst_area = poly_area(fit_x, fit_y)
        mean_strain = round(params["X0_in"], 6)

        hys_records.append({
            "n_cycles": n,
            "hysteresis_area": round(hyst_area, 6),
            "stiffness": round(stiffness, 6),
            "creep": round(mean_strain, 6)
        })

        if n in selected_cycles:
            for i in range(50):
                loop_points_records.append({
                    "n_cycles": n,
                    "point_index": i,
                    "fit_x": fit_x[i],
                    "fit_y": fit_y[i],
                })

    return pd.DataFrame(hys_records), pd.DataFrame(loop_points_records)

def run_on_folder(PREPROCESSED_FOLDER):
    tests_fp = os.path.join(PREPROCESSED_FOLDER, "tests.csv")
    if not os.path.exists(tests_fp):
        print("tests.csv not found")
        return

    tests_df = pd.read_csv(tests_fp)
    for fname in os.listdir(PREPROCESSED_FOLDER):
        if not fname.startswith("measure") or not fname.endswith(".csv"):
            continue

        file_number = int(re.search(r"(\d+)\.csv", fname).group(1))
        test_meta_row = tests_df[tests_df["sequential number"] == file_number]
        if test_meta_row.empty:
            continue
        test_meta = test_meta_row.to_dict(orient="records")[0]

        try:
            print(f"   📄 Reading: {fname}")
            df = pd.read_csv(os.path.join(PREPROCESSED_FOLDER, fname), low_memory=False)
            hyst_df, loops_df = process_hysteresis(df, test_meta)
            if not hyst_df.empty:
                hyst_df.to_csv(os.path.join(PREPROCESSED_FOLDER, "HYS_" + fname), index=False)

            if not loops_df.empty:
                loops_df.to_csv(os.path.join(PREPROCESSED_FOLDER, "Loops_" + fname), index=False)
        except Exception as e:
            print(f"Error with {fname}: {e}")

def main():
    for folder_name in os.listdir(BASE_FOLDER):
        PREPROCESSED_FOLDER = os.path.join(BASE_FOLDER, folder_name)
        if not os.path.isdir(PREPROCESSED_FOLDER):
            continue
        if is_fawithoutfracture(PREPROCESSED_FOLDER):
            print(f"❌ Skipping experiment: {folder_name}")
            continue
        print(f"✅ Processing: {folder_name}")
        run_on_folder(PREPROCESSED_FOLDER)

if __name__ == "__main__":
    main()

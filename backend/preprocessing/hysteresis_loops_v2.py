import os
import re
import numpy as np
import pandas as pd

PREPROCESSED_FOLDER = "../Data/preprocessed/TST_Mannino_2023-10_FA"

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
    max_load_nominal = test_meta.get("maximum load")

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
        '''
        smax = fit_y.max()
        smin = fit_y.min()
        e_at_smax = fit_x[np.argmax(fit_y)]
        e_at_smin = fit_x[np.argmin(fit_y)]

        emax = fit_x.max()
        emin = fit_x.min()
        s_at_emax = fit_y[np.argmax(fit_x)]
        s_at_emin = fit_y[np.argmin(fit_x)]

        stiff1 = (smax - smin) / (e_at_smax - e_at_smin) if (e_at_smax - e_at_smin) != 0 else np.nan
        stiff2 = (s_at_emax - s_at_emin) / (emax - emin) if (emax - emin) != 0 else np.nan
        stiffness = 0.5 * (stiff1 + stiff2)
        '''
        # New stiffness calculation with linear fit of all the 50 points
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

    return pd.DataFrame(hys_records)

def main():
    tests_fp = os.path.join(PREPROCESSED_FOLDER, "tests.csv")
    if not os.path.exists(tests_fp):
        print("tests.csv non trovato.")
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
            df = pd.read_csv(os.path.join(PREPROCESSED_FOLDER, fname), low_memory=False)
            hyst_df = process_hysteresis(df, test_meta)
            if not hyst_df.empty:
                hys_fp = "HYS_" + fname
                hyst_df.to_csv(os.path.join(PREPROCESSED_FOLDER, hys_fp), index=False)
        except Exception as e:
            print(f"Error with {fname}: {e}")

if __name__ == "__main__":
    main()

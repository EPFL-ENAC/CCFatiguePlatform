/**
 * Declarative schema for the Fatigue Failure methods.
 *
 * Each method only lists the file(s) and parameter(s) it actually needs, keyed by
 * the same id across methods. FatigueFailure.vue renders the union of file inputs
 * and parameter fields for whichever methods are currently selected, so a param
 * shared by several methods (e.g. desirableAngle) is only entered once.
 *
 * Every file and param also carries a `column` (longitudinal/transverse/shear),
 * mirroring the 3 physical fatigue-data categories (X/Y/F), so the form can be
 * laid out as 3 cards instead of one flat grid. Params additionally carry a
 * `section` (source/main/secondary) for ordering within a card, and a `group`
 * (tensile/compressive/shear/angle/ratio/strength) so same-family fields (e.g.
 * every tensile strength) are clustered together within a section rather than
 * interleaved - see FAF_GROUP_ORDER.
 */

export const FAF_COLUMNS = {
  longitudinal: { label: "Longitudinal (X)" },
  transverse: { label: "Transverse (Y)" },
  shear: { label: "Shear / Off-axis (F)" },
};

// Params that always live in their own panel (angle + static strength at
// that angle), separate from the per-column loading-direction cards, for
// every method - not just HashinRotem.
export const ANGLE_PANEL_PARAMS = [
  "desirableAngle",
  "tensileStrengthAtDesirableAngle",
];

// Sort order applied within a column/section so same-family fields cluster together
export const FAF_GROUP_ORDER = [
  "tensile",
  "compressive",
  "shear",
  "angle",
  "ratio",
  "strength",
];

// Parameter registry: id -> { label, type, default, column, section, group, items?, labelOverrides? }
export const FAF_PARAMS = {
  snModel: {
    label: "SN Model",
    type: "select",
    items: ["Lin-Log", "Log-Log"],
    default: "Log-Log",
    column: "shear",
    section: "source",
    group: "strength",
  },
  desirableAngle: {
    label: "Desirable Angle",
    type: "number",
    default: 30,
    column: "shear",
    section: "secondary",
    group: "angle",
  },
  offAxisAngle: {
    label: "Off Axis Angle",
    type: "number",
    default: 160,
    labelOverrides: { HashinRotem: "Off Axis Angle 1" },
    column: "shear",
    section: "secondary",
    group: "angle",
  },
  offAxisAngle2: {
    label: "Off Axis Angle 2",
    type: "number",
    default: 45,
    column: "shear",
    section: "secondary",
    group: "angle",
  },
  // FTPF-only: makes explicit whether the F file is a direct shear S-N
  // curve or a measured off-axis curve to back-calculate shear strength
  // from, rather than leaving it implicit in whether offAxisAngle is 0.
  // No `column`/`section` - rendered by a dedicated template block (like
  // hashinPanel2Type/hashinPanel3Type below) so it can sit above the file
  // input, not after it like the generic per-column loop would place it.
  ftpfFType: {
    label: "Type",
    type: "select",
    items: ["Shear", "Off-axis"],
    default: "Off-axis",
  },
  tensileTransverseStrength: {
    label: "Tensile Transverse Strength",
    type: "number",
    default: 84.94,
    column: "transverse",
    section: "main",
    group: "tensile",
  },
  compressiveTransverseStrength: {
    label: "Compressive Transverse Strength",
    type: "number",
    default: 83.64,
    column: "transverse",
    section: "main",
    group: "compressive",
  },
  shearStrength: {
    label: "Shear Strength",
    type: "number",
    default: 61.38,
    column: "shear",
    section: "main",
    group: "shear",
  },
  tensileStrength1: {
    label: "Tensile Strength 1",
    type: "number",
    default: 84.94,
    column: "shear",
    section: "secondary",
    group: "tensile",
  },
  tensileStrength2: {
    label: "Tensile Strength 2",
    type: "number",
    default: 139.12,
    column: "shear",
    section: "secondary",
    group: "tensile",
  },
  tensileStrengthAtDesirableAngle: {
    label: "Tensile Strength at Desirable Angle",
    type: "number",
    default: 89.47,
    column: "shear",
    section: "secondary",
    group: "tensile",
  },
  // Restricted per panel (rather than all 3 types on both) so the 4
  // reconstruction cases stay reachable (Transverse+Shear=direct,
  // Transverse+Off-axis=case2, Off-axis+Shear=case3, Off-axis+Off-
  // axis=case1) while making a duplicate non-off-axis type unselectable
  // in the first place, rather than caught after the fact by validation.
  hashinPanel2Type: {
    label: "Panel 2 Type",
    type: "select",
    items: ["Transverse", "Off-axis"],
    default: "Transverse",
  },
  hashinPanel3Type: {
    label: "Panel 3 Type",
    type: "select",
    items: ["Shear", "Off-axis"],
    default: "Shear",
  },
  referenceAngle: {
    label: "Reference Angle",
    type: "number",
    default: 90,
    column: "shear",
    section: "secondary",
    group: "angle",
  },
  referenceStressRatio: {
    label: "Reference Stress Ratio",
    type: "number",
    default: 0.1,
    column: "shear",
    section: "secondary",
    group: "ratio",
  },
  referenceStaticStrength: {
    label: "Reference Static Strength",
    type: "number",
    default: 244.84,
    column: "shear",
    section: "secondary",
    group: "strength",
  },
  targetStressRatio: {
    label: "Target Stress Ratio",
    type: "number",
    default: 0.1,
    column: "shear",
    section: "secondary",
    group: "ratio",
  },
  targetStaticStrength: {
    label: "Target Static Strength",
    type: "number",
    default: 139.12,
    column: "shear",
    section: "secondary",
    group: "strength",
  },
  tensileAxialStrength: {
    label: "Tensile Axial Strength",
    type: "number",
    default: 244.84,
    column: "longitudinal",
    section: "main",
    group: "tensile",
  },
  compressiveAxialStrength: {
    label: "Compressive Axial Strength",
    type: "number",
    default: 216.68,
    column: "longitudinal",
    section: "main",
    group: "compressive",
  },
};

// File registry: id -> { label, accept, conventionPrefix, tooltip, column }
export const FAF_FILES = {
  xFile: {
    label: "Longitudinal fatigue data (SNC json file)",
    accept: ".json",
    conventionPrefix: "SNC",
    tooltip: "Longitudinal fatigue data.",
    column: "longitudinal",
  },
  yFile: {
    label: "Transverse fatigue data. (SNC json file)",
    accept: ".json",
    conventionPrefix: "SNC",
    tooltip: "Transverse fatigue data.",
    column: "transverse",
  },
  fFile: {
    label: "Shear or off-axis fatigue data (SNC json file)",
    accept: ".json",
    conventionPrefix: "SNC",
    tooltip: "Shear or off-axis fatigue data.",
    column: "shear",
  },
  aggFile: {
    label: "Reference fatigue data (AGG csv file)",
    accept: ".csv",
    conventionPrefix: "AGG",
    tooltip:
      "Reference fatigue data recorded at several stress ratios, all at the same reference angle.",
    column: "shear",
  },
  sncFile: {
    label: "Reference S-N curve (SNC json file)",
    accept: ".json",
    conventionPrefix: "SNC",
    tooltip:
      "Reference S-N curve, fitted at a single reference angle/stress ratio.",
    column: "shear",
  },
  xcFile: {
    label: "Longitudinal compression fatigue data (SNC json file)",
    accept: ".json",
    conventionPrefix: "SNC",
    tooltip: "Longitudinal compression S-N curve (X').",
    column: "longitudinal",
  },
  ycFile: {
    label: "Transverse compression fatigue data (SNC json file)",
    accept: ".json",
    conventionPrefix: "SNC",
    tooltip: "Transverse compression S-N curve (Y').",
    column: "transverse",
  },
  // Dedicated to Hashin-Rotem - not shared with yFile/fFile (FTPF/Sims-
  // Brogdon), since Hashin-Rotem's panel 2/3 meaning is user-selected
  // (Transverse/Shear/Off-axis) rather than fixed, and sharing a file slot
  // whose meaning differs per selected method would be ambiguous. No
  // `column` - rendered by a dedicated template block, not the generic
  // per-column loop.
  hashinPanel2File: {
    label: "Panel 2 fatigue data (SNC json file)",
    accept: ".json",
    conventionPrefix: "SNC",
    tooltip: "Fatigue data for whichever type Panel 2 is set to.",
  },
  hashinPanel3File: {
    label: "Panel 3 fatigue data (SNC json file)",
    accept: ".json",
    conventionPrefix: "SNC",
    tooltip: "Fatigue data for whichever type Panel 3 is set to.",
  },
};

// Method registry: id -> { files, params, run(api, values, files) }
export const FAF_METHODS = {
  FTPF: {
    files: ["xFile", "yFile", "fFile", "xcFile", "ycFile"],
    params: ["snModel", "desirableAngle", "offAxisAngle", "ftpfFType"],
    // ftpfFType drives what's sent, not offAxisAngle directly - when the F
    // file is "Shear" (direct shear S-N curve), off_axis_angle is always
    // 0 regardless of whatever value sits in the shared offAxisAngle
    // field (which may still be populated for e.g. SimsBrogdon).
    run: (api, v, f) =>
      api.runFatigueFailureFtpfFile(
        v.snModel,
        v.desirableAngle,
        v.ftpfFType === "Shear" ? 0 : v.offAxisAngle,
        f.xFile,
        f.yFile,
        f.fFile,
        {
          xcFile: f.xcFile,
          ycFile: f.ycFile,
        }
      ),
  },
  HashinRotem: {
    files: ["xFile", "hashinPanel2File", "hashinPanel3File"],
    params: [
      "snModel",
      "desirableAngle",
      "offAxisAngle",
      "offAxisAngle2",
      "tensileTransverseStrength",
      "shearStrength",
      "tensileStrength1",
      "tensileStrength2",
      "tensileStrengthAtDesirableAngle",
      "hashinPanel2Type",
      "hashinPanel3Type",
    ],
    // Every one of the above is still required by run() below. Most of
    // them shouldn't surface in the generic per-column cards for
    // Hashin-Rotem specifically - offAxisAngle(2)/tensileStrength1/2 are
    // already editable inline in Panel 2/3 (Off-axis case), and snModel
    // just sends its default. tensileTransverseStrength/shearStrength are
    // rendered unconditionally in the dedicated "Material Properties
    // (Common)" card in FatigueFailure.vue instead of inline in Panel 2/3
    // - they're used by reconstruct_s2f_s12f() in every case (direct,
    // case 1/2/3), including configurations where no panel is set to
    // Transverse/Shear, so gating their visibility on a panel's type
    // would hide them exactly when they're still needed. desirableAngle
    // and tensileStrengthAtDesirableAngle feed the verified S-N curve at
    // the desired angle (hashin_equation_23 in faf_hashinrotem.py) so
    // they stay in uiParams to keep showing in the shared "Desired Angle"
    // panel. uiParams (rather than params) is what
    // activeParamKeys/columnParams draw from, so this only affects what
    // renders, not what's sent to the backend.
    uiParams: ["desirableAngle", "tensileStrengthAtDesirableAngle"],
    // Tension loading only - Hashin-Rotem doesn't support stress_ratio > 1
    // compression-compression loading, so no compressive_* params here.
    run: (api, v, f) =>
      api.runFatigueFailureHashinrotemFile(
        v.snModel,
        v.desirableAngle,
        v.offAxisAngle,
        v.offAxisAngle2,
        v.tensileTransverseStrength,
        v.shearStrength,
        v.tensileStrength1,
        v.tensileStrength2,
        v.tensileStrengthAtDesirableAngle,
        v.hashinPanel2Type,
        v.hashinPanel3Type,
        f.xFile,
        f.hashinPanel2File,
        f.hashinPanel3File
      ),
  },
  SimsBrogdon: {
    files: ["xFile", "yFile", "fFile"],
    params: ["snModel", "desirableAngle", "offAxisAngle"],
    run: (api, v, f) =>
      api.runFatigueFailureFile(
        "SimsBrogdon",
        v.snModel,
        v.desirableAngle,
        v.offAxisAngle,
        f.xFile,
        f.yFile,
        f.fFile
      ),
  },
  ShokriehTaheri: {
    files: ["aggFile"],
    params: [
      "referenceAngle",
      "referenceStressRatio",
      "desirableAngle",
      "targetStressRatio",
      "tensileAxialStrength",
      "compressiveAxialStrength",
      "tensileTransverseStrength",
      "compressiveTransverseStrength",
      "shearStrength",
    ],
    run: (api, v, f) =>
      api.runFatigueFailureShokriehtaheriFile(
        v.referenceAngle,
        v.referenceStressRatio,
        v.desirableAngle,
        v.targetStressRatio,
        v.tensileAxialStrength,
        v.compressiveAxialStrength,
        v.tensileTransverseStrength,
        v.compressiveTransverseStrength,
        v.shearStrength,
        f.aggFile
      ),
  },
  FawazEllyin: {
    files: ["sncFile"],
    params: [
      "snModel",
      "referenceAngle",
      "referenceStaticStrength",
      "desirableAngle",
      "targetStressRatio",
      "targetStaticStrength",
    ],
    run: (api, v, f) =>
      api.runFatigueFailureFawazellyinFile(
        v.snModel,
        v.referenceAngle,
        v.referenceStaticStrength,
        v.desirableAngle,
        v.targetStressRatio,
        v.targetStaticStrength,
        f.sncFile
      ),
  },
  Kawai: {
    files: ["aggFile"],
    params: [
      "referenceAngle",
      "referenceStressRatio",
      "referenceStaticStrength",
      "desirableAngle",
      "targetStressRatio",
      "tensileAxialStrength",
      "tensileTransverseStrength",
      "shearStrength",
    ],
    run: (api, v, f) =>
      api.runFatigueFailureKawaiFile(
        v.referenceAngle,
        v.referenceStressRatio,
        v.referenceStaticStrength,
        v.desirableAngle,
        v.targetStressRatio,
        v.tensileAxialStrength,
        v.tensileTransverseStrength,
        v.shearStrength,
        f.aggFile
      ),
  },
};

export function getParamLabel(paramKey, methods) {
  const param = FAF_PARAMS[paramKey];
  const override = methods
    .map((method) => param.labelOverrides && param.labelOverrides[method])
    .find((label) => label);
  return override || param.label;
}

export function getColumnLabel(columnKey, methods) {
  const column = FAF_COLUMNS[columnKey];
  const override = methods
    .map((method) => column.labelOverrides && column.labelOverrides[method])
    .find((label) => label);
  return override || column.label;
}

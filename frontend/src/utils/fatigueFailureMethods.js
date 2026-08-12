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
  compressiveStrength1: {
    label: "Compressive Strength 1",
    type: "number",
    default: 83.64,
    column: "shear",
    section: "secondary",
    group: "compressive",
  },
  tensileStrength2: {
    label: "Tensile Strength 2",
    type: "number",
    default: 139.12,
    column: "shear",
    section: "secondary",
    group: "tensile",
  },
  compressiveStrength2: {
    label: "Compressive Strength 2",
    type: "number",
    default: 106.4,
    column: "shear",
    section: "secondary",
    group: "compressive",
  },
  tensileStrengthAtDesirableAngle: {
    label: "Tensile Strength at Desirable Angle",
    type: "number",
    default: 89.47,
    column: "shear",
    section: "secondary",
    group: "tensile",
  },
  compressiveStrengthAtDesirableAngle: {
    label: "Compressive Strength at Desirable Angle",
    type: "number",
    default: 145.52,
    column: "shear",
    section: "secondary",
    // Grouped with "angle" (not "compressive") so it renders right after
    // Desirable Angle instead of with the other compressive fields.
    group: "angle",
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
};

// Method registry: id -> { files, params, run(api, values, files) }
export const FAF_METHODS = {
  FTPT: {
    files: ["xFile", "yFile", "fFile"],
    params: ["snModel", "desirableAngle", "offAxisAngle"],
    run: (api, v, f) =>
      api.runFatigueFailureFile(
        "FTPT",
        v.snModel,
        v.desirableAngle,
        v.offAxisAngle,
        f.xFile,
        f.yFile,
        f.fFile
      ),
  },
  HashinRotem: {
    files: ["xFile", "yFile", "fFile"],
    params: [
      "snModel",
      "desirableAngle",
      "offAxisAngle",
      "offAxisAngle2",
      "tensileTransverseStrength",
      "compressiveTransverseStrength",
      "shearStrength",
      "tensileStrength1",
      "compressiveStrength1",
      "tensileStrength2",
      "compressiveStrength2",
      "tensileStrengthAtDesirableAngle",
      "compressiveStrengthAtDesirableAngle",
    ],
    run: (api, v, f) =>
      api.runFatigueFailureFile(
        "HashinRotem",
        v.snModel,
        v.desirableAngle,
        v.offAxisAngle,
        f.xFile,
        f.yFile,
        f.fFile,
        {
          offAxisAngle2: v.offAxisAngle2,
          tensileTransverseStrength: v.tensileTransverseStrength,
          compressiveTransverseStrength: v.compressiveTransverseStrength,
          shearStrength: v.shearStrength,
          tensileStrength1: v.tensileStrength1,
          compressiveStrength1: v.compressiveStrength1,
          tensileStrength2: v.tensileStrength2,
          compressiveStrength2: v.compressiveStrength2,
          tensileStrengthAtDesirableAngle: v.tensileStrengthAtDesirableAngle,
          compressiveStrengthAtDesirableAngle:
            v.compressiveStrengthAtDesirableAngle,
        }
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
      api.runFatigueFailureShokriehTaheriFile(
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
      api.runFatigueFailureFawazEllyinFile(
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

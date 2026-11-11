export const FAF_METHODS = {
  FTPF: {},
  HashinRotem: { targetStaticStrength: true },
  SimsBrogdon: {},
  ShokriehTaheri: { targetStressRatio: true, aggFile: true },
  FawazEllyin: { targetStressRatio: true, targetStaticStrength: true },
  Kawai: { targetStressRatio: true, aggFile: true },
};

const NUMBER_TOLERANCE = 1e-9;
const AGG_REQUIRED_COLUMNS = [
  "stress_ratio",
  "stress_cluster_number",
  "stress_max",
  "cycles_to_failure",
];

const LOAD_TYPE_ALIASES = {
  normal: "tensile",
  tension: "tensile",
  tensile: "tensile",
  compression: "compressive",
  compressive: "compressive",
  shear: "shear",
};

const MODEL_ALIASES = {
  linlog: "Lin-Log",
  loglog: "Log-Log",
};

export function runFatigueFailureMethod(
  api,
  method,
  values,
  materialFile,
  aggFile
) {
  return api.runFatigueFailureMaterialFile(
    method,
    values.targetAngle,
    materialFile,
    {
      targetStressRatio: values.targetStressRatio,
      targetStaticStrength: values.targetStaticStrength,
      aggFile,
    }
  );
}

function isObject(value) {
  return value !== null && typeof value === "object" && !Array.isArray(value);
}

function isFiniteNumber(value) {
  return typeof value === "number" && Number.isFinite(value);
}

function sameNumber(left, right) {
  return Math.abs(left - right) <= NUMBER_TOLERANCE;
}

function normalizeLoadType(value) {
  if (value === undefined || value === null) return "tensile";
  if (typeof value !== "string") return null;
  return (
    LOAD_TYPE_ALIASES[value.trim().toLowerCase().replace(/_/g, "-")] || null
  );
}

function normalizeModel(value) {
  if (typeof value !== "string") return null;
  const normalized = value.trim().toLowerCase().replace(/[_-]/g, "");
  return MODEL_ALIASES[normalized] || null;
}

function describeAngle(angle) {
  return angle === null ? "shear" : `${angle}°`;
}

function uniqueProblems(problems) {
  return [...new Set(problems)];
}

function inspectMaterial(material) {
  const problems = [];
  const strengths = [];
  const curves = [];

  if (!isObject(material)) {
    return {
      problems: ["Material file: the JSON root must be an object."],
      strengths,
      curves,
    };
  }

  if (!Array.isArray(material.static_strengths)) {
    problems.push("Material file: static_strengths must be an array.");
  } else if (material.static_strengths.length === 0) {
    problems.push("Material file: static_strengths must not be empty.");
  } else {
    material.static_strengths.forEach((strength, index) => {
      const path = `static_strengths[${index}]`;
      if (!isObject(strength)) {
        problems.push(`${path} must be an object.`);
        return;
      }

      const loadType = normalizeLoadType(strength.type);
      if (loadType === null) {
        problems.push(`${path}.type must be tensile, compressive, or shear.`);
      }

      if (!isFiniteNumber(strength.value)) {
        problems.push(`${path}.value must be a finite number.`);
      } else if (strength.value <= 0) {
        problems.push(`${path}.value must be greater than zero.`);
      }

      let angle = null;
      if (loadType !== "shear") {
        if (!isFiniteNumber(strength.angle)) {
          problems.push(`${path}.angle must be a finite number.`);
        } else {
          angle = strength.angle;
        }
      } else if (
        strength.angle !== undefined &&
        strength.angle !== null &&
        !isFiniteNumber(strength.angle)
      ) {
        problems.push(`${path}.angle must be a finite number when provided.`);
      }

      if (
        loadType !== null &&
        isFiniteNumber(strength.value) &&
        strength.value > 0 &&
        (loadType === "shear" || angle !== null)
      ) {
        const duplicate = strengths.find(
          (candidate) =>
            candidate.loadType === loadType &&
            (loadType === "shear" || sameNumber(candidate.angle, angle))
        );
        if (duplicate) {
          problems.push(
            `${path} duplicates ${duplicate.path}: only one ${loadType} ` +
              `static strength at ${describeAngle(angle)} is allowed.`
          );
        }
        strengths.push({
          path,
          loadType,
          angle,
          value: strength.value,
        });
      }
    });
  }

  if (!Array.isArray(material.SN_curves)) {
    problems.push("Material file: SN_curves must be an array.");
  } else if (material.SN_curves.length === 0) {
    problems.push("Material file: SN_curves must not be empty.");
  } else {
    material.SN_curves.forEach((curve, index) => {
      const path = `SN_curves[${index}]`;
      if (!isObject(curve)) {
        problems.push(`${path} must be an object.`);
        return;
      }

      const loadType = normalizeLoadType(curve.type);
      if (loadType === null) {
        problems.push(`${path}.type must be tensile, compressive, or shear.`);
      }

      let angle = null;
      if (loadType !== "shear") {
        if (!isFiniteNumber(curve.angle)) {
          problems.push(`${path}.angle must be a finite number.`);
        } else {
          angle = curve.angle;
        }
      } else if (
        curve.angle !== undefined &&
        curve.angle !== null &&
        !isFiniteNumber(curve.angle)
      ) {
        problems.push(`${path}.angle must be a finite number when provided.`);
      }

      if (!isFiniteNumber(curve.stress_ratio)) {
        problems.push(`${path}.stress_ratio must be a finite number.`);
      }

      const model = normalizeModel(curve.model);
      if (model === null) {
        problems.push(`${path}.model must be Lin-Log or Log-Log.`);
      }

      if (!isFiniteNumber(curve.a)) {
        problems.push(`${path}.a must be a finite number.`);
      } else if (model === "Log-Log" && curve.a <= 0) {
        problems.push(`${path}.a must be greater than zero for Log-Log.`);
      }
      if (!isFiniteNumber(curve.b)) {
        problems.push(`${path}.b must be a finite number.`);
      }

      const confidenceInterval =
        curve.confidence_interval === undefined
          ? 50
          : curve.confidence_interval;
      if (!isFiniteNumber(confidenceInterval)) {
        problems.push(`${path}.confidence_interval must be a finite number.`);
      }
      ["lrsq", "fp"].forEach((field) => {
        if (
          curve[field] !== undefined &&
          curve[field] !== null &&
          !isFiniteNumber(curve[field])
        ) {
          problems.push(
            `${path}.${field} must be a finite number when provided.`
          );
        }
      });

      const valid =
        loadType !== null &&
        (loadType === "shear" || angle !== null) &&
        isFiniteNumber(curve.stress_ratio) &&
        model !== null &&
        isFiniteNumber(curve.a) &&
        !(model === "Log-Log" && curve.a <= 0) &&
        isFiniteNumber(curve.b) &&
        isFiniteNumber(confidenceInterval);
      if (!valid) return;

      const duplicate = curves.find(
        (candidate) =>
          candidate.loadType === loadType &&
          (loadType === "shear" || sameNumber(candidate.angle, angle)) &&
          sameNumber(candidate.stressRatio, curve.stress_ratio)
      );
      if (duplicate) {
        problems.push(
          `${path} duplicates ${duplicate.path}: only one ${loadType} S-N ` +
            `curve at ${describeAngle(angle)} and R=${
              curve.stress_ratio
            } is allowed.`
        );
      }
      curves.push({
        path,
        loadType,
        angle,
        stressRatio: curve.stress_ratio,
        model,
        a: curve.a,
        b: curve.b,
        confidenceInterval,
      });
    });
  }

  return { problems, strengths, curves };
}

function matchingStrengths(strengths, loadType, angle = null) {
  return strengths.filter(
    (strength) =>
      strength.loadType === loadType &&
      (loadType === "shear" || sameNumber(strength.angle, angle))
  );
}

function matchingCurves(curves, loadType, angle = null, stressRatio = null) {
  return curves.filter(
    (curve) =>
      curve.loadType === loadType &&
      (loadType === "shear" || sameNumber(curve.angle, angle)) &&
      (stressRatio === null || sameNumber(curve.stressRatio, stressRatio))
  );
}

function requireStrength(state, method, loadType, angle = null) {
  const candidates = matchingStrengths(state.strengths, loadType, angle);
  if (candidates.length === 0) {
    state.problems.push(
      `${method}: missing ${loadType} static strength at ${describeAngle(
        angle
      )}.`
    );
    return null;
  }
  if (candidates.length > 1) return null;
  return candidates[0];
}

function requireCurve(state, method, loadType, angle, stressRatio) {
  const candidates = matchingCurves(state.curves, loadType, angle, stressRatio);
  if (candidates.length === 0) {
    state.problems.push(
      `${method}: missing ${loadType} S-N curve at ${describeAngle(
        angle
      )} and R=${stressRatio}.`
    );
    return null;
  }
  if (candidates.length > 1) return null;
  return candidates[0];
}

function primaryCampaign(state, method) {
  const longitudinal = matchingCurves(state.curves, "tensile", 0);
  if (longitudinal.length === 0) {
    state.problems.push(`${method}: missing longitudinal X S-N curve at 0°.`);
    return null;
  }

  const ratios = [];
  longitudinal.forEach((curve) => {
    if (!ratios.some((ratio) => sameNumber(ratio, curve.stressRatio))) {
      ratios.push(curve.stressRatio);
    }
  });
  if (ratios.length > 1) {
    state.problems.push(
      `${method}: multiple longitudinal X campaigns were found ` +
        `(R=${ratios.join(", ")}); provide one unambiguous campaign.`
    );
    return null;
  }
  if (longitudinal.length > 1) return null;
  return { curve: longitudinal[0], stressRatio: ratios[0] };
}

function checkCurveCompatibility(state, method, curves) {
  if (curves.some((curve) => curve === null)) return;
  const models = [...new Set(curves.map((curve) => curve.model))];
  if (models.length > 1) {
    state.problems.push(
      `${method}: selected S-N curves use different models (${models.join(
        ", "
      )}).`
    );
  }
  const confidenceIntervals = [];
  curves.forEach((curve) => {
    if (
      !confidenceIntervals.some((value) =>
        sameNumber(value, curve.confidenceInterval)
      )
    ) {
      confidenceIntervals.push(curve.confidenceInterval);
    }
  });
  if (confidenceIntervals.length > 1) {
    state.problems.push(
      `${method}: selected S-N curves use different confidence intervals ` +
        `(${confidenceIntervals.join(", ")}).`
    );
  }
}

function selectShearOrOffAxis(state, method, stressRatio) {
  const shear = matchingCurves(state.curves, "shear", null, stressRatio);
  if (shear.length === 1) return shear[0];
  if (shear.length > 1) return null;

  const offAxis = state.curves.filter(
    (curve) =>
      curve.loadType === "tensile" &&
      sameNumber(curve.stressRatio, stressRatio) &&
      !sameNumber(curve.angle, 0) &&
      !sameNumber(curve.angle, 90)
  );
  if (offAxis.length === 0) {
    state.problems.push(
      `${method}: missing a shear or off-axis S-N curve at R=${stressRatio}.`
    );
    return null;
  }
  if (offAxis.length > 1) {
    state.problems.push(
      `${method}: multiple off-axis curves could replace the shear curve at ` +
        `R=${stressRatio} (${offAxis
          .map((curve) => `${curve.angle}°`)
          .join(", ")}); ` +
        "provide one unambiguous curve."
    );
    return null;
  }
  return offAxis[0];
}

function validateFtpf(state) {
  const method = "FTPF";
  const campaign = primaryCampaign(state, method);
  if (!campaign) return;
  const { curve: xCurve, stressRatio } = campaign;
  const yCurve = requireCurve(state, method, "tensile", 90, stressRatio);
  const fCurve = selectShearOrOffAxis(state, method, stressRatio);

  const compressionCurve = (angle) => {
    const atCampaignRatio = matchingCurves(
      state.curves,
      "compressive",
      angle,
      stressRatio
    );
    const candidates =
      atCampaignRatio.length > 0
        ? atCampaignRatio
        : matchingCurves(state.curves, "compressive", angle);
    if (candidates.length === 0) {
      state.problems.push(
        `${method}: missing compressive S-N curve at ${angle}°.`
      );
      return null;
    }
    if (candidates.length > 1) {
      state.problems.push(
        `${method}: compressive S-N curve at ${angle}° is ambiguous.`
      );
      return null;
    }
    return candidates[0];
  };

  checkCurveCompatibility(state, method, [
    xCurve,
    yCurve,
    fCurve,
    compressionCurve(0),
    compressionCurve(90),
  ]);
}

function validateSimsBrogdon(state) {
  const method = "SimsBrogdon";
  const campaign = primaryCampaign(state, method);
  if (!campaign) return;
  const yCurve = requireCurve(
    state,
    method,
    "tensile",
    90,
    campaign.stressRatio
  );
  const fCurve = selectShearOrOffAxis(state, method, campaign.stressRatio);
  checkCurveCompatibility(state, method, [campaign.curve, yCurve, fCurve]);
}

function validateHashinRotem(state) {
  const method = "HashinRotem";
  const campaign = primaryCampaign(state, method);
  if (!campaign) return;
  const { curve: xCurve, stressRatio } = campaign;
  if (stressRatio < 0 || stressRatio >= 1) {
    state.problems.push(
      `${method}: stress ratio must satisfy 0 ≤ R < 1; received R=${stressRatio}.`
    );
  }

  requireStrength(state, method, "tensile", 0);
  requireStrength(state, method, "tensile", 90);
  requireStrength(state, method, "shear");

  const transverse = matchingCurves(state.curves, "tensile", 90, stressRatio);
  const shear = matchingCurves(state.curves, "shear", null, stressRatio);
  const offAxis = state.curves.filter(
    (curve) =>
      curve.loadType === "tensile" &&
      sameNumber(curve.stressRatio, stressRatio) &&
      !sameNumber(curve.angle, 0) &&
      !sameNumber(curve.angle, 90)
  );

  if (transverse.length > 1 || shear.length > 1) return;

  let panels = [];
  if (transverse.length === 1 && shear.length === 1) {
    panels = [transverse[0], shear[0]];
  } else if (transverse.length === 1) {
    if (offAxis.length !== 1) {
      state.problems.push(
        `${method}: X+Y requires exactly one off-axis curve when no shear ` +
          `curve is available at R=${stressRatio}; found ${offAxis.length}.`
      );
      return;
    }
    panels = [transverse[0], offAxis[0]];
  } else if (shear.length === 1) {
    if (offAxis.length !== 1) {
      state.problems.push(
        `${method}: X+S requires exactly one off-axis curve when no transverse ` +
          `curve is available at R=${stressRatio}; found ${offAxis.length}.`
      );
      return;
    }
    panels = [offAxis[0], shear[0]];
  } else {
    if (offAxis.length !== 2) {
      state.problems.push(
        `${method}: provide Y+S, Y+one off-axis, S+one off-axis, or two ` +
          `distinct off-axis curves at R=${stressRatio}; found ${offAxis.length} ` +
          "off-axis curves."
      );
      return;
    }
    if (sameNumber(offAxis[0].angle, offAxis[1].angle)) {
      state.problems.push(
        `${method}: the two off-axis angles must be distinct.`
      );
      return;
    }
    panels = offAxis;
  }

  panels
    .filter((curve) => curve.loadType === "tensile" && curve.angle !== 90)
    .forEach((curve) => requireStrength(state, method, "tensile", curve.angle));
  checkCurveCompatibility(state, method, [xCurve, ...panels]);
}

function selectReferenceCurveGroup(state, method, targetAngle = 0) {
  const groups = new Map();
  state.curves
    .filter((curve) => curve.loadType === "tensile")
    .forEach((curve) => {
      const existing = [...groups.entries()].find(([angle]) =>
        sameNumber(angle, curve.angle)
      );
      if (existing) existing[1].push(curve);
      else groups.set(curve.angle, [curve]);
    });
  if (groups.size === 0) {
    state.problems.push(`${method}: no tensile S-N reference curve was found.`);
    return null;
  }
  return [...groups.entries()].sort((left, right) => {
    const leftRatios = new Set(left[1].map((curve) => curve.stressRatio)).size;
    const rightRatios = new Set(right[1].map((curve) => curve.stressRatio))
      .size;
    if (leftRatios !== rightRatios) return rightRatios - leftRatios;
    const leftAxis = sameNumber(left[0], 0) || sameNumber(left[0], 90) ? 1 : 0;
    const rightAxis =
      sameNumber(right[0], 0) || sameNumber(right[0], 90) ? 1 : 0;
    if (leftAxis !== rightAxis) return leftAxis - rightAxis;
    const leftDistance = Math.abs(left[0] - targetAngle);
    const rightDistance = Math.abs(right[0] - targetAngle);
    if (leftDistance !== rightDistance) return leftDistance - rightDistance;
    return left[0] - right[0];
  })[0];
}

function validateShokriehTaheri(state) {
  const method = "ShokriehTaheri";
  selectReferenceCurveGroup(state, method);
  requireStrength(state, method, "tensile", 0);
  requireStrength(state, method, "compressive", 0);
  requireStrength(state, method, "tensile", 90);
  requireStrength(state, method, "compressive", 90);
  requireStrength(state, method, "shear");
}

function validateKawai(state, targetAngle) {
  const method = "Kawai";
  const group = selectReferenceCurveGroup(state, method, targetAngle);
  requireStrength(state, method, "tensile", 0);
  requireStrength(state, method, "tensile", 90);
  requireStrength(state, method, "shear");
  if (group) requireStrength(state, method, "tensile", group[0]);
}

function validateFawazEllyin(state) {
  const candidates = state.curves.filter(
    (curve) =>
      curve.loadType !== "compressive" &&
      curve.angle !== null &&
      matchingStrengths(state.strengths, curve.loadType, curve.angle).length ===
        1
  );
  if (candidates.length === 0) {
    state.problems.push(
      "FawazEllyin: an S-N curve with a matching static strength is required."
    );
  }
}

/**
 * Validate a combined fatigue-failure material and the requirements of the
 * selected methods. Returned messages are suitable for direct display.
 */
export function findMissingFatigueFailureInformation(
  material,
  methods = [],
  targetConditions = {}
) {
  const state = inspectMaterial(material);
  const selectedMethods = new Set(methods);

  if (selectedMethods.has("FTPF")) validateFtpf(state);
  if (selectedMethods.has("HashinRotem")) validateHashinRotem(state);
  if (selectedMethods.has("SimsBrogdon")) validateSimsBrogdon(state);
  if (selectedMethods.has("ShokriehTaheri")) validateShokriehTaheri(state);
  if (selectedMethods.has("FawazEllyin")) validateFawazEllyin(state);
  if (selectedMethods.has("Kawai")) {
    validateKawai(
      state,
      isFiniteNumber(targetConditions.targetAngle)
        ? targetConditions.targetAngle
        : 0
    );
  }

  return uniqueProblems(state.problems);
}

/** Validate parsed AGG CSV data used by Kawai and Shokrieh-Taheri. */
export function findAggFileProblems(parseResult, methods = []) {
  const problems = [];
  const parseErrors = parseResult?.errors || [];
  parseErrors.forEach((error) => {
    const row = Number.isInteger(error.row)
      ? ` at CSV row ${error.row + 2}`
      : "";
    problems.push(
      `AGG file${row}: ${error.message || "could not parse this row"}.`
    );
  });

  const fields = parseResult?.meta?.fields || [];
  const fieldSet = new Set(fields);
  AGG_REQUIRED_COLUMNS.forEach((column) => {
    if (!fieldSet.has(column)) {
      problems.push(`AGG file: missing required column "${column}".`);
    }
  });

  const rows = parseResult?.data || [];
  if (rows.length === 0) {
    problems.push("AGG file: the CSV must contain at least one data row.");
    return uniqueProblems(problems);
  }

  const validRows = [];
  rows.forEach((row, index) => {
    const path = `AGG row ${index + 2}`;
    let valid = true;
    ["stress_ratio", "stress_max", "cycles_to_failure"].forEach((field) => {
      if (!fieldSet.has(field)) return;
      if (!isFiniteNumber(row[field])) {
        problems.push(`${path}: ${field} must be a finite number.`);
        valid = false;
      }
    });
    if (
      fieldSet.has("stress_cluster_number") &&
      (!Number.isInteger(row.stress_cluster_number) ||
        row.stress_cluster_number <= 0)
    ) {
      problems.push(
        `${path}: stress_cluster_number must be a positive integer.`
      );
      valid = false;
    }
    if (isFiniteNumber(row.stress_max) && row.stress_max <= 0) {
      problems.push(`${path}: stress_max must be greater than zero.`);
      valid = false;
    }
    if (
      isFiniteNumber(row.cycles_to_failure) &&
      (!Number.isInteger(row.cycles_to_failure) || row.cycles_to_failure <= 0)
    ) {
      problems.push(`${path}: cycles_to_failure must be a positive integer.`);
      valid = false;
    }
    if (valid) validRows.push({ row, rowNumber: index + 2 });
  });

  const selectedMethods = new Set(methods);
  if (selectedMethods.has("Kawai")) {
    validRows.forEach(({ row, rowNumber }) => {
      if (row.stress_ratio < -1 || row.stress_ratio >= 1) {
        problems.push(
          `Kawai: AGG row ${rowNumber} has R=${row.stress_ratio}; ` +
            "stress_ratio must satisfy -1 ≤ R < 1."
        );
      }
    });
  }

  if (selectedMethods.has("ShokriehTaheri")) {
    const groups = new Map();
    validRows.forEach(({ row }) => {
      const key = String(row.stress_ratio);
      if (!groups.has(key)) groups.set(key, []);
      groups.get(key).push(row);
    });
    groups.forEach((group, ratio) => {
      if (group.length < 2) {
        problems.push(
          `ShokriehTaheri: AGG group R=${ratio} needs at least two rows for regression.`
        );
      } else if (new Set(group.map((row) => row.stress_max)).size < 2) {
        problems.push(
          `ShokriehTaheri: AGG group R=${ratio} needs at least two distinct ` +
            "stress_max values for regression."
        );
      }
    });
  }

  return uniqueProblems(problems);
}

/** Convert generated-client, FastAPI validation, and network errors to text. */
export function fatigueFailureErrorMessage(error) {
  const detail = error?.body?.detail ?? error?.response?.body?.detail;
  if (Array.isArray(detail)) {
    return detail
      .map((item) => {
        if (typeof item === "string") return item;
        const location = Array.isArray(item?.loc)
          ? item.loc.filter((part) => part !== "body").join(".")
          : "Input";
        return `${location || "Input"}: ${item?.msg || "invalid value"}`;
      })
      .join("; ");
  }
  if (typeof detail === "string") return detail;
  return (
    error?.body?.message ||
    error?.response?.body?.message ||
    error?.error?.message ||
    error?.message ||
    "The analysis request failed."
  );
}

export const FAF_METHODS = {
  FTPF: {},
  HashinRotem: { targetStaticStrength: true },
  SimsBrogdon: {},
  ShokriehTaheri: { targetStressRatio: true, aggFile: true },
  FawazEllyin: { targetStressRatio: true, targetStaticStrength: true },
  Kawai: { targetStressRatio: true, aggFile: true },
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

/**
 * Validation stub for the combined fatigue-failure material JSON.
 *
 * It intentionally is not connected to the form yet. The backend remains
 * authoritative; this function is ready for a future inline validation UI.
 * It returns human-readable paths for information that is absent, without
 * trying to decide which records a selected analysis method should use.
 */
export function findMissingFatigueFailureInformation(material) {
  const missing = [];
  if (!material || typeof material !== "object" || Array.isArray(material)) {
    return ["The JSON root must be an object."];
  }

  if (!Array.isArray(material.static_strengths)) {
    missing.push("static_strengths must be an array.");
  } else {
    material.static_strengths.forEach((strength, index) => {
      const path = `static_strengths[${index}]`;
      if (!strength || typeof strength !== "object") {
        missing.push(`${path} must be an object.`);
        return;
      }
      if (strength.value === undefined || strength.value === null) {
        missing.push(`${path}.value is required.`);
      }
      const type = String(strength.type || "tensile").toLowerCase();
      if (
        type !== "shear" &&
        (strength.angle === undefined || strength.angle === null)
      ) {
        missing.push(`${path}.angle is required.`);
      }
    });
  }

  if (!Array.isArray(material.SN_curves)) {
    missing.push("SN_curves must be an array.");
  } else {
    material.SN_curves.forEach((curve, index) => {
      const path = `SN_curves[${index}]`;
      if (!curve || typeof curve !== "object") {
        missing.push(`${path} must be an object.`);
        return;
      }
      const type = String(curve.type || "tensile").toLowerCase();
      if (
        type !== "shear" &&
        (curve.angle === undefined || curve.angle === null)
      ) {
        missing.push(`${path}.angle is required.`);
      }
      ["stress_ratio", "model", "a", "b"].forEach((field) => {
        if (curve[field] === undefined || curve[field] === null) {
          missing.push(`${path}.${field} is required.`);
        }
      });
    });
  }

  return missing;
}

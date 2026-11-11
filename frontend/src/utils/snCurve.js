const CLD_CYCLE_LEVELS = [1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9];
const SNC_METADATA_COLUMNS = ["type", "angle", "model", "confidence_interval"];

export function snCurvesFromDocument(document) {
  if (Array.isArray(document)) return document;
  if (document && Array.isArray(document.SN_curves)) {
    return document.SN_curves;
  }
  throw new Error("S-N curve JSON must contain a non-empty SN_curves array.");
}

function finiteNumber(value, path) {
  if (typeof value !== "number" || !Number.isFinite(value)) {
    throw new Error(`${path} must be a finite number.`);
  }
  return value;
}

function normalizedModel(value, path) {
  if (typeof value !== "string") throw new Error(`${path} must be a string.`);
  const key = value.trim().toLowerCase().replace(/[_-]/g, "");
  const models = {
    linlog: "Lin-Log",
    loglog: "Log-Log",
    sendeckyj: "Sendeckyj",
    whitney: "Whitney",
  };
  if (!models[key]) {
    throw new Error(`${path} must be Lin-Log, Log-Log, Sendeckyj, or Whitney.`);
  }
  return models[key];
}

function normalizedLoadType(value, path) {
  if (typeof value !== "string") throw new Error(`${path} must be a string.`);
  const normalized = value.trim().toLowerCase();
  if (!["tensile", "compressive", "shear"].includes(normalized)) {
    throw new Error(`${path} must be tensile, compressive, or shear.`);
  }
  return normalized;
}

function curveMetadata(curve, index) {
  const path = `SN_curves[${index}]`;
  // Legacy SNC JSON arrays had no material-direction metadata. Preserve the
  // former implicit longitudinal tensile interpretation when reading them.
  const type = normalizedLoadType(curve.type ?? "tensile", `${path}.type`);
  const angle =
    type === "shear" ? null : finiteNumber(curve.angle ?? 0, `${path}.angle`);
  return {
    type,
    angle,
    model: normalizedModel(curve.model, `${path}.model`),
    confidence_interval: finiteNumber(
      curve.confidence_interval,
      `${path}.confidence_interval`
    ),
  };
}

function probability(curve, index) {
  let value = finiteNumber(
    curve.confidence_interval,
    `SN_curves[${index}].confidence_interval`
  );
  if (value > 1) value /= 100;
  if (!(value > 0 && value < 1)) {
    throw new Error(
      `SN_curves[${index}].confidence_interval must represent a probability strictly between 0 and 1.`
    );
  }
  return value;
}

export function evaluateSnCurve(curve, cycles, index = 0) {
  const path = `SN_curves[${index}]`;
  const model = normalizedModel(curve.model, `${path}.model`);
  const n = finiteNumber(cycles, "cycles_to_failure");
  let stress;

  if (model === "Lin-Log") {
    stress =
      finiteNumber(curve.a, `${path}.a`) +
      finiteNumber(curve.b, `${path}.b`) * Math.log10(n);
  } else if (model === "Log-Log") {
    stress =
      finiteNumber(curve.a, `${path}.a`) *
      n ** -finiteNumber(curve.b, `${path}.b`);
  } else if (model === "Sendeckyj") {
    const alpha = finiteNumber(curve.a, `${path}.a`);
    const beta = finiteNumber(curve.b, `${path}.b`);
    const c = finiteNumber(curve.cstar, `${path}.cstar`);
    const s = finiteNumber(curve.sstar, `${path}.sstar`);
    const sigmaE = beta * (-Math.log(probability(curve, index))) ** (1 / alpha);
    stress = sigmaE / (1 - c + c * Math.max(n, 2)) ** s;
  } else {
    const alpha = finiteNumber(curve.alpha_f, `${path}.alpha_f`);
    const sigma0 = finiteNumber(curve.sigma0, `${path}.sigma0`);
    const power = finiteNumber(curve.power, `${path}.power`);
    stress =
      sigma0 *
      (-Math.log(probability(curve, index))) ** (power / alpha) *
      n ** -power;
  }

  if (!Number.isFinite(stress) || stress <= 0) {
    throw new Error(`${path} produces an invalid stress at N=${n}.`);
  }
  return stress;
}

export function expandSnCurveDocument(document) {
  const curves = snCurvesFromDocument(document);
  if (curves.length === 0) {
    throw new Error("S-N curve JSON must contain a non-empty SN_curves array.");
  }

  return curves.flatMap((curve, index) => {
    const metadata = curveMetadata(curve, index);
    const stressRatio = finiteNumber(
      curve.stress_ratio,
      `SN_curves[${index}].stress_ratio`
    );
    return CLD_CYCLE_LEVELS.map((cycles) => ({
      ...metadata,
      stress_ratio: stressRatio,
      cycles_to_failure: cycles,
      stress_max: evaluateSnCurve(curve, cycles, index),
    }));
  });
}

export function validateSncRows(rows) {
  if (!Array.isArray(rows) || rows.length === 0) return rows;

  const availableColumns = new Set(rows.flatMap((row) => Object.keys(row)));
  const present = SNC_METADATA_COLUMNS.filter((column) =>
    availableColumns.has(column)
  );
  if (present.length === 0) return rows;

  const missing = SNC_METADATA_COLUMNS.filter(
    (column) => !availableColumns.has(column)
  );
  if (missing.length > 0) {
    throw new Error(
      `An enriched SNC CSV must contain all metadata columns; missing: ${missing.join(
        ", "
      )}.`
    );
  }

  const directions = new Set();
  const curvesByRatio = new Map();
  rows.forEach((row, index) => {
    const line = index + 2;
    const type = normalizedLoadType(row.type, `SNC CSV line ${line}: type`);
    let angle = null;
    if (type === "shear") {
      if (row.angle !== null && row.angle !== undefined && row.angle !== "") {
        throw new Error(
          `SNC CSV line ${line}: angle must be empty for shear curves.`
        );
      }
    } else {
      angle = Number(row.angle);
      if (!Number.isFinite(angle)) {
        throw new Error(`SNC CSV line ${line}: angle must be a finite number.`);
      }
    }

    const model = normalizedModel(row.model, `SNC CSV line ${line}: model`);
    const confidence = Number(row.confidence_interval);
    if (!Number.isFinite(confidence)) {
      throw new Error(
        `SNC CSV line ${line}: confidence_interval must be finite.`
      );
    }
    const probability = confidence > 1 ? confidence / 100 : confidence;
    if (!(probability > 0 && probability < 1)) {
      throw new Error(
        `SNC CSV line ${line}: confidence_interval must represent a probability strictly between 0 and 1.`
      );
    }

    const stressRatio = Number(row.stress_ratio);
    if (!Number.isFinite(stressRatio)) {
      throw new Error(
        `SNC CSV line ${line}: stress_ratio must be a finite number.`
      );
    }

    const direction = JSON.stringify([type, angle]);
    directions.add(direction);
    const curveMetadataKey = JSON.stringify([type, angle, model, confidence]);
    const existing = curvesByRatio.get(stressRatio);
    if (existing !== undefined && existing !== curveMetadataKey) {
      throw new Error(
        `SNC CSV contains conflicting metadata for R=${stressRatio}.`
      );
    }
    curvesByRatio.set(stressRatio, curveMetadataKey);
  });

  if (directions.size !== 1) {
    throw new Error(
      "CLD requires S-N curves for one load type and one angle; the uploaded SNC CSV contains multiple material directions."
    );
  }
  return rows;
}

export async function parseSncFile(file, parseCsv) {
  const isJson =
    file?.type === "application/json" ||
    String(file?.name || "")
      .toLowerCase()
      .endsWith(".json");
  if (!isJson) {
    const parsed = await parseCsv(file);
    validateSncRows(parsed.data);
    return parsed;
  }

  let document;
  try {
    document = JSON.parse(await file.text());
  } catch (error) {
    throw new Error(`S-N curve file is not valid JSON: ${error.message}`);
  }
  const data = expandSnCurveDocument(document);
  validateSncRows(data);
  return { data, errors: [], meta: {} };
}

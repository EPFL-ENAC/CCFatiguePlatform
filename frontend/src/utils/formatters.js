const SUPERSCRIPT_DIGITS = {
  "-": "⁻",
  0: "⁰",
  1: "¹",
  2: "²",
  3: "³",
  4: "⁴",
  5: "⁵",
  6: "⁶",
  7: "⁷",
  8: "⁸",
  9: "⁹",
};

export function toSuperscript(value) {
  return String(value)
    .split("")
    .map((char) => SUPERSCRIPT_DIGITS[char] ?? char)
    .join("");
}

export function formatPowerOfTen(exponent) {
  return `10${toSuperscript(exponent)}`;
}

export function formatScientificBase10(value, fractionDigits = 1) {
  const num = Number(value);
  if (!Number.isFinite(num)) return String(value);
  if (num === 0) return "0";

  const sign = num < 0 ? "-" : "";
  let exponent = Math.floor(Math.log10(Math.abs(num)));
  let mantissa = Math.abs(num) / 10 ** exponent;
  let roundedMantissa = Number(mantissa.toFixed(fractionDigits));

  if (roundedMantissa >= 10) {
    roundedMantissa /= 10;
    exponent += 1;
  }

  return `${sign}${roundedMantissa.toFixed(
    fractionDigits
  )} × ${formatPowerOfTen(exponent)}`;
}

export function formatLogTick(value) {
  const num = Number(value);
  if (!Number.isFinite(num) || num <= 0) return "";

  const exponent = Math.log10(num);
  const roundedExponent = Math.round(exponent);

  return Math.abs(exponent - roundedExponent) < 1e-8
    ? formatPowerOfTen(roundedExponent)
    : "";
}

export function formatNormalizedTick(value) {
  const num = Number(value);
  if (!Number.isFinite(num)) return String(value);
  if (Math.abs(num) < 1e-10) return "0";
  return num.toFixed(1);
}

function trimFixedNumber(value, fractionDigits = 2) {
  return Number(value.toFixed(fractionDigits)).toString();
}

export function formatCycleCountTick(value, options = {}) {
  const { forceScientific = false } = options;
  const num = Number(value);
  if (!Number.isFinite(num)) return String(value);
  if (num === 0) return "0";

  const sign = num < 0 ? "-" : "";
  const absValue = Math.abs(num);

  if (forceScientific || absValue >= 1000) {
    let exponent = Math.floor(Math.log10(absValue));
    let mantissa = absValue / 10 ** exponent;
    mantissa = Number(mantissa.toFixed(2));

    if (mantissa >= 10) {
      mantissa /= 10;
      exponent += 1;
    }

    if (exponent === 0) {
      return `${sign}${trimFixedNumber(mantissa, 2)}`;
    }

    return `${sign}${trimFixedNumber(mantissa, 2)}e${exponent}`;
  }

  return Number.isInteger(num) ? String(num) : trimFixedNumber(num, 2);
}

export function formatAxisTick(value, options = {}) {
  const {
    cycleCount = false,
    log = false,
    normalized = false,
    scaleExponent = null,
    forceScientific = false,
  } = options;
  const num = Number(value);

  if (!Number.isFinite(num)) return String(value);
  if (log) return formatLogTick(num);
  if (normalized) return formatNormalizedTick(num);
  if (cycleCount) return formatCycleCountTick(num, { forceScientific });
  if (num === 0) return "0";

  if (scaleExponent !== null && Number.isFinite(scaleExponent)) {
    return (num / 10 ** scaleExponent).toFixed(2);
  }

  const absValue = Math.abs(num);
  if (forceScientific) {
    return formatScientificBase10(num, absValue < 1 ? 2 : 1);
  }

  if (absValue >= 1000) return formatScientificBase10(num, 1);
  if (absValue >= 100) return num.toFixed(0);
  if (absValue >= 10) return num.toFixed(1);
  return num.toFixed(2);
}

export const formatTick = (value) => formatAxisTick(value);

export function formatAxisName(name, scaleExponent = null) {
  if (scaleExponent === null || !Number.isFinite(scaleExponent)) return name;
  return `${name} (×${formatPowerOfTen(scaleExponent)})`;
}

export function getSeriesDimensionValues(series, dimension = 0) {
  return (series || [])
    .flatMap((seriesItem) => seriesItem?.data || [])
    .map((point) => {
      if (Array.isArray(point)) return Number(point[dimension]);
      if (Array.isArray(point?.value)) return Number(point.value[dimension]);
      return dimension === 0 ? Number(point) : Number.NaN;
    })
    .filter(Number.isFinite);
}

export function getAxisScaleExponent(values, minValue = null, maxValue = null) {
  const dataCandidates = (values || [])
    .map(Number)
    .filter(Number.isFinite)
    .map(Math.abs)
    .filter((value) => value > 0);

  const boundCandidates = [minValue, maxValue]
    .map(Number)
    .filter(Number.isFinite)
    .map(Math.abs)
    .filter((value) => value > 0);

  const candidates = dataCandidates.length ? dataCandidates : boundCandidates;
  if (!candidates.length) return null;

  const maxAbs = Math.max(...candidates);
  if (maxAbs < 1) return Math.floor(Math.log10(maxAbs));

  const maxAxisAbs = Math.max(maxAbs, ...boundCandidates);
  if (maxAxisAbs >= 1000) return Math.floor(Math.log10(maxAxisAbs));

  return null;
}

export function shouldUseConsistentScientificNotation(
  values,
  minValue = null,
  maxValue = null,
  options = {}
) {
  const {
    cycleCount = false,
    log = false,
    normalized = false,
    scaleExponent = null,
  } = options;

  if (log || normalized) return false;
  if (scaleExponent !== null && Number.isFinite(scaleExponent)) return false;

  const candidates = [...(values || []), minValue, maxValue]
    .map(Number)
    .filter(Number.isFinite)
    .map(Math.abs)
    .filter((value) => value > 0);

  if (!candidates.length) return false;

  const minAbs = Math.min(...candidates);
  const maxAbs = Math.max(...candidates);

  if (cycleCount) {
    return maxAbs >= 1000 && minAbs < 1000;
  }

  const hasScientificDefault = candidates.some((value) => value >= 1000);
  const hasPlainDefault = candidates.some((value) => value < 1000);

  return hasScientificDefault && hasPlainDefault;
}

// --------------------------------------------------
// NICE‑TICK LOGIC (1‑2‑5 progression)
// --------------------------------------------------

const AXIS_EPS = 1e-10;
const NICE_STEPS = [1, 2, 5, 10];

function cleanAxisNumber(value) {
  if (!Number.isFinite(value)) return value;
  if (Math.abs(value) < AXIS_EPS) return 0;
  return Number(value.toPrecision(12));
}

function autoTargetTicks(minVal, maxVal) {
  // fallback for invalid input
  if (!Number.isFinite(minVal) || !Number.isFinite(maxVal) || minVal === maxVal)
    return 5;

  // ensure minVal ≤ maxVal
  if (minVal > maxVal) [minVal, maxVal] = [maxVal, minVal];

  // scale ratio (≥ 1)
  const ratio = Math.abs(maxVal) / Math.max(Math.abs(minVal), Number.EPSILON);

  if (ratio < 1.2) return 4; // very narrow range
  if (ratio < 2) return 6; // 1–2 ×
  if (ratio < 5) return 8; // 2–5 ×
  if (ratio < 10) return 10; // 5–10 ×
  if (ratio < 100) return 12; // 10–100 ×

  return 5; // huge ranges → few ticks
}

function niceTickStep(maxVal, minVal) {
  // fallback for invalid input
  if (!Number.isFinite(maxVal) || !Number.isFinite(minVal)) return 1;
  if (maxVal <= minVal) {
    const absValue = Math.abs(maxVal || minVal);
    if (absValue === 0) return 1;
    return 10 ** Math.floor(Math.log10(absValue));
  }
  const targetTicks = autoTargetTicks(minVal, maxVal);
  const raw = (maxVal - minVal) / targetTicks; // raw spacing
  const exponent = Math.floor(Math.log10(raw));
  const base = 10 ** exponent;
  const fraction = raw / base;
  const niceFraction = NICE_STEPS.find((s) => fraction <= s) ?? 10;
  return niceFraction * base;
}

function majorBoundaryStep(values) {
  const maxAbs = Math.max(
    ...(values || [])
      .map((value) => Math.abs(value))
      .filter((value) => value > 0)
  );

  if (!Number.isFinite(maxAbs) || maxAbs === 0) return 1;

  return niceTickStep(maxAbs, 0);
}

export function computeAxisBoundsFromValues(values, options = {}) {
  const { extraMin = false, extraMax = false } = options;
  const finiteValues = (values || []).map(Number).filter(Number.isFinite);

  if (!finiteValues.length) {
    return { min: null, max: null, interval: null };
  }

  const maxVal = Math.max(...finiteValues);
  const minVal = Math.min(...finiteValues);
  const step = majorBoundaryStep(finiteValues);
  const tolerance = Math.abs(step) * AXIS_EPS;
  const snappedMin =
    Math.floor((minVal + tolerance) / step) * step - (extraMin ? step : 0);
  const snappedMax =
    Math.ceil((maxVal - tolerance) / step) * step + (extraMax ? step : 0);

  return {
    min: cleanAxisNumber(snappedMin),
    max: cleanAxisNumber(snappedMax),
    interval: cleanAxisNumber(step),
  };
}

function axisMax(values) {
  const { max } = computeAxisBoundsFromValues(values);
  return max;
}

function axisMin(values) {
  const { min } = computeAxisBoundsFromValues(values);
  return min;
}

function axisMaxPlus(values) {
  const { max } = computeAxisBoundsFromValues(values, { extraMax: true });
  return max;
}

function axisMinMinus(values) {
  const { min } = computeAxisBoundsFromValues(values, { extraMin: true });
  return min;
}

function logAxisMin(values, fallbackMin) {
  const vals = (values || [])
    .map(Number)
    .filter((v) => Number.isFinite(v) && v > 0);
  if (!vals.length) return fallbackMin;
  return 10 ** Math.floor(Math.log10(Math.min(...vals)));
}

function logAxisMax(values, fallbackMax) {
  const vals = (values || [])
    .map(Number)
    .filter((v) => Number.isFinite(v) && v > 0);
  if (!vals.length) return fallbackMax;
  return 10 ** Math.ceil(Math.log10(Math.max(...vals)));
}

export function snapLogAxisMin(value) {
  const num = Number(value);
  if (!Number.isFinite(num) || num <= 0) return null;
  return 10 ** Math.floor(Math.log10(num));
}

export function snapLogAxisMax(value) {
  const num = Number(value);
  if (!Number.isFinite(num) || num <= 0) return null;
  return 10 ** Math.ceil(Math.log10(num));
}

export function computeLogAxisBoundsFromValues(
  values,
  fallbackMin = 1e-10,
  fallbackMax = 1e-1
) {
  const min = logAxisMin(values, fallbackMin);
  const max = logAxisMax(values, fallbackMax);

  return {
    min,
    max,
    splitNumber: Math.max(3, Math.round(Math.log10(max / min)) + 1),
  };
}

// --------------------------------------------------
// PUBLIC API
// --------------------------------------------------

export function computeYAxisMax(series) {
  return axisMax(getSeriesDimensionValues(series, 1));
}

export function computeXAxisMax(series) {
  return axisMax(getSeriesDimensionValues(series, 0));
}

export function computeYAxisMin(series) {
  return axisMin(getSeriesDimensionValues(series, 1));
}

export function computeXAxisMin(series) {
  return axisMin(getSeriesDimensionValues(series, 0));
}

// --------------------------------------------------
// LOG-SCALE LIMITS  (one extra decade if needed)
// --------------------------------------------------

/**
 * Compute log-scale axis limits so that:
 *   • if the lowest point is in the lower half of its decade → extend one decade down
 *   • if the highest point sits in the upper half of its decade → extend one decade up
 *
 * @param {Array}  series       Array of series objects [{ data: [[x,y], …] }, …]
 * @param {number} dim          1 = Y values, 0 = X values
 * @param {number} fallbackMin  Returned when series has no positive/finite values
 * @param {number} fallbackMax  Returned when series has no positive/finite values
 * @returns {{ min:number, max:number, splitNumber:number }}
 */
function _logLimits(series, dim = 1, fallbackMin = 1e-10, fallbackMax = 1e-1) {
  // Extract positive, finite values along the chosen dimension
  const vals = getSeriesDimensionValues(series, dim).filter((v) => v > 0);

  if (!vals.length) {
    return { min: fallbackMin, max: fallbackMax, splitNumber: 4 };
  }

  const minVal = Math.min(...vals);
  const maxVal = Math.max(...vals);

  // ----- lower bound --------------------------------------------------------
  const tickLo = 10 ** Math.floor(Math.log10(minVal)); // 10^n  ≤  minVal
  const tickHi = tickLo * 10; // 10^(n+1)
  const fracLo = (minVal - tickLo) / (tickHi - tickLo);
  const axisMin = fracLo < 0.5 ? tickLo / 10 : tickLo; // extend if < 50 %

  // ----- upper bound --------------------------------------------------------
  const tickUp = 10 ** Math.ceil(Math.log10(maxVal)); // 10^m  ≥  maxVal
  const tickDn = tickUp / 10;
  const fracHi = (tickUp - maxVal) / (tickUp - tickDn);
  const axisMax = fracHi > 0.5 ? tickUp * 10 : tickUp; // extend if > 50 %

  return {
    min: axisMin,
    max: axisMax,
    splitNumber: Math.max(3, Math.round(Math.log10(axisMax / axisMin)) + 1),
  };
}

export const computeLogYAxisLimits = (series, tMin, tMax) =>
  _logLimits(series, 1, tMin, tMax);

export const computeLogXAxisLimits = (series, tMin, tMax) =>
  _logLimits(series, 0, tMin, tMax);

// --------------------------------------------------
// VALUE FORMATTING (unchanged)
// --------------------------------------------------

const fixed = (v, d = 2) => {
  const n = Number(v);
  return isFinite(n) ? n.toFixed(d) : "-";
};
const sci = (v, d = 2) => {
  const n = Number(v);
  return isFinite(n) ? n.toExponential(d) : "-";
};

export const format0 = (v) => fixed(v, 0);
export const format2 = (v) => fixed(v, 2);
export const format3 = (v) => fixed(v, 3);
export const format4 = (v) => fixed(v, 4);
export const format5 = (v) => fixed(v, 5);
export const formatScientific2 = (v) => sci(v, 2);

// --------------------------------------------------
// PUBLIC API  (+1 nice-tick versions)
// --------------------------------------------------

export function computeYAxisMax2(series) {
  return axisMaxPlus(getSeriesDimensionValues(series, 1));
}

export function computeXAxisMax2(series) {
  return axisMaxPlus(getSeriesDimensionValues(series, 0));
}

export function computeYAxisMin2(series) {
  return axisMinMinus(getSeriesDimensionValues(series, 1));
}

export function computeXAxisMin2(series) {
  return axisMinMinus(getSeriesDimensionValues(series, 0));
}

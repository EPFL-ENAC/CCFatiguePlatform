export const formatTick = (value) => {
  const num = Number(value);
  if (!isFinite(num)) return value;
  return num.toLocaleString(undefined, {
    useGrouping: false,
    maximumFractionDigits: 3,
  });
};

// --------------------------------------------------
// NICE‑TICK LOGIC (1‑2‑5 progression)
// --------------------------------------------------

const EPS = Number.EPSILON;
const NICE_STEPS = [1, 2, 5, 10];

// --------------------------------------------------
// NEW TICK LOGIC: based on max/min ratio
// --------------------------------------------------

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
  if (!Number.isFinite(maxVal) || !Number.isFinite(minVal) || maxVal <= minVal)
    return 1;
  const targetTicks = autoTargetTicks(minVal, maxVal);
  const raw = maxVal / targetTicks; // raw spacing
  const exponent = Math.floor(Math.log10(raw));
  const base = 10 ** exponent;
  const fraction = raw / base;
  const niceFraction = NICE_STEPS.find((s) => fraction <= s) ?? 10;
  return niceFraction * base;
}

function axisMax(values) {
  if (!values.length) return null;
  const maxVal = Math.max(...values);
  const minVal = Math.min(...values);
  const step = niceTickStep(maxVal, minVal);
  let maxAxis = Math.ceil(maxVal / step) * step;
  // avoid last tick exactly matching max
  if (Math.abs(maxVal - maxAxis) < EPS) maxAxis += step;
  return maxAxis;
}

function axisMin(values) {
  if (!values.length) return null;
  const maxVal = Math.max(...values);
  const minVal = Math.min(...values);
  const step = niceTickStep(maxVal, minVal);
  return Math.floor(minVal / step) * step;
}

// --------------------------------------------------
// PUBLIC API
// --------------------------------------------------

export function computeYAxisMax(series) {
  const vals = series
    .flatMap((s) => s?.data || [])
    .map(([, y]) => y)
    .filter(Number.isFinite);
  return axisMax(vals);
}

export function computeXAxisMax(series) {
  const vals = series
    .flatMap((s) => s?.data || [])
    .map(([x]) => x)
    .filter(Number.isFinite);
  return axisMax(vals);
}

export function computeYAxisMin(series) {
  const vals = series
    .flatMap((s) => s?.data || [])
    .map(([, y]) => y)
    .filter(Number.isFinite);
  return axisMin(vals);
}

export function computeXAxisMin(series) {
  const vals = series
    .flatMap((s) => s?.data || [])
    .map(([x]) => x)
    .filter(Number.isFinite);
  return axisMin(vals);
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
  const vals = series
    .flatMap((s) => s?.data || [])
    .map((pt) => pt[dim])
    .filter((v) => v > 0 && isFinite(v));

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

function axisMaxPlus(values) {
  if (!values.length) return null;
  const maxVal = Math.max(...values);
  const minVal = Math.min(...values);
  const step = niceTickStep(maxVal, minVal);
  // → first nice tick ≥ maxVal, then one extra step
  let maxAxis = Math.ceil(maxVal / step) * step + step;
  // edge-case: data already lies exactly on a tick
  if (Math.abs(maxVal - (maxAxis - step)) < EPS) maxAxis += step;
  return maxAxis;
}

/**
 * Internal: same logic as axisMin, but moves one more step inwards.
 */
function axisMinMinus(values) {
  if (!values.length) return null;
  const maxVal = Math.max(...values);
  const minVal = Math.min(...values);
  const step = niceTickStep(maxVal, minVal);
  // ← first nice tick ≤ minVal, then one extra step
  return Math.floor(minVal / step) * step - step;
}

// --------------------------------------------------
// PUBLIC API  (+1 nice-tick versions)
// --------------------------------------------------

export function computeYAxisMax2(series) {
  const vals = series
    .flatMap((s) => s?.data || [])
    .map(([, y]) => y)
    .filter(Number.isFinite);
  return axisMaxPlus(vals);
}

export function computeXAxisMax2(series) {
  const vals = series
    .flatMap((s) => s?.data || [])
    .map(([x]) => x)
    .filter(Number.isFinite);
  return axisMaxPlus(vals);
}

export function computeYAxisMin2(series) {
  const vals = series
    .flatMap((s) => s?.data || [])
    .map(([, y]) => y)
    .filter(Number.isFinite);
  return axisMinMinus(vals);
}

export function computeXAxisMin2(series) {
  const vals = series
    .flatMap((s) => s?.data || [])
    .map(([x]) => x)
    .filter(Number.isFinite);
  return axisMinMinus(vals);
}

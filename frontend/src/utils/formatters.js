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

/**
 * Return a "nice" tick spacing so that ~targetTicks cover [0, maxVal].
 */
function autoTargetTicks(maxVal) {
  if (maxVal <= 0) return 5; // fallback

  if (maxVal < 3) return 10; // 0-3  → step 0.1
  if (maxVal < 30) return 8; // 3-30 → step 1

  // 30-300 mm → about 1 tick every 10 mm   (≈14 ticks at 140 mm)
  if (maxVal < 300) return Math.min(15, Math.ceil(maxVal / 10));

  return 5; // over 300 → standard scale
}

function niceTickStep(maxVal, targetTicks = autoTargetTicks(maxVal)) {
  if (maxVal <= 0) return 1;
  const raw = maxVal / targetTicks; // rough spacing
  const exponent = Math.floor(Math.log10(raw));
  const base = 10 ** exponent;
  const fraction = raw / base;
  const niceFraction = NICE_STEPS.find((s) => fraction <= s) ?? 10;
  return niceFraction * base;
}

function axisMax(values) {
  if (!values.length) return null;
  const maxVal = Math.max(...values);
  const step = niceTickStep(maxVal);
  let maxAxis = Math.ceil(maxVal / step) * step;
  // Ensure the axis max is a bit larger than the max value
  // This avoids the last tick being exactly at the max value
  if (Math.abs(maxVal - maxAxis) < EPS) maxAxis += step;
  return maxAxis;
}

function axisMin(values) {
  if (!values.length) return null;
  const minVal = Math.min(...values);
  // Use the same step as max for visual consistency
  const step = niceTickStep(Math.max(...values.map(Math.abs)));
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
// LOG‑SCALE Y AXIS LIMITS (unchanged)
// --------------------------------------------------

export function computeLogYAxisLimits(
  series,
  targetMin = 1e-10,
  targetMax = 1e-1
) {
  const vals = series
    .flatMap((s) => s?.data || [])
    .map(([, y]) => y)
    .filter((v) => v > 0 && isFinite(v));
  if (!vals.length) return { min: targetMin, max: targetMax, splitNumber: 4 };
  const minExp = Math.floor(Math.log10(Math.min(...vals)));
  const maxExp = Math.ceil(Math.log10(Math.max(...vals)));
  return {
    min: 10 ** minExp,
    max: 10 ** maxExp,
    splitNumber: Math.max(3, maxExp - minExp + 1),
  };
}

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

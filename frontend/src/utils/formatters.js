export const formatTick = (value) => {
  const num = Number(value);
  if (!isFinite(num)) return value;

  // No thousands separator, max 2 decimals
  return num.toLocaleString(undefined, {
    useGrouping: false, // removes the thousands separator
    maximumFractionDigits: 3,
  });
};

export function computeYAxisMax(series) {
  const allYValues = series
    .flatMap((s) => s?.data || [])
    .map(([, y]) => y)
    .filter((v) => typeof v === "number" && isFinite(v));

  if (!allYValues.length) return null;

  const maxVal = Math.max(...allYValues);

  // Dynamic calculation of the "step" based on the order of magnitude
  const magnitude = Math.pow(10, Math.floor(Math.log10(maxVal)));
  const tickStep = magnitude / 2;

  // Round up to the next convenient tick
  return Math.ceil(maxVal / tickStep) * tickStep;
}

export function computeXAxisMax(series) {
  const allXValues = series
    .flatMap((s) => s?.data || [])
    .map(([x]) => x)
    .filter((v) => typeof v === "number" && isFinite(v));

  if (!allXValues.length) return null;

  const maxVal = Math.max(...allXValues);

  // Dynamic calculation of the "step" based on the order of magnitude
  const magnitude = Math.pow(10, Math.floor(Math.log10(maxVal)));
  const tickStep = magnitude / 2;

  // Round up to the next convenient tick
  return Math.ceil(maxVal / tickStep) * tickStep;
}
export function computeYAxisMin(series) {
  const allYValues = series
    .flatMap((s) => s?.data || [])
    .map(([, y]) => y)
    .filter((v) => typeof v === "number" && isFinite(v));

  if (!allYValues.length) return null;

  const minVal = Math.min(...allYValues);

  // Compute the order of magnitude of the minimum value
  const magnitude = Math.pow(10, Math.floor(Math.log10(Math.abs(minVal) || 1)));
  const tickStep = magnitude / 2;

  // Round down to the nearest convenient tick step
  return Math.floor(minVal / tickStep) * tickStep;
}

export function computeXAxisMin(series) {
  const allXValues = series
    .flatMap((s) => s?.data || [])
    .map(([x]) => x)
    .filter((v) => typeof v === "number" && isFinite(v));

  if (!allXValues.length) return null;

  const minVal = Math.min(...allXValues);

  // Compute the order of magnitude of the minimum value
  const magnitude = Math.pow(10, Math.floor(Math.log10(Math.abs(minVal) || 1)));
  const tickStep = magnitude / 2;

  // Round down to the nearest convenient tick step
  return Math.floor(minVal / tickStep) * tickStep;
}

export function computeLogYAxisLimits(
  series,
  targetMin = 1e-10,
  targetMax = 1e-1
) {
  const allYValues = series
    .flatMap((s) => s?.data || [])
    .map(([, y]) => y)
    .filter((v) => typeof v === "number" && isFinite(v) && v > 0);

  if (!allYValues.length) {
    return { min: targetMin, max: targetMax };
  }

  const minVal = Math.min(...allYValues);
  const maxVal = Math.max(...allYValues);

  const minExp = Math.floor(Math.log10(minVal));
  const maxExp = Math.ceil(Math.log10(maxVal));

  // Add a helper to compute splitNumber dynamically, e.g. 4 or 5 ticks by default
  const splitNumber = Math.max(3, maxExp - minExp + 1);

  return {
    min: Math.pow(10, minExp),
    max: Math.pow(10, maxExp),
    splitNumber,
  };
}

export const formatFixed = (value, decimals = 2) => {
  const num = Number(value);
  return isFinite(num) ? num.toFixed(decimals) : "-";
};

export const formatScientific = (value, decimals = 2) => {
  const num = Number(value);
  return isFinite(num) ? num.toExponential(decimals) : "-";
};

export const format0 = (v) => formatFixed(v, 0);
export const format2 = (v) => formatFixed(v, 2);
export const format3 = (v) => formatFixed(v, 3);
export const format4 = (v) => formatFixed(v, 4);
export const format5 = (v) => formatFixed(v, 5);

export const formatScientific2 = (v) => formatScientific(v, 2);

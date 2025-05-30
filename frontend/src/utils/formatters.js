import { round } from "lodash";

export const formatNumber3 = (value) => {
  const num = Number(value);
  if (isNaN(num)) return value;

  const abs = Math.abs(num);

  if (abs >= 1000 || (abs > 0 && abs < 0.001)) {
    // power calculated manually
    const exponent = Math.floor(Math.log10(abs));
    const mantissa = num / Math.pow(10, exponent);
    const fixedMantissa = mantissa.toFixed(3);
    return `${fixedMantissa}e${exponent >= 0 ? "+" : ""}${exponent}`;
  }

  return num.toLocaleString(undefined, {
    minimumFractionDigits: 3,
    maximumFractionDigits: 3,
  });
};

export const formatNumber4 = (value) => {
  const num = Number(value);
  if (isNaN(num)) return value;

  const abs = Math.abs(num);

  if (abs >= 10000 || (abs > 0 && abs < 0.0001)) {
    // power calculated manually
    const exponent = Math.floor(Math.log10(abs));
    const mantissa = num / Math.pow(10, exponent);
    const fixedMantissa = mantissa.toFixed(4);
    return `${fixedMantissa}e${exponent >= 0 ? "+" : ""}${exponent}`;
  }

  return num.toLocaleString(undefined, {
    minimumFractionDigits: 4,
    maximumFractionDigits: 4,
  });
};

export const formatNumber5 = (value) => {
  const num = Number(value);
  if (isNaN(num)) return value;

  const abs = Math.abs(num);

  if (abs >= 100000 || (abs > 0 && abs < 0.00001)) {
    // power calculated manually
    const exponent = Math.floor(Math.log10(abs));
    const mantissa = num / Math.pow(10, exponent);
    const fixedMantissa = mantissa.toFixed(5);
    return `${fixedMantissa}e${exponent >= 0 ? "+" : ""}${exponent}`;
  }

  return num.toLocaleString(undefined, {
    minimumFractionDigits: 5,
    maximumFractionDigits: 5,
  });
};

export const formatNumber2 = (value) =>
  Math.abs(value) >= 1000 || (Math.abs(value) < 0.001 && value !== 0)
    ? round(value, 2).toExponential()
    : round(value, 2);

export const formatTick = (value) => {
  const num = Number(value);
  if (!isFinite(num)) return value;

  // No thousands separator, max 2 decimals
  return num.toLocaleString(undefined, {
    useGrouping: false, // ⬅️ removes the thousands separator
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
  targetMin = 1e-6,
  targetMax = 1e-2
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

import { round } from "lodash";

export const formatNumber3 = (value) => {
  const rounded = round(value, 3);
  return Math.abs(value) >= 1000 || (Math.abs(value) < 0.001 && value !== 0)
    ? Number(rounded).toExponential(3) // force 3 decimals in exponential format
    : rounded;
};

export const formatNumber2 = (value) =>
  Math.abs(value) >= 1000 || (Math.abs(value) < 0.001 && value !== 0)
    ? round(value, 2).toExponential()
    : round(value, 2);

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

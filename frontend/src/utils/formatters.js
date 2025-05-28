import { round } from "lodash";

export const formatNumber3 = (value) =>
  Math.abs(value) >= 1000 || (Math.abs(value) < 0.001 && value !== 0)
    ? round(value, 3).toExponential()
    : round(value, 3);

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

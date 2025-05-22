import { round } from "lodash";

export const formatNumber3 = (value) =>
  Math.abs(value) >= 1000 || (Math.abs(value) < 0.001 && value !== 0)
    ? round(value, 3).toExponential()
    : round(value, 3);

export const formatNumber2 = (value) =>
  Math.abs(value) >= 1000 || (Math.abs(value) < 0.001 && value !== 0)
    ? round(value, 2).toExponential()
    : round(value, 2);

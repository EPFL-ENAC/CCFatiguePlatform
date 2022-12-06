import { round } from "lodash";

export const formatNumber = (value) =>
  Math.abs(value) >= 1000 || (Math.abs(value) < 0.001 && value !== 0)
    ? round(value, 3).toExponential()
    : round(value, 3);

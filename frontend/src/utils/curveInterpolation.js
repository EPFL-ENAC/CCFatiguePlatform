/**
 * Interpolate y at an arbitrary x along a curve given as [x, y] pairs,
 * using log-log interpolation between the two bracketing points (falling
 * back to linear interpolation on either axis when values aren't strictly
 * positive). Clamps to the first/last point outside the curve's range.
 *
 * Generalizes the log-log interpolation pattern used for CLD isolife
 * curves (see CldAnalysis.vue's interpolateSigmaMax) so it can be reused
 * for other discrete cycle-count grids (S-N curves, Tsai-Hill/Tsai-Wu
 * per-N coefficients, etc).
 */
export function interpolateLogLog(points, x) {
  const entries = points
    .filter(
      ([px, py]) =>
        px != null && py != null && Number.isFinite(px) && Number.isFinite(py)
    )
    .sort((a, b) => a[0] - b[0]);

  if (entries.length === 0) return null;
  if (entries.length === 1) return entries[0][1];
  if (x <= entries[0][0]) return entries[0][1];
  if (x >= entries[entries.length - 1][0])
    return entries[entries.length - 1][1];

  for (let i = 0; i < entries.length - 1; i++) {
    const [x1, y1] = entries[i];
    const [x2, y2] = entries[i + 1];
    if (x >= x1 && x <= x2) {
      const t =
        x1 > 0 && x2 > 0 && x > 0
          ? (Math.log(x) - Math.log(x1)) / (Math.log(x2) - Math.log(x1))
          : (x - x1) / (x2 - x1);
      if (y1 > 0 && y2 > 0) {
        return Math.exp(Math.log(y1) + t * (Math.log(y2) - Math.log(y1)));
      }
      return y1 + t * (y2 - y1);
    }
  }
  return null;
}

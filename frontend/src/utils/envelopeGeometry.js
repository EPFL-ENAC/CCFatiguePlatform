/**
 * Shared geometry helpers for 2D quadratic failure envelopes (Tsai-Hill,
 * Tsai-Wu/FTPF) written as A(x1)*x2^2 + B(x1)*x2 + C(x1) = 0 - i.e. solved
 * for x2 (e.g. sigma2) as a function of x1 (e.g. sigma1) at each sample
 * point, since both criteria's envelopes are quadratic in each stress
 * component individually rather than a simple parametric ellipse (Tsai-Wu's
 * envelope isn't centered at the origin because of the linear F1/F2 terms).
 */

/**
 * Sample a quadratic-in-x2 implicit curve across an x1 range and build a
 * closed boundary polygon (upper branch left-to-right, then lower branch
 * right-to-left). Points where the quadratic has no real root (curve not
 * present at that x1) are skipped.
 * @param {(x1: number) => [number, number, number]} getABC
 * @param {number} x1Min
 * @param {number} x1Max
 * @param {number} samples
 * @returns {[number, number][]} closed polygon, or [] if never real
 */
export function buildQuadraticEnvelope(getABC, x1Min, x1Max, samples = 200) {
  const upper = [];
  const lower = [];
  for (let i = 0; i <= samples; i++) {
    const x1 = x1Min + ((x1Max - x1Min) * i) / samples;
    const [a, b, c] = getABC(x1);
    if (a === 0 || !Number.isFinite(a)) continue;
    const discriminant = b * b - 4 * a * c;
    if (discriminant < 0) continue;
    const sqrtD = Math.sqrt(discriminant);
    const x2Plus = (-b + sqrtD) / (2 * a);
    const x2Minus = (-b - sqrtD) / (2 * a);
    upper.push([x1, Math.max(x2Plus, x2Minus)]);
    lower.push([x1, Math.min(x2Plus, x2Minus)]);
  }
  if (upper.length === 0) return [];
  return [...upper, ...lower.reverse()];
}

/**
 * Whether a point is inside the region bounded by the same A/B/C quadratic
 * used by buildQuadraticEnvelope (valid since A > 0 for both Tsai-Hill and
 * Tsai-Wu's F22 term, making it an upward parabola in x2 at any fixed x1 -
 * the interior is where the parabola dips below zero, i.e. between its two
 * roots).
 * @param {(x1: number) => [number, number, number]} getABC
 * @param {number} x1
 * @param {number} x2
 */
export function isInsideQuadraticEnvelope(getABC, x1, x2) {
  const [a, b, c] = getABC(x1);
  return a * x2 * x2 + b * x2 + c <= 0;
}

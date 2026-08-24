<template>
  <v-card outlined class="mt-4">
    <v-card-title class="text-subtitle-1">
      FTPF (Tsai-Wu) — dissymmetric envelope
      <v-spacer />
      <v-chip v-if="appliedPoint" :color="statusColor" text-color="white" small>
        FI = {{ formatNumber(fi) }} · reserve
        {{ formatNumber(reservePercent) }}%
      </v-chip>
    </v-card-title>
    <v-card-text>
      <v-row dense>
        <v-col cols="12" sm="3">
          <v-text-field
            v-model.number="n"
            label="Cycles N"
            type="number"
            dense
            hint="Envelope evaluated at this N"
            persistent-hint
          ></v-text-field>
        </v-col>
        <v-col cols="12" sm="3">
          <v-text-field
            v-model.number="sigma1"
            label="Applied σ1 [MPa]"
            type="number"
            dense
          ></v-text-field>
        </v-col>
        <v-col cols="12" sm="3">
          <v-text-field
            v-model.number="sigma2"
            label="Applied σ2 [MPa]"
            type="number"
            dense
          ></v-text-field>
        </v-col>
        <v-col cols="12" sm="3">
          <v-text-field
            v-model.number="sigma12"
            label="Applied σ12 [MPa]"
            type="number"
            dense
          ></v-text-field>
        </v-col>
      </v-row>

      <div v-if="!curveReady" class="text-caption grey--text">
        No data available at N = {{ n }}.
      </div>
      <envelope-chart
        v-else
        :boundary-points="boundaryPoints"
        :applied-point="appliedPoint"
        :is-inside="isInside"
        :extra-series="loadVectorSeries"
        x-axis-name="σ1 [MPa]"
        y-axis-name="σ2 [MPa]"
      ></envelope-chart>

      <div v-if="appliedPoint && k != null" class="text-caption mt-2">
        {{ loadVectorSummary }}
      </div>
    </v-card-text>
  </v-card>
</template>

<script>
import EnvelopeChart from "@/components/charts/EnvelopeChart";
import { interpolateLogLog } from "@/utils/curveInterpolation";
import {
  buildQuadraticEnvelope,
  isInsideQuadraticEnvelope,
} from "@/utils/envelopeGeometry";

export default {
  name: "FtpfEnvelope",
  components: { EnvelopeChart },
  props: {
    // Parsed FTPF FAF CSV rows: { cycles_to_failure, x, y, xc, yc, f11,
    // f22, f66, f1, f2, f12, ... }
    rows: { type: Array, default: () => [] },
  },
  data() {
    return {
      n: 1e5,
      sigma1: null,
      sigma2: null,
      sigma12: 0,
    };
  },
  computed: {
    xAtN() {
      return interpolateLogLog(
        this.rows.map((r) => [r.cycles_to_failure, r.x]),
        this.n
      );
    },
    xcAtN() {
      return interpolateLogLog(
        this.rows.map((r) => [r.cycles_to_failure, r.xc]),
        this.n
      );
    },
    f11AtN() {
      return interpolateLogLog(
        this.rows.map((r) => [r.cycles_to_failure, r.f11]),
        this.n
      );
    },
    f22AtN() {
      return interpolateLogLog(
        this.rows.map((r) => [r.cycles_to_failure, r.f22]),
        this.n
      );
    },
    f66AtN() {
      return interpolateLogLog(
        this.rows.map((r) => [r.cycles_to_failure, r.f66]),
        this.n
      );
    },
    f1AtN() {
      return interpolateLogLog(
        this.rows.map((r) => [r.cycles_to_failure, r.f1]),
        this.n
      );
    },
    f2AtN() {
      return interpolateLogLog(
        this.rows.map((r) => [r.cycles_to_failure, r.f2]),
        this.n
      );
    },
    f12AtN() {
      return interpolateLogLog(
        this.rows.map((r) => [r.cycles_to_failure, r.f12]),
        this.n
      );
    },
    curveReady() {
      return [
        this.xAtN,
        this.xcAtN,
        this.f11AtN,
        this.f22AtN,
        this.f66AtN,
        this.f1AtN,
        this.f2AtN,
        this.f12AtN,
      ].every(Number.isFinite);
    },
    // F11·σ1² + F22·σ2² + 2·F12·σ1·σ2 + F1·σ1 + F2·σ2 + F66·σ12² = 1,
    // rearranged as a quadratic in σ2: A·σ2² + B·σ2 + C = 0.
    getABC() {
      const {
        f11AtN: f11,
        f22AtN: f22,
        f66AtN: f66,
        f1AtN: f1,
        f2AtN: f2,
        f12AtN: f12,
      } = this;
      const shearTerm = f66 * this.sigma12 ** 2;
      return (sigma1) => [
        f22,
        2 * f12 * sigma1 + f2,
        f11 * sigma1 ** 2 + f1 * sigma1 + shearTerm - 1,
      ];
    },
    boundaryPoints() {
      if (!this.curveReady) return [];
      const x1Min = -1.3 * this.xcAtN;
      const x1Max = 1.3 * this.xAtN;
      return buildQuadraticEnvelope(this.getABC, x1Min, x1Max, 240);
    },
    appliedPoint() {
      if (!Number.isFinite(this.sigma1) || !Number.isFinite(this.sigma2)) {
        return null;
      }
      return [this.sigma1, this.sigma2];
    },
    isInside() {
      if (!this.curveReady || !this.appliedPoint) return null;
      return isInsideQuadraticEnvelope(this.getABC, this.sigma1, this.sigma2);
    },
    // A, B, k (strength ratio) - solved by substituting σ = k·σ_applied
    // into the failure equation.
    ftpfA() {
      if (!this.curveReady || !this.appliedPoint) return null;
      const { f11AtN: f11, f22AtN: f22, f66AtN: f66, f12AtN: f12 } = this;
      const s12 = Number.isFinite(this.sigma12) ? this.sigma12 : 0;
      return (
        f11 * this.sigma1 ** 2 +
        f22 * this.sigma2 ** 2 +
        2 * f12 * this.sigma1 * this.sigma2 +
        f66 * s12 ** 2
      );
    },
    ftpfB() {
      if (!this.curveReady || !this.appliedPoint) return null;
      return this.f1AtN * this.sigma1 + this.f2AtN * this.sigma2;
    },
    k() {
      if (this.ftpfA == null || this.ftpfB == null || this.ftpfA === 0)
        return null;
      return (
        (-this.ftpfB + Math.sqrt(this.ftpfB ** 2 + 4 * this.ftpfA)) /
        (2 * this.ftpfA)
      );
    },
    fi() {
      return this.k ? 1 / this.k : null;
    },
    reservePercent() {
      return this.k != null ? (this.k - 1) * 100 : null;
    },
    statusColor() {
      return this.k != null && this.k >= 1 ? "green" : "red";
    },
    loadVectorSeries() {
      if (!this.appliedPoint || this.k == null) return [];
      return [
        {
          id: "ftpf-load-vector",
          type: "line",
          name: "Load vector",
          data: [
            [0, 0],
            [this.sigma1 * this.k, this.sigma2 * this.k],
          ],
          showSymbol: false,
          lineStyle: { color: "#616161", type: "dashed", width: 1.5 },
          z: 1,
        },
      ];
    },
    loadVectorSummary() {
      if (!this.appliedPoint || this.k == null) return "";
      const kLabel = this.formatNumber(this.k);
      const x = this.formatNumber(this.sigma1 * this.k);
      const y = this.formatNumber(this.sigma2 * this.k);
      return `k = ${kLabel} (load-vector intersection at σ1=${x}, σ2=${y})`;
    },
  },
  methods: {
    formatNumber(value) {
      return Number.isFinite(value) ? value.toFixed(2) : "—";
    },
  },
};
</script>

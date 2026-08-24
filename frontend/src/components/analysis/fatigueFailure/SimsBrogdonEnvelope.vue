<template>
  <v-card outlined class="mt-4">
    <v-card-title class="text-subtitle-1">
      Sims-Brogdon — Tsai-Hill safety envelope
      <v-spacer />
      <v-chip v-if="statusLabel" :color="statusColor" text-color="white" small>
        {{ statusLabel }}
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
          <div class="text-caption grey--text text--darken-1">
            Applied σ12: {{ formatNumber(sigma12) }} MPa (max
            {{ formatNumber(sigma12Max) }} at this N)
          </div>
          <v-slider
            v-model="sigma12"
            :min="0"
            :max="sigma12Max"
            :step="sigma12Max / 200 || 0.1"
            dense
            hide-details
          ></v-slider>
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
        x-axis-name="σ1 [MPa]"
        y-axis-name="σ2 [MPa]"
      ></envelope-chart>
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
  name: "SimsBrogdonEnvelope",
  components: { EnvelopeChart },
  props: {
    // Parsed SimsBrogdon FAF CSV rows: { cycles_to_failure, x, y, s, ... }
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
    xCurve() {
      return this.rows.map((r) => [r.cycles_to_failure, r.x]);
    },
    yCurve() {
      return this.rows.map((r) => [r.cycles_to_failure, r.y]);
    },
    sCurve() {
      return this.rows.map((r) => [r.cycles_to_failure, r.s]);
    },
    xAtN() {
      return interpolateLogLog(this.xCurve, this.n);
    },
    yAtN() {
      return interpolateLogLog(this.yCurve, this.n);
    },
    sAtN() {
      return interpolateLogLog(this.sCurve, this.n);
    },
    curveReady() {
      return (
        Number.isFinite(this.xAtN) &&
        Number.isFinite(this.yAtN) &&
        Number.isFinite(this.sAtN) &&
        this.xAtN > 0 &&
        this.yAtN > 0 &&
        this.sAtN > 0
      );
    },
    sigma12Max() {
      return this.curveReady ? this.sAtN : 0;
    },
    // Tsai-Hill: (σ1/x)² − σ1σ2/x² + (σ2/y)² + (σ12/s)² = 1, rearranged as
    // a quadratic in σ2: A·σ2² + B·σ2 + C = 0.
    getABC() {
      const x = this.xAtN;
      const y = this.yAtN;
      const s = this.sAtN;
      const shearTerm = (this.sigma12 / s) ** 2;
      return (sigma1) => [
        1 / y ** 2,
        -sigma1 / x ** 2,
        (sigma1 / x) ** 2 + shearTerm - 1,
      ];
    },
    boundaryPoints() {
      if (!this.curveReady) return [];
      const range = this.xAtN * 1.5;
      return buildQuadraticEnvelope(this.getABC, -range, range, 240);
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
    statusLabel() {
      if (!this.appliedPoint || this.isInside === null) return "";
      return this.isInside ? "Inside envelope" : "Outside envelope";
    },
    statusColor() {
      return this.isInside ? "green" : "red";
    },
  },
  watch: {
    sigma12Max(newMax) {
      if (this.sigma12 > newMax) this.sigma12 = newMax;
    },
  },
  methods: {
    formatNumber(value) {
      return Number.isFinite(value) ? value.toFixed(2) : "—";
    },
  },
};
</script>

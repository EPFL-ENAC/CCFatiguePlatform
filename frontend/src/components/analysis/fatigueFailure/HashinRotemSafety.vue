<template>
  <v-card outlined class="mt-4">
    <v-card-title class="text-subtitle-1">
      Hashin-Rotem — fiber / matrix safety
    </v-card-title>
    <v-card-text>
      <v-row dense>
        <v-col cols="12" sm="3">
          <v-text-field
            v-model.number="n"
            label="Cycles N"
            type="number"
            dense
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
            label="Applied σ6 [MPa]"
            type="number"
            dense
          ></v-text-field>
        </v-col>
      </v-row>

      <div v-if="!curveReady" class="text-caption grey--text">
        No data available at N = {{ n }}.
      </div>
      <template v-else>
        <v-sheet color="blue lighten-5" rounded class="pa-3 mt-2">
          <div class="text-subtitle-1 font-weight-medium">Fiber Mode</div>
          <v-row dense class="mt-1">
            <v-col cols="12" sm="4" class="text-center">
              <v-progress-circular
                :value="fibreGaugeValue"
                :color="fibreColor"
                :width="fibreDominant ? 10 : 6"
                size="120"
              >
                {{ formatNumber(fiFibre) }}
              </v-progress-circular>
              <div class="text-caption mt-1">
                FI fiber
                <span v-if="fibreDominant" class="font-weight-bold">
                  (critical)
                </span>
              </div>
            </v-col>
            <v-col cols="12" sm="8">
              <simple-chart
                :series="fibreSeries"
                x-axis-name="N"
                y-axis-name="σ1 [MPa]"
                x-axis-type="log"
                y-axis-plain
                :axis-label-formatter="formatPlainStress"
                :height="260"
              ></simple-chart>
            </v-col>
          </v-row>
        </v-sheet>

        <v-sheet color="amber lighten-5" rounded class="pa-3 mt-3">
          <div class="text-subtitle-1 font-weight-medium">
            Matrix Mode
            <span
              class="text-caption font-weight-regular grey--text text--darken-2"
            >
              (evaluated at N = {{ formatN(n) }}, same as Fiber Mode above)
            </span>
          </div>
          <v-row dense class="mt-1">
            <v-col cols="12" sm="4" class="text-center">
              <v-progress-circular
                :value="matriceGaugeValue"
                :color="matriceColor"
                :width="matriceDominant ? 10 : 6"
                size="120"
              >
                {{ formatNumber(fiMatrice) }}
              </v-progress-circular>
              <div class="text-caption mt-1">
                FI matrix
                <span v-if="matriceDominant" class="font-weight-bold">
                  (critical)
                </span>
              </div>
            </v-col>
            <v-col cols="12" sm="8">
              <envelope-chart
                :boundary-points="boundaryPoints"
                :applied-point="appliedPoint"
                :is-inside="isInside"
                x-axis-name="σ2 [MPa]"
                y-axis-name="σ6 [MPa]"
              ></envelope-chart>
            </v-col>
          </v-row>
        </v-sheet>
      </template>
    </v-card-text>
  </v-card>
</template>

<script>
import EnvelopeChart from "@/components/charts/EnvelopeChart";
import SimpleChart from "@/components/charts/SimpleChart";
import { interpolateLogLog } from "@/utils/curveInterpolation";
import {
  buildQuadraticEnvelope,
  isInsideQuadraticEnvelope,
} from "@/utils/envelopeGeometry";

export default {
  name: "HashinRotemSafety",
  components: { EnvelopeChart, SimpleChart },
  props: {
    // Parsed HashinRotem FAF CSV rows: { cycles_to_failure, s1f, s2f,
    // s12f, ... }
    rows: { type: Array, default: () => [] },
  },
  data() {
    return {
      n: 1e5,
      sigma1: null,
      sigma2: null,
      sigma12: null,
    };
  },
  computed: {
    s1fAtN() {
      return interpolateLogLog(
        this.rows.map((r) => [r.cycles_to_failure, r.s1f]),
        this.n
      );
    },
    s2fAtN() {
      return interpolateLogLog(
        this.rows.map((r) => [r.cycles_to_failure, r.s2f]),
        this.n
      );
    },
    s12fAtN() {
      return interpolateLogLog(
        this.rows.map((r) => [r.cycles_to_failure, r.s12f]),
        this.n
      );
    },
    curveReady() {
      return (
        Number.isFinite(this.s1fAtN) &&
        Number.isFinite(this.s2fAtN) &&
        Number.isFinite(this.s12fAtN) &&
        this.s1fAtN > 0 &&
        this.s2fAtN > 0 &&
        this.s12fAtN > 0
      );
    },
    fiFibre() {
      if (!this.curveReady || !Number.isFinite(this.sigma1)) return null;
      return this.sigma1 / this.s1fAtN;
    },
    fiMatrice() {
      if (
        !this.curveReady ||
        !Number.isFinite(this.sigma2) ||
        !Number.isFinite(this.sigma12)
      ) {
        return null;
      }
      return Math.sqrt(
        (this.sigma2 / this.s2fAtN) ** 2 + (this.sigma12 / this.s12fAtN) ** 2
      );
    },
    fibreDominant() {
      if (this.fiFibre == null || this.fiMatrice == null) return false;
      return this.fiFibre >= this.fiMatrice;
    },
    matriceDominant() {
      if (this.fiFibre == null || this.fiMatrice == null) return false;
      return this.fiMatrice > this.fiFibre;
    },
    fibreSeries() {
      if (!this.curveReady) return [];
      const series = [
        {
          type: "line",
          name: "Fiber strength",
          data: this.rows.map((r) => [r.cycles_to_failure, r.s1f]),
          // Dashed vertical marker at the selected N - same evaluation
          // point as the "Cycles N" field above and the Matrix Mode block
          // below. No label: N is already visible in the "Cycles N" field
          // and on the X axis itself.
          markLine: {
            silent: true,
            symbol: "none",
            lineStyle: { type: "dashed", color: "#757575" },
            label: { show: false },
            data: [{ xAxis: this.n }],
          },
        },
      ];
      if (Number.isFinite(this.sigma1)) {
        series.push({
          type: "scatter",
          name: "Applied (N, σ1)",
          symbolSize: 14,
          itemStyle: { color: this.fibreColor },
          data: [[this.n, this.sigma1]],
        });
      }
      return series;
    },
    fibreGaugeValue() {
      return this.fiFibre != null ? Math.min(this.fiFibre * 100, 100) : 0;
    },
    matriceGaugeValue() {
      return this.fiMatrice != null ? Math.min(this.fiMatrice * 100, 100) : 0;
    },
    fibreColor() {
      return this.fiFibre != null && this.fiFibre < 1 ? "green" : "red";
    },
    matriceColor() {
      return this.fiMatrice != null && this.fiMatrice < 1 ? "green" : "red";
    },
    // (σ2/S2f)² + (σ12/S12f)² = 1 - quadratic in σ12 (x2) at each σ2 (x1)
    // sample: A·σ12² + B·σ12 + C = 0. No cross term - simpler than the
    // Sims-Brogdon/FTPF envelopes (axis-aligned ellipse).
    getABC() {
      const s2f = this.s2fAtN;
      const s12f = this.s12fAtN;
      return (sigma2) => [1 / s12f ** 2, 0, (sigma2 / s2f) ** 2 - 1];
    },
    boundaryPoints() {
      if (!this.curveReady) return [];
      return buildQuadraticEnvelope(
        this.getABC,
        -this.s2fAtN,
        this.s2fAtN,
        200
      );
    },
    appliedPoint() {
      if (!Number.isFinite(this.sigma2) || !Number.isFinite(this.sigma12)) {
        return null;
      }
      return [this.sigma2, this.sigma12];
    },
    isInside() {
      if (!this.curveReady || !this.appliedPoint) return null;
      return isInsideQuadraticEnvelope(this.getABC, this.sigma2, this.sigma12);
    },
  },
  methods: {
    formatNumber(value) {
      return Number.isFinite(value) ? value.toFixed(2) : "—";
    },
    // Plain MPa values on the fibre chart's Y axis (no "(x10^3)" scaling) -
    // paired with SimpleChart's yAxisPlain to also drop the axis-name
    // suffix.
    formatPlainStress(value) {
      return Number.isFinite(value) ? Math.round(value).toLocaleString() : "";
    },
    formatN(value) {
      return Number.isFinite(value) ? Math.round(value).toLocaleString() : "—";
    },
  },
};
</script>

<template>
  <v-card outlined class="mt-4">
    <v-card-title class="text-subtitle-1">
      Fawaz-Ellyin — multiaxial safety margin
      <v-spacer />
      <v-chip v-if="fi != null" :color="statusColor" text-color="white" small>
        FI(N) = {{ formatNumber(fi) }}
      </v-chip>
    </v-card-title>
    <v-card-text>
      <v-row dense align="center">
        <v-col cols="12" sm="4">
          <v-text-field
            v-model.number="sigmaApp"
            label="Applied equivalent stress σeq,app [MPa]"
            type="number"
            dense
          ></v-text-field>
        </v-col>
        <v-col cols="12" sm="8">
          <div class="text-caption grey--text text--darken-1">
            N = {{ formatCycles(n) }} cycles · σeq,adm(N) =
            {{ formatNumber(stressAtN) }} MPa
          </div>
          <v-slider
            v-model="logN"
            :min="logNMin"
            :max="logNMax"
            :step="(logNMax - logNMin) / 200 || 0.01"
            dense
            hide-details
          ></v-slider>
        </v-col>
      </v-row>

      <div v-if="!curveReady" class="text-caption grey--text">
        No data available.
      </div>
      <simple-chart
        v-else
        :aspect-ratio="2.4"
        :series="series"
        x-axis-name="N"
        y-axis-name="Equivalent Stress [MPa]"
        x-axis-type="log"
      ></simple-chart>
    </v-card-text>
  </v-card>
</template>

<script>
import SimpleChart from "@/components/charts/SimpleChart";
import { interpolateLogLog } from "@/utils/curveInterpolation";

export default {
  name: "FawazEllyinMargin",
  components: { SimpleChart },
  props: {
    // Parsed FawazEllyin FAF CSV rows: { cycles_to_failure, stress_max, ... }
    rows: { type: Array, default: () => [] },
  },
  data() {
    return {
      sigmaApp: null,
      logN: null,
    };
  },
  computed: {
    curve() {
      return this.rows
        .map((r) => [Number(r.cycles_to_failure), Number(r.stress_max)])
        .filter(([x, y]) => Number.isFinite(x) && Number.isFinite(y) && x > 0);
    },
    curveReady() {
      return this.curve.length > 1;
    },
    nMin() {
      return this.curveReady ? Math.min(...this.curve.map((p) => p[0])) : 1;
    },
    nMax() {
      return this.curveReady ? Math.max(...this.curve.map((p) => p[0])) : 1;
    },
    logNMin() {
      return Math.log10(this.nMin);
    },
    logNMax() {
      return Math.log10(this.nMax);
    },
    n() {
      return this.logN != null ? 10 ** this.logN : this.nMin;
    },
    stressAtN() {
      return this.curveReady ? interpolateLogLog(this.curve, this.n) : null;
    },
    fi() {
      if (!Number.isFinite(this.sigmaApp) || !Number.isFinite(this.stressAtN)) {
        return null;
      }
      return this.sigmaApp / this.stressAtN;
    },
    statusColor() {
      return this.fi != null && this.fi < 1 ? "green" : "red";
    },
    series() {
      const markLines = [];
      if (Number.isFinite(this.n)) {
        markLines.push({ xAxis: this.n, label: { formatter: () => "N" } });
      }
      if (Number.isFinite(this.sigmaApp)) {
        markLines.push({
          yAxis: this.sigmaApp,
          label: { formatter: () => "σeq,app" },
        });
      }

      // The exact N / σeq,adm(N) values are already shown in the caption
      // above the chart, so the marker itself carries no label - avoids
      // ECharts falling back to the raw, unrounded interpolated float.
      const markPoint =
        Number.isFinite(this.n) && Number.isFinite(this.stressAtN)
          ? {
              data: [{ coord: [this.n, this.stressAtN], name: "cursor" }],
              symbolSize: 10,
              label: { show: false },
            }
          : undefined;

      return [
        {
          type: "line",
          name: "σeq,adm(N)",
          data: this.curve,
          markLine: markLines.length
            ? { symbol: "none", lineStyle: { type: "dashed" }, data: markLines }
            : undefined,
          markPoint,
        },
      ];
    },
  },
  watch: {
    logNMin: {
      immediate: true,
      handler(min) {
        if (this.logN == null && Number.isFinite(min)) {
          this.logN = (min + this.logNMax) / 2;
        }
      },
    },
  },
  methods: {
    formatNumber(value) {
      return Number.isFinite(value) ? value.toFixed(2) : "—";
    },
    formatCycles(value) {
      return Number.isFinite(value) ? Math.round(value).toLocaleString() : "—";
    },
  },
};
</script>

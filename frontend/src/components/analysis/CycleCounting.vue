<template>
  <v-card :loading="loading">
    <v-card-title>
      Cycle Counting
      <v-spacer />
      <info-tooltip>
        Cycle counting is used to summarize irregular load-versus-time histories
        <br />
        by providing the number of occurrences of cycles of various sizes.
        <br />
        <a
          href="/downloads/CycleCounting_Methods.pdf"
          target="_blank"
          rel="noopener"
          variant="text"
          density="compact"
          class="pa-0 text-decoration-underline"
          style="color: blue; text-transform: none; min-width: 0"
        >
          See detailed description of the methods (PDF)
        </a>
      </info-tooltip>
    </v-card-title>

    <v-card-subtitle>
      <v-row align="end">
        <v-col>
          <v-file-input
            v-model="file"
            show-size
            accept=".csv"
            :error-messages="errorMessages"
            :disabled="loading"
            label="LDS csv file"
            @change="updateOutput"
          >
            <template #append>
              <info-tooltip>
                See the
                <a
                  href="/downloads/LDS_Data_Convention.pdf"
                  target="_blank"
                  rel="noopener"
                  variant="text"
                  density="compact"
                  class="pa-0 text-decoration-underline"
                  style="color: blue; text-transform: none; min-width: 0"
                >
                  LDS Data Convention (PDF)
                </a>
              </info-tooltip>
            </template>
          </v-file-input>
        </v-col>

        <v-col>
          <v-select
            v-model="selectedMethods"
            :items="methods"
            label="Select Methods"
            multiple
            chips
            clearable
            :disabled="loading"
            @change="updateOutput"
          />
        </v-col>
      </v-row>
    </v-card-subtitle>

    <v-card-text v-if="stats" class="pt-0">
      <div class="text-h6 font-weight-bold text-center mb-4">
        Cycle spectrum (Cumulative percentage vs Stress range)
      </div>

      <v-row>
        <!-- SUMMARY -->
        <v-col cols="12" sm="4" md="3">
          <v-card outlined class="pa-3">
            <div class="mt-2 text-caption font-weight-bold mb-2">
              Cycles (Selected)
            </div>

            <v-divider class="my-2" />

            <div v-for="r in methodResults" :key="r.method" class="py-1">
              <div class="d-flex align-center mb-1">
                <span :style="dotStyle(r.color)" />
                <span
                  class="text-body-2 font-weight-bold"
                  :style="{ color: r.color }"
                >
                  {{ r.method }}
                </span>
              </div>

              <div class="d-flex justify-space-between">
                <span class="text-body-2">Full cycles</span>
                <span
                  class="text-body-2 font-weight-bold"
                  :style="{ color: r.color }"
                >
                  {{ r.statsPart.fullCycleCount }}
                </span>
              </div>
              <div class="d-flex justify-space-between">
                <span class="text-body-2">Half cycles</span>
                <span
                  class="text-body-2 font-weight-bold"
                  :style="{ color: r.color }"
                >
                  {{ r.statsPart.halfCycleCount }}
                </span>
              </div>
              <div class="d-flex justify-space-between">
                <span class="text-body-2">Constant amplitudes</span>
                <span
                  class="text-body-2 font-weight-bold"
                  :style="{ color: r.color }"
                >
                  {{ r.statsPart.constantAmplitudes }}
                </span>
              </div>
            </div>

            <v-snackbar v-model="showDownloadMessage" timeout="2500">
              {{ downloadMessage }}
            </v-snackbar>
          </v-card>
        </v-col>

        <!-- 2D chart -->
        <v-col cols="12" sm="8" md="9">
          <simple-chart
            v-if="series.length"
            :height="560"
            :series="series"
            :y-axis-min="stats.yAxisMin"
            :y-axis-max="stats.yAxisMax"
            :y-axis-interval="stats.yInterval"
            x-axis-name="Cumulative Percentage of Spectrum Cycles"
            y-axis-name="Stress Range [MPa]"
            :axis-label-formatter="formatInt"
            :tooltip-formatter="formatInt"
          />
        </v-col>
      </v-row>

      <div class="my-8">
        <v-divider />
      </div>

      <div class="text-h6 font-weight-bold text-center mb-2">
        Markov Matrix (Range–Mean Cycle Distribution)
      </div>

      <v-row v-if="selectedMethods && selectedMethods.length" dense>
        <v-col
          v-for="m in selectedMethods"
          :key="m"
          cols="12"
          :sm="
            selectedMethods.length === 1
              ? 12
              : selectedMethods.length === 2
              ? 6
              : 4
          "
        >
          <div class="d-flex align-center justify-center mb-3">
            <span :style="dotStyle(methodColors[m])" />
            <span class="font-weight-bold" :style="{ color: methodColors[m] }">
              {{ m }}
            </span>
          </div>

          <Markov3D
            v-if="markovByMethod[m] && markovByMethod[m].markovData?.length"
            :data="markovByMethod[m].markovData"
            :x-centers="markovByMethod[m].xCenters"
            :y-centers="markovByMethod[m].yCenters"
            x-label="Stress range [MPa]"
            y-label="Stress mean [MPa]"
            z-label="Number of cycles"
            title=""
            :height="700"
            :resize-key="markovLayoutKey"
          />

          <v-card
            v-if="markovByMethod[m]"
            outlined
            class="pa-2 mt-2"
            style="background: white; border-radius: 6px"
          >
            <div class="text-body-2">
              Bin widths:
              <b
                >{{ markovByMethod[m].markovSize }}×{{
                  markovByMethod[m].markovSize
                }}</b
              >
              · ΔRangE
              <b>{{ formatNumber(markovByMethod[m].rangeBinSize) }} MPa</b>
              · ΔMean
              <b>{{ formatNumber(markovByMethod[m].meanBinSize) }} MPa</b>
            </div>
          </v-card>

          <div v-else class="text-caption grey--text text-center mt-2">
            No Markov data for this method.
          </div>
        </v-col>
      </v-row>

      <div @click="onDownloadClick">
        <div class="mt-3 d-flex justify-end" style="width: 100%">
          <v-btn :disabled="downloadDisabled">
            Download CYC
            <info-tooltip>
              <span v-if="selectedMethods.length !== 1">
                Please select just one method to download CYC.
              </span>
              <span v-else>
                See the
                <a
                  href="/downloads/CYC_output_guide.pdf"
                  target="_blank"
                  rel="noopener"
                  variant="text"
                  density="compact"
                  class="pa-0 text-decoration-underline"
                  style="color: blue; text-transform: none; min-width: 0"
                >
                  CYC Data Convention (PDF)
                </a>
              </span>
            </info-tooltip>
          </v-btn>
        </div>
      </div>
    </v-card-text>
  </v-card>
</template>

<script>
import CycleCountingMethod from "@/backend/model/CycleCountingMethod";
import Markov3D from "@/components/charts/Markov3D";
import SimpleChart from "@/components/charts/SimpleChart";
import InfoTooltip from "@/components/InfoTooltip";
import { getOutputFileName } from "@/utils/analysis";
import { parserConfig } from "@/utils/papaparse";
import download from "downloadjs";
import { parse } from "papaparse";

const METHODS = Object.values(new CycleCountingMethod());

/**
 * === CONFIG / TUNING ===
 * These values are safe to change if you need to tune the UI or performance.
 *
 * - COLOR_PALETTE: Colors assigned to methods in selection order.
 * - MATRIX_SIZE_DEFAULT: Markov matrix resolution (NxN bins). Higher = finer but heavier to render.
 * - TOP_N_DEFAULT: Keep only the TOP_N highest bins for the 3D chart (performance safeguard).
 * - Y_INTERVAL: Rounding step for spectrum y-axis (MPa).
 */
const COLOR_PALETTE = [
  "#d32f2f",
  "#1976d2",
  "#388e3c",
  "#f57c00",
  "#7b1fa2",
  "#00796b",
];
const MATRIX_SIZE_DEFAULT = 64;
const TOP_N_DEFAULT = 1500;
const Y_INTERVAL = 2;

/**
 * Convert to number; returns NaN for empty values.
 * Keeps downstream filters simple (Number.isFinite checks).
 */
function toNum(v) {
  return v === null || v === undefined || v === "" ? NaN : Number(v);
}

/**
 * Parse a CYC CSV string (output from backend) into row objects.
 * Rows that are fully empty are removed.
 *
 * Expected keys (used later):
 * - cum_n_cycles
 * - stress_range
 * - stress_mean
 * - n_cycles
 */
function parseCycCsvToRows(csvText) {
  const cycResults = parse(csvText, parserConfig);
  return (cycResults?.data || []).filter(
    (r) =>
      r &&
      Object.values(r).some(
        (v) => v !== null && v !== undefined && String(v).trim() !== ""
      )
  );
}

/**
 * Build spectrum curve and basic counts from parsed rows.
 *
 * Output:
 * - points: [cum%, range] for plotting
 * - statsPart: counts + y-axis bounds
 *
 * Notes:
 * - Ensures the curve reaches 100% by adding a final point if needed.
 * - y-axis bounds are snapped to Y_INTERVAL (MPa).
 */
function buildSpectrum(rows) {
  const points = rows
    .map((r) => [toNum(r.cum_n_cycles), toNum(r.stress_range)])
    .filter(([x, y]) => Number.isFinite(x) && Number.isFinite(y))
    .sort((a, b) => a[0] - b[0]);

  if (points.length) {
    const last = points[points.length - 1];
    if (last[0] < 100) points.push([100, last[1]]);
  }

  const ys = points.map((p) => p[1]);
  const yMin = ys.length ? Math.min(...ys) : 0;
  const yMax = ys.length ? Math.max(...ys) : 1;

  const yAxisMin = Math.floor(yMin / Y_INTERVAL) * Y_INTERVAL;
  const yAxisMax = Math.ceil(yMax / Y_INTERVAL) * Y_INTERVAL;

  // n_cycles may contain halves (e.g., 0.5) depending on method/output convention.
  const ncy = rows.map((r) => toNum(r.n_cycles)).filter(Number.isFinite);
  const fullCycleCount = ncy.reduce((acc, v) => acc + Math.floor(v), 0);
  const halfCycleCount = ncy.reduce(
    (acc, v) => acc + (v % 1 >= 0.5 ? 1 : 0),
    0
  );

  return {
    points,
    statsPart: {
      fullCycleCount,
      halfCycleCount,
      constantAmplitudes: rows.length,
      yAxisMin,
      yAxisMax,
      yInterval: Y_INTERVAL,
    },
  };
}

/**
 * Build Markov matrix data (Range–Mean distribution) for 3D visualization.
 *
 * Important:
 * - The 3D component expects indices [i, j, z] + optional centers for tooltips.
 * - We store: [i, j, cycles, xCenter, yCenter]
 *
 * Performance:
 * - We keep only TOP_N bins after aggregation (largest bins first).
 */
function buildMarkov(
  rows,
  MATRIX_SIZE = MATRIX_SIZE_DEFAULT,
  TOP_N = TOP_N_DEFAULT
) {
  const pts = rows
    .map((r) => ({
      range: toNum(r.stress_range),
      mean: toNum(r.stress_mean),
      cycles: toNum(r.n_cycles),
    }))
    .filter(
      (p) =>
        Number.isFinite(p.range) &&
        Number.isFinite(p.mean) &&
        Number.isFinite(p.cycles)
    );

  if (!pts.length) {
    return {
      markovData: [],
      xCenters: [],
      yCenters: [],
      rangeBinSize: null,
      meanBinSize: null,
      markovSize: MATRIX_SIZE,
    };
  }

  const rMin = Math.min(...pts.map((p) => p.range));
  const rMax = Math.max(...pts.map((p) => p.range));
  const mMin = Math.min(...pts.map((p) => p.mean));
  const mMax = Math.max(...pts.map((p) => p.mean));

  const rSpan = rMax - rMin || 1;
  const mSpan = mMax - mMin || 1;

  const rangeBinSize = rSpan / MATRIX_SIZE;
  const meanBinSize = mSpan / MATRIX_SIZE;

  // Bin centers in MPa (used by Markov3D for axes & tooltip)
  const xCenters = Array.from(
    { length: MATRIX_SIZE },
    (_, i) => rMin + (i + 0.5) * rangeBinSize
  );
  const yCenters = Array.from(
    { length: MATRIX_SIZE },
    (_, j) => mMin + (j + 0.5) * meanBinSize
  );

  const clamp = (v, lo, hi) => Math.max(lo, Math.min(hi, v));
  const acc = new Map();

  // Aggregate cycles into bins
  for (const p of pts) {
    const i = clamp(
      Math.floor(((p.range - rMin) / rSpan) * MATRIX_SIZE),
      0,
      MATRIX_SIZE - 1
    );
    const j = clamp(
      Math.floor(((p.mean - mMin) / mSpan) * MATRIX_SIZE),
      0,
      MATRIX_SIZE - 1
    );
    const key = `${i}|${j}`;
    acc.set(key, (acc.get(key) || 0) + p.cycles);
  }

  const data3d = [];
  for (const [key, z] of acc.entries()) {
    if (z <= 0) continue;
    const [i, j] = key.split("|").map(Number);
    data3d.push([i, j, z, xCenters[i], yCenters[j]]);
  }

  // Keep largest bins (most representative) for faster rendering
  data3d.sort((a, b) => b[2] - a[2]);

  return {
    markovData: data3d.slice(0, TOP_N),
    xCenters,
    yCenters,
    rangeBinSize,
    meanBinSize,
    markovSize: MATRIX_SIZE,
  };
}

export default {
  name: "CycleCounting",
  components: { InfoTooltip, SimpleChart, Markov3D },

  data() {
    return {
      file: null,
      loading: false,

      downloadMessage: "",
      showDownloadMessage: false,

      methods: METHODS,
      selectedMethods: [METHODS[0]],

      errorMessages: null,
      series: [],
      stats: null,
      output: null,

      markovByMethod: {},
      methodResults: [],
      methodColors: {},

      markovLayoutKey: 0,
    };
  },

  computed: {
    /**
     * Download is enabled only when:
     * - a file has been processed
     * - exactly one method is selected (output file includes method in its name)
     */
    downloadDisabled() {
      return this.loading || !this.output || this.selectedMethods.length !== 1;
    },
  },

  watch: {
    /**
     * When method selection changes:
     * - bump resize key to force Markov3D to relayout
     * - clear outputs if nothing is selected
     * - otherwise recompute results for selected methods
     */
    selectedMethods(newVal) {
      this.markovLayoutKey++;

      if (!newVal || !newVal.length) {
        this.series = [];
        this.methodResults = [];
        this.markovByMethod = {};
        this.stats = null;
        this.output = null;
        return;
      }
      this.updateOutput();
    },
  },

  methods: {
    /** Small helper for colored dot shown near method labels */
    dotStyle(color) {
      return {
        width: "10px",
        height: "10px",
        borderRadius: "50%",
        background: color,
        display: "inline-block",
        marginRight: "8px",
      };
    },

    /** Integer formatting for axis/tooltip (no grouping separators) */
    formatInt(v) {
      const n = Number(v);
      if (!Number.isFinite(n)) return "-";
      return n.toLocaleString(undefined, {
        minimumFractionDigits: 0,
        maximumFractionDigits: 0,
        useGrouping: false,
      });
    },

    /** MPa formatting (one decimal place for bin sizes, etc.) */
    formatNumber(v) {
      if (v === null || v === undefined || Number.isNaN(v)) return "-";
      return Number(v).toFixed(1); // 1 decimal (MPa)
    },

    /**
     * Assign a stable color per method for the current session.
     * Colors are assigned in the order methods appear in `selectedMethods`.
     */
    getMethodColor(method) {
      if (this.methodColors[method]) return this.methodColors[method];
      const idx = Object.keys(this.methodColors).length % COLOR_PALETTE.length;
      this.$set(this.methodColors, method, COLOR_PALETTE[idx]);
      return this.methodColors[method];
    },

    /**
     * Main pipeline:
     * 1) backend computes CYC CSV for each selected method
     * 2) parse CSV into rows
     * 3) build Spectrum points + cycle counts
     * 4) build Markov matrix data for 3D plot
     * 5) stats/output are taken from the FIRST selected method (reference)
     */
    async updateOutput() {
      if (!this.file || !this.selectedMethods?.length) return;

      this.loading = true;
      this.errorMessages = null;

      try {
        const results = await Promise.all(
          this.selectedMethods.map(async (m) => {
            const csv = await this.$analysisApi.runCycleCountingFile(
              m,
              this.file
            );
            const rows = parseCycCsvToRows(csv);
            const { points, statsPart } = buildSpectrum(rows);
            const color = this.getMethodColor(m);
            const markov = buildMarkov(rows);

            return { method: m, csv, rows, points, statsPart, color, markov };
          })
        );

        this.methodResults = results;

        // Markov per method (already computed once)
        const markovMap = {};
        for (const r of results) markovMap[r.method] = r.markov;
        this.markovByMethod = markovMap;

        // Series for spectrum
        this.series = results.map((r) => ({
          type: "line",
          step: "end",
          name: r.method,
          data: r.points,
          lineStyle: { color: r.color, width: 2 },
          itemStyle: { color: r.color },
        }));

        // Reference stats = first selected method (same behavior as before)
        const ref = results[0];
        this.stats = {
          ...ref.statsPart,
          markovSize: ref.markov.markovSize,
          rangeBinSize: ref.markov.rangeBinSize,
          meanBinSize: ref.markov.meanBinSize,
        };

        // Reference output used for download (download is enabled only when 1 method is selected)
        this.output = ref.csv;
      } catch (e) {
        // Any parsing/backend error falls back to a clean state
        this.file = null;
        this.series = [];
        this.methodResults = [];
        this.markovByMethod = {};
        this.stats = null;
        this.output = null;
        this.errorMessages = "Invalid input";
      } finally {
        this.loading = false;
      }
    },

    /**
     * Download the computed CYC CSV (only valid when 1 method is selected).
     * File name uses the platform naming convention: LDS -> CYC + method suffix.
     */
    downloadOutput() {
      if (!this.output || !this.file) return;

      if (this.selectedMethods.length !== 1) {
        this.downloadMessage = "Please select just one method to download.";
        this.showDownloadMessage = true;
        return;
      }

      const method = this.selectedMethods[0];
      const outputName = getOutputFileName(
        "LDS",
        "CYC",
        this.file.name,
        method
      );
      download(this.output, outputName + ".csv", "text/csv");
    },

    /**
     * Click handler for the download area:
     * - shows a snackbar if selection is invalid
     * - otherwise triggers the file download
     */
    onDownloadClick() {
      if (this.loading || !this.output) return;

      if (this.selectedMethods.length !== 1) {
        this.downloadMessage = "Please select just one method to download.";
        this.showDownloadMessage = true;
        return;
      }
      this.downloadOutput();
    },
  },
};
</script>

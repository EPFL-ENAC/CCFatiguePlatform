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
                  >LDS Data Convention (PDF)</a
                >
              </info-tooltip>
            </template>
          </v-file-input>
        </v-col>
        <v-col>
          <v-select
            v-model="method"
            :items="methods"
            label="Select Method"
            :disabled="loading"
            @change="updateOutput"
          >
          </v-select>
        </v-col>
      </v-row>
    </v-card-subtitle>
    <!-- GRAPH (plein largeur) -->
    <v-card-text v-if="stats" class="pt-0">
      <div class="text-h6 font-weight-bold text-center mb-4">
        Cycle spectrum (Cumulative percentage vs Stress range)
      </div>
      <!-- ROW 1 : Summary (left) + 2D chart (right) -->
      <v-row>
        <!-- SUMMARY LEFT (3 values only) -->
        <v-col cols="12" sm="4" md="3">
          <v-card outlined class="pa-3">
            <div class="mt-2 text-caption font-weight-bold mb-2">Cycles</div>
            <v-divider class="mb-2" />

            <div class="d-flex justify-space-between">
              <span class="text-body-2">Full cycles</span>
              <span class="text-body-2 font-weight-bold red--text">
                {{ stats.fullCycleCount }}
              </span>
            </div>

            <div class="d-flex justify-space-between">
              <span class="text-body-2">Half cycles</span>
              <span class="text-body-2 font-weight-bold red--text">
                {{ stats.halfCycleCount }}
              </span>
            </div>

            <div class="d-flex justify-space-between">
              <span class="text-body-2">Constant amplitudes</span>
              <span class="text-body-2 font-weight-bold red--text">
                {{ stats.constantAmplitudes }}
              </span>
            </div>
          </v-card>
        </v-col>

        <!-- 2D chart -->
        <v-col cols="12" sm="8" md="9">
          <simple-chart
            v-if="series.length > 0"
            :aspect-ratio="2"
            :series="series"
            :y-axis-min="stats?.yAxisMin"
            :y-axis-max="stats?.yAxisMax"
            :y-axis-interval="stats?.yInterval"
            x-axis-name="Cumulative Percentage of Spectrum Cycles"
            y-axis-name="Stress Range [MPa]"
          />
        </v-col>
      </v-row>

      <!-- spacing between 2D block and Markov -->
      <div class="my-3" />

      <!-- ROW 2 : Markov full width -->
      <div class="text-h6 font-weight-bold text-center mb-2">
        Markov Matrix (Range–Mean Cycle Distribution)
      </div>

      <Markov3D
        v-if="markovData && markovData.length"
        :data="markovData"
        :x-centers="markovXCenters"
        :y-centers="markovYCenters"
        x-label="Stress range [MPa]"
        y-label="Stress mean [MPa]"
        z-label="Number of cycles"
        title=""
      />

      <!-- Row under Markov: info left + download right -->
      <div v-if="stats" class="mt-2">
        <v-row align="center">
          <!-- Info card ONLY on the left -->
          <v-col cols="12" sm="8">
            <v-card
              outlined
              class="pa-2"
              style="background: white; border-radius: 6px"
            >
              <div class="text-body-2 font-weight-bold">
                Bin widths (Markov discretization):
              </div>
              <div class="text-body-2">
                Matrix
                <span class="font-weight-bold">
                  {{ stats.markovSize }}×{{ stats.markovSize }}
                </span>
                · ΔRange
                <span class="font-weight-bold">
                  {{ formatNumber(stats.rangeBinSize) }} MPa
                </span>
                · ΔMean
                <span class="font-weight-bold">
                  {{ formatNumber(stats.meanBinSize) }} MPa
                </span>
              </div>
            </v-card>
          </v-col>

          <!-- Download button on the right (outside the card) -->
          <v-col cols="12" sm="4" class="d-flex justify-end">
            <v-btn
              :disabled="loading && output != null"
              @click="downloadOutput"
            >
              Download CYC
              <info-tooltip>
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
              </info-tooltip>
            </v-btn>
          </v-col>
        </v-row>
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

const methods = Object.values(new CycleCountingMethod());

export default {
  name: "CycleCounting",
  components: {
    InfoTooltip,
    SimpleChart,
    Markov3D,
  },
  data() {
    return {
      file: null,
      loading: false,
      output: null,
      methods: methods,
      method: methods[0],
      errorMessages: null,
      series: [],
      stats: null,
      markovData: [],
      markovXCenters: [],
      markovYCenters: [],
    };
  },
  computed: {
    hasInput: function () {
      return this.file != null;
    },
  },
  methods: {
    updateOutput() {
      if (this.file && this.method) {
        this.loading = true;
        this.$analysisApi
          .runCycleCountingFile(this.method, this.file)
          .then(async (data) => {
            this.output = data;

            const cycResults = parse(data, parserConfig);
            const rows = (cycResults?.data || []).filter(
              (r) =>
                r &&
                Object.values(r).some(
                  (v) =>
                    v !== null && v !== undefined && String(v).trim() !== ""
                )
            );

            const toNum = (v) =>
              v === null || v === undefined || v === "" ? NaN : Number(v);

            // --------------------
            // Build Markov-like (64x64) from CYC output (stress_range / stress_mean / n_cycles)
            // --------------------
            const MATRIX_SIZE = 64;

            // defaults (so ESLint + pts.length === 0 are OK)
            let rangeBinSize = null;
            let meanBinSize = null;

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

            if (pts.length) {
              const rMin = Math.min(...pts.map((p) => p.range));
              const rMax = Math.max(...pts.map((p) => p.range));
              const mMin = Math.min(...pts.map((p) => p.mean));
              const mMax = Math.max(...pts.map((p) => p.mean));
              const rSpan = rMax - rMin || 1;
              const mSpan = mMax - mMin || 1;

              rangeBinSize = rSpan / MATRIX_SIZE;
              meanBinSize = mSpan / MATRIX_SIZE;

              const xCenters = Array.from({ length: MATRIX_SIZE }, (_, i) => {
                return rMin + (i + 0.5) * (rSpan / MATRIX_SIZE);
              });
              const yCenters = Array.from({ length: MATRIX_SIZE }, (_, j) => {
                return mMin + (j + 0.5) * (mSpan / MATRIX_SIZE);
              });

              this.markovXCenters = xCenters;
              this.markovYCenters = yCenters;
              const mat = Array.from({ length: MATRIX_SIZE }, () =>
                Array(MATRIX_SIZE).fill(0)
              );

              const clamp = (v, lo, hi) => Math.max(lo, Math.min(hi, v));

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
                mat[i][j] += p.cycles;
              }

              const data3d = [];
              for (let i = 0; i < MATRIX_SIZE; i++) {
                for (let j = 0; j < MATRIX_SIZE; j++) {
                  const z = mat[i][j];
                  if (z > 0) data3d.push([i, j, z, xCenters[i], yCenters[j]]);
                }
              }

              const TOP_N = 1500;
              data3d.sort((a, b) => b[2] - a[2]);

              this.markovData = data3d.slice(0, TOP_N);
            } else {
              this.markovData = [];
            }

            // --------------------
            // Existing stats + curve
            // --------------------
            const ncy = rows
              .map((r) => toNum(r.n_cycles))
              .filter(Number.isFinite);

            const fullCycleCount = ncy.reduce(
              (acc, v) => acc + Math.floor(v),
              0
            );
            const halfCycleCount = ncy.reduce((acc, v) => {
              const frac = v - Math.floor(v);
              return acc + Math.round(frac * 2);
            }, 0);

            let points = rows
              .map((r) => [toNum(r.cum_n_cycles), toNum(r.stress_range)])
              .filter(([x, y]) => Number.isFinite(x) && Number.isFinite(y));

            points.sort((a, b) => a[0] - b[0]);

            const ys = points.map((p) => p[1]);
            const yMin = Math.min(...ys);
            const yMax = Math.max(...ys);
            const yInterval = 2;
            const yAxisMin = Math.floor(yMin / yInterval) * yInterval;
            const yAxisMax = Math.ceil(yMax / yInterval) * yInterval;

            if (points.length) {
              const last = points[points.length - 1];
              if (last[0] < 100) points.push([100, last[1]]);
            }

            this.stats = {
              fullCycleCount,
              halfCycleCount,
              constantAmplitudes: rows.length,
              yAxisMin,
              yAxisMax,
              yInterval,
              markovSize: MATRIX_SIZE,
              rangeBinSize,
              meanBinSize,
            };

            this.series = [
              {
                type: "line",
                step: "end",
                name: this.method,
                data: points,
              },
            ];

            this.errorMessages = null;
          })
          .catch(() => {
            this.file = null;
            this.series = [];
            this.errorMessages = "Invalid input";
          })
          .finally(() => (this.loading = false));
      }
    },
    downloadOutput() {
      if (this.output) {
        const outputName = getOutputFileName(
          "LDS",
          "CYC",
          this.file.name,
          this.method
        );
        download(this.output, outputName + ".csv", "text/csv");
      }
    },
    openCycleCountingPDF() {
      window.open("/downloads/CycleCounting_Methods.pdf", "_blank");
    },
    readFileAsText(file) {
      return new Promise((resolve, reject) => {
        const reader = new FileReader();
        reader.onload = () => resolve(String(reader.result || ""));
        reader.onerror = reject;
        reader.readAsText(file);
      });
    },

    getFirstNumericColumn(rows, preferredKeys = []) {
      if (!rows || !rows.length) return null;

      // 1) si une colonne préférée existe (ex: "stress", "stress_max", etc.)
      const keys = Object.keys(rows[0] || {});
      for (const k of preferredKeys) {
        if (keys.includes(k)) return k;
      }

      // 2) sinon, on cherche la première colonne qui contient des nombres
      for (const k of keys) {
        for (let i = 0; i < Math.min(rows.length, 30); i++) {
          const v = rows[i]?.[k];
          const n = Number(v);
          if (Number.isFinite(n)) return k;
        }
      }
      return null;
    },
    formatNumber(v) {
      if (v === null || v === undefined || Number.isNaN(v)) return "-";
      return Number(v).toFixed(1);
    },
  },
};
</script>

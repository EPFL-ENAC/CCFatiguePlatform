<template>
  <v-card :loading="loading">
    <v-card-title>
      S-N Curve
      <v-spacer />

      <info-tooltip>
        This module plots curves on the (Stress - Number of cycles) plane.
        <br />
        The curves are associated with 4 different methods (Lin-log, Log-log,
        Sendeckyj, Whitney) and can be considered as constitutive laws for
        fatigue life predictions.
        <br />
        On the graphs we also plot the individual results gathered over the
        tests with inputs (Cycles to failure - Stress at failure)
        <br />
        <a
          href="/downloads/SNCurve_methods.pdf"
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
      <v-row align="center">
        <v-col>
          <v-file-input
            v-model="file"
            chips
            show-size
            accept=".csv"
            label="AGG csv file"
            :disabled="loading"
            @change="onFileChange"
          >
            <template #append>
              <info-tooltip>
                See the
                <a
                  href="/downloads/AGG_Data_Convention.pdf"
                  target="_blank"
                  rel="noopener"
                  style="color: blue; text-transform: none; min-width: 0"
                >
                  AGG Data Convention (PDF)
                </a>
              </info-tooltip>
            </template>
          </v-file-input>
        </v-col>
      </v-row>

      <v-row align="end">
        <v-col>
          <v-select
            v-model="selectedMethods"
            label="Select S-N curve method(s)"
            :items="methods"
            chips
            multiple
            variant="underlined"
            density="comfortable"
            :disabled="loading"
            @change="updateOutput"
          />
        </v-col>

        <v-col>
          <v-select
            v-model="selectedRRatios"
            label="Select R ratio"
            :items="rRatios"
            chips
            multiple
            variant="underlined"
            density="comfortable"
            :disabled="loading"
            @change="updateOutput"
          />
        </v-col>

        <v-col>
          <v-select
            v-model="graphType"
            label="Graph type"
            :items="graphTypeOptions"
            item-title="text"
            item-value="value"
            :disabled="loading"
          />
        </v-col>
      </v-row>
    </v-card-subtitle>

    <v-card-text v-if="series.length > 0">
      <v-row>
        <v-col cols="12">
          <div
            style="font-weight: 700; font-size: 23px; color: #212121"
            class="text-center mb-4"
          >
            Stress vs Number of Cycles (S-N)
          </div>
        </v-col>
      </v-row>

      <v-row align="start">
        <v-col cols="12" md="3" class="mt-6">
          <v-card variant="outlined" class="pa-3">
            <div class="text-subtitle-2 mb-2">S-N (Selected)</div>
            <v-divider class="mb-2" />

            <div
              v-for="card in selectedMethodCards"
              :key="card.key"
              class="method-banner-card mb-4"
            >
              <div class="d-flex align-center justify-space-between mb-2">
                <div class="d-flex align-center">
                  <span
                    class="mr-2"
                    :style="{
                      display: 'inline-block',
                      width: '12px',
                      height: '12px',
                      borderRadius: '50%',
                      background: card.color,
                    }"
                  />
                  <span
                    class="method-banner-title"
                    :style="{ color: card.color }"
                  >
                    {{ card.method }}
                  </span>
                </div>

                <div class="method-banner-ratio">R = {{ card.rRatio }}</div>
              </div>

              <div class="method-banner-meta mb-3">
                <span v-if="card.method === 'Sendeckyj'">
                  Probability: {{ sendeckyjProbability }}%
                </span>
                <span v-else-if="card.method === 'Whitney'">
                  Probability: {{ whitneyProbability }}%
                </span>
              </div>

              <div class="method-banner-section-label">Equation</div>
              <div class="method-banner-equation mb-3">
                <code>{{ card.equation }}</code>
              </div>

              <div class="method-banner-section-label">Parameters</div>
              <div
                v-if="card.params.length === 0"
                class="method-banner-empty mb-3"
              >
                No fit data
              </div>
              <div v-else class="method-banner-params mb-3">
                <div
                  v-for="param in card.params"
                  :key="param.name"
                  class="method-banner-param"
                >
                  <span class="param-name">{{ param.name }}</span>
                  <span class="param-value">{{ param.value }}</span>
                </div>
              </div>

              <div
                v-if="showReliabilityBands"
                class="method-banner-dashed mb-3"
              >
                <span
                  :style="{
                    color: card.color,
                    fontFamily: 'monospace',
                    fontWeight: 'bold',
                    fontSize: '15px',
                    letterSpacing: '2px',
                  }"
                >
                  - - - -
                </span>
                <span class="ml-2">{{ card.dashedMeaning }}</span>
              </div>

              <div v-if="card.method === 'Sendeckyj'" class="mb-3">
                <div class="method-banner-section-label">
                  Sendeckyj probability [%]
                </div>
                <v-text-field
                  v-model.number="sendeckyjProbability"
                  type="number"
                  min="1"
                  max="99"
                  step="1"
                  variant="outlined"
                  density="comfortable"
                  hide-details
                  :disabled="loading"
                  @change="updateOutput"
                />
              </div>

              <div v-if="card.method === 'Whitney'" class="mb-3">
                <div class="method-banner-section-label">
                  Whitney probability [%]
                </div>
                <v-text-field
                  v-model.number="whitneyProbability"
                  type="number"
                  min="1"
                  max="99"
                  step="1"
                  variant="outlined"
                  density="comfortable"
                  hide-details
                  :disabled="loading"
                  @change="updateOutput"
                />
              </div>
            </div>
          </v-card>
        </v-col>

        <v-col cols="12" md="9">
          <simple-chart
            :aspect-ratio="2"
            :series="series"
            :x-axis-type="computedXAxisType"
            :y-axis-type="computedYAxisType"
            x-axis-name="Number of cycles"
            y-axis-name="σₘₐₓ [MPa]"
            :x-axis-min="computedXAxisMin"
            :x-axis-max="computedXAxisMax"
            :y-axis-min="computedYAxisMin"
            :y-axis-max="computedYAxisMax"
          />

          <div class="mt-4 px-3">
            <div class="text-subtitle-2 mb-2">Number of cycles (X axis)</div>
            <div class="d-flex justify-space-between text-caption mb-1">
              <span>Start: {{ sliderDisplayStart }}</span>
              <span>End: {{ sliderDisplayEnd }}</span>
            </div>

            <v-range-slider
              v-model="activeCycleRange"
              :min="activeCycleBounds[0]"
              :max="activeCycleBounds[1]"
              :step="activeSliderStep"
              strict
              hide-details
              class="mt-0"
            />

            <div class="d-flex justify-space-between text-caption mt-1">
              <span
                v-for="tick in bottomTicks"
                :key="tick.label"
                style="min-width: 40px; text-align: center"
              >
                {{ tick.label }}
              </span>
            </div>
          </div>
          <div class="mt-6 px-3">
            <div class="text-subtitle-2 mb-2">Stress range (Y axis)</div>

            <div class="d-flex justify-space-between text-caption mb-1">
              <span>Start: {{ sliderDisplayStressStart }}</span>
              <span>End: {{ sliderDisplayStressEnd }}</span>
            </div>

            <v-range-slider
              v-model="activeStressRange"
              :min="activeStressBounds[0]"
              :max="activeStressBounds[1]"
              :step="activeStressSliderStep"
              strict
              hide-details
              class="mt-0"
            />

            <div class="d-flex justify-space-between text-caption mt-1">
              <span
                v-for="tick in leftTicks"
                :key="tick.label"
                style="min-width: 40px; text-align: center"
              >
                {{ tick.label }}
              </span>
            </div>
          </div>
        </v-col>
      </v-row>
    </v-card-text>

    <v-card-actions v-if="hasInput" class="justify-end">
      <div @click="onDownloadClick">
        <v-btn :disabled="downloadDisabled">
          Download SNC
          <info-tooltip>
            <span v-if="selectedMethods.length !== 1">
              Please select just one method to download SNC.
            </span>
            <span v-else>
              See the
              <a
                href="/downloads/SNC_output_guide.pdf"
                target="_blank"
                rel="noopener"
                style="color: blue; text-transform: none; min-width: 0"
              >
                SNC Data Convention (PDF)
              </a>
            </span>
          </info-tooltip>
        </v-btn>
      </div>
    </v-card-actions>

    <v-snackbar v-model="showDownloadMessage" timeout="2500">
      {{ downloadMessage }}
    </v-snackbar>
  </v-card>
</template>

<script>
import SnCurveMethod from "@/backend/model/SnCurveMethod";
import SimpleChart from "@/components/charts/SimpleChart";
import InfoTooltip from "@/components/InfoTooltip";
import { getOutputFileName } from "@/utils/analysis";
import { parseFile, parserConfig } from "@/utils/papaparse";
import download from "downloadjs";
import { parse } from "papaparse";

const methods = Object.values(new SnCurveMethod());

export default {
  name: "SnCurve",
  components: {
    InfoTooltip,
    SimpleChart,
  },

  data() {
    return {
      file: null,
      loading: false,

      downloadMessage: "",
      showDownloadMessage: false,

      methods,
      selectedMethods: [methods[0]],
      rRatios: [],
      selectedRRatios: [],
      outputs: {},
      series: [],

      sendeckyjProbability: 50,
      whitneyProbability: 50,
      requestId: 0,

      graphType: "linlog",
      graphTypeOptions: [
        { text: "Lin-Lin", value: "linlin" },
        { text: "Lin-Log", value: "linlog" },
        { text: "Log-Log", value: "loglog" },
      ],

      logCycleRange: [0, 8],
      logCycleBounds: [0, 8],
      linearCycleRange: [1, 100],
      linearCycleBounds: [1, 100],
      logStressRange: [0, 3],
      logStressBounds: [0, 3],
      linearStressRange: [0, 100],
      linearStressBounds: [0, 100],
    };
  },

  computed: {
    hasInput() {
      return !!this.file;
    },

    downloadDisabled() {
      return (
        this.loading ||
        !this.outputs ||
        Object.keys(this.outputs).length === 0 ||
        this.selectedMethods.length !== 1
      );
    },

    isLogY() {
      return this.computedYAxisType === "log";
    },

    activeStressRange: {
      get() {
        return this.isLogY ? this.logStressRange : this.linearStressRange;
      },
      set(value) {
        if (this.isLogY) {
          this.logStressRange = value;
        } else {
          this.linearStressRange = value;
        }
      },
    },

    activeStressBounds() {
      return this.isLogY ? this.logStressBounds : this.linearStressBounds;
    },

    activeStressSliderStep() {
      return this.isLogY
        ? 0.01
        : Math.max(
            (this.linearStressBounds[1] - this.linearStressBounds[0]) / 500,
            1
          );
    },
    showReliabilityBands() {
      return (
        this.selectedMethods.length === 1 && this.selectedRRatios.length === 1
      );
    },

    computedXAxisType() {
      return this.graphType === "linlin" ? "value" : "log";
    },

    computedYAxisType() {
      return this.graphType === "loglog" ? "log" : "value";
    },

    isLogX() {
      return this.computedXAxisType === "log";
    },

    activeCycleRange: {
      get() {
        return this.isLogX ? this.logCycleRange : this.linearCycleRange;
      },
      set(value) {
        if (this.isLogX) {
          this.logCycleRange = value;
        } else {
          this.linearCycleRange = value;
        }
      },
    },

    activeCycleBounds() {
      return this.isLogX ? this.logCycleBounds : this.linearCycleBounds;
    },

    activeSliderStep() {
      return this.isLogX
        ? 0.01
        : Math.max(
            (this.linearCycleBounds[1] - this.linearCycleBounds[0]) / 500,
            1
          );
    },
    computedXAxisMin() {
      return this.isLogX
        ? Math.pow(10, this.logCycleRange[0])
        : this.linearCycleRange[0];
    },

    computedXAxisMax() {
      return this.isLogX
        ? Math.pow(10, this.logCycleRange[1])
        : this.linearCycleRange[1];
    },
    computedYAxisMin() {
      return this.isLogY
        ? Math.pow(10, this.logStressRange[0])
        : this.linearStressRange[0];
    },

    computedYAxisMax() {
      return this.isLogY
        ? Math.pow(10, this.logStressRange[1])
        : this.linearStressRange[1];
    },

    sliderDisplayStressStart() {
      return this.formatStressValue(this.computedYAxisMin);
    },

    sliderDisplayStressEnd() {
      return this.formatStressValue(this.computedYAxisMax);
    },

    leftTicks() {
      if (this.isLogY) {
        const start = Math.ceil(this.logStressBounds[0]);
        const end = Math.floor(this.logStressBounds[1]);

        return Array.from({ length: end - start + 1 }, (_, i) => ({
          value: start + i,
          label: `10^${start + i}`,
        }));
      }

      const [min, max] = this.linearStressBounds;
      const span = max - min;

      if (span <= 0) {
        return [{ value: min, label: this.formatStressValue(min) }];
      }

      const step = span / 4;
      return Array.from({ length: 5 }, (_, i) => {
        const value = min + i * step;
        return {
          value,
          label: this.formatStressValue(value),
        };
      });
    },

    sliderDisplayStart() {
      return this.formatCycleValue(this.computedXAxisMin);
    },

    sliderDisplayEnd() {
      return this.formatCycleValue(this.computedXAxisMax);
    },

    bottomTicks() {
      if (this.isLogX) {
        const start = Math.ceil(this.logCycleBounds[0]);
        const end = Math.floor(this.logCycleBounds[1]);

        return Array.from({ length: end - start + 1 }, (_, i) => ({
          value: start + i,
          label: `10^${start + i}`,
        }));
      }

      const [min, max] = this.linearCycleBounds;
      const span = max - min;

      if (span <= 0) {
        return [{ value: min, label: this.formatCycleValue(min) }];
      }

      const step = span / 4;
      return Array.from({ length: 5 }, (_, i) => {
        const value = min + i * step;
        return {
          value,
          label: this.formatCycleValue(value),
        };
      });
    },

    visibleYBounds() {
      const xMin = this.computedXAxisMin ?? -Infinity;
      const xMax = this.computedXAxisMax ?? Infinity;

      const visibleY = this.series
        .flatMap((serie) => serie.data || [])
        .filter(
          (point) =>
            Array.isArray(point) &&
            Number.isFinite(Number(point[0])) &&
            Number.isFinite(Number(point[1])) &&
            Number(point[0]) >= xMin &&
            Number(point[0]) <= xMax
        )
        .map((point) => Number(point[1]));

      if (!visibleY.length) return null;

      return {
        min: Math.min(...visibleY),
        max: Math.max(...visibleY),
      };
    },

    selectedMethodCards() {
      return this.selectedMethods.flatMap((method) => {
        const output = this.outputs?.[method];
        if (!output) return [];

        const json = this.parseJsonData(output.json_data);

        return this.selectedRRatios.map((rRatio) => {
          const fit = this.getFitForRatio(json, rRatio);

          return {
            key: `${method}-${rRatio}`,
            method,
            rRatio,
            color: this.methodColor(method, rRatio),
            equation: this.methodEquation(method),
            params: this.formatParams(method, fit),
            dashedMeaning: this.dashedMeaning(method),
          };
        });
      });
    },
  },
  watch: {
    visibleYBounds: {
      handler() {
        this.updateStressBounds();
      },
      deep: true,
      immediate: true,
    },
  },
  methods: {
    resetState() {
      this.outputs = {};
      this.series = [];
      this.rRatios = [];
      this.selectedRRatios = [];
    },

    niceLinearStressMax(value) {
      const n = Number(value);
      if (!Number.isFinite(n) || n <= 0) return 10;

      if (n < 10) return Math.ceil(n);
      if (n < 100) return Math.ceil(n / 5) * 5;
      if (n < 1000) return Math.ceil(n / 10) * 10;
      if (n < 10000) return Math.ceil(n / 50) * 50;

      return Math.ceil(n / 100) * 100;
    },

    niceLinearStressMin(value) {
      const n = Number(value);
      if (!Number.isFinite(n) || n <= 0) return 0;

      if (n < 10) return Math.max(0, Math.floor(n));
      if (n < 100) return Math.max(0, Math.floor(n / 5) * 5);
      if (n < 1000) return Math.max(0, Math.floor(n / 10) * 10);
      if (n < 10000) return Math.max(0, Math.floor(n / 50) * 50);

      return Math.max(0, Math.floor(n / 100) * 100);
    },

    formatStressValue(value) {
      const n = Number(value);
      if (!Number.isFinite(n)) return "-";

      if (n >= 1000 || n < 0.01) {
        return n.toExponential(1);
      }

      return n.toLocaleString(undefined, {
        maximumFractionDigits: 2,
        useGrouping: false,
      });
    },

    clampRange(range, bounds) {
      if (!Array.isArray(range) || !Array.isArray(bounds)) return bounds;

      let [r0, r1] = range;
      const [b0, b1] = bounds;

      r0 = Math.max(b0, Math.min(r0, b1));
      r1 = Math.max(b0, Math.min(r1, b1));

      if (r0 > r1) return [b0, b1];
      return [r0, r1];
    },

    updateStressBounds() {
      if (!this.visibleYBounds) return;

      const minY = this.visibleYBounds.min;
      const maxY = this.visibleYBounds.max;

      if (!Number.isFinite(minY) || !Number.isFinite(maxY) || minY <= 0) return;

      this.logStressBounds = [Math.floor(Math.log10(minY)), Math.log10(maxY)];
      this.logStressRange = this.clampRange(
        this.logStressRange,
        this.logStressBounds
      );

      this.linearStressBounds = [
        this.niceLinearStressMin(minY),
        this.niceLinearStressMax(maxY),
      ];
      this.linearStressRange = this.clampRange(
        this.linearStressRange,
        this.linearStressBounds
      );
    },

    spreadOverlappingPoints(points) {
      const groups = new Map();

      points.forEach((point) => {
        const key = `${point[0]}__${point[1]}`;
        if (!groups.has(key)) groups.set(key, []);
        groups.get(key).push(point);
      });

      const spreadPoints = [];

      groups.forEach((group) => {
        if (group.length === 1) {
          spreadPoints.push(group[0]);
          return;
        }

        const mid = (group.length - 1) / 2;

        group.forEach((point, index) => {
          const x = Number(point[0]);
          const y = Number(point[1]);

          const offset = (index - mid) * 0.012; // léger décalage visuel
          const jitteredX = x * Math.pow(10, offset); // mieux pour axe log

          spreadPoints.push([jitteredX, y]);
        });
      });

      return spreadPoints;
    },

    onFileChange() {
      this.resetState();
      this.updateOutput();
    },

    parseJsonData(json) {
      try {
        return typeof json === "string" ? JSON.parse(json) : json;
      } catch {
        return null;
      }
    },

    niceLinearCycleMax(value) {
      const n = Number(value);
      if (!Number.isFinite(n) || n <= 0) return 1;

      if (n < 100) return Math.ceil(n / 10) * 10;
      if (n < 1000) return Math.ceil(n / 50) * 50;
      if (n < 10000) return Math.ceil(n / 100) * 100;
      if (n < 100000) return Math.ceil(n / 500) * 500;
      if (n < 1000000) return Math.ceil(n / 10000) * 10000;
      if (n < 10000000) return Math.ceil(n / 100000) * 100000;
      if (n < 100000000) return Math.ceil(n / 1000000) * 1000000;
      if (n < 1000000000) return Math.ceil(n / 10000000) * 10000000;

      return Math.ceil(n / 100000000) * 100000000;
    },

    niceLinearCycleMin(value) {
      const n = Number(value);
      if (!Number.isFinite(n) || n <= 0) return 1;

      if (n < 100) return Math.max(1, Math.floor(n / 10) * 10);
      if (n < 1000) return Math.max(1, Math.floor(n / 50) * 50);
      if (n < 10000) return Math.max(1, Math.floor(n / 100) * 100);
      if (n < 100000) return Math.max(1, Math.floor(n / 500) * 500);
      if (n < 1000000) return Math.max(1, Math.floor(n / 10000) * 10000);
      if (n < 10000000) return Math.max(1, Math.floor(n / 100000) * 100000);
      if (n < 100000000) return Math.max(1, Math.floor(n / 1000000) * 1000000);
      if (n < 1000000000) {
        return Math.max(1, Math.floor(n / 10000000) * 10000000);
      }

      return Math.max(1, Math.floor(n / 100000000) * 100000000);
    },

    niceStressBound(value, direction = "up") {
      const n = Number(value);
      if (!Number.isFinite(n)) return null;

      const step = 5;
      return direction === "down"
        ? Math.floor(n / step) * step
        : Math.ceil(n / step) * step;
    },

    formatCycleValue(value) {
      const n = Number(value);
      if (!Number.isFinite(n)) return "-";

      if (n >= 1e6) {
        const exp = Math.floor(Math.log10(n));
        const mantissa = n / Math.pow(10, exp);
        const rounded =
          Math.abs(mantissa) >= 10
            ? mantissa.toFixed(0)
            : mantissa.toFixed(1).replace(/\.0$/, "");

        return `${rounded}e${exp}`;
      }

      return n.toLocaleString(undefined, {
        maximumFractionDigits: 0,
        useGrouping: false,
      });
    },

    fmtNumber(value, name = "") {
      if (value === undefined || value === null || value === "") return "—";

      const n = Number(value);
      if (!Number.isFinite(n)) return String(value);

      const key = String(name).toLowerCase();

      if (["c", "cstar", "c*"].includes(key)) {
        return this.formatAdaptiveNumber(n, 8);
      }

      if (["s", "sstar", "s*", "power"].includes(key)) {
        return this.formatAdaptiveNumber(n, 5);
      }

      if (
        ["alpha", "alpha_f", "beta", "a", "b", "q0", "sigma0"].includes(key)
      ) {
        return this.formatAdaptiveNumber(n, 2);
      }

      return this.formatAdaptiveNumber(n, 4);
    },

    formatAdaptiveNumber(value, maxDecimals = 4) {
      const n = Number(value);
      if (!Number.isFinite(n)) return "—";

      const abs = Math.abs(n);

      if (abs === 0) return "0";

      if (abs >= 1000) {
        return n.toLocaleString(undefined, {
          minimumFractionDigits: 0,
          maximumFractionDigits: 0,
          useGrouping: false,
        });
      }

      if (abs >= 100) {
        return n.toLocaleString(undefined, {
          minimumFractionDigits: 0,
          maximumFractionDigits: Math.min(maxDecimals, 1),
          useGrouping: false,
        });
      }

      if (abs >= 1) {
        return n.toLocaleString(undefined, {
          minimumFractionDigits: 0,
          maximumFractionDigits: Math.min(maxDecimals, 2),
          useGrouping: false,
        });
      }

      if (abs >= 0.01) {
        return n.toLocaleString(undefined, {
          minimumFractionDigits: 0,
          maximumFractionDigits: Math.max(maxDecimals, 4),
          useGrouping: false,
        });
      }

      return n.toLocaleString(undefined, {
        minimumFractionDigits: 0,
        maximumFractionDigits: Math.max(maxDecimals, 8),
        useGrouping: false,
      });
    },

    deepFindValue(obj, keyAliases) {
      if (!obj || typeof obj !== "object") return undefined;

      const aliases = (keyAliases || []).map((key) =>
        String(key).toLowerCase()
      );

      for (const key of Object.keys(obj)) {
        if (aliases.includes(String(key).toLowerCase())) {
          return obj[key];
        }
      }

      for (const value of Object.values(obj)) {
        if (value && typeof value === "object") {
          const found = this.deepFindValue(value, keyAliases);
          if (found !== undefined) return found;
        }
      }

      return undefined;
    },

    getFitForRatio(json, rRatio) {
      if (!json) return null;

      const key = String(rRatio);

      if (json.by_r?.[key] || json.by_r?.[rRatio]) {
        return json.by_r[key] || json.by_r[rRatio];
      }

      if (json.params?.[key] || json.params?.[rRatio]) {
        return json.params[key] || json.params[rRatio];
      }

      if (json.parameters?.[key] || json.parameters?.[rRatio]) {
        return json.parameters[key] || json.parameters[rRatio];
      }

      if (json[key] || json[rRatio]) {
        return json[key] || json[rRatio];
      }

      const fits =
        (Array.isArray(json) && json) ||
        (Array.isArray(json.results) && json.results) ||
        (Array.isArray(json.fits) && json.fits) ||
        null;

      if (fits) {
        return fits.find((item) => String(item.stress_ratio) === key) || null;
      }

      if (
        json.alpha ||
        json.beta ||
        json.A ||
        json.B ||
        json.slope ||
        json.intercept
      ) {
        return json;
      }

      return null;
    },

    formatParams(method, fit) {
      if (!fit) return [];

      const scope =
        fit.params && typeof fit.params === "object" ? fit.params : fit;

      const getAny = (aliases) => this.deepFindValue(scope, aliases);

      if (method === "LinLog" || method === "LogLog") {
        const A = getAny(["A", "a", "intercept", "c0"]);
        const B = getAny(["B", "b", "slope", "c1"]);

        return [
          {
            name: "A",
            value: this.fmtNumber(A, "A"),
          },
          {
            name: "B",
            value: this.fmtNumber(B, "B"),
          },
        ];
      }

      if (method === "Sendeckyj") {
        return [
          { name: "alpha", value: this.fmtNumber(fit.a, "alpha") },
          { name: "beta", value: this.fmtNumber(fit.b, "beta") },
          { name: "s", value: this.fmtNumber(fit.sstar, "s") },
          { name: "c", value: this.fmtNumber(fit.cstar, "c") },
        ];
      }

      if (method === "Whitney") {
        return [
          { name: "alpha_f", value: this.fmtNumber(fit.alpha_f, "alpha_f") },
          { name: "q0", value: this.fmtNumber(fit.q0, "q0") },
          { name: "sigma0", value: this.fmtNumber(fit.sigma0, "sigma0") },
          { name: "power", value: this.fmtNumber(fit.power, "power") },
        ];
      }

      return [];
    },

    buildLineSeries(method, rRatio, rows, yKey, color, dashed = false) {
      return {
        type: "line",
        name: `${method} ${rRatio}`,
        showSymbol: false,
        data: rows.map((row) => [
          Number(row.cycles_to_failure),
          Number(row[yKey]),
        ]),
        lineStyle: {
          width: dashed ? 1 : 2,
          type: dashed ? "dashed" : "solid",
          color,
        },
        itemStyle: { color },
        silent: dashed,
        showInLegend: !dashed,
      };
    },

    buildScatterSeries(rRatio, data) {
      const symbols = ["circle", "triangle", "rect", "cross", "diamond"];
      const index = this.rRatios.findIndex(
        (value) => Number(value) === Number(rRatio)
      );
      const symbol = symbols[index % symbols.length];

      const rawPoints = data
        .filter((row) => Number(row.stress_ratio) === Number(rRatio))
        .map((row) => [Number(row.cycles_to_failure), Number(row.stress_max)]);

      const displayPoints = this.spreadOverlappingPoints(rawPoints);

      return {
        type: "scatter",
        name: `Experiment data R=${rRatio}`,
        symbol,
        symbolSize: 7,
        itemStyle: {
          color: "#000000",
          opacity: 0.8,
        },
        data: displayPoints,
        rRatio,
      };
    },

    updateOutput() {
      if (!this.file || this.selectedMethods.length === 0) {
        this.outputs = {};
        this.series = [];
        return;
      }

      const currentRequestId = ++this.requestId;
      const selectedMethodsSnapshot = [...this.selectedMethods];
      const selectedRRatiosSnapshot = [...this.selectedRRatios];

      this.loading = true;
      this.outputs = {};
      this.series = [];

      parseFile(this.file)
        .then((parsedInputFile) => {
          if (currentRequestId !== this.requestId) return null;

          const detectedRRatios = [
            ...new Set(
              parsedInputFile.data
                .map((row) => Number(row.stress_ratio))
                .filter(Number.isFinite)
            ),
          ].sort((a, b) => a - b);

          this.rRatios = detectedRRatios;

          let activeRRatios = selectedRRatiosSnapshot.filter((rRatio) =>
            detectedRRatios.includes(rRatio)
          );

          if (activeRRatios.length === 0 && detectedRRatios.length > 0) {
            activeRRatios = [detectedRRatios[0]];
          }

          this.selectedRRatios = activeRRatios;

          return Promise.all(
            selectedMethodsSnapshot.map((method) => {
              const confidenceToSend =
                method === "Sendeckyj"
                  ? this.sendeckyjProbability
                  : method === "Whitney"
                  ? this.whitneyProbability
                  : undefined;

              return this.$analysisApi
                .runSnCurveFile(method, this.file, confidenceToSend)
                .then((result) => {
                  const parsedResult = parse(result.csv_data, parserConfig);

                  const rows =
                    activeRRatios.length > 0
                      ? parsedResult.data.filter((row) =>
                          activeRRatios.includes(Number(row.stress_ratio))
                        )
                      : parsedResult.data;

                  return {
                    method,
                    analysisResult: result,
                    rows,
                  };
                });
            })
          ).then((analysisResults) => ({
            parsedInputFile,
            analysisResults,
            activeRRatios,
          }));
        })
        .then((payload) => {
          if (!payload || currentRequestId !== this.requestId) return;

          const { parsedInputFile, analysisResults, activeRRatios } = payload;

          this.outputs = Object.fromEntries(
            analysisResults.map(({ method, analysisResult }) => [
              method,
              analysisResult,
            ])
          );

          this.series = [
            ...analysisResults.flatMap(({ method, rows }) =>
              activeRRatios.flatMap((rRatio) => {
                const filteredRows = rows.filter(
                  (row) => Number(row.stress_ratio) === Number(rRatio)
                );

                const color = this.methodColor(method, rRatio);

                const baseSeries = [
                  this.buildLineSeries(
                    method,
                    rRatio,
                    filteredRows,
                    "stress_max",
                    color
                  ),
                ];

                if (this.showReliabilityBands) {
                  baseSeries.push(
                    this.buildLineSeries(
                      method,
                      rRatio,
                      filteredRows,
                      "stress_lowerbound",
                      color,
                      true
                    ),
                    this.buildLineSeries(
                      method,
                      rRatio,
                      filteredRows,
                      "stress_upperbound",
                      color,
                      true
                    )
                  );
                }

                return baseSeries;
              })
            ),
            ...activeRRatios.map((rRatio) =>
              this.buildScatterSeries(rRatio, parsedInputFile.data)
            ),
          ];

          const allX = this.series
            .flatMap((serie) => serie.data || [])
            .map((point) =>
              Array.isArray(point) ? Number(point[0]) : Number(point)
            )
            .filter((value) => Number.isFinite(value) && value > 0);

          if (allX.length > 0) {
            const minX = Math.min(...allX);
            const maxX = Math.max(...allX);

            this.logCycleBounds = [
              Math.floor(Math.log10(minX)),
              Math.log10(maxX),
            ];
            this.logCycleRange = [...this.logCycleBounds];

            this.linearCycleBounds = [
              this.niceLinearCycleMin(minX),
              this.niceLinearCycleMax(maxX),
            ];
            this.linearCycleRange = [...this.linearCycleBounds];
          }
          const allY = this.series
            .flatMap((serie) => serie.data || [])
            .map((point) => (Array.isArray(point) ? Number(point[1]) : null))
            .filter((value) => Number.isFinite(value) && value > 0);

          if (allY.length > 0) {
            const minY = Math.min(...allY);
            const maxY = Math.max(...allY);

            this.logStressBounds = [
              Math.floor(Math.log10(minY)),
              Math.log10(maxY),
            ];
            this.logStressRange = [...this.logStressBounds];

            this.linearStressBounds = [
              this.niceLinearStressMin(minY),
              this.niceLinearStressMax(maxY),
            ];
            this.linearStressRange = [...this.linearStressBounds];
          }
        })
        .catch((error) => {
          if (currentRequestId !== this.requestId) return;

          console.error("SnCurve error:", error);
          console.error("SnCurve error.response:", error?.response);
          console.error("SnCurve error.response.data:", error?.response?.data);

          let errorMessage =
            error?.response?.data?.detail ||
            error?.response?.data?.message ||
            error?.response?.data ||
            error?.message;

          if (typeof errorMessage === "object") {
            errorMessage = JSON.stringify(errorMessage);
          }

          alert(errorMessage || "S-N curve analysis failed");

          this.outputs = {};
          this.series = [];
        })
        .finally(() => {
          if (currentRequestId === this.requestId) {
            this.loading = false;
          }
        });
    },

    downloadOutput() {
      if (!this.file) return;

      if (this.selectedMethods.length !== 1) {
        this.downloadMessage = "Please select just one method to download SNC.";
        this.showDownloadMessage = true;
        return;
      }

      const method = this.selectedMethods[0];
      const output = this.outputs?.[method];
      if (!output) return;

      const outputName = getOutputFileName(
        "AGG",
        "SNC",
        this.file.name,
        method
      );

      download(output.csv_data, `${outputName}.csv`, "text/csv");
      download(output.json_data, `${outputName}.json`, "application/json");
    },

    onDownloadClick() {
      if (
        this.loading ||
        !this.outputs ||
        Object.keys(this.outputs).length === 0
      ) {
        return;
      }

      if (this.selectedMethods.length !== 1) {
        this.downloadMessage = "Please select just one method to download SNC.";
        this.showDownloadMessage = true;
        return;
      }

      this.downloadOutput();
    },

    methodColor(method, rRatio = null) {
      const palettes = {
        LinLog: ["#d62728", "#ff6b6b", "#8b0000", "#ff9ea1"],
        LogLog: ["#2ca02c", "#7bd87b", "#0b6e0b", "#9be79b"],
        Sendeckyj: ["#1f77b4", "#66b3ff", "#0b4f8a", "#9ccfff"],
        Whitney: ["#9467bd", "#c5a3ff", "#5e3a87", "#d8c2ff"],
      };

      const palette = palettes[method] || ["#000000"];
      if (rRatio == null) return palette[0];

      const index = this.rRatios.findIndex(
        (value) => Number(value) === Number(rRatio)
      );

      return palette[index >= 0 ? index % palette.length : 0];
    },

    methodEquation(method) {
      if (method === "LinLog") return "σ_max = A + B·log10(N)";
      if (method === "LogLog") return "σ_max = A · N^(-B)";
      if (method === "Sendeckyj") return "σ_max = b + a·(N + c*)^{-s*}";
      if (method === "Whitney") {
        return "σ_max = sigma0 · (-ln(Ps))^(power/alpha_f) · N^(-power)";
      }
      return "";
    },

    dashedMeaning(method) {
      return method === "Sendeckyj" || method === "Whitney"
        ? "95% survival probability"
        : "95% confidence interval";
    },
  },
};
</script>

<style scoped>
.method-banner-card {
  border: 1px solid #e0e0e0;
  border-radius: 12px;
  padding: 16px;
  background: #fafafa;
}

.method-banner-title {
  font-size: 18px;
  font-weight: 700;
  line-height: 1.2;
}

.method-banner-ratio {
  font-size: 14px;
  font-weight: 600;
  color: #555;
  background: #f0f0f0;
  border-radius: 999px;
  padding: 4px 10px;
}

.method-banner-meta {
  font-size: 14px;
  color: #666;
  font-weight: 500;
}

.method-banner-section-label {
  font-size: 14px;
  font-weight: 700;
  color: #222;
  margin-bottom: 6px;
}

.method-banner-equation {
  background: #f3f4f6;
  border-radius: 8px;
  padding: 10px 12px;
  font-size: 14px;
  white-space: normal;
  word-break: break-word;
}

.method-banner-params {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 8px 12px;
}

.method-banner-param {
  display: flex;
  flex-direction: column;
  background: white;
  border: 1px solid #ececec;
  border-radius: 8px;
  padding: 8px 10px;
}

.param-name {
  font-size: 13px;
  font-weight: 700;
  color: #444;
}

.param-value {
  font-size: 14px;
  color: #111;
}

.method-banner-dashed {
  display: flex;
  align-items: center;
  font-size: 14px;
  color: #555;
}

.method-banner-empty {
  font-size: 14px;
  color: #777;
  font-style: italic;
}
</style>

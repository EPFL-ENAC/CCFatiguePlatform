<template>
  <v-card :loading="loading">
    <v-card-title>
      Constant Life Diagram
      <v-spacer />
      <info-tooltip>
        The Constant Life Diagram (CLD) allows us to predict the outcome of
        tests for different values of stress ratio (R). The results are
        represented in a Mean Stress - Stress Amplitude plane and define what is
        considered to be the safe use region.
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
            :label="fileInputLabel"
            @change="updateOutput"
          >
            <template #append>
              <info-tooltip>
                See the
                <a
                  href="https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/Data/SNC_Data_Convention.md"
                  >SNC Data Convention</a
                >
              </info-tooltip>
            </template>
          </v-file-input>
        </v-col>
        <v-col>
          <v-select
            v-model="selectedMethods"
            :items="methods"
            :item-text="methodLabel"
            label="Select Method(s)"
            chips
            multiple
            :disabled="loading"
            @change="updateOutput"
          >
          </v-select>
        </v-col>
      </v-row>
      <v-row>
        <v-col>
          <v-text-field
            v-model.number="ucs"
            label="UCS"
            type="number"
            @change="updateOutput"
          ></v-text-field>
        </v-col>
        <v-col>
          <v-text-field
            v-model.number="uts"
            label="UTS"
            type="number"
            @change="updateOutput"
          ></v-text-field>
        </v-col>
        <v-col v-if="isPiecewiseLinear || isPiecewiseNonLinear">
          <v-select
            v-model="snCurveMethod"
            :items="snCurveMethods"
            label="SN Curve Method (optional)"
            clearable
            :disabled="loading"
            hint="Select to use an AGG file as input instead of SNC"
            persistent-hint
            @change="updateOutput"
          ></v-select>
        </v-col>
      </v-row>
    </v-card-subtitle>

    <v-card-text v-if="series.length > 0">
      <v-alert
        v-if="methodWarnings.length > 0"
        type="warning"
        icon="mdi-alert"
        outlined
        dense
        class="mb-4"
      >
        <div v-for="(item, idx) in methodWarnings" :key="idx">
          <strong v-if="selectedMethods.length > 1">
            {{ methodLabel(item.method) }}:
          </strong>
          {{ item.warning }}
        </div>
      </v-alert>

      <v-row align="start">
        <!-- Side panel: method legend + Boerstra params -->
        <v-col v-if="showPanel" cols="12" md="3" class="mt-2">
          <v-card variant="outlined" class="pa-3">
            <div class="text-subtitle-2 mb-3" style="font-weight: 700">
              Methods
            </div>
            <div
              v-for="m in selectedMethods"
              :key="m"
              class="d-flex align-center mb-2"
            >
              <svg
                width="32"
                height="6"
                style="flex-shrink: 0; margin-right: 8px"
              >
                <line
                  x1="0"
                  y1="3"
                  x2="32"
                  y2="3"
                  stroke="#333"
                  stroke-width="2"
                  :stroke-dasharray="methodDashArray(m)"
                />
              </svg>
              <span style="font-size: 13px">{{ methodLabel(m) }}</span>
            </div>

            <template v-if="selectedMethods.includes('Boerstra')">
              <v-divider class="my-3" />
              <div class="text-subtitle-2 mb-3" style="font-weight: 700">
                Boerstra Parameters
              </div>

              <div class="param-section-label mb-2">
                Initial value (optional)
              </div>

              <v-text-field
                v-model.number="npReference"
                label="Reference life (Np)"
                type="number"
                density="compact"
                variant="outlined"
                hide-details
                class="mb-2"
                :disabled="loading"
                @change="updateOutput"
              />

              <v-divider class="mb-3" />
              <div class="param-section-label mb-2">Fitted values</div>
              <div
                v-if="!boerstraParams"
                class="text-caption text-grey font-italic mb-2"
              >
                Run analysis to see fitted parameters.
              </div>
              <div v-else class="boerstra-params">
                <div
                  v-for="param in boerstraParamCards"
                  :key="param.name"
                  class="boerstra-param-row"
                >
                  <!-- eslint-disable-next-line vue/no-v-html -->
                  <span class="boerstra-param-name" v-html="param.name"></span>
                  <span class="boerstra-param-value">{{ param.value }}</span>
                </div>
              </div>
            </template>
          </v-card>
        </v-col>

        <!-- Chart -->
        <v-col cols="12" :md="showPanel ? 9 : 12">
          <div style="max-width: 1000px; margin: 0 auto">
            <simple-chart
              :height="500"
              :series="chartSeries"
              x-axis-name="Mean Stress [MPa]"
              y-axis-name="Stress Amplitude [MPa]"
            ></simple-chart>
          </div>
        </v-col>
      </v-row>

      <!-- R-ratio analysis panel -->
      <v-row class="mt-4">
        <v-col>
          <v-card variant="outlined" class="pa-4">
            <div class="text-subtitle-2 mb-3" style="font-weight: 700">
              CLD Analysis
            </div>
            <v-row>
              <v-col cols="12" md="3">
                <v-text-field
                  v-model="desiredR"
                  label="Desired R-ratio"
                  type="number"
                  step="0.1"
                  density="compact"
                  variant="outlined"
                  hide-details="auto"
                  hint="e.g. 0.1, -1, 0.5"
                  persistent-hint
                />
              </v-col>
              <v-col cols="12" md="3">
                <v-text-field
                  v-model="desiredN"
                  label="Desired N (cycles)"
                  type="number"
                  step="1"
                  min="1"
                  density="compact"
                  variant="outlined"
                  hide-details="auto"
                  hint="optional — e.g. 500 000"
                  persistent-hint
                />
              </v-col>
              <v-col v-if="showExpSnMethodSelector" cols="12" md="3">
                <v-select
                  v-model="expSnMethod"
                  :items="snCurveMethods"
                  label="Experimental SN method"
                  density="compact"
                  variant="outlined"
                  hide-details="auto"
                  hint="R-ratio found in input — select method to fit experimental SN curve"
                  persistent-hint
                  :loading="expSnLoading"
                />
              </v-col>
            </v-row>

            <template v-if="rRatioValid && rRatioIntersectionRows.length > 0">
              <v-divider class="my-3" />
              <div class="d-flex align-center mb-2">
                <span class="param-section-label">
                  σ<sub>max</sub> at intersection with R =
                  {{ parseFloat(desiredR).toFixed(2) }}
                </span>
                <v-spacer />
                <v-btn @click="downloadRRatioTable"> Download Analysis </v-btn>
              </div>
              <v-simple-table dense class="r-ratio-table mt-2">
                <template #default>
                  <thead>
                    <tr>
                      <th>N (cycles)</th>
                      <th v-for="m in selectedMethods" :key="m">
                        σ<sub>max</sub>
                        <template v-if="selectedMethods.length > 1">
                          — {{ methodLabel(m) }}
                        </template>
                        [MPa]
                      </th>
                      <th v-if="desiredRFoundInInput && expSnData">
                        σ<sub>max</sub> exp. [MPa]
                      </th>
                    </tr>
                  </thead>
                  <tbody>
                    <tr
                      v-for="row in rRatioIntersectionRows"
                      :key="String(row.N) + (row.isDesired ? '-d' : '')"
                      :class="row.isDesired ? 'desired-n-row' : ''"
                    >
                      <td>
                        <strong v-if="row.isDesired">
                          {{ formatN(row.N) }} ★
                        </strong>
                        <span v-else>{{ superscript(row.N) }}</span>
                      </td>
                      <td v-for="m in selectedMethods" :key="m">
                        {{ formatSigmaMax(row.perMethod[m]) }}
                      </td>
                      <td v-if="desiredRFoundInInput && expSnData">
                        {{ formatSigmaMax(row.expSigmaMax) }}
                      </td>
                    </tr>
                  </tbody>
                </template>
              </v-simple-table>
            </template>
          </v-card>
        </v-col>
      </v-row>
    </v-card-text>

    <v-card-actions v-if="hasInput" class="justify-end">
      <v-btn :disabled="downloadDisabled" @click="downloadOutput">
        Download CLD
        <info-tooltip>
          See the
          <a
            href="https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/Data/CLD_Data_Convention.md"
            >CLD Data Convention</a
          >
        </info-tooltip>
      </v-btn>
    </v-card-actions>
  </v-card>
</template>

<script>
import CldMethod from "@/backend/model/CldMethod";
import SimpleChart from "@/components/charts/SimpleChart";
import InfoTooltip from "@/components/InfoTooltip";
import { getOutputFileName } from "@/utils/analysis";
import { parseFile, parserConfig } from "@/utils/papaparse";
import download from "downloadjs";
import { groupBy } from "lodash";
import { parse, unparse } from "papaparse";

const METHOD_LABELS = {
  SimplifiedHarris: "Simplified Harris",
};
const methods = Object.values(new CldMethod());

const CYCLE_LEVELS = [1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9];
const CYCLE_COLORS = [
  "#d62728",
  "#ff7f0e",
  "#bcbd22",
  "#2ca02c",
  "#17becf",
  "#1f77b4",
  "#9467bd",
];

export default {
  name: "CldAnalysis",
  components: {
    InfoTooltip,
    SimpleChart,
  },
  data() {
    return {
      file: null,
      loading: false,
      outputs: {},
      methods: methods,
      selectedMethods: [methods[0]],
      ucs: 27.1,
      uts: 27.7,
      npReference: null,
      series: [],
      cldDataByMethod: {},
      desiredR: "",
      desiredN: "",
      snCurveMethod: null,
      snCurveMethods: ["LinLog", "LogLog", "Sendeckyj", "Whitney"],
      errorMessages: null,
      requestId: 0,
      parsedInputData: [],
      expSnMethod: null,
      expSnData: null,
      expSnLoading: false,
    };
  },
  computed: {
    hasInput: function () {
      return this.file != null;
    },

    isPiecewiseLinear() {
      return this.selectedMethods.includes("PiecewiseLinear");
    },

    isPiecewiseNonLinear() {
      return this.selectedMethods.includes("PiecewiseNonLinear");
    },

    fileInputLabel() {
      if (
        (this.isPiecewiseLinear || this.isPiecewiseNonLinear) &&
        this.snCurveMethod
      ) {
        return "AGG csv file (raw fatigue test data)";
      }
      return "SNC csv file";
    },

    downloadDisabled() {
      return this.loading || Object.keys(this.outputs).length === 0;
    },

    showPanel() {
      return (
        this.selectedMethods.length > 1 ||
        this.selectedMethods.includes("Boerstra")
      );
    },

    boerstraParams() {
      if (!this.selectedMethods.includes("Boerstra")) return null;
      const out = this.outputs["Boerstra"];
      if (!out?.json_data) return null;
      try {
        return typeof out.json_data === "string"
          ? JSON.parse(out.json_data)
          : out.json_data;
      } catch {
        return null;
      }
    },

    methodWarnings() {
      const messages = [];
      for (const method of this.selectedMethods) {
        const out = this.outputs[method];
        if (!out?.json_data) continue;
        let parsed;
        try {
          parsed =
            typeof out.json_data === "string"
              ? JSON.parse(out.json_data)
              : out.json_data;
        } catch {
          continue;
        }
        for (const warning of parsed?.warnings ?? []) {
          messages.push({ method, warning });
        }
      }
      return messages;
    },

    rRatioValid() {
      const r = parseFloat(this.desiredR);
      return (
        this.desiredR !== "" &&
        this.desiredR !== null &&
        Number.isFinite(r) &&
        r !== 1
      );
    },

    desiredNValid() {
      const n = parseFloat(this.desiredN);
      return (
        this.desiredN !== "" &&
        this.desiredN !== null &&
        Number.isFinite(n) &&
        n > 0
      );
    },

    cldSaMax() {
      const allPoints = Object.values(this.cldDataByMethod).flatMap((byN) =>
        Object.values(byN).flat()
      );
      if (allPoints.length === 0) return null;
      return Math.max(...allPoints.map((p) => p[1])) * 1.05;
    },

    rRatioLineData() {
      if (!this.rRatioValid || Object.keys(this.cldDataByMethod).length === 0)
        return [];
      const R = parseFloat(this.desiredR);

      const allPoints = Object.values(this.cldDataByMethod).flatMap((byN) =>
        Object.values(byN).flat()
      );
      if (allPoints.length === 0) return [];

      const smValues = allPoints.map((p) => p[0]);
      const smMin = Math.min(...smValues);
      const smMax = Math.max(...smValues);
      const saMax = this.cldSaMax; // max CLD σ_a * 1.05

      if (Math.abs(R + 1) < 1e-6) {
        return [
          [0, 0],
          [0, saMax],
        ];
      }

      const k = (1 - R) / (1 + R);
      if (k >= 0) {
        // Positive slope: clip at right edge or top
        const smEnd = Math.min(smMax, saMax / k);
        return [
          [0, 0],
          [smEnd, k * smEnd],
        ];
      } else {
        // Negative slope (k < 0): line is in σ_m < 0 half-plane, clip at top or left edge
        const smStart = Math.max(smMin, saMax / k); // saMax/k < 0
        return [
          [smStart, k * smStart],
          [0, 0],
        ];
      }
    },

    rRatioIntersectionsData() {
      if (!this.rRatioValid || Object.keys(this.cldDataByMethod).length === 0)
        return {};
      const R = parseFloat(this.desiredR);
      const result = {};
      for (const [method, byN] of Object.entries(this.cldDataByMethod)) {
        result[method] = {};
        for (const [nKey, points] of Object.entries(byN)) {
          const hit = this.findRRatioIntersection(points, R);
          if (hit !== null) result[method][Number(nKey)] = hit;
        }
      }
      return result;
    },

    rRatioIntersectionRows() {
      if (!this.rRatioValid) return [];
      const activeMethods = Object.keys(this.rRatioIntersectionsData);
      if (activeMethods.length === 0) return [];

      const rows = CYCLE_LEVELS.map((N) => {
        const perMethod = {};
        for (const m of activeMethods) {
          const data = this.rRatioIntersectionsData[m] ?? {};
          perMethod[m] =
            data[N]?.sigma_max ?? this.interpolateSigmaMax(N, data);
        }
        return {
          N,
          perMethod,
          isDesired: false,
          expSigmaMax: this.interpolateExpSigmaMax(N),
        };
      });

      if (this.desiredNValid) {
        const Nd = parseFloat(this.desiredN);
        const perMethod = {};
        for (const m of activeMethods) {
          perMethod[m] = this.interpolateSigmaMax(
            Nd,
            this.rRatioIntersectionsData[m] ?? {}
          );
        }
        rows.push({
          N: Nd,
          perMethod,
          isDesired: true,
          expSigmaMax: this.interpolateExpSigmaMax(Nd),
        });
      }

      return rows;
    },

    desiredIsolifeSeries() {
      if (!this.desiredNValid || Object.keys(this.cldDataByMethod).length === 0)
        return [];
      const Nd = parseFloat(this.desiredN);
      const highlightColor = "#e040fb";
      const multiMethod = Object.keys(this.cldDataByMethod).length > 1;
      return Object.entries(this.cldDataByMethod)
        .map(([method, byN]) => {
          const points = this.buildDesiredIsolifeCurve(byN, Nd);
          if (points.length === 0) return null;
          const name = multiMethod
            ? `N = ${this.formatN(Nd)} ★ (${this.methodLabel(method)})`
            : `N = ${this.formatN(Nd)} ★`;
          return {
            type: "line",
            name,
            data: points,
            lineStyle: {
              type: this.methodLineStyle(method),
              color: highlightColor,
              width: 2,
            },
            itemStyle: { color: highlightColor },
            showSymbol: false,
            z: 9,
          };
        })
        .filter((s) => s !== null);
    },

    chartSeries() {
      const base = [...this.series, ...this.desiredIsolifeSeries];
      if (this.rRatioValid && this.rRatioLineData.length >= 2) {
        const R = parseFloat(this.desiredR);
        base.push({
          type: "line",
          name: `R = ${R.toFixed(2)}`,
          data: this.rRatioLineData,
          lineStyle: { type: "dashed", color: "#888", width: 1.5 },
          itemStyle: { color: "#888" },
          showSymbol: false,
          z: 10,
        });
      }
      return base;
    },

    boerstraParamCards() {
      if (!this.boerstraParams) return [];
      const fmt = (v) =>
        v == null
          ? "—"
          : Number(v).toLocaleString(undefined, {
              maximumFractionDigits: 4,
              useGrouping: false,
            });
      return [
        { name: "m<sub>0</sub>", value: fmt(this.boerstraParams.m0) },
        { name: "D", value: fmt(this.boerstraParams.d) },
        { name: "αT", value: fmt(this.boerstraParams.alpha_t) },
        { name: "αC", value: fmt(this.boerstraParams.alpha_c) },
        { name: "σ<sub>AP</sub>", value: fmt(this.boerstraParams.sigma_apex) },
        { name: "N<sub>p</sub>", value: fmt(this.boerstraParams.np_reference) },
      ];
    },

    inputRRatios() {
      const ratios = new Set();
      for (const row of this.parsedInputData) {
        const r = parseFloat(row.stress_ratio);
        if (Number.isFinite(r)) ratios.add(r);
      }
      return ratios;
    },

    matchingInputR() {
      if (!this.rRatioValid) return null;
      const R = parseFloat(this.desiredR);
      for (const r of this.inputRRatios) {
        if (Math.abs(r - R) < 1e-4) return r;
      }
      return null;
    },

    desiredRFoundInInput() {
      return this.matchingInputR !== null;
    },

    showExpSnMethodSelector() {
      if (!this.desiredRFoundInInput) return false;
      if (
        (this.isPiecewiseLinear || this.isPiecewiseNonLinear) &&
        this.snCurveMethod
      )
        return false;
      return true;
    },

    effectiveExpSnMethod() {
      if (
        (this.isPiecewiseLinear || this.isPiecewiseNonLinear) &&
        this.snCurveMethod
      )
        return this.snCurveMethod;
      return this.expSnMethod;
    },
  },
  watch: {
    desiredR() {
      this.maybeRunExpSn();
    },
    expSnMethod(val) {
      if (val) this.runExperimentalSn();
    },
    snCurveMethod() {
      this.maybeRunExpSn();
    },
  },
  methods: {
    methodLabel(method) {
      return METHOD_LABELS[method] ?? method;
    },

    superscript(n) {
      const sup = "⁰¹²³⁴⁵⁶⁷⁸⁹";
      const exp = Math.log10(Number(n));
      return Number.isInteger(exp) && exp > 0
        ? "10" +
            String(Math.round(exp))
              .split("")
              .map((d) => sup[d])
              .join("")
        : String(n);
    },

    cycleColor(cycle) {
      const i = CYCLE_LEVELS.indexOf(cycle);
      return CYCLE_COLORS[i >= 0 ? i : 0];
    },

    methodLineStyle(method) {
      const styles = {
        Harris: "solid",
        SimplifiedHarris: [6, 3, 2, 3],
        PiecewiseLinear: "dashed",
        PiecewiseNonLinear: [10, 3, 3, 3],
        Kawai: "dotted",
        Boerstra: [8, 3],
      };
      return styles[method] || "solid";
    },

    methodDashArray(method) {
      const da = {
        Harris: "none",
        SimplifiedHarris: "6 3 2 3",
        PiecewiseLinear: "8 4",
        PiecewiseNonLinear: "10 3 3 3",
        Kawai: "2 4",
        Boerstra: "12 4",
      };
      return da[method] || "none";
    },

    findRRatioIntersection(curve, R) {
      if (!curve || curve.length < 2) return null;

      if (Math.abs(R + 1) < 1e-6) {
        for (let i = 0; i < curve.length - 1; i++) {
          const [m1, a1] = curve[i];
          const [m2, a2] = curve[i + 1];
          if (m1 * m2 <= 0 && m1 !== m2) {
            const t = m1 / (m1 - m2);
            const sigma_a = a1 + t * (a2 - a1);
            return { sigma_m: 0, sigma_a, sigma_max: sigma_a };
          }
        }
        return null;
      }

      const k = (1 - R) / (1 + R);
      for (let i = 0; i < curve.length - 1; i++) {
        const [m1, a1] = curve[i];
        const [m2, a2] = curve[i + 1];
        const f1 = a1 - k * m1;
        const f2 = a2 - k * m2;
        if (f1 * f2 <= 0 && f1 !== f2) {
          const t = f1 / (f1 - f2);
          const sigma_m = m1 + t * (m2 - m1);
          const sigma_a = a1 + t * (a2 - a1);
          if (sigma_a < 0) return null;
          return { sigma_m, sigma_a, sigma_max: sigma_m + sigma_a };
        }
      }
      return null;
    },

    interpolateSigmaMax(N, byN) {
      const entries = Object.entries(byN)
        .map(([k, v]) => [Number(k), v.sigma_max])
        .filter(([, sm]) => sm != null && Number.isFinite(sm))
        .sort(([a], [b]) => a - b);
      if (entries.length === 0) return null;
      if (entries.length === 1) return entries[0][1];
      if (N <= entries[0][0]) return entries[0][1];
      if (N >= entries[entries.length - 1][0])
        return entries[entries.length - 1][1];
      for (let i = 0; i < entries.length - 1; i++) {
        const [n1, sm1] = entries[i];
        const [n2, sm2] = entries[i + 1];
        if (N >= n1 && N <= n2) {
          const t =
            (Math.log(N) - Math.log(n1)) / (Math.log(n2) - Math.log(n1));
          if (sm1 > 0 && sm2 > 0) {
            return Math.exp(
              Math.log(sm1) + t * (Math.log(sm2) - Math.log(sm1))
            );
          }
          return sm1 + t * (sm2 - sm1);
        }
      }
      return null;
    },

    interpolateSigmaAmplitudeOnCurve(curve, sigmaMean) {
      if (!curve || curve.length === 0) return null;
      if (sigmaMean < curve[0][0] || sigmaMean > curve[curve.length - 1][0])
        return null;
      for (let i = 0; i < curve.length - 1; i++) {
        const [m1, a1] = curve[i];
        const [m2, a2] = curve[i + 1];
        if (sigmaMean >= m1 && sigmaMean <= m2) {
          if (m1 === m2) return a1;
          return a1 + ((sigmaMean - m1) / (m2 - m1)) * (a2 - a1);
        }
      }
      return null;
    },

    buildDesiredIsolifeCurve(byN, desiredN) {
      const nKeys = Object.keys(byN)
        .map(Number)
        .sort((a, b) => a - b);
      if (nKeys.length === 0) return [];

      let N1, N2;
      if (desiredN <= nKeys[0]) {
        if (nKeys.length < 2) return [...byN[nKeys[0]]];
        [N1, N2] = [nKeys[0], nKeys[1]];
      } else if (desiredN >= nKeys[nKeys.length - 1]) {
        if (nKeys.length < 2) return [...byN[nKeys[nKeys.length - 1]]];
        [N1, N2] = [nKeys[nKeys.length - 2], nKeys[nKeys.length - 1]];
      } else {
        for (let i = 0; i < nKeys.length - 1; i++) {
          if (desiredN >= nKeys[i] && desiredN <= nKeys[i + 1]) {
            [N1, N2] = [nKeys[i], nKeys[i + 1]];
            break;
          }
        }
      }

      if (N1 == null) return [];
      if (desiredN === N1) return [...byN[N1]];
      if (desiredN === N2) return [...byN[N2]];

      const curve1 = byN[N1];
      const curve2 = byN[N2];
      const t =
        (Math.log(desiredN) - Math.log(N1)) / (Math.log(N2) - Math.log(N1));

      const allSm = [...curve1.map((p) => p[0]), ...curve2.map((p) => p[0])];
      const uniqueSm = [...new Set(allSm)].sort((a, b) => a - b);
      const smMin = uniqueSm[0];
      const smMax = uniqueSm[uniqueSm.length - 1];

      const inner = uniqueSm
        .map((sm) => {
          const sa1 = this.interpolateSigmaAmplitudeOnCurve(curve1, sm) ?? 0;
          const sa2 = this.interpolateSigmaAmplitudeOnCurve(curve2, sm) ?? 0;
          if (sa1 <= 0 && sa2 <= 0) return null;
          let sa;
          if (sa1 > 0 && sa2 > 0) {
            const logSa = Math.log(sa1) + t * (Math.log(sa2) - Math.log(sa1));
            sa = Math.exp(logSa);
          } else if (sa1 > 0) {
            sa = sa1 * (1 - t);
          } else {
            sa = sa2 * t;
          }
          if (sa <= 0) return null;
          return [sm, sa];
        })
        .filter((p) => p !== null);

      if (inner.length === 0) return [];
      const result = [];
      if (inner[0][0] > smMin) result.push([smMin, 0]);
      result.push(...inner);
      if (inner[inner.length - 1][0] < smMax) result.push([smMax, 0]);
      return result;
    },

    formatN(N) {
      return Number(N).toLocaleString(undefined, {
        maximumFractionDigits: 0,
        useGrouping: true,
      });
    },

    formatSigmaMax(v) {
      if (v == null || !Number.isFinite(v)) return "—";
      return Number(v).toFixed(2);
    },

    updateOutput() {
      if (!this.file || this.selectedMethods.length === 0) {
        this.outputs = {};
        this.series = [];
        this.cldDataByMethod = {};
        return;
      }

      const currentRequestId = ++this.requestId;
      const methodsSnapshot = [...this.selectedMethods];

      this.loading = true;
      this.outputs = {};
      this.series = [];
      this.cldDataByMethod = {};
      this.parsedInputData = [];
      this.expSnData = null;

      Promise.all([
        parseFile(this.file),
        ...methodsSnapshot.map((method) => {
          if (method === "PiecewiseLinear" && this.snCurveMethod) {
            return this.$analysisApi
              .runCldPiecewiseLinearFromSnFile(
                this.snCurveMethod,
                this.ucs,
                this.uts,
                this.file
              )
              .then((result) => ({ method, result }));
          }
          if (method === "PiecewiseNonLinear" && this.snCurveMethod) {
            return this.$analysisApi
              .runCldPiecewiseNonLinearFromSnFile(
                this.snCurveMethod,
                this.ucs,
                this.uts,
                this.file
              )
              .then((result) => ({ method, result }));
          }
          const opts = {};
          if (method === "Boerstra") {
            opts.npReference = this.npReference;
          }

          return this.$analysisApi
            .runCldFile(
              method,
              this.ucs,
              this.uts,
              this.file,
              Object.keys(opts).length > 0 ? opts : undefined
            )
            .then((result) => ({ method, result }));
        }),
      ])
        .then((results) => {
          if (currentRequestId !== this.requestId) return;

          const [parsedInput, ...methodResults] = results;
          this.parsedInputData = parsedInput.data;

          this.outputs = Object.fromEntries(
            methodResults.map(({ method, result }) => [method, result])
          );

          const parsedByMethod = methodResults.map(({ method, result }) => {
            const parsed = parse(result.csv_data, parserConfig);
            const allGrouped = groupBy(
              parsed.data,
              (row) => row.cycles_to_failure
            );
            // For PiecewiseLinear, only display isolife curves at powers of 10
            const powersOfTen = new Set(CYCLE_LEVELS);
            const grouped =
              method === "PiecewiseLinear"
                ? Object.fromEntries(
                    Object.entries(allGrouped).filter(([k]) =>
                      powersOfTen.has(Number(k))
                    )
                  )
                : allGrouped;
            return { method, grouped };
          });

          this.cldDataByMethod = Object.fromEntries(
            parsedByMethod.map(({ method, grouped }) => [
              method,
              Object.fromEntries(
                Object.entries(grouped).map(([nKey, rows]) => [
                  Number(nKey),
                  [...rows]
                    .sort(
                      (a, b) => Number(a.stress_mean) - Number(b.stress_mean)
                    )
                    .map((item) => [
                      Number(item.stress_mean),
                      Number(item.stress_amplitude),
                    ]),
                ])
              ),
            ])
          );

          const allCldSeries = parsedByMethod.flatMap(({ method, grouped }) => {
            const lineStyle = this.methodLineStyle(method);
            return Object.entries(grouped).map(([cyclesKey, rows]) => {
              const n = Number(cyclesKey);
              const color = this.cycleColor(n);
              return {
                type: "line",
                name: this.superscript(cyclesKey),
                data: [...rows]
                  .sort((a, b) => Number(a.stress_mean) - Number(b.stress_mean))
                  .map((item) => [
                    Number(item.stress_mean),
                    Number(item.stress_amplitude),
                  ]),
                lineStyle: { type: lineStyle, color },
                itemStyle: { color },
              };
            });
          });

          const scatterPoints = parsedInput.data
            .map((row) => {
              const R = Number(row.stress_ratio);
              const Smax = Number(row.stress_max);
              if (!Number.isFinite(R) || !Number.isFinite(Smax)) return null;
              const sm =
                R > 1 ? -(1 + 1 / R) * (Smax / 2) : ((1 + R) * Smax) / 2;
              const sa =
                R > 1 ? (1 - 1 / R) * (Smax / 2) : ((1 - R) * Smax) / 2;
              return sa > 0 ? [sm, sa] : null;
            })
            .filter((p) => p !== null);

          this.series = [
            ...allCldSeries,
            {
              type: "scatter",
              name: "Experimental data",
              symbolSize: 6,
              showSymbol: true,
              itemStyle: { color: "#444", opacity: 0.8 },
              data: scatterPoints,
            },
          ];
          this.errorMessages = null;
          this.maybeRunExpSn();
        })
        .catch((error) => {
          if (currentRequestId !== this.requestId) return;
          this.outputs = {};
          this.series = [];
          this.cldDataByMethod = {};
          const detail =
            error?.response?.data?.detail ||
            error?.response?.data?.message ||
            error?.message;
          this.errorMessages =
            typeof detail === "string" ? detail : "Analysis failed";
        })
        .finally(() => {
          if (currentRequestId === this.requestId) {
            this.loading = false;
          }
        });
    },

    interpolateExpSigmaMax(N) {
      if (!this.expSnData) return null;
      const entries = Object.entries(this.expSnData)
        .map(([k, v]) => [Number(k), Number(v)])
        .filter(([, v]) => Number.isFinite(v) && v !== 0)
        .sort(([a], [b]) => a - b);
      if (entries.length === 0) return null;
      if (entries.length === 1) return entries[0][1];
      if (N <= entries[0][0]) return entries[0][1];
      if (N >= entries[entries.length - 1][0])
        return entries[entries.length - 1][1];
      for (let i = 0; i < entries.length - 1; i++) {
        const [n1, sm1] = entries[i];
        const [n2, sm2] = entries[i + 1];
        if (N >= n1 && N <= n2) {
          const t =
            (Math.log(N) - Math.log(n1)) / (Math.log(n2) - Math.log(n1));
          if (sm1 > 0 && sm2 > 0) {
            return Math.exp(
              Math.log(sm1) + t * (Math.log(sm2) - Math.log(sm1))
            );
          }
          return sm1 + t * (sm2 - sm1);
        }
      }
      return null;
    },

    maybeRunExpSn() {
      if (this.desiredRFoundInInput && this.effectiveExpSnMethod) {
        this.runExperimentalSn();
      } else {
        this.expSnData = null;
      }
    },

    async runExperimentalSn() {
      if (!this.desiredRFoundInInput || !this.effectiveExpSnMethod) return;

      const R = this.matchingInputR;
      const filtered = this.parsedInputData.filter(
        (row) => Math.abs(parseFloat(row.stress_ratio) - R) < 1e-4
      );
      if (filtered.length === 0) return;

      const csvStr = unparse(filtered);
      const blob = new Blob([csvStr], { type: "text/csv" });
      const file = new File([blob], "exp_sn_input.csv", { type: "text/csv" });

      this.expSnLoading = true;
      try {
        const result = await this.$analysisApi.runSnCurveFile(
          this.effectiveExpSnMethod,
          file
        );
        const parsed = parse(result.csv_data, parserConfig);
        const map = {};
        for (const row of parsed.data) {
          const n = Number(row.cycles_to_failure);
          const sm = Number(row.stress_max);
          // For R > 1 (both compressive) the SNC convention stores |σ_min| as
          // stress_max (a positive magnitude). Convert to algebraic σ_max = −|σ_min|/R.
          const sigmaMax = R > 1 ? -sm / R : sm;
          if (Number.isFinite(n) && Number.isFinite(sigmaMax) && sigmaMax !== 0)
            map[n] = sigmaMax;
        }
        this.expSnData = map;
      } catch {
        this.expSnData = null;
      } finally {
        this.expSnLoading = false;
      }
    },

    downloadOutput() {
      if (!this.file || Object.keys(this.outputs).length === 0) return;
      Object.entries(this.outputs).forEach(([method, result]) => {
        if (result?.csv_data) {
          const outputName = getOutputFileName(
            "SNC",
            "CLD",
            this.file.name,
            method
          );
          download(result.csv_data, outputName + ".csv", "text/csv");
        }
      });
    },

    downloadRRatioTable() {
      if (!this.rRatioValid || this.rRatioIntersectionRows.length === 0) return;
      const R = parseFloat(this.desiredR).toFixed(2);
      const showExp = this.desiredRFoundInInput && this.expSnData;

      const headers = [
        "N_cycles",
        ...this.selectedMethods.map((m) => `sigma_max_${m}_MPa`),
        ...(showExp ? ["sigma_max_exp_MPa"] : []),
      ];

      const rows = this.rRatioIntersectionRows.map((row) => [
        row.N,
        ...this.selectedMethods.map((m) =>
          row.perMethod[m] != null && Number.isFinite(row.perMethod[m])
            ? row.perMethod[m].toFixed(4)
            : ""
        ),
        ...(showExp
          ? [
              row.expSigmaMax != null && Number.isFinite(row.expSigmaMax)
                ? row.expSigmaMax.toFixed(4)
                : "",
            ]
          : []),
      ]);

      const csv = unparse({ fields: headers, data: rows });
      const baseName = this.file
        ? this.file.name.replace(/\.[^.]+$/, "")
        : "cld";
      download(csv, `${baseName}_R${R}_sigma_max.csv`, "text/csv");
    },
  },
};
</script>

<style scoped>
.param-section-label {
  font-size: 13px;
  font-weight: 700;
  color: #333;
}

.boerstra-params {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 8px 10px;
}

.boerstra-param-row {
  display: flex;
  flex-direction: column;
  background: white;
  border: 1px solid #ececec;
  border-radius: 8px;
  padding: 8px 10px;
}

.boerstra-param-name {
  font-size: 12px;
  font-weight: 700;
  color: #555;
}

.boerstra-param-value {
  font-size: 13px;
  color: #111;
  word-break: break-all;
}

.r-ratio-table th {
  font-size: 13px !important;
  white-space: nowrap;
}

.r-ratio-table td {
  font-size: 13px !important;
}

.desired-n-row {
  background: #eef6fb;
}

.desired-n-row td {
  font-weight: 700;
}
</style>

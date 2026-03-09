<template>
  <v-card :loading="loading">
    <v-card-title>
      S-N Curve
      <v-spacer />

      <v-btn icon variant="text" size="small" @click="collapsed = !collapsed">
        <v-icon>
          {{ collapsed ? "mdi-chevron-down" : "mdi-chevron-up" }}
        </v-icon>
      </v-btn>

      <info-tooltip>
        This module plots curves on the (Stress - Number of cycles) plane. The
        curves are associated with 3 different methods (Lin-Log, Log-Log,
        Sendeckyj) and can be considered as constitutive laws for fatigue life
        predictions. On the graphs we also plot the individual results gathered
        over the tests with inputs (Cycles to failure - Stress at failure)
      </info-tooltip>
    </v-card-title>

    <v-expand-transition>
      <div v-show="!collapsed">
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
                @change="updateOutput"
              >
                <template #append>
                  <info-tooltip>
                    See the
                    <a
                      href="https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/Data/AGG_Data_Convention.md"
                    >
                      AGG Data Convention
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
                label="select S-N curve method(s)"
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
                label="select R ratio"
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
                v-model="xAxisType"
                label="X-Axis scale"
                :items="[
                  { text: 'Cycle count', value: 'value' },
                  { text: 'Log(Cycle count)', value: 'log' },
                ]"
                item-title="text"
                item-value="value"
                :disabled="loading"
              />
            </v-col>
          </v-row>
        </v-card-subtitle>

        <!-- Only change: add a small left cartouche + keep same chart -->
        <v-card-text v-if="series.length > 0">
          <v-row align="start">
            <!-- LEFT CARTOUCHE -->
            <v-col cols="12" md="3">
              <v-card variant="outlined" class="pa-3">
                <div class="text-subtitle-2 mb-2">S-N (Selected)</div>
                <v-divider class="mb-2" />

                <div v-for="m in selectedMethodCards" :key="m.key" class="mb-3">
                  <div class="d-flex align-center">
                    <span
                      class="mr-2"
                      :style="{
                        display: 'inline-block',
                        width: '10px',
                        height: '10px',
                        borderRadius: '50%',
                        background: m.color,
                      }"
                    />
                    <span class="text-body-2" :style="{ color: m.color }">
                      <b>{{ m.title }}</b>
                    </span>
                  </div>

                  <div class="text-caption mt-2">
                    <div><b>Equation</b></div>
                    <div style="white-space: normal">
                      <code>{{ m.equation }}</code>
                    </div>
                  </div>

                  <div class="text-caption mt-2">
                    <div><b>Parameters</b></div>
                    <div v-if="m.params.length === 0">No fit data</div>
                    <template v-else>
                      <div v-for="p in m.params" :key="p.name">
                        {{ p.name }}: {{ p.value }}
                      </div>
                    </template>
                  </div>

                  <div class="text-caption mt-2 d-flex align-center">
                    <span
                      :style="{
                        color: m.color,
                        fontFamily: 'monospace',
                        marginRight: '6px',
                      }"
                    >
                      ----
                    </span>
                    <span>: {{ m.dashedMeaning }}</span>
                  </div>

                  <v-divider class="mt-3" />
                </div>
              </v-card>
            </v-col>

            <!-- RIGHT CHART (same component, same props as initial) -->
            <v-col cols="12" md="9">
              <simple-chart
                :aspect-ratio="2"
                :series="series"
                title="S-N Curves"
                :x-axis-type="xAxisType"
                :x-axis-name="
                  xAxisType === 'log'
                    ? 'log₁₀(Number of cycles)'
                    : 'Number of cycles'
                "
                y-axis-name="σₘₐₓ [MPa]"
              />
            </v-col>
          </v-row>
        </v-card-text>

        <v-card-actions v-if="hasInput" class="justify-end">
          <v-btn :disabled="loading && outputs != null" @click="downloadOutput">
            Download SNC
            <info-tooltip>
              See the
              <a
                href="https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/Data/SNC_Data_Convention.md"
              >
                SNC Data Convention
              </a>
            </info-tooltip>
          </v-btn>
        </v-card-actions>
      </div>
    </v-expand-transition>
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
const rRatios = [-1, 0.1, 10, 0.5];

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
      methods: methods,
      selectedMethods: [methods[0]],
      rRatios: rRatios,
      selectedRRatios: [rRatios[0]],
      outputs: {},
      series: [],
      xAxisType: "log",
      collapsed: false,
    };
  },
  computed: {
    hasInput() {
      return this.file;
    },

    selectedMethodCards() {
      const cards = [];

      for (const method of this.selectedMethods) {
        const out = this.outputs?.[method];
        if (!out) continue;

        let json = out.json_data;
        try {
          if (typeof json === "string") json = JSON.parse(json);
        } catch (e) {
          json = null;
        }

        for (const r of this.selectedRRatios) {
          const fit = this.getFitForRatio(json, r);
          const color = this.methodColor(method);

          cards.push({
            key: `${method}-${r}`,
            title: `${method} R=${r}`,
            color,
            equation: this.methodEquation(method),
            params: this.formatParams(method, fit),
            dashedMeaning: this.dashedMeaning(method),
          });
        }
      }

      return cards;
    },
  },
  methods: {
    deepFindValue(obj, keyAliases) {
      if (!obj || typeof obj !== "object") return undefined;

      const aliases = (keyAliases || []).map((k) => String(k).toLowerCase());

      // direct keys
      for (const k of Object.keys(obj)) {
        if (aliases.includes(String(k).toLowerCase())) return obj[k];
      }

      // recurse
      for (const v of Object.values(obj)) {
        if (v && typeof v === "object") {
          const found = this.deepFindValue(v, keyAliases);
          if (found !== undefined) return found;
        }
      }
      return undefined;
    },

    fmtNumber(v) {
      if (v === undefined || v === null || v === "") return "—";
      const n = Number(v);
      if (Number.isFinite(n)) return n.toPrecision(6); // propre, pas trop long
      return String(v);
    },
    // Robust finder: tries several common JSON layouts
    getFitForRatio(json, r) {
      if (!json) return null;
      const rStr = String(r);

      // common formats
      if (json.by_r && (json.by_r[rStr] || json.by_r[r]))
        return json.by_r[rStr] || json.by_r[r];
      if (json.params && (json.params[rStr] || json.params[r]))
        return json.params[rStr] || json.params[r];
      if (json.parameters && (json.parameters[rStr] || json.parameters[r]))
        return json.parameters[rStr] || json.parameters[r];
      if (json[rStr] || json[r]) return json[rStr] || json[r];

      const arr =
        (Array.isArray(json) && json) ||
        (Array.isArray(json.results) && json.results) ||
        (Array.isArray(json.fits) && json.fits) ||
        null;

      if (arr) {
        return arr.find((x) => String(x.stress_ratio) === rStr) || null;
      }

      // fallback: json itself might already be the fit object
      if (
        json.alpha ||
        json.beta ||
        json.A ||
        json.B ||
        json.slope ||
        json.intercept
      )
        return json;

      return null;
    },

    formatParams(method, fit) {
      if (!fit) return [];

      // cherche d'abord dans fit.params, sinon dans fit entier (deep)
      const scope =
        fit?.params && typeof fit.params === "object" ? fit.params : fit;

      const getAny = (aliases) => this.deepFindValue(scope, aliases);

      if (method === "LinLog") {
        const A = getAny(["A", "a", "intercept", "c0"]);
        const B = getAny(["B", "b", "slope", "c1"]);
        return [
          { name: "A", value: this.fmtNumber(A) },
          { name: "B", value: this.fmtNumber(B) },
        ];
      }

      if (method === "LogLog") {
        const A = getAny(["A", "a", "intercept", "c0"]);
        const B = getAny(["B", "b", "slope", "c1"]);
        return [
          { name: "A", value: this.fmtNumber(A) },
          { name: "B", value: this.fmtNumber(B) },
        ];
      }
      if (method === "Sendeckyj") {
        return [
          { name: "alpha", value: this.fmtNumber(fit.a) },
          { name: "beta", value: this.fmtNumber(fit.b) },
          { name: "s", value: this.fmtNumber(fit.sstar) },
          { name: "c", value: this.fmtNumber(fit.cstar) },
        ];
      }
      return [];
    },

    updateOutput() {
      if (
        this.selectedMethods.length > 0 &&
        this.selectedRRatios.length > 0 &&
        this.file
      ) {
        this.loading = true;

        Promise.all([
          ...this.selectedMethods.map((method) =>
            this.$analysisApi
              .runSnCurveFile(method, this.file)
              .then((analysisResult) => {
                const results = parse(analysisResult.csv_data, parserConfig);
                const rows = results.data.filter((row) =>
                  this.selectedRRatios.includes(row.stress_ratio)
                );
                return { method, analysisResult, rows };
              })
          ),
          parseFile(this.file).then((parsed) => ({ parsedInputFile: parsed })),
        ])
          .then((data) => {
            const analysisResults = data.filter((x) => "analysisResult" in x);
            const parsedInputFile = data.find(
              (x) => "parsedInputFile" in x
            ).parsedInputFile;

            this.outputs = Object.fromEntries(
              analysisResults.map((item) => [item.method, item.analysisResult])
            );

            // Same as initial, with ONLY necessary changes:
            // - force same color for mean/lower/upper
            // - hide dashed series from legend
            this.series = [
              ...analysisResults.flatMap((item) =>
                this.selectedRRatios.flatMap((rRatio) => {
                  const color = this.methodColor(item.method);

                  return [
                    {
                      type: "line",
                      name: `${item.method} ${rRatio}`,
                      showSymbol: false,
                      data: item.rows
                        .filter((row) => row.stress_ratio === rRatio)
                        .map((row) => [row.cycles_to_failure, row.stress_max]),
                      lineStyle: { width: 2, color },
                      itemStyle: { color },
                    },
                    {
                      type: "line",
                      name: `${item.method} ${rRatio}`,
                      showSymbol: false,
                      data: item.rows
                        .filter((row) => row.stress_ratio === rRatio)
                        .map((row) => [
                          row.cycles_to_failure,
                          row.stress_lowerbound,
                        ]),
                      lineStyle: { type: "dashed", width: 1, color },
                      itemStyle: { color },
                      silent: true,
                      showInLegend: false,
                    },
                    {
                      type: "line",
                      name: `${item.method} ${rRatio}`,
                      showSymbol: false,
                      data: item.rows
                        .filter((row) => row.stress_ratio === rRatio)
                        .map((row) => [
                          row.cycles_to_failure,
                          row.stress_upperbound,
                        ]),
                      lineStyle: { type: "dashed", width: 1, color },
                      itemStyle: { color },
                      silent: true,
                      showInLegend: false,
                    },
                  ];
                })
              ),

              // Input points (unchanged)
              ...this.selectedRRatios.flatMap((rRatio) => [
                {
                  symbolSize: 5,
                  type: "scatter",
                  data: parsedInputFile.data
                    .filter((row) => row.stress_ratio === rRatio)
                    .map((row) => [row.cycles_to_failure, row.stress_max]),
                  rRatio,
                },
              ]),
            ];
          })
          .catch(() => {
            this.outputs = {};
            this.series = [];
          })
          .finally(() => (this.loading = false));
      }
    },

    downloadOutput() {
      for (const [key, value] of Object.entries(this.outputs)) {
        const outputName = getOutputFileName("AGG", "SNC", this.file.name, key);
        download(value.csv_data, outputName + ".csv", "text/csv");
        download(value.json_data, outputName + ".json", "application/json");
      }
    },

    methodColor(method) {
      // stable palette
      const map = {
        LinLog: "#d62728",
        LogLog: "#2ca02c",
        Sendeckyj: "#1f77b4",
      };
      return map[method] || "#000";
    },

    methodEquation(method) {
      if (method === "LinLog") return "σ_max = A + B·log10(N)";
      if (method === "LogLog") return "log10(σ_max) = A + B·log10(N)";
      if (method === "Sendeckyj") return "σ_max = b + a·(N + c*)^{-s*}";
      return "";
    },

    dashedMeaning(method) {
      if (method === "Sendeckyj") return "Survival probability 95% (P = 0.95)";
      return "95% band";
    },
  },
};
</script>

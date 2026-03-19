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
            :items="xAxisOptions"
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
              class="mb-3"
            >
              <div class="d-flex align-center">
                <span
                  class="mr-2"
                  :style="{
                    display: 'inline-block',
                    width: '10px',
                    height: '10px',
                    borderRadius: '50%',
                    background: card.color,
                  }"
                />
                <span class="text-body-2" :style="{ color: card.color }">
                  <b>{{ card.title }}</b>
                </span>
              </div>

              <div class="text-caption mt-2">
                <div><b>Equation</b></div>
                <div style="white-space: normal">
                  <code>{{ card.equation }}</code>
                </div>
              </div>

              <div class="text-caption mt-2">
                <div><b>Parameters</b></div>
                <div v-if="card.params.length === 0">No fit data</div>
                <template v-else>
                  <div v-for="param in card.params" :key="param.name">
                    {{ param.name }}: {{ param.value }}
                  </div>
                </template>
              </div>

              <div class="text-caption mt-2 d-flex align-center">
                <span
                  :style="{
                    color: card.color,
                    fontFamily: 'monospace',
                    fontWeight: 'bold',
                    fontSize: '14px',
                    letterSpacing: '2px',
                    marginRight: '8px',
                  }"
                >
                  - - - -
                </span>
                <span>: {{ card.dashedMeaning }}</span>
              </div>

              <v-divider class="mt-3" />
            </div>
          </v-card>
        </v-col>

        <v-col cols="12" md="9">
          <simple-chart
            :aspect-ratio="2"
            :series="series"
            :x-axis-type="xAxisType"
            :x-axis-name="'Number of cycles'"
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
            href="/downloads/SNC_output_guide.pdf"
            target="_blank"
            rel="noopener"
            style="color: blue; text-transform: none; min-width: 0"
          >
            SNC Data Convention (PDF)
          </a>
        </info-tooltip>
      </v-btn>
    </v-card-actions>
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
      methods,
      selectedMethods: [methods[0]],
      outputs: {},
      series: [],
      xAxisType: "log",
      rRatios: [],
      selectedRRatios: [],
      requestId: 0,
      xAxisOptions: [
        { text: "Cycle count", value: "value" },
        { text: "Log(Cycle count)", value: "log" },
      ],
    };
  },
  computed: {
    hasInput() {
      return !!this.file;
    },

    selectedMethodCards() {
      const cards = [];

      for (const method of this.selectedMethods) {
        const output = this.outputs?.[method];
        if (!output) continue;

        let json = output.json_data;
        try {
          if (typeof json === "string") {
            json = JSON.parse(json);
          }
        } catch {
          json = null;
        }

        for (const rRatio of this.selectedRRatios) {
          const fit = this.getFitForRatio(json, rRatio);

          cards.push({
            key: `${method}-${rRatio}`,
            title: `${method} R=${rRatio}`,
            color: this.methodColor(method, rRatio),
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
    resetState() {
      this.outputs = {};
      this.series = [];
      this.rRatios = [];
      this.selectedRRatios = [];
    },

    onFileChange() {
      this.resetState();
      this.updateOutput();
    },

    fmtNumber(value) {
      if (value === undefined || value === null || value === "") {
        return "—";
      }

      const n = Number(value);
      return Number.isFinite(n) ? n.toPrecision(6) : String(value);
    },

    deepFindValue(obj, keyAliases) {
      if (!obj || typeof obj !== "object") {
        return undefined;
      }

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
          if (found !== undefined) {
            return found;
          }
        }
      }

      return undefined;
    },

    getFitForRatio(json, rRatio) {
      if (!json) {
        return null;
      }

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
      if (!fit) {
        return [];
      }

      const scope =
        fit.params && typeof fit.params === "object" ? fit.params : fit;

      const getAny = (aliases) => this.deepFindValue(scope, aliases);

      if (method === "LinLog" || method === "LogLog") {
        return [
          {
            name: "A",
            value: this.fmtNumber(getAny(["A", "a", "intercept", "c0"])),
          },
          {
            name: "B",
            value: this.fmtNumber(getAny(["B", "b", "slope", "c1"])),
          },
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
      if (method === "Whitney") {
        return [
          { name: "alpha_f", value: this.fmtNumber(fit.alpha_f) },
          { name: "q0", value: this.fmtNumber(fit.q0) },
          { name: "sigma0", value: this.fmtNumber(fit.sigma0) },
          { name: "power", value: this.fmtNumber(fit.power) },
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

      return {
        type: "scatter",
        name: `Experiment data R=${rRatio}`,
        symbol: symbol,
        symbolSize: 6,
        itemStyle: {
          color: "#000000",
        },
        data: data
          .filter((row) => Number(row.stress_ratio) === Number(rRatio))
          .map((row) => [
            Number(row.cycles_to_failure),
            Number(row.stress_max),
          ]),
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
                .filter((value) => Number.isFinite(value))
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
            selectedMethodsSnapshot.map((method) =>
              this.$analysisApi
                .runSnCurveFile(method, this.file)
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
                })
            )
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

                return [
                  this.buildLineSeries(
                    method,
                    rRatio,
                    filteredRows,
                    "stress_max",
                    color
                  ),
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
                  ),
                ];
              })
            ),
            ...activeRRatios.map((rRatio) =>
              this.buildScatterSeries(rRatio, parsedInputFile.data)
            ),
          ];
        })
        .catch((error) => {
          if (currentRequestId !== this.requestId) return;
          console.error("SnCurve error:", error);
          alert(
            error?.response?.data?.detail ||
              error?.response?.data ||
              error?.message ||
              "Whitney failed"
          );
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
      for (const method of this.selectedMethods) {
        const output = this.outputs?.[method];
        if (!output) continue;

        const outputName = getOutputFileName(
          "AGG",
          "SNC",
          this.file.name,
          method
        );

        download(output.csv_data, `${outputName}.csv`, "text/csv");
        download(output.json_data, `${outputName}.json`, "application/json");
      }
    },

    methodColor(method, rRatio = null) {
      const palettes = {
        LinLog: ["#d62728", "#ff6b6b", "#8b0000", "#ff9ea1"],
        LogLog: ["#2ca02c", "#7bd87b", "#0b6e0b", "#9be79b"],
        Sendeckyj: ["#1f77b4", "#66b3ff", "#0b4f8a", "#9ccfff"],
        Whitney: ["#9467bd", "#c5a3ff", "#5e3a87", "#d8c2ff"],
      };

      const palette = palettes[method] || ["#000000"];

      if (rRatio === null || rRatio === undefined) {
        return palette[0];
      }

      const index = this.rRatios.findIndex(
        (value) => Number(value) === Number(rRatio)
      );

      return palette[index >= 0 ? index % palette.length : 0];
    },

    methodEquation(method) {
      if (method === "LinLog") return "σ_max = A + B·log10(N)";
      if (method === "LogLog") return "σ_max = A · N^(-B)";
      if (method === "Sendeckyj") return "σ_max = b + a·(N + c*)^{-s*}";
      if (method === "Whitney")
        return "σ_max = sigma0 · (-ln(Ps))^(power/alpha_f) · N^(-power)";
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

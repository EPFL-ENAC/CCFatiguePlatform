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
    <v-card-text v-if="stats" class="pt-0">
      <v-row>
        <v-col cols="12" sm="4" md="3">
          <v-card outlined class="pa-3">
            <div class="text-caption font-weight-bold text-uppercase mb-2">
              Summary
            </div>

            <div class="mt-3 text-caption font-weight-bold mb-2">Cycles</div>
            <v-divider class="mb-2" />
            <div class="d-flex justify-space-between">
              <span class="text-body-2">Full cycles</span>
              <span class="text-body-2 font-weight-bold red--text">{{
                stats.fullCycleCount
              }}</span>
            </div>

            <div class="d-flex justify-space-between">
              <span class="text-body-2">Half cycles</span>
              <span class="text-body-2 font-weight-bold red--text">{{
                stats.halfCycleCount
              }}</span>
            </div>

            <div class="d-flex justify-space-between">
              <span class="text-body-2">Equivalent number of cycles</span>
              <span class="text-body-2 font-weight-bold red--text">
                {{ Math.round(stats.eqCycles) }}
              </span>
            </div>

            <div class="mt-3 text-caption font-weight-bold mb-2">Signal</div>
            <v-divider class="mb-2" />
            <div class="d-flex justify-space-between">
              <span class="text-body-2">Stress min</span>
              <span class="text-body-2 font-weight-bold red--text">
                {{ formatNumber(stats.signalMin) }} MPa
              </span>
            </div>
            <div class="d-flex justify-space-between">
              <span class="text-body-2">Stress max</span>
              <span class="text-body-2 font-weight-bold red--text">
                {{ formatNumber(stats.signalMax) }} MPa
              </span>
            </div>

            <div class="mt-3 text-caption font-weight-bold mb-2">
              Stress range
            </div>
            <v-divider class="mb-2" />
            <div class="d-flex justify-space-between">
              <span class="text-body-2">Min</span>
              <span class="text-body-2 font-weight-bold red--text">
                {{ formatNumber(stats.stressRangeMin) }} MPa
              </span>
            </div>
            <div class="d-flex justify-space-between">
              <span class="text-body-2">Max</span>
              <span class="text-body-2 font-weight-bold red--text">
                {{ formatNumber(stats.stressRangeMax) }} MPa
              </span>
            </div>

            <div class="mt-3 text-caption font-weight-bold mb-2">
              Stress mean
            </div>
            <v-divider class="mb-2" />
            <div class="d-flex justify-space-between">
              <span class="text-body-2">Min</span>
              <span class="text-body-2 font-weight-bold red--text">
                {{ formatNumber(stats.stressMeanMin) }} MPa
              </span>
            </div>
            <div class="d-flex justify-space-between">
              <span class="text-body-2">Max</span>
              <span class="text-body-2 font-weight-bold red--text">
                {{ formatNumber(stats.stressMeanMax) }} MPa
              </span>
            </div>
          </v-card>
        </v-col>

        <v-col cols="12" sm="8" md="8">
          <!-- le graphique reste comme avant -->
          <simple-chart
            v-if="series.length > 0"
            :aspect-ratio="2"
            :series="series"
            x-axis-name="Cummulative Percentage of Spectrum Cycles"
            y-axis-name="Stress Range [MPa]"
          />
        </v-col>
      </v-row>
    </v-card-text>
    <v-card-actions v-if="hasInput" class="justify-end">
      <v-btn :disabled="loading && output != null" @click="downloadOutput">
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
            >CYC Data Convention (PDF)</a
          >
        </info-tooltip>
      </v-btn>
    </v-card-actions>
  </v-card>
</template>
<script>
import CycleCountingMethod from "@/backend/model/CycleCountingMethod";
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
            this.stats = null;
            this.output = data;

            // ---------- 1) LDS (input) => min/max du signal ----------
            const ldsText = await this.readFileAsText(this.file);

            // ⚠️ on force header:true pour récupérer les colonnes
            const ldsParsed = parse(ldsText, { ...parserConfig, header: true });
            const ldsRows = ldsParsed?.data || [];

            const toNum = (v) => {
              if (v === null || v === undefined || v === "") return NaN;
              return Number(v);
            };
            const min = (arr) => (arr.length ? Math.min(...arr) : null);
            const max = (arr) => (arr.length ? Math.max(...arr) : null);
            const sum = (arr) => arr.reduce((acc, x) => acc + x, 0);

            // adapte la liste si tu connais le nom exact dans LDS
            const ldsStressKey = this.getFirstNumericColumn(ldsRows, [
              "stress",
              "stress_max",
              "sigma",
              "stress_MPa",
            ]);

            const ldsStressVals = ldsStressKey
              ? ldsRows
                  .map((r) => toNum(r[ldsStressKey]))
                  .filter(Number.isFinite)
              : [];

            const signalMin = min(ldsStressVals);
            const signalMax = max(ldsStressVals);

            // ---------- 2) CYC (output) => cycles + range/mean min/max ----------
            const cycResults = parse(data, parserConfig);
            const rows = cycResults?.data || [];

            const nums = (key) =>
              rows.map((r) => toNum(r[key])).filter((x) => Number.isFinite(x));

            const ncy = nums("n_cycles");
            const srange = nums("stress_range");
            const smean = nums("stress_mean");

            // cycles :
            // - eqCycles : somme des n_cycles (équivalent en cycles)
            // - fullCycleCount : nombre de cycles complets (compte) = somme des parties entières
            // - halfCycleCount : nombre de demi-cycles (compte) = 2 * somme des parties fractionnaires
            //   (ex: 0.5 => 1 demi-cycle ; 2.5 => 1 demi-cycle ; 52.5 => 1 demi-cycle)

            const eqCycles = sum(ncy);

            const fullCycleCount = ncy.reduce(
              (acc, v) => acc + Math.floor(v),
              0
            );

            // fractionnaire (0 ou 0.5 normalement). On arrondit pour éviter les erreurs flottantes.
            const halfCycleCount = ncy.reduce((acc, v) => {
              const frac = v - Math.floor(v);
              const halfs = Math.round(frac * 2); // 0.5 -> 1, 0 -> 0
              return acc + halfs;
            }, 0);

            // ---------- stats finales ----------
            this.stats = {
              eqCycles,
              fullCycleCount,
              halfCycleCount,
              signalMin,
              signalMax,
              stressRangeMin: min(srange),
              stressRangeMax: max(srange),
              stressMeanMin: min(smean),
              stressMeanMax: max(smean),
            };

            // chart inchangé
            this.series = [
              {
                type: "line",
                name: this.method,
                data: rows.map((item) => [
                  item.cum_n_cycles,
                  item.stress_range,
                ]),
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
      return Number(v).toFixed(3);
    },
  },
};
</script>

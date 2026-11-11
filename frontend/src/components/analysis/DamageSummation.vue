<template>
  <v-card :loading="loading" class="damage-summation">
    <v-card-title>
      Damage Summation
      <v-spacer />
      <info-tooltip>
        Runs the complete uniaxial CCfatigue workflow: cycle counting, optional
        S-N fitting, CLD construction, and Palmgren-Miner damage summation.
      </info-tooltip>
    </v-card-title>

    <v-card-subtitle class="pb-0">
      One spectrum pass is one complete repetition of the uploaded load history.
      The result reports both damage per pass and predicted passes to failure.
    </v-card-subtitle>

    <v-card-text>
      <v-row>
        <v-col cols="12" md="6">
          <v-card outlined class="pa-4 setup-card fill-height">
            <div class="section-title">1. Input data</div>

            <v-file-input
              v-model="loadHistoryFile"
              label="Load history (LDS CSV)"
              accept=".csv"
              show-size
              :disabled="loading"
              @change="clearOutput"
            />

            <v-select
              v-model="fatigueDataType"
              label="Fatigue data source"
              :items="fatigueDataTypes"
              item-text="text"
              item-value="value"
              :disabled="loading"
              @change="onFatigueDataTypeChange"
            />

            <v-file-input
              v-model="fatigueDataFile"
              :label="fatigueFileLabel"
              :accept="fatigueFileAccept"
              show-size
              :disabled="loading"
              @change="clearOutput"
            />

            <v-row dense>
              <v-col cols="12" sm="6">
                <v-text-field
                  v-model.number="ucs"
                  label="UCS [MPa]"
                  type="number"
                  min="0"
                  :disabled="loading"
                  @change="clearOutput"
                />
              </v-col>
              <v-col cols="12" sm="6">
                <v-text-field
                  v-model.number="uts"
                  label="UTS [MPa]"
                  type="number"
                  min="0"
                  :disabled="loading"
                  @change="clearOutput"
                />
              </v-col>
            </v-row>
          </v-card>
        </v-col>

        <v-col cols="12" md="6">
          <v-card outlined class="pa-4 setup-card fill-height">
            <div class="section-title">2. Analysis pipeline</div>

            <v-select
              v-model="cycleCountingMethod"
              label="Cycle-counting method"
              :items="cycleCountingMethods"
              :disabled="loading"
              @change="clearOutput"
            />

            <v-select
              v-if="usesAggInput"
              v-model="snCurveMethod"
              label="S-N curve model"
              :items="snCurveMethods"
              :disabled="loading"
              @change="clearOutput"
            />

            <v-select
              v-model="cldMethod"
              label="Constant-life diagram model"
              :items="cldMethods"
              item-text="text"
              item-value="value"
              :disabled="loading"
              @change="clearOutput"
            />

            <template v-if="usesAggInput">
              <v-row dense>
                <v-col cols="12" sm="6">
                  <v-select
                    v-model="loadType"
                    label="Loading type"
                    :items="loadTypes"
                    item-text="text"
                    item-value="value"
                    :disabled="loading"
                    @change="clearOutput"
                  />
                </v-col>
                <v-col v-if="loadType !== 'shear'" cols="12" sm="6">
                  <v-text-field
                    v-model.number="angle"
                    label="Material angle [deg]"
                    type="number"
                    :disabled="loading"
                    @change="clearOutput"
                  />
                </v-col>
              </v-row>

              <v-text-field
                v-if="needsProbability"
                v-model.number="confidenceInterval"
                label="Reliability probability [%]"
                type="number"
                min="0.01"
                max="99.99"
                :disabled="loading"
                @change="clearOutput"
              />
            </template>
          </v-card>
        </v-col>
      </v-row>

      <v-card outlined class="pa-4 mt-4 setup-card">
        <div class="section-title">3. Spectrum stress sweep</div>
        <div class="text-body-2 grey--text text--darken-1 mb-3">
          Every peak and valley in the load history is multiplied by each scale
          factor. Include factor 1.0 to see damage for the uploaded spectrum as
          supplied.
        </div>
        <v-row dense align="end">
          <v-col cols="12" sm="4">
            <v-text-field
              v-model.number="factorMin"
              label="Minimum scale factor"
              type="number"
              min="0.0001"
              step="0.1"
              :disabled="loading"
              @change="clearOutput"
            />
          </v-col>
          <v-col cols="12" sm="4">
            <v-text-field
              v-model.number="factorMax"
              label="Maximum scale factor"
              type="number"
              min="0.0001"
              step="0.1"
              :disabled="loading"
              @change="clearOutput"
            />
          </v-col>
          <v-col cols="12" sm="4">
            <v-text-field
              v-model.number="factorCount"
              label="Number of evaluated factors"
              type="number"
              min="1"
              max="100"
              step="1"
              :disabled="loading"
              @change="clearOutput"
            />
          </v-col>
        </v-row>
      </v-card>

      <v-alert v-if="validationMessage" type="info" outlined dense class="mt-4">
        {{ validationMessage }}
      </v-alert>
      <v-alert v-if="errorMessage" type="error" outlined dense class="mt-4">
        {{ errorMessage }}
      </v-alert>

      <div class="d-flex justify-end mt-4">
        <v-btn
          color="primary"
          large
          :loading="loading"
          :disabled="!canRun || loading"
          @click="runAnalysis"
        >
          Run damage summation
        </v-btn>
      </div>

      <template v-if="rows.length">
        <v-divider class="my-6" />

        <div class="section-title mb-4">Results near scale factor 1.0</div>
        <v-row>
          <v-col
            v-for="card in summaryCards"
            :key="card.label"
            cols="12"
            sm="6"
            lg="3"
          >
            <v-card outlined class="pa-4 result-card">
              <div class="result-label">{{ card.label }}</div>
              <div class="result-value">{{ card.value }}</div>
              <div class="result-unit">{{ card.unit }}</div>
            </v-card>
          </v-col>
        </v-row>

        <v-alert
          v-for="warning in warnings"
          :key="warning"
          type="warning"
          outlined
          dense
          class="mt-3"
        >
          {{ warning }}
        </v-alert>

        <div class="chart-title mt-7">Predicted spectrum life</div>
        <simple-chart
          :height="560"
          :series="chartSeries"
          x-axis-name="Spectrum passes to failure"
          y-axis-name="Maximum stress [MPa]"
          x-axis-type="log"
        />

        <v-simple-table class="mt-6 result-table">
          <thead>
            <tr>
              <th>Scale factor</th>
              <th>Maximum stress [MPa]</th>
              <th>Damage per pass</th>
              <th>Spectrum passes to failure</th>
              <th>Static-overload bins</th>
            </tr>
          </thead>
          <tbody>
            <tr v-for="row in rows" :key="row.scale_factor">
              <td>{{ formatNumber(row.scale_factor) }}</td>
              <td>{{ formatNumber(row.stress_max) }}</td>
              <td>{{ formatScientific(row.damage_per_pass) }}</td>
              <td>
                {{ formatScientific(row.spectrum_passes_to_failure) }}
              </td>
              <td>{{ row.static_overload_bins }}</td>
            </tr>
          </tbody>
        </v-simple-table>
      </template>
    </v-card-text>

    <v-card-actions v-if="output" class="justify-end px-4 pb-4">
      <v-btn text color="primary" @click="downloadCsv">Download DAS CSV</v-btn>
      <v-btn text color="primary" @click="downloadJson">
        Download analysis details
      </v-btn>
    </v-card-actions>
  </v-card>
</template>

<script>
import CldMethod from "@/backend/model/CldMethod";
import CycleCountingMethod from "@/backend/model/CycleCountingMethod";
import SnCurveMethod from "@/backend/model/SnCurveMethod";
import SimpleChart from "@/components/charts/SimpleChart";
import InfoTooltip from "@/components/InfoTooltip";
import { parserConfig } from "@/utils/papaparse";
import download from "downloadjs";
import { parse } from "papaparse";

const CLD_LABELS = {
  Harris: "Harris",
  SimplifiedHarris: "Simplified Harris",
  PiecewiseLinear: "Piecewise Linear",
  PiecewiseNonLinear: "Piecewise Non-Linear",
  Kawai: "Kawai",
  Boerstra: "Boerstra",
};

export default {
  name: "DamageSummation",
  components: { InfoTooltip, SimpleChart },
  data() {
    return {
      loadHistoryFile: null,
      fatigueDataFile: null,
      fatigueDataType: "SNC",
      fatigueDataTypes: [
        { text: "Fitted S-N curves (SNC CSV or JSON)", value: "SNC" },
        { text: "Raw constant-amplitude tests (AGG CSV)", value: "AGG" },
      ],
      cycleCountingMethods: Object.values(new CycleCountingMethod()),
      cycleCountingMethod: "Rainflow",
      snCurveMethods: Object.values(new SnCurveMethod()),
      snCurveMethod: "LogLog",
      cldMethods: Object.values(new CldMethod()).map((value) => ({
        text: CLD_LABELS[value] || value,
        value,
      })),
      cldMethod: "PiecewiseLinear",
      loadTypes: [
        { text: "Tensile", value: "tensile" },
        { text: "Compressive", value: "compressive" },
        { text: "Shear", value: "shear" },
      ],
      loadType: "tensile",
      angle: 0,
      confidenceInterval: 50,
      ucs: 367.2,
      uts: 416.5,
      factorMin: 0.4,
      factorMax: 1.4,
      factorCount: 11,
      loading: false,
      output: null,
      rows: [],
      metadata: null,
      errorMessage: null,
    };
  },
  computed: {
    usesAggInput() {
      return this.fatigueDataType === "AGG";
    },
    fatigueFileLabel() {
      return this.usesAggInput
        ? "Constant-amplitude fatigue tests (AGG CSV)"
        : "Fitted S-N curves (SNC CSV or SN_curves JSON)";
    },
    fatigueFileAccept() {
      return this.usesAggInput ? ".csv" : ".csv,.json";
    },
    needsProbability() {
      return ["Sendeckyj", "Whitney"].includes(this.snCurveMethod);
    },
    validationMessage() {
      if (!this.loadHistoryFile) return "Upload an LDS load-history CSV file.";
      if (!this.fatigueDataFile)
        return `Upload an ${this.fatigueDataType} fatigue-data file.`;
      if (this.usesAggInput && !this.snCurveMethod)
        return "Select an S-N curve model for the AGG data.";
      if (!(Number(this.ucs) > 0) || !(Number(this.uts) > 0))
        return "UCS and UTS must both be greater than zero.";
      if (!(Number(this.factorMin) > 0) || !(Number(this.factorMax) > 0))
        return "Spectrum scale factors must be greater than zero.";
      if (Number(this.factorMax) < Number(this.factorMin))
        return "The maximum scale factor must not be below the minimum.";
      const count = Number(this.factorCount);
      if (!Number.isInteger(count) || count < 1 || count > 100)
        return "The number of evaluated factors must be an integer from 1 to 100.";
      if (
        this.usesAggInput &&
        this.loadType !== "shear" &&
        !Number.isFinite(Number(this.angle))
      )
        return "Enter a finite material angle.";
      return null;
    },
    canRun() {
      return !this.validationMessage;
    },
    summary() {
      return this.metadata?.summary || null;
    },
    warnings() {
      return this.metadata?.warnings || [];
    },
    summaryCards() {
      if (!this.summary) return [];
      return [
        {
          label: "Evaluated scale factor",
          value: this.formatNumber(this.summary.scale_factor),
          unit: "x input spectrum",
        },
        {
          label: "Damage per pass",
          value: this.formatScientific(this.summary.damage_per_pass),
          unit: "Miner damage index",
        },
        {
          label: "Predicted life",
          value: this.formatScientific(this.summary.spectrum_passes_to_failure),
          unit: "spectrum passes",
        },
        {
          label: "Counted spectrum",
          value: this.formatScientific(this.metadata?.cycles_per_spectrum_pass),
          unit: "cycles per pass",
        },
      ];
    },
    chartSeries() {
      const data = this.rows
        .map((row) => [
          Number(row.spectrum_passes_to_failure),
          Number(row.stress_max),
        ])
        .filter(
          ([passes, stress]) =>
            Number.isFinite(passes) && passes > 0 && Number.isFinite(stress)
        )
        .sort((a, b) => a[0] - b[0]);
      return [
        {
          type: "line",
          name: `${this.cycleCountingMethod} / ${
            this.usesAggInput ? this.snCurveMethod : "supplied SNC"
          } / ${CLD_LABELS[this.cldMethod] || this.cldMethod}`,
          data,
          showSymbol: true,
        },
      ];
    },
  },
  methods: {
    onFatigueDataTypeChange() {
      this.fatigueDataFile = null;
      this.clearOutput();
    },
    clearOutput() {
      this.output = null;
      this.rows = [];
      this.metadata = null;
      this.errorMessage = null;
    },
    parseMetadata(value) {
      if (!value) return {};
      if (typeof value === "string") return JSON.parse(value);
      return value;
    },
    async runAnalysis() {
      if (!this.canRun) return;
      this.loading = true;
      this.errorMessage = null;
      try {
        const output = await this.$analysisApi.runDamageSummationFile(
          this.loadHistoryFile,
          this.fatigueDataFile,
          {
            fatigueDataType: this.fatigueDataType,
            cycleCountingMethod: this.cycleCountingMethod,
            snCurveMethod: this.usesAggInput ? this.snCurveMethod : null,
            cldMethod: this.cldMethod,
            ucs: Number(this.ucs),
            uts: Number(this.uts),
            confidenceInterval: this.needsProbability
              ? Number(this.confidenceInterval)
              : null,
            loadType: this.loadType,
            angle: this.loadType === "shear" ? null : Number(this.angle),
            factorMin: Number(this.factorMin),
            factorMax: Number(this.factorMax),
            factorCount: Number(this.factorCount),
          }
        );
        const parsed = parse(output.csv_data, parserConfig);
        this.rows = (parsed.data || []).filter((row) =>
          Number.isFinite(Number(row.scale_factor))
        );
        this.metadata = this.parseMetadata(output.json_data);
        this.output = output;
      } catch (error) {
        const detail =
          error?.response?.body?.detail ||
          error?.response?.text ||
          error?.message ||
          "Damage summation failed.";
        this.errorMessage = String(detail);
        this.output = null;
        this.rows = [];
        this.metadata = null;
      } finally {
        this.loading = false;
      }
    },
    formatNumber(value) {
      const number = Number(value);
      if (!Number.isFinite(number)) return "-";
      return number.toLocaleString(undefined, {
        maximumFractionDigits: 4,
        useGrouping: false,
      });
    },
    formatScientific(value) {
      const number = Number(value);
      if (!Number.isFinite(number)) return "-";
      if (number === 0) return "0";
      if (Math.abs(number) >= 1e4 || Math.abs(number) < 1e-3)
        return number.toExponential(3);
      return number.toLocaleString(undefined, {
        maximumFractionDigits: 4,
        useGrouping: false,
      });
    },
    outputBaseName() {
      const inputName = (this.loadHistoryFile?.name || "spectrum").replace(
        /\.[^/.]+$/,
        ""
      );
      const snMethod = this.usesAggInput ? this.snCurveMethod : "SuppliedSNC";
      return `DAS_${inputName}_${this.cycleCountingMethod}_${snMethod}_${this.cldMethod}`;
    },
    downloadCsv() {
      if (!this.output) return;
      download(
        this.output.csv_data,
        `${this.outputBaseName()}.csv`,
        "text/csv"
      );
    },
    downloadJson() {
      if (!this.output) return;
      download(
        this.output.json_data,
        `${this.outputBaseName()}.json`,
        "application/json"
      );
    },
  },
};
</script>

<style scoped>
.damage-summation {
  color: #212121;
}

.setup-card {
  border-radius: 10px;
  border-color: #d8dde6;
}

.section-title {
  color: #263238;
  font-size: 1.05rem;
  font-weight: 700;
  letter-spacing: 0.15px;
  margin-bottom: 14px;
}

.chart-title {
  color: #212121;
  font-size: 1.4rem;
  font-weight: 700;
  text-align: center;
}

.result-card {
  border-left: 4px solid var(--v-primary-base);
  min-height: 128px;
}

.result-label {
  color: #607d8b;
  font-size: 0.78rem;
  font-weight: 700;
  letter-spacing: 0.06em;
  text-transform: uppercase;
}

.result-value {
  color: #102a43;
  font-size: 1.55rem;
  font-weight: 700;
  margin-top: 10px;
}

.result-unit {
  color: #78909c;
  font-size: 0.78rem;
  margin-top: 2px;
}

.result-table th {
  white-space: nowrap;
}
</style>

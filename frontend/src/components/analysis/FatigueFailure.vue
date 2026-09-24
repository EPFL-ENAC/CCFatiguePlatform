<template>
  <v-card :loading="loading">
    <v-card-title>
      Fatigue Failure
      <v-spacer />
      <info-tooltip>
        Upload one material JSON file containing the static strengths and S-N
        curves, then provide the target analysis conditions. Several methods can
        be selected to compare their predictions on the same chart.
      </info-tooltip>
    </v-card-title>

    <v-card-subtitle>
      <v-row>
        <v-col cols="12" md="6">
          <v-select
            v-model="selectedMethods"
            :items="methodNames"
            label="Select Methods"
            multiple
            chips
            deletable-chips
            :disabled="loading"
            @change="updateOutput"
          ></v-select>
        </v-col>
        <v-col cols="12" md="6">
          <v-file-input
            v-model="materialFile"
            accept=".json,application/json"
            label="Material data (JSON file)"
            show-size
            :disabled="loading"
            @change="onMaterialFileChange"
          >
            <template #append>
              <info-tooltip>
                The file must contain the <code>static_strengths</code> and
                <code>SN_curves</code> arrays shown in
                <code>Data/samples/templates/general.json</code>.
              </info-tooltip>
            </template>
          </v-file-input>
        </v-col>
        <v-col v-if="needsAggFile" cols="12" md="6">
          <v-file-input
            v-model="aggFile"
            accept=".csv,text/csv"
            label="Aggregate fatigue data (AGG CSV file)"
            show-size
            :disabled="loading"
            @change="updateOutput"
          >
            <template #append>
              <info-tooltip>
                Required by Kawai and Shokrieh–Taheri. The CSV must follow the
                AGG data convention.
              </info-tooltip>
            </template>
          </v-file-input>
        </v-col>
      </v-row>

      <v-alert v-if="materialPreviewError" type="error" dense text>
        {{ materialPreviewError }}
      </v-alert>

      <v-row v-if="materialPreview" class="mb-2">
        <v-col cols="12" md="6">
          <v-card outlined height="100%">
            <v-card-title class="text-subtitle-1">
              Static strengths ({{ materialPreview.staticStrengths.length }})
            </v-card-title>
            <v-card-text>
              <v-list
                v-if="materialPreview.staticStrengths.length > 0"
                dense
                class="py-0"
              >
                <v-list-item
                  v-for="(strength, index) in materialPreview.staticStrengths"
                  :key="`strength-${index}`"
                >
                  <v-list-item-content>
                    <v-list-item-title>
                      {{ staticStrengthLabel(strength) }}
                    </v-list-item-title>
                  </v-list-item-content>
                  <v-list-item-action-text>
                    {{ displayValue(strength.value) }} MPa
                  </v-list-item-action-text>
                </v-list-item>
              </v-list>
              <span v-else class="text-caption grey--text">
                No static strengths in this file.
              </span>
            </v-card-text>
          </v-card>
        </v-col>

        <v-col cols="12" md="6">
          <v-card outlined height="100%">
            <v-card-title class="text-subtitle-1">
              S-N curves ({{ materialPreview.snCurves.length }})
            </v-card-title>
            <v-card-text>
              <v-list
                v-if="materialPreview.snCurves.length > 0"
                dense
                class="py-0"
              >
                <v-list-item
                  v-for="(curve, index) in materialPreview.snCurves"
                  :key="`curve-${index}`"
                >
                  <v-list-item-content>
                    <v-list-item-title>
                      [{{ loadTypeLabel(curve) }}]
                      {{ angleLabel(curve) }}
                    </v-list-item-title>
                  </v-list-item-content>
                  <v-list-item-action>
                    <info-tooltip>
                      <span
                        v-for="([field, value], detailIndex) in curveDetails(
                          curve
                        )"
                        :key="`${field}-${detailIndex}`"
                      >
                        <strong>{{ fieldLabel(field) }}:</strong>
                        {{ displayValue(value) }}<br />
                      </span>
                    </info-tooltip>
                  </v-list-item-action>
                </v-list-item>
              </v-list>
              <span v-else class="text-caption grey--text">
                No S-N curves in this file.
              </span>
            </v-card-text>
          </v-card>
        </v-col>
      </v-row>

      <v-card outlined>
        <v-card-title class="text-subtitle-1">Target conditions</v-card-title>
        <v-card-text>
          <v-row dense>
            <v-col cols="12" md="4">
              <v-text-field
                v-model.number="values.targetAngle"
                label="Target Angle (°)"
                type="number"
                :disabled="loading"
                @change="updateOutput"
              ></v-text-field>
            </v-col>
            <v-col v-if="needsTargetStressRatio" cols="12" md="4">
              <v-text-field
                v-model.number="values.targetStressRatio"
                label="Target Stress Ratio"
                type="number"
                :disabled="loading"
                @change="updateOutput"
              ></v-text-field>
            </v-col>
            <v-col v-if="needsTargetStaticStrength" cols="12" md="4">
              <v-text-field
                v-model.number="values.targetStaticStrength"
                label="Static Strength at Target Angle (MPa)"
                type="number"
                min="0"
                :disabled="loading"
                @change="updateOutput"
              ></v-text-field>
            </v-col>
          </v-row>
        </v-card-text>
      </v-card>

      <v-alert v-if="selectedMethods.includes('Kawai')" type="info" dense text>
        This tab shows Kawai's raw S-N curve. For the full margin-of-safety
        analysis with this criterion, see the
        <a href="#" @click.prevent="$emit('go-to-module', 'cld-analysis')"
          >CLD tab</a
        >.
      </v-alert>

      <v-alert
        v-for="(message, method) in errorsByMethod"
        :key="method"
        type="error"
        dense
        text
      >
        {{ method }}: {{ message }}
      </v-alert>
    </v-card-subtitle>

    <v-card-text v-if="series.length > 0">
      <simple-chart
        :aspect-ratio="2"
        :series="series"
        x-axis-name="N"
        y-axis-name="Maximum Cyclic Stress [MPa]"
        x-axis-type="log"
      ></simple-chart>

      <hashin-rotem-safety
        v-if="parsedRows.HashinRotem"
        :rows="parsedRows.HashinRotem"
      ></hashin-rotem-safety>
      <sims-brogdon-envelope
        v-if="parsedRows.SimsBrogdon"
        :rows="parsedRows.SimsBrogdon"
      ></sims-brogdon-envelope>
      <ftpf-envelope
        v-if="parsedRows.FTPF"
        :rows="parsedRows.FTPF"
      ></ftpf-envelope>
      <fawaz-ellyin-margin
        v-if="parsedRows.FawazEllyin"
        :rows="parsedRows.FawazEllyin"
      ></fawaz-ellyin-margin>
    </v-card-text>

    <v-card-actions v-if="outputMethods.length > 0" class="justify-end">
      <v-btn
        v-for="method in outputMethods"
        :key="method"
        :disabled="loading"
        @click="downloadOutput(method)"
      >
        Download {{ method }} FAF
        <info-tooltip>
          See the
          <a
            href="https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/Data/FAF_Data_Convention.md"
            >FAF Data Convention</a
          >
        </info-tooltip>
      </v-btn>
    </v-card-actions>
  </v-card>
</template>

<script>
import FawazEllyinMargin from "@/components/analysis/fatigueFailure/FawazEllyinMargin";
import FtpfEnvelope from "@/components/analysis/fatigueFailure/FtpfEnvelope";
import HashinRotemSafety from "@/components/analysis/fatigueFailure/HashinRotemSafety";
import SimsBrogdonEnvelope from "@/components/analysis/fatigueFailure/SimsBrogdonEnvelope";
import SimpleChart from "@/components/charts/SimpleChart";
import InfoTooltip from "@/components/InfoTooltip";
import {
  FAF_METHODS,
  runFatigueFailureMethod,
} from "@/utils/fatigueFailureMethods";
import { parserConfig } from "@/utils/papaparse";
import download from "downloadjs";
import { parse } from "papaparse";

export default {
  name: "FatigueFailure",
  components: {
    InfoTooltip,
    SimpleChart,
    FawazEllyinMargin,
    FtpfEnvelope,
    HashinRotemSafety,
    SimsBrogdonEnvelope,
  },
  data() {
    return {
      methodNames: Object.keys(FAF_METHODS),
      selectedMethods: ["FTPF"],
      materialFile: null,
      aggFile: null,
      materialPreview: null,
      materialPreviewError: null,
      values: {
        targetAngle: 30,
        targetStressRatio: 0.1,
        targetStaticStrength: 89.47,
      },
      loading: false,
      outputs: {},
      errorsByMethod: {},
      series: [],
      parsedRows: {},
      requestVersion: 0,
    };
  },
  computed: {
    needsAggFile() {
      return this.selectedMethods.some((method) => FAF_METHODS[method].aggFile);
    },
    needsTargetStressRatio() {
      return this.selectedMethods.some(
        (method) => FAF_METHODS[method].targetStressRatio
      );
    },
    needsTargetStaticStrength() {
      return this.selectedMethods.some(
        (method) => FAF_METHODS[method].targetStaticStrength
      );
    },
    runnableMethods() {
      if (!this.materialFile || !Number.isFinite(this.values.targetAngle)) {
        return [];
      }
      return this.selectedMethods.filter((method) => {
        const requirements = FAF_METHODS[method];
        if (requirements.aggFile && !this.aggFile) {
          return false;
        }
        if (
          requirements.targetStressRatio &&
          !Number.isFinite(this.values.targetStressRatio)
        ) {
          return false;
        }
        if (
          requirements.targetStaticStrength &&
          (!Number.isFinite(this.values.targetStaticStrength) ||
            this.values.targetStaticStrength <= 0)
        ) {
          return false;
        }
        return true;
      });
    },
    outputMethods() {
      return Object.keys(this.outputs);
    },
  },
  methods: {
    async onMaterialFileChange() {
      const file = this.materialFile;
      this.materialPreview = null;
      this.materialPreviewError = null;

      if (file) {
        try {
          const parsed = JSON.parse(await file.text());
          if (!parsed || typeof parsed !== "object" || Array.isArray(parsed)) {
            throw new Error("The JSON root must be an object.");
          }
          if (this.materialFile !== file) {
            return;
          }
          this.materialPreview = {
            staticStrengths: Array.isArray(parsed.static_strengths)
              ? parsed.static_strengths
              : [],
            snCurves: Array.isArray(parsed.SN_curves) ? parsed.SN_curves : [],
          };
        } catch (error) {
          if (this.materialFile !== file) {
            return;
          }
          this.materialPreviewError = `Could not display material file: ${error.message}`;
        }
      }

      this.updateOutput();
    },
    normalizedLoadType(item) {
      return String(item?.type || "tensile").toLowerCase();
    },
    loadTypeLabel(item) {
      const labels = {
        tensile: "Tension",
        tension: "Tension",
        normal: "Tension",
        compressive: "Compression",
        compression: "Compression",
        shear: "Shear",
      };
      return labels[this.normalizedLoadType(item)] || "Unknown";
    },
    staticStrengthLabel(strength) {
      const labels = {
        tensile: "Tensile strength",
        tension: "Tensile strength",
        normal: "Tensile strength",
        compressive: "Compressive strength",
        compression: "Compressive strength",
        shear: "Shear strength",
      };
      const label =
        labels[this.normalizedLoadType(strength)] || "Unknown strength";
      if (strength?.angle === undefined || strength?.angle === null) {
        return label;
      }
      return `${label} at ${this.displayValue(strength.angle)}°`;
    },
    angleLabel(item) {
      return item?.angle === undefined || item?.angle === null
        ? "Angle —"
        : `Angle ${this.displayValue(item.angle)}°`;
    },
    curveDetails(curve) {
      const details = Object.entries(curve);
      if (!Object.prototype.hasOwnProperty.call(curve, "type")) {
        details.unshift(["type", "tensile (default)"]);
      }
      if (!Object.prototype.hasOwnProperty.call(curve, "angle")) {
        details.splice(1, 0, ["angle", "not applicable"]);
      }
      return details;
    },
    fieldLabel(field) {
      return field
        .replace(/_/g, " ")
        .replace(/\b\w/g, (letter) => letter.toUpperCase());
    },
    displayValue(value) {
      if (value === null) {
        return "null";
      }
      if (typeof value === "object") {
        return JSON.stringify(value);
      }
      return String(value);
    },
    async updateOutput() {
      const runnable = this.runnableMethods;
      const requestVersion = ++this.requestVersion;
      if (runnable.length === 0) {
        this.loading = false;
        this.series = [];
        this.outputs = {};
        this.errorsByMethod = {};
        this.parsedRows = {};
        return;
      }

      this.loading = true;
      const results = await Promise.allSettled(
        runnable.map((method) =>
          runFatigueFailureMethod(
            this.$analysisApi,
            method,
            this.values,
            this.materialFile,
            this.aggFile
          ).then((data) => ({ method, data }))
        )
      );

      if (requestVersion !== this.requestVersion) {
        return;
      }

      const outputs = {};
      const errorsByMethod = {};
      const series = [];
      const parsedRows = {};

      results.forEach((result, index) => {
        const method = runnable[index];
        if (result.status === "rejected") {
          const error = result.reason;
          errorsByMethod[method] =
            error?.body?.detail ||
            error?.body?.message ||
            error?.message ||
            "Invalid input";
          return;
        }

        outputs[method] = result.value.data;
        const parsed = parse(result.value.data.csv_data, parserConfig);
        parsedRows[method] = parsed.data;
        const groups = {};
        parsed.data.forEach((item) => {
          const key = item.stress_ratio;
          if (!groups[key]) {
            groups[key] = [];
          }
          groups[key].push([item.cycles_to_failure, item.stress_max]);
        });
        Object.keys(groups).forEach((stressRatio) => {
          series.push({
            type: "line",
            name: `${method} (R=${stressRatio})`,
            data: groups[stressRatio],
          });
        });
      });

      this.outputs = outputs;
      this.errorsByMethod = errorsByMethod;
      this.series = series;
      this.parsedRows = parsedRows;
      this.loading = false;
    },
    downloadOutput(method) {
      const output = this.outputs[method];
      if (!output) {
        return;
      }
      const angle = this.values.targetAngle;
      const outputName = Number.isFinite(angle)
        ? `FAF_${angle}deg_${method}`
        : `FAF_${method}`;
      download(output.csv_data, outputName + ".csv", "text/csv");
      download(output.json_data, outputName + ".json", "application/json");
    },
  },
};
</script>

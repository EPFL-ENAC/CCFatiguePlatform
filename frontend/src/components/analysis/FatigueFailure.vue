<template>
  <v-card :loading="loading">
    <v-card-title>
      Fatigue Failure
      <v-spacer />
      <info-tooltip>
        The fatigue failure module allows us to predict the S-N curve that will
        arise from off axis loadings. It is designed for experiments where
        samples are subjected to multiaxial loadings. Select several methods to
        compare their predictions on the same chart - parameters shared by
        several methods only need to be entered once.
      </info-tooltip>
    </v-card-title>
    <v-card-subtitle>
      <v-row>
        <v-col>
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
      </v-row>

      <v-alert v-if="selectedMethods.includes('Kawai')" type="info" dense text>
        This tab shows Kawai's raw S-N curve. For the full margin-of-safety
        analysis with this criterion (life isolines, estimate at a given
        operating point), see the
        <a href="#" @click.prevent="$emit('go-to-module', 'cld-analysis')"
          >CLD tab</a
        >.
      </v-alert>

      <v-row>
        <v-col
          v-for="columnKey in activeColumnKeys"
          :key="columnKey"
          cols="12"
          md="4"
        >
          <v-card outlined height="100%">
            <v-card-title class="text-subtitle-1">
              {{ columnLabel(columnKey) }}
            </v-card-title>
            <v-card-text>
              <template
                v-if="
                  columnFiles(columnKey).length > 0 ||
                  columnParams(columnKey, 'source').length > 0
                "
              >
                <div class="text-overline">Source / Model</div>
                <v-select
                  v-if="
                    columnKey === 'shear' && selectedMethods.includes('FTPF')
                  "
                  v-model="values.ftpfFType"
                  :items="FAF_PARAMS.ftpfFType.items"
                  :label="fTypeLabel('FTPF')"
                  dense
                  :disabled="loading"
                  @change="updateOutput"
                ></v-select>
                <v-select
                  v-if="
                    columnKey === 'shear' &&
                    selectedMethods.includes('SimsBrogdon')
                  "
                  v-model="values.simsBrogdonFType"
                  :items="FAF_PARAMS.simsBrogdonFType.items"
                  :label="fTypeLabel('SimsBrogdon')"
                  dense
                  :disabled="loading"
                  @change="updateOutput"
                ></v-select>
                <div v-for="fileKey in columnFiles(columnKey)" :key="fileKey">
                  <v-file-input
                    v-model="files[fileKey]"
                    show-size
                    dense
                    :accept="FAF_FILES[fileKey].accept"
                    :disabled="loading"
                    :label="fileLabel(fileKey)"
                    @change="onFileChange(fileKey)"
                  >
                    <template #append>
                      <info-tooltip>
                        {{ FAF_FILES[fileKey].tooltip }}<br />Format:
                        {{ FAF_FILES[fileKey].accept }} file.<br />See the
                        <a :href="conventionUrl(fileKey)"
                          >{{ FAF_FILES[fileKey].conventionPrefix }} Data
                          Convention</a
                        >
                      </info-tooltip>
                    </template>
                  </v-file-input>
                  <div
                    v-if="filePreviews[fileKey]"
                    class="text-caption grey--text text--darken-1 mt-n2 mb-3"
                  >
                    <template v-if="filePreviews[fileKey].kind === 'snc'">
                      R = {{ filePreviews[fileKey].stressRatio }} &middot; a =
                      {{ formatNumber(filePreviews[fileKey].a) }} &middot; b =
                      {{ formatNumber(filePreviews[fileKey].b) }} &middot; CI =
                      {{ filePreviews[fileKey].confidenceInterval }}%
                    </template>
                    <template v-else-if="filePreviews[fileKey].kind === 'agg'">
                      Groups: R =
                      {{ filePreviews[fileKey].stressRatios.join(", ") }} ({{
                        filePreviews[fileKey].rowCount
                      }}
                      rows)
                    </template>
                    <template v-else> Could not parse file. </template>
                  </div>
                </div>
                <v-row dense>
                  <v-col
                    v-for="paramKey in columnParams(columnKey, 'source')"
                    :key="paramKey"
                    cols="12"
                  >
                    <v-select
                      v-if="FAF_PARAMS[paramKey].type === 'select'"
                      v-model="values[paramKey]"
                      :items="FAF_PARAMS[paramKey].items"
                      :label="paramLabel(paramKey)"
                      dense
                      :disabled="loading"
                      @change="updateOutput"
                    ></v-select>
                    <v-text-field
                      v-else
                      v-model.number="values[paramKey]"
                      :label="paramLabel(paramKey)"
                      type="number"
                      dense
                      @change="updateOutput"
                    ></v-text-field>
                  </v-col>
                </v-row>
              </template>

              <template v-if="columnParams(columnKey, 'main').length > 0">
                <div class="text-overline mt-4">Main Properties</div>
                <v-row
                  v-for="groupBlock in columnParamGroups(columnKey, 'main')"
                  :key="groupBlock.group"
                  dense
                >
                  <v-col
                    v-for="paramKey in groupBlock.paramKeys"
                    :key="paramKey"
                    cols="6"
                  >
                    <v-text-field
                      v-model.number="values[paramKey]"
                      :label="paramLabel(paramKey)"
                      type="number"
                      dense
                      @change="updateOutput"
                    ></v-text-field>
                  </v-col>
                </v-row>
              </template>

              <template v-if="columnParams(columnKey, 'secondary').length > 0">
                <div class="text-overline mt-4">Secondary / Off-axis</div>
                <v-row
                  v-for="groupBlock in columnParamGroups(
                    columnKey,
                    'secondary'
                  )"
                  :key="groupBlock.group"
                  dense
                >
                  <v-col
                    v-for="paramKey in groupBlock.paramKeys"
                    :key="paramKey"
                    cols="6"
                  >
                    <v-text-field
                      v-model.number="values[paramKey]"
                      :label="paramLabel(paramKey)"
                      type="number"
                      dense
                      @change="updateOutput"
                    ></v-text-field>
                  </v-col>
                </v-row>
              </template>
            </v-card-text>
          </v-card>
        </v-col>

        <template v-if="selectedMethods.includes('HashinRotem')">
          <v-col cols="12" md="4">
            <v-card outlined height="100%">
              <v-card-title class="text-subtitle-1">
                Panel 2 — {{ values.hashinPanel2Type }}
              </v-card-title>
              <v-card-text>
                <v-select
                  v-model="values.hashinPanel2Type"
                  :items="FAF_PARAMS.hashinPanel2Type.items"
                  label="Type"
                  dense
                  :disabled="loading"
                  @change="updateOutput"
                ></v-select>
                <v-file-input
                  v-model="files.hashinPanel2File"
                  show-size
                  dense
                  accept=".json"
                  :disabled="loading"
                  :label="hashinPanelFileLabel(values.hashinPanel2Type)"
                  @change="onFileChange('hashinPanel2File')"
                ></v-file-input>
                <template v-if="values.hashinPanel2Type === 'Off-axis'">
                  <v-text-field
                    v-model.number="values.offAxisAngle"
                    label="Off-axis Angle (°)"
                    type="number"
                    dense
                    @change="updateOutput"
                  ></v-text-field>
                  <v-text-field
                    v-model.number="values.tensileStrength1"
                    label="Static Strength at this Angle"
                    type="number"
                    dense
                    @change="updateOutput"
                  ></v-text-field>
                </template>
              </v-card-text>
            </v-card>
          </v-col>
          <v-col cols="12" md="4">
            <v-card outlined height="100%">
              <v-card-title class="text-subtitle-1">
                Panel 3 — {{ values.hashinPanel3Type }}
              </v-card-title>
              <v-card-text>
                <v-select
                  v-model="values.hashinPanel3Type"
                  :items="FAF_PARAMS.hashinPanel3Type.items"
                  label="Type"
                  dense
                  :disabled="loading"
                  @change="updateOutput"
                ></v-select>
                <v-file-input
                  v-model="files.hashinPanel3File"
                  show-size
                  dense
                  accept=".json"
                  :disabled="loading"
                  :label="hashinPanelFileLabel(values.hashinPanel3Type)"
                  @change="onFileChange('hashinPanel3File')"
                ></v-file-input>
                <template v-if="values.hashinPanel3Type === 'Off-axis'">
                  <v-text-field
                    v-model.number="values.offAxisAngle2"
                    label="Off-axis Angle (°)"
                    type="number"
                    dense
                    @change="updateOutput"
                  ></v-text-field>
                  <v-text-field
                    v-model.number="values.tensileStrength2"
                    label="Static Strength at this Angle"
                    type="number"
                    dense
                    @change="updateOutput"
                  ></v-text-field>
                </template>
              </v-card-text>
            </v-card>
          </v-col>
        </template>
      </v-row>

      <v-row v-if="selectedMethods.includes('HashinRotem')">
        <v-col cols="12">
          <v-card outlined>
            <v-card-title class="text-subtitle-1">
              Material Properties (Common)
            </v-card-title>
            <v-card-text>
              <v-row dense>
                <v-col cols="12" md="4">
                  <v-text-field
                    v-model.number="values.tensileTransverseStrength"
                    :label="paramLabel('tensileTransverseStrength')"
                    type="number"
                    dense
                    @change="updateOutput"
                  ></v-text-field>
                </v-col>
                <v-col cols="12" md="4">
                  <v-text-field
                    v-model.number="values.shearStrength"
                    :label="paramLabel('shearStrength')"
                    type="number"
                    dense
                    @change="updateOutput"
                  ></v-text-field>
                </v-col>
              </v-row>
            </v-card-text>
          </v-card>
          <v-alert
            v-if="hashinRotemPanelError"
            type="error"
            dense
            text
            class="mt-2"
          >
            {{ hashinRotemPanelError }}
          </v-alert>
        </v-col>
      </v-row>

      <v-row v-if="angleParamKeys.length > 0">
        <v-col cols="12">
          <v-card outlined>
            <v-card-title class="text-subtitle-1"> Desired Angle </v-card-title>
            <v-card-text>
              <v-row dense>
                <v-col
                  v-for="paramKey in angleParamKeys"
                  :key="paramKey"
                  cols="12"
                  md="4"
                >
                  <v-text-field
                    v-model.number="values[paramKey]"
                    :label="paramLabel(paramKey)"
                    type="number"
                    dense
                    @change="updateOutput"
                  ></v-text-field>
                </v-col>
              </v-row>
            </v-card-text>
          </v-card>
        </v-col>
      </v-row>

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
  ANGLE_PANEL_PARAMS,
  FAF_COLUMNS,
  FAF_FILES,
  FAF_GROUP_ORDER,
  FAF_METHODS,
  FAF_PARAMS,
  getColumnLabel,
  getParamLabel,
} from "@/utils/fatigueFailureMethods";
import { parserConfig } from "@/utils/papaparse";
import download from "downloadjs";
import { parse } from "papaparse";

function defaultValues() {
  const values = {};
  Object.keys(FAF_PARAMS).forEach((key) => {
    values[key] = FAF_PARAMS[key].default;
  });
  return values;
}

function defaultFiles() {
  const files = {};
  Object.keys(FAF_FILES).forEach((key) => {
    files[key] = null;
  });
  return files;
}

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
      FAF_FILES,
      FAF_PARAMS,
      methodNames: Object.keys(FAF_METHODS),
      selectedMethods: ["FTPF"],
      files: defaultFiles(),
      filePreviews: {},
      values: defaultValues(),
      loading: false,
      outputs: {},
      errorsByMethod: {},
      series: [],
      parsedRows: {},
      parsedMeta: {},
    };
  },
  computed: {
    activeFileKeys: function () {
      const keys = new Set();
      this.selectedMethods.forEach((method) => {
        FAF_METHODS[method].files.forEach((key) => keys.add(key));
        (FAF_METHODS[method].optionalFiles || []).forEach((key) =>
          keys.add(key)
        );
      });
      return Object.keys(FAF_FILES).filter((key) => keys.has(key));
    },
    // Params relevant to the generic per-column cards / "Desired Angle"
    // panel - defaults to the method's full `params` list, but a method
    // can narrow this via `uiParams` when some of its params are still
    // required for its backend call (sent from `values` at their default)
    // without belonging in the shared UI (e.g. Hashin-Rotem's panel 2/3
    // render those fields inline instead).
    activeParamKeys: function () {
      const keys = new Set();
      this.selectedMethods.forEach((method) =>
        (FAF_METHODS[method].uiParams ?? FAF_METHODS[method].params).forEach(
          (key) => keys.add(key)
        )
      );
      // offAxisAngle is only meaningful for FTPF/SimsBrogdon when their
      // respective Type selector is set to Off-axis - hide it unless at
      // least one selected method that contributes it still needs it.
      if (keys.has("offAxisAngle")) {
        const ftpfNeedsIt =
          this.selectedMethods.includes("FTPF") &&
          this.values.ftpfFType !== "Shear";
        const simsBrogdonNeedsIt =
          this.selectedMethods.includes("SimsBrogdon") &&
          this.values.simsBrogdonFType !== "Shear";
        if (!ftpfNeedsIt && !simsBrogdonNeedsIt) {
          keys.delete("offAxisAngle");
        }
      }
      return Object.keys(FAF_PARAMS).filter((key) => keys.has(key));
    },
    activeColumnKeys: function () {
      return Object.keys(FAF_COLUMNS).filter(
        (columnKey) =>
          this.columnFiles(columnKey).length > 0 ||
          ["source", "main", "secondary"].some(
            (section) => this.columnParams(columnKey, section).length > 0
          )
      );
    },
    // Desired angle + static strength at that angle always live in their
    // own panel, separate from the per-column loading-direction cards.
    angleParamKeys: function () {
      return ANGLE_PANEL_PARAMS.filter((key) =>
        this.activeParamKeys.includes(key)
      );
    },
    // Panel 2 is restricted to [Transverse, Off-axis] and Panel 3 to
    // [Shear, Off-axis] (fatigueFailureMethods.js), so a duplicate
    // non-off-axis type can no longer be selected in the first place - the
    // only case left to guard is both panels set to Off-axis with the same
    // angle, mirroring the backend's det~=0 guard.
    hashinRotemPanelError: function () {
      const type2 = this.values.hashinPanel2Type;
      const type3 = this.values.hashinPanel3Type;
      if (
        type2 === "Off-axis" &&
        type3 === "Off-axis" &&
        Number(this.values.offAxisAngle) === Number(this.values.offAxisAngle2)
      ) {
        return "Panel 2 and Panel 3 are both Off-axis with the same angle - they must differ.";
      }
      return null;
    },
    runnableMethods: function () {
      return this.selectedMethods.filter((method) => {
        if (method === "HashinRotem" && this.hashinRotemPanelError) {
          return false;
        }
        return FAF_METHODS[method].files.every(
          (key) => this.files[key] != null
        );
      });
    },
    outputMethods: function () {
      return Object.keys(this.outputs);
    },
  },
  methods: {
    columnFiles(columnKey) {
      return this.activeFileKeys.filter(
        (fileKey) => FAF_FILES[fileKey].column === columnKey
      );
    },
    columnParams(columnKey, section) {
      return this.activeParamKeys
        .filter(
          (paramKey) =>
            !ANGLE_PANEL_PARAMS.includes(paramKey) &&
            FAF_PARAMS[paramKey].column === columnKey &&
            FAF_PARAMS[paramKey].section === section
        )
        .slice()
        .sort(
          (a, b) =>
            FAF_GROUP_ORDER.indexOf(FAF_PARAMS[a].group) -
            FAF_GROUP_ORDER.indexOf(FAF_PARAMS[b].group)
        );
    },
    // Splits the already-grouped param list into one block per family, so
    // each family can start on its own row instead of flowing across rows.
    columnParamGroups(columnKey, section) {
      const groups = [];
      this.columnParams(columnKey, section).forEach((paramKey) => {
        const group = FAF_PARAMS[paramKey].group;
        const lastGroup = groups[groups.length - 1];
        if (!lastGroup || lastGroup.group !== group) {
          groups.push({ group, paramKeys: [paramKey] });
        } else {
          lastGroup.paramKeys.push(paramKey);
        }
      });
      return groups;
    },
    paramLabel(paramKey) {
      return getParamLabel(paramKey, this.selectedMethods);
    },
    columnLabel(columnKey) {
      return getColumnLabel(columnKey, this.selectedMethods);
    },
    hashinPanelFileLabel(panelType) {
      return `${panelType} fatigue data (SNC json file)`;
    },
    // fFile is shared between FTPF and SimsBrogdon, both of which make its
    // meaning explicit via their own Type selector - reflect that in the
    // label when either is selected (FTPF takes precedence if both are).
    fileLabel(fileKey) {
      if (fileKey === "fFile") {
        if (this.selectedMethods.includes("FTPF")) {
          return this.values.ftpfFType === "Shear"
            ? "Shear fatigue data (SNC json file)"
            : "Off-axis fatigue data (SNC json file)";
        }
        if (this.selectedMethods.includes("SimsBrogdon")) {
          return this.values.simsBrogdonFType === "Shear"
            ? "Shear fatigue data (SNC json file)"
            : "Off-axis fatigue data (SNC json file)";
        }
      }
      return FAF_FILES[fileKey].label;
    },
    // Disambiguates the per-method "Type" selects when both FTPF and
    // SimsBrogdon are selected at once - they share the same fFile column
    // but each drives its own backend call independently.
    fTypeLabel(method) {
      const both =
        this.selectedMethods.includes("FTPF") &&
        this.selectedMethods.includes("SimsBrogdon");
      return both ? `Type (${method})` : "Type";
    },
    conventionUrl(fileKey) {
      return `https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/Data/${FAF_FILES[fileKey].conventionPrefix}_Data_Convention.md`;
    },
    formatNumber(value) {
      return typeof value === "number" ? Number(value.toPrecision(4)) : value;
    },
    async onFileChange(fileKey) {
      await this.updatePreview(fileKey);
      this.updateOutput();
    },
    async updatePreview(fileKey) {
      const file = this.files[fileKey];
      if (!file) {
        this.$delete(this.filePreviews, fileKey);
        return;
      }
      try {
        const text = await file.text();
        if (FAF_FILES[fileKey].accept === ".json") {
          const records = JSON.parse(text);
          const first = Array.isArray(records) ? records[0] : records;
          this.$set(this.filePreviews, fileKey, {
            kind: "snc",
            stressRatio: first.stress_ratio,
            a: first.a,
            b: first.b,
            confidenceInterval: first.confidence_interval,
          });
        } else {
          const parsed = parse(text, parserConfig);
          const stressRatios = [
            ...new Set(parsed.data.map((row) => row.stress_ratio)),
          ];
          this.$set(this.filePreviews, fileKey, {
            kind: "agg",
            stressRatios,
            rowCount: parsed.data.length,
          });
        }
      } catch (e) {
        this.$set(this.filePreviews, fileKey, { kind: "error" });
      }
    },
    async updateOutput() {
      const runnable = this.runnableMethods;
      if (runnable.length === 0) {
        this.series = [];
        this.outputs = {};
        this.errorsByMethod = {};
        return;
      }
      this.loading = true;

      const results = await Promise.allSettled(
        runnable.map((method) =>
          FAF_METHODS[method]
            .run(this.$analysisApi, this.values, this.files)
            .then((data) => ({ method, data }))
        )
      );

      const outputs = {};
      const errorsByMethod = {};
      const series = [];
      const parsedRows = {};
      const parsedMeta = {};
      results.forEach((result, index) => {
        const method = runnable[index];
        if (result.status === "rejected") {
          // This project's generated API client (ApiClient.js) uses
          // superagent, not axios - the parsed error response body lands
          // on error.body (FastAPI's HTTPException -> {"detail": "..."}),
          // not error.response.data like an axios-based client would have.
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
        parsedMeta[method] = this.parseJsonData(result.value.data.json_data);
        // Group rows by stress_ratio: some methods return one curve per
        // reference stress ratio group, others a single curve
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
      this.parsedMeta = parsedMeta;
      this.loading = false;
    },
    parseJsonData(json) {
      try {
        const parsed = typeof json === "string" ? JSON.parse(json) : json;
        return Array.isArray(parsed) ? parsed[0] : parsed;
      } catch {
        return {};
      }
    },
    downloadOutput(method) {
      const output = this.outputs[method];
      if (!output) {
        return;
      }
      const angle = this.values.desirableAngle;
      const outputName = Number.isFinite(angle)
        ? `FAF_${angle}deg_${method}`
        : `FAF_${method}`;
      download(output.csv_data, outputName + ".csv", "text/csv");
      download(output.json_data, outputName + ".json", "application/json");
    },
  },
};
</script>

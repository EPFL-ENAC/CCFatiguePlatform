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

      <v-row>
        <v-col
          v-for="columnKey in activeColumnKeys"
          :key="columnKey"
          cols="12"
          md="4"
        >
          <v-card outlined height="100%">
            <v-card-title class="text-subtitle-1">
              {{ FAF_COLUMNS[columnKey].label }}
            </v-card-title>
            <v-card-text>
              <template
                v-if="
                  columnFiles(columnKey).length > 0 ||
                  columnParams(columnKey, 'source').length > 0
                "
              >
                <div class="text-overline">Source / Model</div>
                <div v-for="fileKey in columnFiles(columnKey)" :key="fileKey">
                  <v-file-input
                    v-model="files[fileKey]"
                    show-size
                    dense
                    :accept="FAF_FILES[fileKey].accept"
                    :disabled="loading"
                    :label="FAF_FILES[fileKey].label"
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
import SimpleChart from "@/components/charts/SimpleChart";
import InfoTooltip from "@/components/InfoTooltip";
import { getOutputFileName } from "@/utils/analysis";
import {
  FAF_COLUMNS,
  FAF_FILES,
  FAF_GROUP_ORDER,
  FAF_METHODS,
  FAF_PARAMS,
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
  },
  data() {
    return {
      FAF_COLUMNS,
      FAF_FILES,
      FAF_PARAMS,
      methodNames: Object.keys(FAF_METHODS),
      selectedMethods: ["FTPT"],
      files: defaultFiles(),
      filePreviews: {},
      values: defaultValues(),
      loading: false,
      outputs: {},
      errorsByMethod: {},
      series: [],
    };
  },
  computed: {
    activeFileKeys: function () {
      const keys = new Set();
      this.selectedMethods.forEach((method) =>
        FAF_METHODS[method].files.forEach((key) => keys.add(key))
      );
      return Object.keys(FAF_FILES).filter((key) => keys.has(key));
    },
    activeParamKeys: function () {
      const keys = new Set();
      this.selectedMethods.forEach((method) =>
        FAF_METHODS[method].params.forEach((key) => keys.add(key))
      );
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
    runnableMethods: function () {
      return this.selectedMethods.filter((method) =>
        FAF_METHODS[method].files.every((key) => this.files[key] != null)
      );
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
      results.forEach((result, index) => {
        const method = runnable[index];
        if (result.status === "rejected") {
          errorsByMethod[method] = "Invalid input";
          return;
        }
        outputs[method] = result.value.data;
        const parsed = parse(result.value.data.csv_data, parserConfig);
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
      this.loading = false;
    },
    downloadOutput(method) {
      const output = this.outputs[method];
      if (!output) {
        return;
      }
      const fileKey = FAF_METHODS[method].files[0];
      const inputFileName = this.files[fileKey].name;
      const outputName = getOutputFileName(
        FAF_FILES[fileKey].conventionPrefix,
        "FAF",
        inputFileName,
        method
      );
      download(output.csv_data, outputName + ".csv", "text/csv");
      download(output.json_data, outputName + ".json", "application/json");
    },
  },
};
</script>

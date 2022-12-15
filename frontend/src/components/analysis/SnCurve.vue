<template>
  <v-card :loading="loading">
    <v-card-title>
      S-N Curve
      <v-spacer />
      <info-tooltip>
        This module plots curves on the (Stress - Number of cycles) plane. The
        curves are associated with 3 different methods (Lin-Log, Log-Log,
        Sendeckyj) and can be considered as constitutive laws for fatigue life
        predictions. On the graphs we also plot the individual results gathered
        over the tests with inputs (Cycles to failure - Stress at failure)
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
      <v-row>
        <v-col>
          <v-select
            v-model="selectedMethods"
            label="select S-N curve method(s)"
            :items="methods"
            chips
            multiple
            :disabled="loading"
            @change="updateOutput"
          >
          </v-select>
        </v-col>
        <v-col>
          <v-select
            v-model="selectedRRatios"
            label="select R ratio"
            :items="rRatios"
            chips
            multiple
            :disabled="loading"
            @change="updateOutput"
          >
          </v-select>
        </v-col>
      </v-row>
    </v-card-subtitle>
    <v-card-text v-if="series.length > 0">
      <simple-chart
        :aspect-ratio="2"
        :series="series"
        title="S-N Curves"
        x-axis-name="N"
        y-axis-name="Maximum Cyclic Stress [MPa]"
        x-axis-type="log"
      ></simple-chart>
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
  </v-card>
</template>

<script>
import SnCurveMethod from "@/backend/model/SnCurveMethod";
import SimpleChart from "@/components/charts/SimpleChart";
import InfoTooltip from "@/components/InfoTooltip";
import { getOutputFileName } from "@/utils/analysis";
import { parserConfig } from "@/utils/papaparse";
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
    };
  },
  computed: {
    hasInput: function () {
      return this.file;
    },
  },
  methods: {
    updateOutput() {
      if (
        this.selectedMethods.length > 0 &&
        this.selectedRRatios.length > 0 &&
        this.file
      ) {
        this.loading = true;
        Promise.all(
          this.selectedMethods.map((method) =>
            this.$analysisApi
              .runSnCurveFile(method, this.file)
              .then((analysisResult) => {
                const results = parse(analysisResult.csv_data, parserConfig);
                const rows = results.data.filter((row) =>
                  this.selectedRRatios.includes(row.stress_ratio)
                );
                return {
                  method: method,
                  analysisResult: analysisResult,
                  rows: rows,
                };
              })
          )
        )
          .then((data) => {
            this.outputs = Object.fromEntries(
              data.map((item) => [item.method, item.analysisResult])
            );
            this.series = data.flatMap((item) =>
              this.selectedRRatios.flatMap((rRatio) => [
                {
                  type: "line",
                  name: `${item.method} ${rRatio}`,
                  data: item.rows
                    .filter((row) => row.stress_ratio === rRatio)
                    .map((row) => [row.cycles_to_failure, row.stress_max]),
                },
                {
                  type: "line",
                  name: `${item.method} ${rRatio}`,
                  data: item.rows
                    .filter((row) => row.stress_ratio === rRatio)
                    .map((row) => [
                      row.cycles_to_failure,
                      row.stress_lowerbound,
                    ]),
                  lineStyle: {
                    type: "dashed",
                    width: 1,
                  },
                },
                {
                  type: "line",
                  name: `${item.method} ${rRatio}`,
                  data: item.rows
                    .filter((row) => row.stress_ratio === rRatio)
                    .map((row) => [
                      row.cycles_to_failure,
                      row.stress_upperbound,
                    ]),
                  lineStyle: {
                    type: "dashed",
                    width: 1,
                  },
                },
              ])
            );
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
  },
};
</script>

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
            label="SNC csv file"
            @change="updateOutput"
          >
            <template #append>
              <info-tooltip>
                See the
                <a
                  href="https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/Data/SNC_Data_Convention.md"
                  >SNC Data Convention</a
                >
                <!-- The data required as input is a csv file with 3 distinct
                columns:
                <ul>
                  <li>Stress ratio (R) [-]</li>
                  <li>Number of cycles to failure</li>
                  <li>Stress parameter (Stress at failure) [MPa]</li>
                </ul>
                Additionally to these 3 columns, there are 11 values required
                for analysis:
                <ul>
                  <li>R-ratio</li>
                  <li>PN(f) !Reliability level</li>
                  <li>So !(c^(-1/b))</li>
                  <li>1/k ! (1/b)? -> Pow in fortran code</li>
                  <li>af pooled data</li>
                  <li>Scale pooled data</li>
                  <li>LRSQ</li>
                  <li>RMSE !Root mean square error</li>
                  <li>SSE !Sum of squares due to errors</li>
                  <li>SST !Sum of squares about the mean</li>
                  <li>RSQ !R-square</li>
                </ul> -->
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
    <v-card-text v-if="series.length > 0">
      <simple-chart
        :aspect-ratio="2"
        :series="series"
        x-axis-name="Mean Stress [MPa]"
        y-axis-name="Stress Amplitude [MPa]"
      ></simple-chart>
    </v-card-text>
    <v-card-actions v-if="hasInput" class="justify-end">
      <v-btn :disabled="loading && output != null" @click="downloadOutput">
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
import { parserConfig } from "@/utils/papaparse";
import download from "downloadjs";
import { parse } from "papaparse";

const methods = Object.values(new CldMethod());

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
      output: null,
      methods: methods,
      method: methods[0],
      series: [],
      errorMessages: null,
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
          .runCldFile(this.method, this.file)
          .then((data) => {
            this.output = data;
            const results = parse(data, parserConfig);
            this.series = [
              {
                type: "line",
                name: this.method,
                data: results.data.map((item) => [
                  item.stress_mean,
                  item.stress_amplitude,
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
          "SNC",
          "CLD",
          this.file.name,
          this.method
        );
        download(this.output, outputName + ".csv", "text/csv");
      }
    },
  },
};
</script>

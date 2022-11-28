<template>
  <v-card :loading="loading">
    <v-card-title>
      Cycle Counting
      <v-spacer />
      <info-tooltip
        >Cycle counting is used to summarize irregular load-versus-time
        histories by providing the number of occurrences of cycles of various
        sizes.</info-tooltip
      >
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
                  href="https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/Data/LDS_Data_Convention.md"
                  >LDS Data Convention</a
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
    <v-card-text v-if="series.length > 0">
      <simple-chart
        :aspect-ratio="2"
        :series="series"
        x-axis-name="cum_n_cycles"
        y-axis-name="stress_range"
      >
      </simple-chart>
    </v-card-text>
    <v-card-actions v-if="hasInput" class="justify-end">
      <v-btn :disabled="loading && output != null" @click="downloadOutput">
        Download CYC
      </v-btn>
      <info-tooltip>
        See the
        <a
          href="https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/Data/CYC_Data_Convention.md"
          >CYC Data Convention</a
        >
      </info-tooltip>
    </v-card-actions>
  </v-card>
</template>

<script>
import CycleCountingMethod from "@/backend/model/CycleCountingMethod";
import SimpleChart from "@/components/charts/SimpleChart";
import InfoTooltip from "@/components/InfoTooltip";
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
          .then((data) => {
            this.output = data;
            const results = parse(data, parserConfig);
            this.series = [
              {
                type: "line",
                name: this.method,
                data: results.data.map((item) => [
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
        download(this.output, `CYC-${this.method}.csv`, "text/csv");
      }
    },
  },
};
</script>

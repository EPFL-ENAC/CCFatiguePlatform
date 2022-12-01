<template>
  <v-card :loading="loading">
    <v-card-title>
      S-N Curve
      <v-spacer />
      <info-tooltip>
        This module plots curves on the (Stress - Number of cycles) plane. The
        curves are associated with 4 different methods (Lin-Log, Log-Log,
        Sendeckyj, Whitney) and can be considered as constitutive laws for
        fatigue life predictions. On the graphs we also plot the individual
        results gathered over the tests with inputs (Cycles to failure - Stress
        at failure)
      </info-tooltip>
    </v-card-title>
    <v-card-subtitle>
      <v-row align="center">
        <v-col>
          <v-file-input
            v-model="file"
            chips
            show-size
            accept=".txt,.csv"
            label="Upload file"
            :disabled="loading"
            @change="updateOutput"
          >
            <template #append>
              <info-tooltip>
                The data required as input for this module takes the form of a
                csv file containing 6 columns and as many rows as there were
                testings in the experiment. The columns are populated as
                follows:
                <ul>
                  <li>Stress ratio (R) [-]</li>
                  <li>Reliability level (input parameter)</li>
                  <li>Stress level no. (estimated)</li>
                  <li>Stress parameter [MPa]</li>
                  <li>Number of cycles</li>
                  <li>Residual strength [MPa]</li>
                </ul>
                <p>
                  If a sample doesn't break under loading, we call it a test run
                  off and the value for residual strength is obtained by means
                  of a quasi static loading up to breaking. If the sample
                  doesn't break, residual strength takes the same value as the
                  stress parameter.
                </p>
              </info-tooltip>
            </template>
          </v-file-input>
        </v-col>
        <v-col>
          <v-btn disabled>Browse from Fatigue DB</v-btn>
        </v-col>
      </v-row>
    </v-card-subtitle>
    <v-card-text v-if="hasInput">
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
      <v-row>
        <simple-chart
          :aspect-ratio="2"
          :series="series"
          title="S-N Curves"
          x-axis-name="Number of cycles"
          y-axis-name="Stress"
          x-axis-type="log"
        ></simple-chart>
      </v-row>
    </v-card-text>
    <v-card-actions v-if="hasInput" class="justify-end">
      <v-btn :disabled="loading && output" @click="downloadOutput">
        Download
      </v-btn>
    </v-card-actions>
  </v-card>
</template>

<script>
import SnCurveMethod from "@/backend/model/SnCurveMethod";
import SimpleChart from "@/components/charts/SimpleChart";
import InfoTooltip from "@/components/InfoTooltip";
import download from "downloadjs";
import { zip } from "lodash";

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
      selectedMethods: [],
      methods: methods,
      selectedRRatios: [],
      rRatios: [-1, 0.1, 10, 0.5],
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
        this.$analysisApi
          .runSnCurveFile(this.selectedMethods, this.selectedRRatios, this.file)
          .then((data) => {
            this.outputs = data.outputs;
            this.series = data.lines.map((line) => ({
              type: "line",
              name: line.name,
              data: zip(line.xData, line.yData),
            }));
          })
          .finally(() => (this.loading = false));
      }
    },
    downloadOutput() {
      for (const [key, value] of Object.entries(this.outputs)) {
        download(value, `sn-curve-${key.toLowerCase()}-output.csv`, "text/csv");
      }
    },
  },
};
</script>

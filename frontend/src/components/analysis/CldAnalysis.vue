<template>
  <v-card :loading="loading">
    <v-card-title>
      CLD
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
            accept=".txt,.csv"
            :error-messages="errorMessages"
            :disabled="loading"
            label="Upload file"
            @change="updateOutput"
          >
            <template #append>
              <info-tooltip>
                The data required as input is a csv file with 3 distinct
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
                </ul>
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
    <v-card-text>
      <v-img src="/img/results_dashboard2.png"></v-img>
    </v-card-text>
    <v-card-actions v-if="hasInput" class="justify-end">
      <v-btn :disabled="loading && output != null" @click="downloadOutput">
        Download
      </v-btn>
    </v-card-actions>
  </v-card>
</template>

<script>
import CldMethod from "@/backend/model/CldMethod";
import InfoTooltip from "@/components/InfoTooltip";
import download from "downloadjs";

const methods = Object.values(new CldMethod());

export default {
  name: "CldAnalysis",
  components: {
    InfoTooltip,
  },
  data() {
    return {
      file: null,
      loading: false,
      output: null,
      methods: methods,
      method: methods[0],
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
            this.errorMessages = null;
          })
          .catch(() => {
            this.file = null;
            this.errorMessages = "Invalid input";
          })
          .finally(() => (this.loading = false));
      }
    },
    downloadOutput() {
      if (this.output) {
        download(this.output, `cld-${this.method}-output.txt`, "text/plain");
      }
    },
  },
};
</script>

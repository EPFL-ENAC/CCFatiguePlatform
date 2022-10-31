<template>
  <v-card :loading="loading">
    <v-card-title>
      Fatigue Failure
      <v-spacer />
      <info-tooltip>
        The fatigue failure module allows us to predict the S-N curve that will
        arise from off axis loadings. It is designed for experiments where
        samples are subjected to multiaxial loadings.
      </info-tooltip>
    </v-card-title>
    <v-card-subtitle>
      <v-row align="end">
        <v-col>
          <v-file-input
            v-model="xFile"
            show-size
            accept=".json"
            :error-messages="errorMessages"
            :disabled="loading"
            label="X file"
            @change="updateOutput"
          >
            <template v-slot:append>
              <info-tooltip>
                <p>
                  The data type selection box allows us to enter static, fatigue
                  and reference data that are necessary to the analysis, the
                  values that should be placed as input are dependent on the
                  method used for analysis. Static data have to do with the
                  static parameters of an experiment, they give information
                  relative to the strength of the material. Fatigue data are
                  related to the fatigue analysis, they give information about
                  the fatigue life of the experiment. Reference data are related
                  to the data we wish to extract from the analysis, they give
                  information about the orientation in which we seek more
                  information.
                </p>
                <p>
                  The inputs to provide highly depend on the selected method but
                  there are two main formats used as input data:
                </p>
                <p>
                  MKawai & Shorkieh-Taheri: (Same as input of S-N curve module)
                  Aggregated test results with 6 distinct columns:
                </p>
                <ul>
                  <li>Stress ratio (R) [-]</li>
                  <li>Reliability level (input parameter)</li>
                  <li>Stress level no. (estimated)</li>
                  <li>Stress parameter [MPa]</li>
                  <li>Number of cycles</li>
                  <li>Residual strength [MPa]</li>
                </ul>
                <br />
                <p>
                  Fawaz-Ellyin, FTPF, Hashim-Rothem, Sims-Brogdon: (Same as
                  output of S-N curve module)
                </p>
                <ul>
                  <li>Stress ratio (R) [-]</li>
                  <li>Number of cycles to failure</li>
                  <li>Stress parameter (Stress at failure) [MPa]</li>
                </ul>
                <br />
                <p>
                  Additionally to these 3 columns, there are 11 values required
                  for analysis:
                </p>
                <ul>
                  <li>R R-ratio</li>
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
          <v-file-input
            v-model="yFile"
            show-size
            accept=".json"
            :error-messages="errorMessages"
            :disabled="loading"
            label="Y file"
            @change="updateOutput"
          ></v-file-input>
        </v-col>
        <v-col>
          <v-file-input
            v-model="fFile"
            show-size
            accept=".json"
            :error-messages="errorMessages"
            :disabled="loading"
            label="F file"
            @change="updateOutput"
          ></v-file-input>
        </v-col>
      </v-row>
      <v-row>
        <v-col>
          <v-select
            v-model="method"
            :items="methods"
            label="Select Method"
            :disabled="loading"
            @change="updateOutput"
          ></v-select>
        </v-col>
        <v-col>
          <v-select
            v-model="snModel"
            :items="snModels"
            label="SN Model"
            :disabled="loading"
            @change="updateOutput"
          ></v-select>
        </v-col>
        <v-col>
          <v-text-field
            v-model.number="desirableAngle"
            label="Desirable Angle"
            type="number"
          ></v-text-field>
        </v-col>
        <v-col>
          <v-text-field
            v-model.number="offAxisAngle"
            label="Off Axis Angle"
            type="number"
          ></v-text-field>
        </v-col>
      </v-row>
    </v-card-subtitle>
    <v-card-text>
      <v-img src="/img/results_dashboard3.png"></v-img>
    </v-card-text>
    <v-card-actions v-if="hasInput" class="justify-end">
      <v-btn v-on:click="downloadOutput" :disabled="loading && output != null">
        Download
      </v-btn>
    </v-card-actions>
  </v-card>
</template>

<script>
import InfoTooltip from "@/components/InfoTooltip";
import download from "downloadjs";

export default {
  name: "FatigueFailure",
  components: {
    InfoTooltip,
  },
  data() {
    return {
      xFile: null,
      yFile: null,
      fFile: null,
      loading: false,
      output: null,
      methods: ["FTPT"],
      method: "FTPT",
      snModels: ["Lin-Log", "Log-Log"],
      snModel: "Log-Log",
      desirableAngle: 30,
      offAxisAngle: 160,
      errorMessages: null,
    };
  },
  computed: {
    hasInput: function () {
      return this.xFile != null && this.yFile != null && this.fFile != null;
    },
  },
  methods: {
    updateOutput() {
      if (this.hasInput) {
        this.loading = true;
        this.$analysisApi
          .runFatigueFailureFileAnalysisFatigueFailureFilePost(
            this.method,
            this.snModel,
            this.desirableAngle,
            this.offAxisAngle,
            this.xFile,
            this.yFile,
            this.fFile
          )
          .then((data) => {
            this.output = data;
            this.errorMessages = null;
          })
          .catch(() => {
            this.errorMessages = "Invalid input";
          })
          .finally(() => (this.loading = false));
      }
    },
    downloadOutput() {
      if (this.output) {
        download(
          this.output,
          `fatigue-failure-${this.method}-output.txt`,
          "text/plain"
        );
      }
    },
  },
};
</script>

<template>
  <v-card :loading="loading">
    <v-card-title>
      Damage Summation (uniaxial)
      <v-spacer />
      <info-tooltip>
        The damage summation module is based on the Palgrem-Miner linear damage
        rule; it allows us to define failure in a numerical fashion. This method
        is particularly interesting when dealing with varying amplitude loading,
        as it can be tedious to keep track of the damage inflicted on the sample
        for each loading/unloading cycle.
      </info-tooltip>
    </v-card-title>
    <v-card-subtitle>
      <v-row align="end">
        <v-col>
          <v-file-input
            v-model="sncFile"
            show-size
            accept=".csv"
            :error-messages="errorMessages"
            :disabled="loading"
            label="SNC file"
            @change="updateOutput"
          >
            <template v-slot:append>
              <info-tooltip>
                The data used as input for this method is slightly different
                than that of the previous modules. As damage summation deals
                with the fatigue life as a whole, we use the results from the
                previous analysis. We first browse for the load spectrum which
                is presented as a txt file with one numerical column
                representing the peaks and valleys of the loading. We then
                select the method we will use for counting the cycles, and
                finally choose the appropriate methods for CLD and for S-N
                curves/fatigue failure depending on whether we are in presence
                of a uniaxial or multiaxial loading.
              </info-tooltip>
            </template>
          </v-file-input>
        </v-col>
        <v-col>
          <v-file-input
            v-model="cycFile"
            show-size
            accept=".csv"
            :error-messages="errorMessages"
            :disabled="loading"
            label="CYC file"
            @change="updateOutput"
          ></v-file-input>
        </v-col>
        <v-col>
          <v-select
            v-model="method"
            :items="methods"
            label="Select Method"
            :disabled="loading"
            @change="updateOutput"
          ></v-select>
        </v-col>
      </v-row>
    </v-card-subtitle>
    <v-card-text>
      <v-img src="/img/results_dashboard4.png"></v-img>
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
import DamageSummationMethod from "@/backend/model/DamageSummationMethod";

const methods = Object.values(new DamageSummationMethod());

export default {
  name: "DamageSummation",
  components: {
    InfoTooltip,
  },
  data() {
    return {
      sncFile: null,
      cycFile: null,
      loading: false,
      output: null,
      methods: methods,
      method: methods[0],
      errorMessages: null,
    };
  },
  computed: {
    hasInput: function () {
      return this.sncFile != null && this.cycFile != null;
    },
  },
  methods: {
    updateOutput() {
      if (this.hasInput) {
        this.loading = true;
        this.$analysisApi
          .runDamageSummationFile(this.method, this.sncFile, this.cycFile)
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
          `damage-summation-${this.method}-output.txt`,
          "text/plain"
        );
      }
    },
  },
};
</script>

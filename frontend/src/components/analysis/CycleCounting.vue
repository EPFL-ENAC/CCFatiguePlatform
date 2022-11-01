<template>
  <v-card :loading="loading">
    <v-card-title>
      Cycle Counting
      <v-spacer />
      <info-tooltip>TODO</info-tooltip>
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
            <template v-slot:append>
              <info-tooltip>TODO</info-tooltip>
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
import CycleCountingMethod from "@/backend/model/CycleCountingMethod";

const methods = Object.values(new CycleCountingMethod());

export default {
  name: "CycleCounting",
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
          .runCycleCountingFile(this.method, this.file)
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
        download(
          this.output,
          `cycle-counting-${this.method}-output.txt`,
          "text/plain"
        );
      }
    },
  },
};
</script>

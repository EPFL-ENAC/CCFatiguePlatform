<template>
  <v-container>
    <v-card>
      <v-card-title>Dataset checker</v-card-title>
      <v-card-text>
        <v-file-input
          v-model="experimentZip.file"
          chips
          show-size
          accept=".zip"
          label="Upload experiment ZIP"
          :disabled="experimentZip.loading"
          :loading="experimentZip.loading"
          color="secondary"
          @change="changeExperimentZip"
        />
        <template v-if="experimentCheckResult.success !== null">
          <template v-if="experimentCheckResult.success">
            <v-alert type="success">
              Dataset validation passed <br />
              {{ countWarnings }} {{ "warning" | pluralize(countWarnings) }}.
              <br />
              You can send your dataset for integration
              <a
                href="https://github.com/EPFL-ENAC/CCFatiguePlatform/issues/new?assignees=sbancal&labels=Dataset%2Ctriage&template=dataset_integration_request.yml&title=%5BNew+Dataset%5D+%3A+%7B3LettersDataCode%7D_%7BResearcher%27s+lastname%7D_%7BDate%7D_%7BTest+type%7D"
              >
                here
              </a>
              .
            </v-alert>
          </template>
          <template v-else>
            <v-alert type="error">
              Dataset validation failed<br />
              {{ countWarnings }} {{ "warning" | pluralize(countWarnings) }},
              {{ countErrors }} {{ "error" | pluralize(countErrors) }}. <br />
              Please fix it according to the
              <a
                href="https://github.com/EPFL-ENAC/CCFatiguePlatform/tree/develop/Data"
              >
                Data convention
              </a>
              and test it here again
            </v-alert>
          </template>
          <div class="caption output">
            <div
              v-for="(linesEntry, index) in experimentCheckResult.output"
              :key="index"
              :class="outputClasses[linesEntry.prefix]"
            >
              <div v-for="(line, index2) in linesEntry.lines" :key="index2">
                <span v-for="n in linesEntry.indent" :key="n">
                  &nbsp;&nbsp;&nbsp;
                </span>
                {{ linesEntry.prefix }}{{ line }}
              </div>
            </div>
          </div>
        </template>
      </v-card-text>
    </v-card>
  </v-container>
</template>

<script>
const axios = require("axios");
export default {
  name: "DataUpload",
  data() {
    return {
      experimentZip: {
        file: null,
        loading: false,
      },
      experimentCheckResult: {
        output: [],
        success: null,
      },
      outputClasses: {
        "": "grey lighten-4",
        "(i): ": "grey lighten-4",
        "Warning: ": "warning white--text",
        "ERROR: ": "error white--text",
      },
    };
  },
  computed: {
    countWarnings() {
      return this.experimentCheckResult.output.filter(
        (linesEntry) => linesEntry.prefix === "Warning: "
      ).length;
    },
    countErrors() {
      return this.experimentCheckResult.output.filter(
        (linesEntry) => linesEntry.prefix === "ERROR: "
      ).length;
    },
  },
  methods: {
    changeExperimentZip() {
      this.experimentCheckResult = {
        output: [],
        success: null,
      };
      if (this.experimentZip.file === null) {
        return false;
      }
      this.experimentZip.loading = true;

      const formData = new FormData();
      formData.append("file", this.experimentZip.file);
      axios
        .post(
          `${this.$experimentsApi.apiClient.basePath}/experiments/data_preprocess_check`,
          formData,
          {
            headers: {
              "Content-Type": "multipart/form-data",
            },
          }
        )
        .then((response) => {
          if (response.data instanceof Object) {
            return response.data;
          } else {
            throw new Error(
              "Error parsing data received from " + response.config.url
            );
          }
        })
        .then((data) => {
          this.experimentCheckResult = {
            output: data.output,
            success: data.success,
          };
          this.experimentZip.loading = false;
        })
        .catch((error) => {
          console.log("Error", { error });
        });
      return true;
    },
  },
};
</script>

<style scoped>
a {
  color: inherit;
}
.output {
  height: 30vh;
  overflow-y: scroll;
}
</style>

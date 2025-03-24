<template>
  <v-container>
    <v-card>
      <v-card-title>Upload Guidelines</v-card-title>
      <v-card-text>
        <v-btn color="primary" outlined @click="showPdf = !showPdf">
          {{ showPdf ? "Hide PDF" : "Open PDF" }}
        </v-btn>

        <div v-if="showPdf" class="mt-4">
          <iframe
            :src="pdfURL"
            width="100%"
            height="500px"
            style="border: none"
          ></iframe>
        </div>
      </v-card-text>
    </v-card>
    <!-- Card con bottone per aprire il PDF in un'altra scheda
    <v-card>
    <v-card-title>Dataset Guidelines</v-card-title>
    <v-card-text>
    <v-btn :href="pdfURL" target="_blank" color="primary" outlined>
    Apri PDF
    </v-btn>
    </v-card-text>
    </v-card>
    -->
    <!-- File Download Dropdown -->
    <v-card class="mt-4">
      <v-card-title>Select the experimental campaign type</v-card-title>
      <v-card-text>
        <v-select
          v-model="selectedFile"
          :items="fileOptions"
          label="Choose a file"
          dense
          outlined
        ></v-select>
        <v-btn
          :disabled="!selectedFile"
          color="primary"
          class="mt-2"
          @click="downloadFile"
        >
          Download ZIP
        </v-btn>
      </v-card-text>
    </v-card>

    <!-- Dataset checker -->
    <v-card class="mt-4">
      <v-card-title>Dataset checker</v-card-title>
      <v-card-text>
        <!--  
        <p>
          Refer to the
          <a :href="TSTDataConventionURL">TST Data Convention</a>
          to prepare your Dataset.
        </p>
        -->
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
              <v-btn
                href="https://github.com/EPFL-ENAC/CCFatiguePlatform/issues/new?assignees=sbancal&labels=Dataset%2Ctriage&template=dataset_integration_request.yml&title=%5BNew+Dataset%5D+%3A+%7B3LettersDataCode%7D_%7BResearcher%27s+lastname%7D_%7BDate%7D_%7BTest+type%7D"
                outlined
                small
              >
                here
              </v-btn>
              .
            </v-alert>
          </template>
          <template v-else>
            <v-alert type="error">
              Dataset validation failed<br />
              {{ countWarnings }} {{ "warning" | pluralize(countWarnings) }},
              {{ countErrors }} {{ "error" | pluralize(countErrors) }}. <br />
              Please fix it according to the
              <a :href="TSTDataConventionURL">TST Data convention</a>
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
import axios from "axios";

export default {
  name: "DataUpload",
  data() {
    return {
      showPdf: false,
      pdfURL: "/downloads/Upload_guide.pdf", // Assicurati che il file sia in public/downloads/

      TSTDataConventionURL:
        "https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/main/Data/TST_Data_Convention.md",
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
      selectedFile: null,
      fileOptions: [
        { text: "Quasi static", value: "/downloads/QuasiStatic.zip" },
        {
          text: "Quasi static with fracture",
          value: "/downloads/QuasiStatic_fracture.zip",
        },
        { text: "Fatigue", value: "/downloads/Fatigue.zip" },
        {
          text: "Fatigue with fracture",
          value: "/downloads/Fatigue_fracture.zip",
        },
      ],
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
    downloadFile() {
      if (!this.selectedFile) return;

      const link = document.createElement("a");
      link.href = this.selectedFile;
      link.setAttribute("download", this.selectedFile.split("/").pop());
      document.body.appendChild(link);
      link.click();
      document.body.removeChild(link);
    },
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
          this.experimentZip.loading = false;
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
iframe {
  border-radius: 6px;
}
</style>

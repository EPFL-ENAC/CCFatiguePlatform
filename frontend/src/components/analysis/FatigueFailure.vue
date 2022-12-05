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
            label="Longitudinal fatigue data (SNC json file)"
            @change="updateOutput"
          >
            <template #append>
              <info-tooltip>
                Longitudinal fatigue data.<br />Format: SNC json file.<br />See
                the
                <a
                  href="https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/Data/SNC_Data_Convention.md"
                  >SNC Data Convention</a
                >
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
            label="Transverse fatigue data. (SNC json file)"
            @change="updateOutput"
          >
            <template #append>
              <info-tooltip>
                Transverse fatigue data.<br />Format: SNC json file.<br />See
                the
                <a
                  href="https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/Data/SNC_Data_Convention.md"
                  >SNC Data Convention</a
                >
              </info-tooltip>
            </template>
          </v-file-input>
        </v-col>
        <v-col>
          <v-file-input
            v-model="fFile"
            show-size
            accept=".json"
            :error-messages="errorMessages"
            :disabled="loading"
            label="Shear or off-axis fatigue data (SNC json file)"
            @change="updateOutput"
            ><template #append>
              <info-tooltip>
                Shear or off-axis fatigue data.<br />Format: SNC json file.<br />See
                the
                <a
                  href="https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/Data/SNC_Data_Convention.md"
                  >SNC Data Convention</a
                >
              </info-tooltip>
            </template>
          </v-file-input>
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
    <v-card-text v-if="series.length > 0">
      <simple-chart
        :aspect-ratio="2"
        :series="series"
        x-axis-name="N"
        y-axis-name="Maximum Cyclic Stress [MPa]"
      ></simple-chart>
    </v-card-text>
    <v-card-actions v-if="hasInput" class="justify-end">
      <v-btn :disabled="loading && output != null" @click="downloadOutput">
        Download FAF
        <info-tooltip>
          See the
          <a
            href="https://github.com/EPFL-ENAC/CCFatiguePlatform/blob/develop/Data/FAF_Data_Convention.md"
            >FAF Data Convention</a
          >
        </info-tooltip>
      </v-btn>
    </v-card-actions>
  </v-card>
</template>

<script>
import FatigueFailureMethod from "@/backend/model/FatigueFailureMethod";
import SimpleChart from "@/components/charts/SimpleChart";
import InfoTooltip from "@/components/InfoTooltip";
import { getOutputFileName } from "@/utils/analysis";
import { parserConfig } from "@/utils/papaparse";
import download from "downloadjs";
import { parse } from "papaparse";

const methods = Object.values(new FatigueFailureMethod());

export default {
  name: "FatigueFailure",
  components: {
    InfoTooltip,
    SimpleChart,
  },
  data() {
    return {
      xFile: null,
      yFile: null,
      fFile: null,
      loading: false,
      output: null,
      methods: methods,
      method: methods[0],
      snModels: ["Lin-Log", "Log-Log"],
      snModel: "Log-Log",
      desirableAngle: 30,
      offAxisAngle: 160,
      series: [],
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
          .runFatigueFailureFile(
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
            const results = parse(data.csv_data, parserConfig);
            this.series = [
              {
                type: "line",
                name: this.method,
                data: results.data.map((item) => [
                  item.cycles_to_failure,
                  item.stress_max,
                ]),
              },
            ];
            this.errorMessages = null;
          })
          .catch(() => {
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
          "FAF",
          this.xFile.name,
          this.method
        );
        download(this.output.csv_data, outputName + ".csv", "text/csv");
        download(
          this.output.json_data,
          outputName + ".json",
          "application/json"
        );
      }
    },
  },
};
</script>

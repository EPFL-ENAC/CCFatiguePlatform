<template>
  <v-container>
    <v-row>
      <v-col>
        <experiment-specifications :experiment="displayMetadata" />
      </v-col>
    </v-row>

    <v-row>
      <v-col cols="auto">
        <h2>Test results</h2>
      </v-col>
      <v-spacer />
      <v-col cols="auto">
        <v-btn @click="goBack">Add test(s)</v-btn>
      </v-col>
    </v-row>

    <!-- Fatigue branch -->
    <v-row v-if="experimentType === 'FA'">
      <v-col cols="10">
        <v-row>
          <v-col cols="6">
            <v-card :loading="loading">
              <v-card-title>
                Stress - Strain
                <info-tooltip>
                  This graph shows a selection of loading/unloading loops...
                </info-tooltip>
              </v-card-title>
              <v-card-text>
                <simple-chart
                  :series="stressStrainSeries"
                  :aspect-ratio="2"
                  x-axis-name="Strain [-]"
                  y-axis-name="Stress [MPa]"
                />
              </v-card-text>
            </v-card>
          </v-col>
          <v-col cols="6">
            <v-card :loading="loading">
              <v-card-title>
                Hysteresis loop area evolution
                <info-tooltip>
                  On this graph, we show the evolution of the hysteresis area...
                </info-tooltip>
              </v-card-title>
              <v-card-text>
                <simple-chart
                  :series="hysteresisAreaSeries"
                  :aspect-ratio="2"
                  x-axis-name="Number of cycles [-]"
                  y-axis-name="Hysteresis area [N/mm²]"
                />
              </v-card-text>
            </v-card>
          </v-col>
          <v-col cols="6">
            <v-card :loading="loading">
              <v-card-title>
                Creep evolution
                <info-tooltip>
                  Creep is defined as the average deformation during each
                  cycle...
                </info-tooltip>
              </v-card-title>
              <v-card-text>
                <simple-chart
                  :series="creepSeries"
                  :aspect-ratio="2"
                  x-axis-name="Number of cycles [-]"
                  y-axis-name="Creep [-]"
                />
              </v-card-text>
            </v-card>
          </v-col>
          <v-col cols="6">
            <v-card :loading="loading">
              <v-card-title>
                Stiffness evolution under cyclic loading
                <info-tooltip>
                  Stiffness is representative of the resistance an object
                  opposes...
                </info-tooltip>
              </v-card-title>
              <v-card-text>
                <simple-chart
                  :series="stiffnessSeries"
                  :aspect-ratio="2"
                  x-axis-name="Number of cycles [-]"
                  y-axis-name="Stiffness [N/mm²]"
                />
              </v-card-text>
            </v-card>
          </v-col>
        </v-row>
      </v-col>
      <v-col cols="2">
        <v-card :loading="loading">
          <v-card-text>
            <ul>
              <li>
                <experiment-s-v
                  subject="Specimen number"
                  :values="specimenIds"
                  :colors="valueColors"
                  value-type="bigNumber"
                />
              </li>
              <li>
                <experiment-s-v
                  subject="Stress at failure"
                  :values="formattedStressAtFailure"
                  :colors="valueColors"
                  :unit="units.stress"
                  tooltip="σ_fail is the stress level that induced failure..."
                />
              </li>
              <li>
                <experiment-s-v
                  subject="Strain at failure"
                  :values="formattedStrainAtFailure"
                  :colors="valueColors"
                  unit="%"
                  tooltip="ε_fail is the deformation at the time of failure..."
                />
              </li>
              <li>
                <experiment-s-v
                  subject="Cycle at failure"
                  :values="cycleAtFailure"
                  :colors="valueColors"
                  value-type="bigNumber"
                  tooltip="Number of cycles to failure."
                />
              </li>
              <li>
                <experiment-s-v
                  subject="Run out"
                  :values="runOuts"
                  :colors="valueColors"
                  tooltip="No fatigue failure."
                />
              </li>
              <!--
              <li>
                <experiment-s-v
                  subject="R ratio"
                  :values="stressRatios"
                  :colors="valueColors"
                  tooltip="Stress ratio (σ_min/σ_max)."
                />
              </li>
              -->
              <li>
                <experiment-s-v
                  subject="Total dissipated energy (TDE)"
                  :values="formattedTotalDissipatedEnergies"
                  :colors="valueColors"
                  tooltip="Sum of all hysteresis areas."
                />
              </li>
            </ul>
            <!-- warning messages -->
            <v-alert
              v-if="hasWarnings"
              type="warning"
              dense
              outlined
              class="mt-4"
            >
              Smooth spikes and drop function has been activated!
            </v-alert>
          </v-card-text>
        </v-card>
      </v-col>
    </v-row>

    <!-- Quasi‐static branch -->
    <v-row v-else>
      <v-col v-if="crackSeries.length" cols="6">
        <v-card :loading="loading">
          <v-card-title>Crack Load vs Crack Displacement</v-card-title>
          <v-card-text>
            <double-chart
              :series="crackSeries"
              :aspect-ratio="2"
              x-axis-name="Crack Displacement [mm]"
              :y1-axis-name="'Crack Load — [N]'"
              :y2-axis-name="'Crack Length • [mm]'"
            />
          </v-card-text>
        </v-card>
      </v-col>

      <v-col v-if="loadOptions.length && displacementOptions.length" cols="6">
        <v-card :loading="loading">
          <v-card-title>Load vs Displacement</v-card-title>
          <v-card-text>
            <v-row>
              <v-col>
                <v-select
                  v-model="loadOption"
                  :items="loadOptions"
                  :disabled="loadOptions.length < 2"
                  label="Load"
                />
              </v-col>
              <v-col>
                <v-select
                  v-model="displacementOption"
                  :items="displacementOptions"
                  :disabled="displacementOptions.length < 2"
                  label="Displacement"
                />
              </v-col>
            </v-row>
            <simple-chart
              :series="loadDisplacementSeries"
              :aspect-ratio="2"
              x-axis-name="Machine Displacement [mm]"
              y-axis-name="Machine Load [N]"
            />
          </v-card-text>
        </v-card>
      </v-col>

      <v-col v-if="strainOptions.length && stressOptions.length" cols="6">
        <v-card :loading="loading">
          <v-card-title>Strain vs Stress</v-card-title>
          <v-card-text>
            <v-row>
              <v-col>
                <v-select
                  v-model="strainOption"
                  :items="strainOptions"
                  :disabled="strainOptions.length < 2"
                  label="Strain"
                />
              </v-col>
              <v-col>
                <v-select
                  v-model="stressOption"
                  :items="stressOptions"
                  :disabled="stressOptions.length < 2"
                  label="Stress"
                />
              </v-col>
            </v-row>
            <simple-chart
              :series="strainStressSeries"
              :aspect-ratio="2"
              x-axis-name="Strain [-]"
              y-axis-name="Stress [MPa]"
            />
          </v-card-text>
        </v-card>
      </v-col>

      <v-col cols="2">
        <v-card :loading="loading">
          <v-card-text>
            <ul>
              <li>
                <experiment-s-v
                  subject="Specimen number"
                  :values="specimenIds"
                  :colors="valueColors"
                  value-type="bigNumber"
                />
              </li>
              <li>
                <experiment-s-v
                  subject="Max stress"
                  :values="formattedStressAtFailure"
                  :colors="valueColors"
                  :unit="units.stress"
                  tooltip="Maximum stress recorded in the test."
                />
              </li>
              <li>
                <experiment-s-v
                  subject="Max strain"
                  :values="formattedStrainAtFailure"
                  :colors="valueColors"
                  unit="%"
                  tooltip="Maximum strain recorded in the test."
                />
              </li>
              <li>
                <experiment-s-v
                  subject="Toughness"
                  :values="formattedToughnessValues"
                  :colors="valueColors"
                  :unit="'N/mm²'"
                  tooltip="Area under the stress-strain curve..."
                />
              </li>
            </ul>
          </v-card-text>
        </v-card>
      </v-col>
    </v-row>
  </v-container>
</template>

<script>
import DoubleChart from "@/components/charts/DoubleChart.vue";
import SimpleChart from "@/components/charts/SimpleChart.vue";
import ExperimentSpecifications from "@/components/ExperimentSpecifications.vue";
import ExperimentSV from "@/components/ExperimentSV.vue";
import InfoTooltip from "@/components/InfoTooltip.vue";
import { colorPalette } from "@/utils/style";
import { zip } from "lodash";
import { mapState } from "vuex";

export default {
  name: "TestsDashboard",
  components: {
    DoubleChart,
    SimpleChart,
    ExperimentSpecifications,
    ExperimentSV,
    InfoTooltip,
  },
  props: {
    experimentId: { type: Number, required: true },
    testIds: { type: Array, required: true },
  },
  data() {
    return {
      loading: false,
      colors: colorPalette,
      cycleAtFailure: [],
      stressAtFailure: [],
      strainAtFailure: [],
      specimenIds: [],
      totalDissipatedEnergies: [],
      runOuts: [],
      stressRatios: [],
      stressStrainSeries: [],
      hysteresisAreaSeries: [],
      creepSeries: [],
      stiffnessSeries: [],
      crackSeries: [],
      loadData: {},
      loadOptions: [],
      loadOption: null,
      displacementData: {},
      displacementOptions: [],
      displacementOption: null,
      strainData: {},
      strainOptions: [],
      strainOption: null,
      stressData: {},
      stressOptions: [],
      stressOption: null,
      experimentMetadata: {},
      toughnessValues: [],
      fatigueWarnings: [],
    };
  },
  computed: {
    ...mapState("experiments", {
      experiment: "oneExperiment",
      units: "units",
    }),
    experimentType() {
      return this.experiment.experiment.experiment_type;
    },
    displayMetadata() {
      if (this.experimentType === "QS" && this.experimentMetadata) {
        return {
          ...this.experiment.experiment,
          ...this.experimentMetadata,
        };
      }
      return this.experiment.experiment;
    },
    loadDisplacementSeries() {
      return this.testIds.map((id) => ({
        type: "line",
        name: this.specimenName[id],
        data: zip(
          this.displacementData[id]?.[this.displacementOption] || [],
          this.loadData[id]?.[this.loadOption] || []
        ),
      }));
    },
    strainStressSeries() {
      return this.testIds
        .map((id) => ({
          type: "line",
          name: this.specimenName[id],
          data: zip(
            this.strainData[id]?.[this.strainOption] || [],
            this.stressData[id]?.[this.stressOption] || []
          ),
        }))
        .filter((s) => s.data.length);
    },
    valueColors() {
      return this.testIds.map((_, i) => this.colors[i % this.colors.length]);
    },
    formattedStressAtFailure() {
      return this.stressAtFailure.map((s) =>
        s != null ? Number(s).toFixed(2) : "-"
      );
    },
    formattedStrainAtFailure() {
      return this.strainAtFailure.map((e) =>
        e != null ? Number(e).toFixed(4) : "-"
      );
    },
    formattedToughnessValues() {
      return this.toughnessValues.map((t) =>
        t != null ? Number(t).toFixed(2) : "-"
      );
    },
    formattedTotalDissipatedEnergies() {
      return this.totalDissipatedEnergies.map((e) =>
        e != null ? Number(e).toFixed(4) : "-"
      );
    },
    hasWarnings() {
      return this.testIds.some((_, i) => this.fatigueWarnings?.[i]);
    },
  },
  watch: {
    experimentType: {
      immediate: true,
      handler(val) {
        if (!val) return;
        this.loading = true;
        if (val === "FA") {
          this.loadFatigue();
        } else if (val === "QS") {
          this.loadQuasiStatic();
        }
      },
    },
  },
  created() {
    this.$store.dispatch("experiments/fetchOneExperimentWithTests", {
      experimentId: this.experimentId,
      pagination: { page: 1, size: 20 },
    });
  },
  methods: {
    async loadFatigue() {
      const dataList = await Promise.all(
        this.testIds.map((tid) =>
          this.$experimentsApi.getFatigueTest(this.experimentId, tid)
        )
      );
      this.specimenName = {};
      this.cycleAtFailure = [];
      this.stressAtFailure = [];
      this.strainAtFailure = [];
      this.specimenIds = [];
      this.totalDissipatedEnergies = [];
      this.runOuts = [];
      this.stressRatios = [];
      this.stressStrainSeries = [];
      this.hysteresisAreaSeries = [];
      this.creepSeries = [];
      this.stiffnessSeries = [];

      dataList.forEach((d, i) => {
        const tid = this.testIds[i];
        this.specimenName[tid] = d.specimen_name;
        this.cycleAtFailure.push(d.n_fail);
        this.stressAtFailure.push(d.stress_at_failure);
        this.strainAtFailure.push(d.strain_at_failure);
        this.specimenIds.push(d.specimen_id);
        this.totalDissipatedEnergies.push(d.total_dissipated_energy);
        this.runOuts.push(d.run_out);
        this.stressRatios.push(d.stress_ratio);
        this.fatigueWarnings.push(d.warning_messages || false);

        d.hysteresis_loops.forEach((loop) =>
          this.stressStrainSeries.push({
            type: "line",
            name: d.specimen_name,
            data: zip(loop.strain, loop.stress),
          })
        );
        this.hysteresisAreaSeries.push({
          type: "line",
          name: d.specimen_name,
          data: zip(d.n_cycles, d.hysteresis_area),
        });
        this.creepSeries.push({
          type: "line",
          name: d.specimen_name,
          data: zip(d.n_cycles, d.creep),
        });
        this.stiffnessSeries.push({
          type: "line",
          name: d.specimen_name,
          data: zip(d.n_cycles, d.stiffness),
        });
      });

      this.loading = false;
    },

    async loadQuasiStatic() {
      const dataList = await Promise.all(
        this.testIds.map((tid) =>
          this.$experimentsApi.getQuasiStaticTest(this.experimentId, tid)
        )
      );
      this.experimentMetadata = dataList[0].experiment_metadata;

      this.specimenName = {};
      this.crackSeries = [];
      this.loadData = {};
      this.loadOptions = new Set();
      this.displacementData = {};
      this.displacementOptions = new Set();
      this.strainData = {};
      this.strainOptions = new Set();
      this.stressData = {};
      this.stressOptions = new Set();
      this.specimenIds = [];
      this.stressAtFailure = [];
      this.strainAtFailure = [];
      this.toughnessValues = []; // new

      dataList.forEach((d, i) => {
        const tid = this.testIds[i];
        this.specimenName[tid] = d.specimen_name;
        console.log("🧪 Quasi-static specimen ID:", d.specimen_id);
        if (d.crack_displacement.length) {
          this.crackSeries.push({
            type: "line",
            name: d.specimen_name,
            data: zip(d.crack_displacement, d.crack_load),
          });
          this.crackSeries.push({
            type: "scatter",
            name: d.specimen_name,
            yAxisIndex: 1,
            symbolSize: 6,
            data: zip(d.crack_displacement, d.crack_length),
          });
        }

        Object.keys(d.load).forEach((k) => this.loadOptions.add(k));
        Object.keys(d.displacement).forEach((k) =>
          this.displacementOptions.add(k)
        );
        this.loadData[tid] = d.load;
        this.displacementData[tid] = d.displacement;

        Object.keys(d.strain).forEach((k) => this.strainOptions.add(k));
        Object.keys(d.stress).forEach((k) => this.stressOptions.add(k));
        this.strainData[tid] = d.strain;
        this.stressData[tid] = d.stress;
        if (typeof d.specimen_id !== "undefined") {
          this.specimenIds.push(String(d.specimen_id));
        } else {
          console.warn("⚠️ specimen_id mancante per test", tid);
          this.specimenIds.push("–"); // oppure "N/A" per chiarezza visiva
        }
        const stressValues = Object.values(d.stress)
          .flat()
          .filter(Number.isFinite);
        const strainValues = Object.values(d.strain)
          .flat()
          .filter(Number.isFinite);
        this.stressAtFailure.push(Math.max(...stressValues));
        this.strainAtFailure.push(Math.max(...strainValues));
        this.toughnessValues.push(
          isFinite(d.toughness) ? Number(d.toughness) : null
        );
      });

      this.loadOptions = Array.from(this.loadOptions);
      this.loadOption = this.loadOptions[0] || null;
      this.displacementOptions = Array.from(this.displacementOptions);
      this.displacementOption = this.displacementOptions[0] || null;
      this.strainOptions = Array.from(this.strainOptions);
      this.strainOption = this.strainOptions[0] || null;
      this.stressOptions = Array.from(this.stressOptions);
      this.stressOption = this.stressOptions[0] || null;

      this.loading = false;
    },

    goBack() {
      this.$router.go(-1);
    },
  },
};
</script>

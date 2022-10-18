<template>
  <v-container>
    <v-row>
      <v-col>
        <experiment-specifications :experiment="experiment.experiment" />
      </v-col>
    </v-row>
    <v-row>
      <v-col cols="auto">
        <h2>Test results</h2>
      </v-col>
      <v-spacer></v-spacer>
      <v-col cols="auto">
        <v-btn @click="goBack">Add test(s)</v-btn>
      </v-col>
    </v-row>
    <v-row v-if="experimentType == 'FA'">
      <v-col cols="10">
        <v-row>
          <v-col cols="6">
            <v-card :loading="loading">
              <v-card-title>
                Stress - Strain
                <info-tooltip>
                  This graph shows a selection of loading/unloading loops. These
                  loops, also known as hysteresis loops are important because
                  they provide a visual representation of what is being plotted
                  in the next three graphs.
                </info-tooltip>
              </v-card-title>
              <v-card-text>
                <simple-chart
                  :series="stressStrainSeries"
                  :aspect-ratio="2"
                  x-axis-name="Strain"
                  y-axis-name="Stress"
                ></simple-chart>
              </v-card-text>
            </v-card>
          </v-col>
          <v-col cols="6">
            <v-card :loading="loading">
              <v-card-title>
                Hysteresis loop area evolution
                <info-tooltip>
                  On this graph, we show the evolution of the hysteresis area.
                  This value is defined as the area contained within a
                  hysteresis loop. For visual representation, it represents the
                  area defined by each closed loop on the (stress-strain) plane
                  in graph 1.
                </info-tooltip>
              </v-card-title>
              <v-card-text>
                <simple-chart
                  :series="hysteresisAreaSeries"
                  :aspect-ratio="2"
                  x-axis-name="Number of cycles"
                  y-axis-name="Hysteresis area"
                ></simple-chart>
              </v-card-text>
            </v-card>
          </v-col>
          <v-col cols="6">
            <v-card :loading="loading">
              <v-card-title>
                Creep evolution
                <info-tooltip>
                  Creep is defined as the average deformation during each
                  loading/unloading cycle. It gives an understanding of how much
                  deformation occurs at each cycle.
                </info-tooltip>
              </v-card-title>
              <v-card-text>
                <simple-chart
                  :series="creepSeries"
                  :aspect-ratio="2"
                  x-axis-name="Number of cycles"
                  y-axis-name="Creep"
                ></simple-chart>
              </v-card-text>
            </v-card>
          </v-col>
          <v-col cols="6">
            <v-card :loading="loading">
              <v-card-title>
                Stiffness evolution under cyclic loading
                <info-tooltip>
                  Stiffness is representative of the resistance an object
                  opposes to an applied force. On this graph, we show how this
                  capacity evolves over a fatigue life cycle.
                </info-tooltip>
              </v-card-title>
              <v-card-text>
                <simple-chart
                  :series="stiffnessSeries"
                  :aspect-ratio="2"
                  x-axis-name="Number of cycles"
                  y-axis-name="Stiffness"
                ></simple-chart>
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
                  :colors="colors"
                  subject="Specimen number"
                  :values="specimenIds"
                  valueType="bigNumber"
                />
              </li>
              <li>
                <experiment-s-v
                  :colors="colors"
                  subject="Stress at failure"
                  :values="specimenIds.map((t) => '-')"
                  :unit="units.stress"
                  tooltip="σ_fail is defined as the stress level that induced failure from the tested specimen and is measured in [MPa]"
                />
              </li>
              <li>
                <experiment-s-v
                  :colors="colors"
                  subject="Strain at failure"
                  :values="specimenIds.map((t) => '-')"
                  unit="%"
                  tooltip="ε_fail is defined as the deformation at the time of failure and is measured in [%]"
                />
              </li>
              <li>
                <experiment-s-v
                  :colors="colors"
                  subject="N_cycles"
                  :values="specimenIds.map((t) => '-')"
                  valueType="bigNumber"
                  tooltip="defined as the number of cycles to failure [-]"
                />
              </li>
              <li>
                <experiment-s-v
                  :colors="colors"
                  subject="R"
                  :values="specimenIds.map((t) => '-')"
                  tooltip="defined as the stress ratio (σ_min/σ_max) [-] and has relevance in the context of constant amplitude experiments."
                />
              </li>
              <li>
                <experiment-s-v
                  :colors="colors"
                  subject="Total dissipated energy (TDE)"
                  :values="totalDissipatedEnergies"
                  valueType="bigNumber"
                  tooltip="defined as the sum of all the hysteresis areas over the course of an experiment. It gives a good measure of the amount of energy that has been dissipated in deformation and heat over the course of an experiment."
                />
              </li>
            </ul>
          </v-card-text>
        </v-card>
      </v-col>
    </v-row>
    <v-row v-else>
      <v-col cols="6" v-if="crackSeries.length > 0">
        <v-card :loading="loading">
          <v-card-title>Crack Load vs Crack Displacement</v-card-title>
          <v-card-text>
            <double-chart
              :series="crackSeries"
              :aspect-ratio="2"
              x-axis-name="Crack Displacement"
              :y1-axis-name="'Crack Load \u23AF\u23AF\u23AF'"
              :y2-axis-name="'Crack Length \u2022\u2022\u2022'"
            ></double-chart>
          </v-card-text>
        </v-card>
      </v-col>
      <v-col cols="6" v-if="loadDisplacementSeries.length > 0">
        <v-card :loading="loading">
          <v-card-title>Load vs Displacement</v-card-title>
          <v-card-text>
            <simple-chart
              :series="loadDisplacementSeries"
              :aspect-ratio="2"
              x-axis-name="Machine Displacement"
              y-axis-name="Machine Load"
            ></simple-chart>
          </v-card-text>
        </v-card>
      </v-col>
      <v-col cols="6" v-if="strainStressSeries.length > 0">
        <v-card :loading="loading">
          <v-card-title>Load vs Displacement</v-card-title>
          <v-card-text>
            <v-select
              v-model="strainOption"
              :items="strainOptions"
              label="Strain"
            ></v-select>
            <simple-chart
              :series="strainStressSeries"
              :aspect-ratio="2"
              x-axis-name="Strain"
              y-axis-name="Stress"
            ></simple-chart>
          </v-card-text>
        </v-card>
      </v-col>
    </v-row>
  </v-container>
</template>

<script>
import { mapState } from "vuex";
import InfoTooltip from "@/components/InfoTooltip";
import ExperimentSpecifications from "@/components/ExperimentSpecifications.vue";
import ExperimentSV from "@/components/ExperimentSV.vue";
import SimpleChart from "@/components/charts/SimpleChart.vue";
import { zip } from "lodash";
import { colorPalette } from "@/utils/style";
import DoubleChart from "@/components/charts/DoubleChart.vue";

export default {
  name: "TestsDashboard",
  components: {
    DoubleChart,
    ExperimentSpecifications,
    ExperimentSV,
    InfoTooltip,
    SimpleChart,
  },
  props: {
    experimentId: Number,
    testIds: Array[Number],
  },
  computed: {
    ...mapState("experiments", {
      experiment: "oneExperiment",
      units: "units",
    }),
    experimentType: function () {
      return this.experiment.experiment.experiment_type;
    },
    strainStressSeries: function () {
      return this.testIds
        .map((testId) => ({
          type: "line",
          name: `${testId}`,
          data: zip(
            this.strainOption ? this.strainData[testId][this.strainOption] : [],
            this.stressData[testId]
          ),
        }))
        .filter((line) => line.data.length > 0);
    },
  },
  watch: {
    experimentType: function (val) {
      switch (val) {
        case "FA":
          this.loading = true;
          Promise.all(
            this.testIds.map((testId) =>
              this.$experimentsApi.getFatigueTest(this.experimentId, testId)
            )
          )
            .then((dataList) => {
              this.specimenIds = dataList.map((data) => data.specimen_id);
              this.totalDissipatedEnergies = dataList.map(
                (data) => data.total_dissipated_energy
              );
              this.stressStrainSeries = [];
              this.hysteresisAreaSeries = [];
              this.creepSeries = [];
              this.stiffnessSeries = [];
              zip(this.testIds, dataList).forEach(([testId, data]) => {
                this.stressStrainSeries.push(
                  ...data.hysteresis_loops.map((loop) => ({
                    type: "line",
                    name: `${testId}`,
                    data: zip(loop.strain, loop.stress),
                  }))
                );
                this.hysteresisAreaSeries.push({
                  type: "line",
                  name: `${testId}`,
                  data: zip(data.n_cycles, data.hysteresis_area),
                });
                this.creepSeries.push({
                  type: "line",
                  name: `${testId}`,
                  data: zip(data.n_cycles, data.creep),
                });
                this.stiffnessSeries.push({
                  type: "line",
                  name: `${testId}`,
                  data: zip(data.n_cycles, data.stiffness),
                });
              });
            })
            .finally(() => (this.loading = false));
          break;
        case "QS":
          this.loading = true;
          Promise.all(
            this.testIds.map((testId) =>
              this.$experimentsApi.getQuasiStaticTest(this.experimentId, testId)
            )
          )
            .then((dataList) => {
              this.crackSeries = [];
              this.loadDisplacementSeries = [];
              this.stressStrainSeries = [];
              this.strainData = {};
              this.strainOptions = [];
              zip(this.testIds, dataList).forEach(([testId, data]) => {
                if (
                  data.crack_displacement.length > 0 &&
                  data.crack_load.length > 0 &&
                  data.crack_length.length > 0
                ) {
                  this.crackSeries.push({
                    type: "line",
                    name: `${testId}`,
                    data: zip(data.crack_displacement, data.crack_load),
                  });
                  this.crackSeries.push({
                    type: "scatter",
                    name: `${testId}`,
                    yAxisIndex: 1,
                    symbolSize: 6,
                    data: zip(data.crack_displacement, data.crack_length),
                  });
                }
                if (
                  data.machine_displacement.length > 0 &&
                  data.machine_load.length > 0
                ) {
                  this.loadDisplacementSeries.push({
                    type: "line",
                    name: `${testId}`,
                    data: zip(data.machine_displacement, data.machine_load),
                  });
                }
                if (
                  Object.keys(data.strain).length > 0 &&
                  data.stress.length > 0
                ) {
                  this.strainOptions = [
                    ...new Set([
                      ...this.strainOptions,
                      ...Object.keys(data.strain),
                    ]),
                  ];
                  this.strainData[testId] = data.strain;
                  this.stressData[testId] = data.stress;
                }
              });
              this.strainOption = this.strainOptions[0];
            })
            .finally(() => (this.loading = false));
          break;
        default:
          throw new Error(`unknown experiment type ${val}`);
      }
    },
  },
  data() {
    return {
      loading: false,
      colors: colorPalette,
      // FA
      stressStrainSeries: [],
      hysteresisAreaSeries: [],
      creepSeries: [],
      stiffnessSeries: [],
      specimenIds: [],
      totalDissipatedEnergies: [],
      // QS
      crackSeries: [],
      loadDisplacementSeries: [],
      strainData: {},
      strainOptions: [],
      strainOption: null,
      stressData: {},
    };
  },
  methods: {
    goBack() {
      this.$router.go(-1);
    },
  },
  created() {
    this.$store.dispatch("experiments/fetchOneExperimentWithTests", {
      experimentId: this.experimentId,
      pagination: {
        page: 1,
        size: 20,
      },
    });
  },
};
</script>

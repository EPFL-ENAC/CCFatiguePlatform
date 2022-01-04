<template>
  <div>
    <v-skeleton-loader v-if="!dataIsFetched" type="article" />
    <v-card v-else flat>
      <v-card-title>
        Experiment : {{ test.experiment["Experiment Type"] }} test, by
        {{ test.experiment.Researcher }}, {{ test.experiment.Laboratory }},
        {{ test.experiment.Experiment.Date }}
      </v-card-title>
      <v-card-subtitle>
        <experiment-specifications :experiment="test.experiment">
        </experiment-specifications>
      </v-card-subtitle>
      <v-card-text>
        <v-card>
          <v-card-title>
            Test results
            <v-spacer> </v-spacer>
            <v-btn :to="{ name: 'TestsSelection' }">Add test(s)</v-btn>
          </v-card-title>
          <v-card-text>
            <v-container fluid>
              <v-row>
                <v-col cols="10">
                  <v-row>
                    <v-col cols="6">
                      <v-row>
                        <info-tooltip>
                          This graph shows a selection of loading/unloading
                          loops. These loops, also known as hysteresis loops are
                          important because they provide a visual representation
                          of what is being plotted in the next three graphs.
                        </info-tooltip>
                        <div :id="id.bokeh.stressStrain" class="bokeh"></div>
                      </v-row>
                    </v-col>
                    <v-col cols="6">
                      <v-row>
                        <info-tooltip>
                          On this graph, we show the evolution of the hysteresis
                          area. This value is defined as the area contained
                          within a hysteresis loop. For visual representation,
                          it represents the area defined by each closed loop on
                          the (stress-strain) plane in graph 1.
                        </info-tooltip>
                        <div :id="id.bokeh.hysteresisArea" class="bokeh"></div>
                      </v-row>
                    </v-col>
                    <v-col cols="6">
                      <v-row>
                        <info-tooltip>
                          Creep is defined as the average deformation during
                          each loading/unloading cycle. It gives an
                          understanding of how much deformation occurs at each
                          cycle.
                        </info-tooltip>
                        <div :id="id.bokeh.creep" class="bokeh"></div>
                      </v-row>
                    </v-col>
                    <v-col cols="6">
                      <v-row>
                        <info-tooltip>
                          Stiffness is representative of the resistance an
                          object opposes to an applied force. On this graph, we
                          show how this capacity evolves over a fatigue life
                          cycle.
                        </info-tooltip>
                        <div :id="id.bokeh.stiffness" class="bokeh"></div>
                      </v-row>
                    </v-col>
                  </v-row>
                </v-col>
                <v-col cols="2">
                  <v-card>
                    <v-card-text>
                      <ul>
                        <li>
                          <experiment-s-v
                            subject="Specimen number"
                            :values="test.tests.map((t) => t.number)"
                            valueType="bigNumber"
                            :colors="test.tests.map((t) => t.color)"
                          />
                        </li>
                        <li>
                          <experiment-s-v
                            subject="Stress at failure"
                            :values="[
                              test.experiment.Experiment['Standard Fatigue'][
                                'Stress at Failure'
                              ],
                            ]"
                            :unit="test.experiment['Experiment Units'].Stress"
                            tooltip="σ_fail is defined as the stress level that induced failure from the tested specimen and is measured in [MPa]"
                          />
                        </li>
                        <li>
                          <experiment-s-v
                            subject="Strain at failure"
                            :values="[
                              test.experiment.Experiment['Standard Fatigue'][
                                'Strain at Failure'
                              ],
                            ]"
                            unit="%"
                            tooltip="ε_fail is defined as the deformation at the time of failure and is measured in [%]"
                          />
                        </li>
                        <li>
                          <experiment-s-v
                            subject="N_cycles"
                            :values="[
                              test.experiment.Experiment['Standard Fatigue'][
                                'Number of Cycles to Failure'
                              ],
                            ]"
                            valueType="bigNumber"
                            tooltip="defined as the number of cycles to failure [-]"
                          />
                        </li>
                        <li>
                          <experiment-s-v
                            subject="R"
                            :values="[
                              test.experiment.Experiment['Standard Fatigue'][
                                'Stress Ratio'
                              ],
                            ]"
                            tooltip="defined as the stress ratio (σ_min/σ_max) [-] and has relevance in the context of constant amplitude experiments."
                          />
                        </li>
                        <li>
                          <experiment-s-v
                            subject="Total dissipated energy (TDE)"
                            :values="
                              test.tests.map((t) => t.total_dissipated_energy)
                            "
                            valueType="bigNumber"
                            :colors="test.tests.map((t) => t.color)"
                            tooltip="defined as the sum of all the hysteresis areas over the course of an experiment. It gives a good measure of the amount of energy that has been dissipated in deformation and heat over the course of an experiment."
                          />
                        </li>
                      </ul>
                    </v-card-text>
                  </v-card>
                </v-col>
              </v-row>
            </v-container>
          </v-card-text>
        </v-card>
      </v-card-text>
    </v-card>
  </div>
</template>

<script>
import { mapState, mapGetters } from "vuex";
import ExperimentSV from "@/components/ExperimentSV";
import InfoTooltip from "@/components/InfoTooltip";
import ExperimentSpecifications from "@/components/ExperimentSpecifications.vue";
import * as Bokeh from "bokeh";

export default {
  name: "TestsDashboard",
  components: {
    ExperimentSV,
    ExperimentSpecifications,
    InfoTooltip,
  },
  computed: {
    ...mapState(["test"]),
    ...mapGetters(["dataIsFetched"]),
  },
  data() {
    return {
      id: {
        bokeh: {
          stressStrain: "bokeh-stress-strain",
          creep: "bokeh-creep",
          hysteresisArea: "bokeh-hysteresis-area",
          stiffness: "bokeh-stiffness",
        },
      },
    };
  },
  created() {
    this.$store.dispatch("fetchExperiment", {
      laboratory: "CCLAB",
      researcher: "Vahid",
      experimentType: "FA",
      date: "2021-04-20",
      testNumbers: [2, 5],
    });
    this.unsubscribe = this.$store.subscribe((mutation) => {
      if (mutation.type === "saveTest") {
        Bokeh.embed.embed_item(
          this.test.plot.stress_strain,
          this.id.bokeh.stressStrain
        );
        Bokeh.embed.embed_item(this.test.plot.creep, this.id.bokeh.creep);
        Bokeh.embed.embed_item(
          this.test.plot.hysteresis_area,
          this.id.bokeh.hysteresisArea
        );
        Bokeh.embed.embed_item(
          this.test.plot.stiffness,
          this.id.bokeh.stiffness
        );
      }
    });
  },
  beforeDestroy() {
    this.unsubscribe();
  },
};
</script>

<style scoped>
div.bokeh {
  min-height: 250px;
  height: 25vh;
}
</style>

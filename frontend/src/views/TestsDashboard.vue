<template>
  <v-container>
    <v-skeleton-loader v-if="experiment.loading" type="article" />
    <div v-else>
      <v-row>
        <v-col :cols="12">
          <experiment-specifications :experiment="experiment.experiment" />
        </v-col>
        <v-col :cols="12">
          <v-card flat>
            <v-card-title>
              Test results
              <v-spacer> </v-spacer>
              <v-btn @click="goBack">Add test(s)</v-btn>
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
                            loops. These loops, also known as hysteresis loops
                            are important because they provide a visual
                            representation of what is being plotted in the next
                            three graphs.
                          </info-tooltip>
                          <div
                            :id="bokehPlotsIds.stressStrain"
                            class="bokeh"
                          ></div>
                        </v-row>
                      </v-col>
                      <v-col cols="6">
                        <v-row>
                          <info-tooltip>
                            On this graph, we show the evolution of the
                            hysteresis area. This value is defined as the area
                            contained within a hysteresis loop. For visual
                            representation, it represents the area defined by
                            each closed loop on the (stress-strain) plane in
                            graph 1.
                          </info-tooltip>
                          <div
                            :id="bokehPlotsIds.hysteresisArea"
                            class="bokeh"
                          ></div>
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
                          <div :id="bokehPlotsIds.creep" class="bokeh"></div>
                        </v-row>
                      </v-col>
                      <v-col cols="6">
                        <v-row>
                          <info-tooltip>
                            Stiffness is representative of the resistance an
                            object opposes to an applied force. On this graph,
                            we show how this capacity evolves over a fatigue
                            life cycle.
                          </info-tooltip>
                          <div
                            :id="bokehPlotsIds.stiffness"
                            class="bokeh"
                          ></div>
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
                              :values="bokehTests.map((t) => t.specimen_id)"
                              valueType="bigNumber"
                              :colors="bokehTests.map((t) => t.color)"
                            />
                          </li>
                          <li>
                            <experiment-s-v
                              subject="Stress at failure"
                              :values="bokehTests.map((t) => '-')"
                              :colors="bokehTests.map((t) => t.color)"
                              :unit="units.stress"
                              tooltip="σ_fail is defined as the stress level that induced failure from the tested specimen and is measured in [MPa]"
                            />
                          </li>
                          <li>
                            <experiment-s-v
                              subject="Strain at failure"
                              :values="bokehTests.map((t) => '-')"
                              :colors="bokehTests.map((t) => t.color)"
                              unit="%"
                              tooltip="ε_fail is defined as the deformation at the time of failure and is measured in [%]"
                            />
                          </li>
                          <li>
                            <experiment-s-v
                              subject="N_cycles"
                              :values="bokehTests.map((t) => '-')"
                              :colors="bokehTests.map((t) => t.color)"
                              valueType="bigNumber"
                              tooltip="defined as the number of cycles to failure [-]"
                            />
                          </li>
                          <li>
                            <experiment-s-v
                              subject="R"
                              :values="bokehTests.map((t) => '-')"
                              :colors="bokehTests.map((t) => t.color)"
                              tooltip="defined as the stress ratio (σ_min/σ_max) [-] and has relevance in the context of constant amplitude experiments."
                            />
                          </li>
                          <li>
                            <experiment-s-v
                              subject="Total dissipated energy (TDE)"
                              :values="
                                bokehTests.map((t) => t.total_dissipated_energy)
                              "
                              valueType="bigNumber"
                              :colors="bokehTests.map((t) => t.color)"
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
        </v-col>
      </v-row>
    </div>
  </v-container>
</template>

<script>
import * as Bokeh from "bokeh";
import { mapState } from "vuex";
import InfoTooltip from "@/components/InfoTooltip";
import ExperimentSpecifications from "@/components/ExperimentSpecifications.vue";
import ExperimentSV from "@/components/ExperimentSV";

export default {
  name: "TestsDashboard",
  components: {
    ExperimentSpecifications,
    InfoTooltip,
    ExperimentSV,
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
    ...mapState("bokehPlots", {
      bokehLoading: "loading",
      bokehTests: "tests",
      bokehPlots: "plots",
    }),
  },
  data() {
    return {
      bokehPlotsIds: {
        stressStrain: "bokeh-stress-strain",
        creep: "bokeh-creep",
        hysteresisArea: "bokeh-hysteresis-area",
        stiffness: "bokeh-stiffness",
      },
      bokehPlotsRendered: false,
    };
  },
  methods: {
    goBack() {
      this.$router.go(-1);
    },
    renderBokehPlotsIfGoodTime() {
      if (
        !this.experiment.loading &&
        !this.bokehLoading &&
        !this.bokehPlotsRendered
      ) {
        Bokeh.embed.embed_item(
          this.bokehPlots.stress_strain,
          this.bokehPlotsIds.stressStrain
        );
        Bokeh.embed.embed_item(this.bokehPlots.creep, this.bokehPlotsIds.creep);
        Bokeh.embed.embed_item(
          this.bokehPlots.hysteresis_area,
          this.bokehPlotsIds.hysteresisArea
        );
        Bokeh.embed.embed_item(
          this.bokehPlots.stiffness,
          this.bokehPlotsIds.stiffness
        );
        this.bokehPlotsRendered = true;
      }
    },
  },
  created() {
    const storeUnwatch1 = this.$store.watch(
      (state) => state.bokehPlots.loading,
      () => {
        if (!this.bokehLoading) {
          this.renderBokehPlotsIfGoodTime();
          storeUnwatch1();
        }
      }
    );
    const storeUnwatch2 = this.$store.watch(
      (state) => state.experiments.oneExperiment.loading,
      () => {
        if (!this.experiment.loading) {
          this.renderBokehPlotsIfGoodTime();
          storeUnwatch2();
        }
      }
    );
    this.$store.dispatch("bokehPlots/fetchBokehPlots", {
      experimentId: this.experimentId,
      testIds: this.testIds,
    });
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

<style scoped>
div.bokeh {
  min-height: 250px;
  height: 25vh;
}
div.bk {
  width: 100%;
}
</style>

<template>
  <div class="ma-5">
    <v-skeleton-loader v-if="!dataIsFetched"
      max-width="100%" height="10rem"
      type="paragraph" />

    <template v-else>
      <v-card elevation="0">
        <v-card-title>
          Experiment : {{ test.experience['Experiment Type'] }} test, by {{ test.experience.Researcher }}, {{ test.experience.Laboratory }}, {{ test.experience.Experiment.Date }}
          <v-spacer />
          <v-btn>Add test(s)</v-btn>
        </v-card-title>
      </v-card>

      <experiment-specifications
        :experience="test.experience"
      />

      <v-container fluid>
        <v-row no-gutters>
          <v-col class="col-10">
            <v-card header="Test results" elevation="0">
              <v-card-title>
                Test results
                <info-button>
                  <template v-slot:title>
                    Test results info
                  </template>
                  <p>
                    In the test results sections, we show information that can be derived from the testings using 4 graphs and 5 key values.
                  </p>
                  <p>
                    Graph 1: Stress-Strain (x: strain, y: stress)
                    This graph shows a selection of loading/unloading loops. These loops, also known as hysteresis loops are important because they provide a visual representation of what is being plotted in the next three graphs.
                  </p>
                  <p>
                    Graph 2: Hysteresis loop area (x: number of cycles, y: hysteresis area)
                    On this graph, we show the evolution of the hysteresis area. This value is defined as the area contained within a hysteresis loop. For visual representation, it represents the area defined by each closed loop on the (stress-strain) plane in graph 1. 
                  </p>
                  <p>
                    Graph 3: Creep evolution (x: number of cycles, y: creep)
                    Creep is defined as the average deformation during each loading/unloading cycle. It gives an understanding of how much deformation occurs at each cycle.
                  </p>
                  <p>
                    Graph 4: Stiffness evolution (x: number of cycles, y: stiffness)
                    Stiffness is representative of the resistance an object opposes to an applied force. On this graph, we show how this capacity evolves over a fatigue life cycle.
                  </p>
                  <p>
                    Stress at failure: σ_fail is defined as the stress level that induced failure from the tested specimen and is measured in [MPa]
                  </p>
                  <p>
                    Strain at failure: ε_fail is defined as the deformation at the time of failure and is measured in [%]
                  </p>
                  <p>
                    N_cycles: is defined as the number of cycles to failure [-]
                  </p>
                  <p>
                    R: is defined as the stress ratio (σ_min/σ_max) [-] and has relevance in the context of constant amplitude experiments.
                    Total dissipated energy (TDE): defined as the sum of all the hysteresis areas over the course of an experiment. It gives a good measure of the amount of energy that has been dissipated in deformation and heat over the course of an experiment.
                  </p>
                </info-button>
              </v-card-title>

              <v-row no-gutters>
                <v-col cols="6">
                  <div v-bind:id="bokeh.stressStrain"></div>
                </v-col>
                <v-col cols="6">
                  <div v-bind:id="bokeh.creep"></div>
                </v-col>
                <v-col cols="6">
                  <div v-bind:id="bokeh.hysteresisArea"></div>
                </v-col>
                <v-col cols="6">
                  <div v-bind:id="bokeh.stiffness"></div>
                </v-col>
              </v-row>
            </v-card>
          </v-col>
          <v-col class="col-2 my-auto">
            <v-card>
              <v-card-text>
                <ul>
                  <li>
                    <experiment-s-v
                      subject="Stress at failure"
                      :values="[test.experience.Experiment['Standard Fatigue']['Stress at Failure']]"
                      :unit="test.experience['Experiment Units'].Stress"
                    />
                  </li>
                  <li>
                    <experiment-s-v
                      subject="Strain at failure"
                      :values="[test.experience.Experiment['Standard Fatigue']['Strain at Failure']]"
                      unit="%"
                    />
                  </li>
                  <li>
                    <experiment-s-v
                      subject="N_cycles"
                      :values="[test.experience.Experiment['Standard Fatigue']['Number of Cycles to Failure']]"
                      valueType="bigNumber"
                    />
                  </li>
                  <li>
                    <experiment-s-v
                      subject="R"
                      :values="[test.experience.Experiment['Standard Fatigue']['Stress Ratio']]"
                    />
                  </li>
                  <li>
                    <experiment-s-v
                      subject="Total dissipated energy (TDE)"
                      :values="[test.experience.Experiment['Standard Fatigue']['Total Dissipated Energy']]"
                      valueType="bigNumber"
                    />
                  </li>
                </ul>
              </v-card-text>
            </v-card>
          </v-col>
        </v-row>
      </v-container>
    </template>
  </div>
</template>

<script>
import { mapState, mapGetters } from 'vuex'
import InfoButton from '@/components/InfoButton'
import ExperimentSV from '@/components/ExperimentSV'
import ExperimentSpecifications from '@/components/ExperimentSpecifications.vue'
import * as Bokeh from 'bokeh'

export default {
  name: 'TestDashboard',
  components: {
    InfoButton,
    ExperimentSV,
    ExperimentSpecifications,
  },
  computed: {
    ...mapState([
      'test',
    ]),
    ...mapGetters([
      'dataIsFetched',
    ]),
  },
  data() {
    return {
      bokeh: {
        stressStrain: 'bokeh-stress-strain',
        creep: 'bokeh-creep',
        hysteresisArea: 'bokeh-hysteresis-area',
        stiffness: 'bokeh-stiffness'
      }
    }
  },
  created() {
    this.$store.dispatch('fetchExperience', {
      laboratory: 'CCLAB',
      researcher: 'Vahid',
      experienceType: 'FA',
      date: '2021-04-20',
      testNumber: 2,
    })
  },
  updated() {
    if (this.dataIsFetched) {
      Bokeh.embed.embed_item(this.test.plot.stress_strain, this.bokeh.stressStrain)
      Bokeh.embed.embed_item(this.test.plot.creep, this.bokeh.creep)
      Bokeh.embed.embed_item(this.test.plot.hysteresis_area, this.bokeh.hysteresisArea)
      Bokeh.embed.embed_item(this.test.plot.stiffness, this.bokeh.stiffness)
    }
  },
}
</script>

<style scoped>
iframe {
  height: 600px;
}
</style>

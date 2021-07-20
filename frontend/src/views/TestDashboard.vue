<template>
  <div class="ma-5">
    <v-skeleton-loader v-if="!dataIsFetched"
      max-width="100%" height="10rem"
      type="paragraph" />

    <template v-else>
      <v-card elevation="0">
        <v-card-title>
          Experiment : {{ experience['Experiment Type'] }} test, by {{ experience.Researcher }}, {{ experience.Laboratory }}, {{ experience.Experiment.Date }}
          <v-spacer />
          <v-btn>Add test(s)</v-btn>
        </v-card-title>
      </v-card>

      <v-card header="Test specifications"
        fluid
      >
        <v-card-title>
          Experiment specifications (metadata)
          <info-button>
            <template v-slot:title>
              Experiment specifications metadata info
            </template>
            The metadata are the set of information that define all the test parameters, they also allow us to uniquely define each test.
          </info-button>
        </v-card-title>
        <v-card-text>
          <v-row no-gutters>
            <v-col>
              <h4>GENERAL INFORMATIONS</h4>
              <ul>
                <li>
                  <experiment-s-v
                    subject="Specimen number"
                    :values="[experience.Experiment['Specimen number']]"
                  />
                </li>
                <li>
                  <experiment-s-v
                    subject="Publications"
                    :values="[experience.Publications]"
                    valueType="PublicationsLinks"
                  />
                </li>
                <li>
                  <experiment-s-v
                    subject="Author"
                    :values="[experience.Researcher]"
                  />
                </li>
              </ul>
            </v-col>
            <v-col>
              <h4>TEST CONDITIONS</h4>
              <ul>
                <li>
                  <experiment-s-v
                    subject="Temperature"
                    :values="[experience.Experiment['Test Conditions'].Temperature]"
                    :unit="experience['Experiment Units'].Temperature"
                  />
                </li>
                <li>
                  <experiment-s-v
                    subject="Relative humidity"
                    :values="[experience.Experiment['Test Conditions']['Relative Humidity']]"
                    unit="%"
                  />
                </li>
                <li>
                  <experiment-s-v
                    subject="Grip pressure"
                    :values="[experience.Experiment['Test Conditions']['Grip Pressure']]"
                    :unit="experience['Experiment Units'].Pressure"
                  />
                </li>
                <li>
                  <experiment-s-v
                    subject="Measuring equipment"
                    :values="[experience['Measuring Equipment']]"
                  />
                </li>
              </ul>
            </v-col>
            <v-col>
              <h4>LAMINATE AND ASSEMBLIES</h4>
              <ul>
                <li>
                  <experiment-s-v
                    subject="Curing time"
                    :values="[experience.Experiment['Laminates and Assemblies']['Curing Time']]"
                    :unit="experience['Experiment Units'].Time"
                  />
                </li>
                <li>
                  <experiment-s-v
                    subject="Curing temperature"
                    :values="[experience.Experiment['Laminates and Assemblies']['Curing Temperature']]"
                    :unit="experience['Experiment Units'].Temperature"
                  />
                </li>
                <li>
                  <experiment-s-v
                    subject="Curing Pressure"
                    :values="[experience.Experiment['Laminates and Assemblies']['Curing Pressure']]"
                    :unit="experience['Experiment Units'].Pressure"
                  />
                </li>
                <li>
                  <experiment-s-v
                    subject="Stacking sequence"
                    :values="[experience.Experiment['Laminates and Assemblies']['Stacking Sequence']]"
                  />
                </li>
                <li>
                  <experiment-s-v
                    subject="Fiber content"
                    :values="[experience.Experiment['Laminates and Assemblies']['Fiber Content']]"
                  />
                </li>
              </ul>
            </v-col>
            <v-col>
              <h4>MATERIALS</h4>
              <ul>
                <li>
                  <experiment-s-v
                    subject="Resin"
                    :values="[experience.Experiment['Material Type']['Resin']]"
                  />
                </li>
                <li>
                  <experiment-s-v
                    subject="Hardener"
                    :values="[experience.Experiment['Material Type']['Hardener']]"
                  />
                </li>
                <li>
                  <experiment-s-v
                    subject="Mixing ration"
                    :values="[experience.Experiment['Material Type']['Mixing ratio']]"
                  />
                </li>
                <li>
                  <experiment-s-v
                    subject="Fiber Material"
                    :values="[experience.Experiment['Constituent Materials']['Fiber Material']]"
                  />
                </li>
                <li>
                  <experiment-s-v
                    subject="Fiber Geometry"
                    :values="[experience.Experiment['Constituent Materials']['Fiber Geometry']]"
                  />
                </li>
                <li>
                  <experiment-s-v
                    subject="Area Density"
                    :values="[experience.Experiment['Constituent Materials']['Area Density']]"
                  />
                </li>
              </ul>
            </v-col>
          </v-row>
        </v-card-text>
      </v-card>

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
                  <div id="bokeh-stress-strain"></div>
                </v-col>
                <v-col cols="6">
                  <div id="bokeh-creep"></div>
                </v-col>
                <v-col cols="6">
                  <div id="bokeh-hystarea"></div>
                </v-col>
                <v-col cols="6">
                  <div id="bokeh-stiffness"></div>
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
                      :values="[experience.Experiment['Standard Fatigue']['Stress at Failure']]"
                      :unit="experience['Experiment Units'].Stress"
                    />
                  </li>
                  <li>
                    <experiment-s-v
                      subject="Strain at failure"
                      :values="[experience.Experiment['Standard Fatigue']['Strain at Failure']]"
                      unit="%"
                    />
                  </li>
                  <li>
                    <experiment-s-v
                      subject="N_cycles"
                      :values="[experience.Experiment['Standard Fatigue']['Number of Cycles to Failure']]"
                      valueType="bigNumber"
                    />
                  </li>
                  <li>
                    <experiment-s-v
                      subject="R"
                      :values="[experience.Experiment['Standard Fatigue']['Stress Ratio']]"
                    />
                  </li>
                  <li>
                    <experiment-s-v
                      subject="Total dissipated energy (TDE)"
                      :values="[experience.Experiment['Standard Fatigue']['Total Dissipated Energy']]"
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
import * as Bokeh from 'bokeh'

export default {
  name: 'TestDashboard',
  components: {
    InfoButton,
    ExperimentSV,
  },
  computed: {
    ...mapState([
      'experience',
    ]),
    ...mapGetters([
      'titles',
      'dois',
      'dataIsFetched',
    ]),
  },
  created() {
    this.$store.dispatch('fetchExperience')
  },
  mounted() {
    fetch('/bokeh/creep.json')
      .then(response => response.json())
      .then(item => Bokeh.embed.embed_item(item, 'bokeh-creep'));
    fetch('/bokeh/hyst_area.json')
      .then(response => response.json())
      .then(item => Bokeh.embed.embed_item(item, 'bokeh-hystarea'));
    fetch('/bokeh/select_stress_strain.json')
      .then(response => response.json())
      .then(item => Bokeh.embed.embed_item(item, 'bokeh-stress-strain'));
    fetch('/bokeh/stiffness.json')
      .then(response => response.json())
      .then(item => Bokeh.embed.embed_item(item, 'bokeh-stiffness'));
  },
}
</script>

<style scoped>
iframe {
  height: 600px;
}
</style>

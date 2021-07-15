<template>
  <div class="ma-5">
    <v-skeleton-loader v-if="!dataIsFetched"
      max-width="100%" height="10rem"
      type="paragraph" />

    <template v-else>
      <h1>Experiment : {{ experience['Experiment Type'] }} test, by {{ experience.Researcher }}, {{ experience.Laboratory }}, {{ experience.Experiment.Date }}</h1>
      <v-card header="Test specifications"
        fluid
      >
        <v-card-title>
          Experiment specifications (metadata)
          <info-button>
            <template v-slot:title>
              Test results metadata
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
                  <span class="subject">Specimen:</span> &nbsp;
                  <span class="value">
                    TODO ... not found
                  </span>
                </li>
                <li>
                  <span class="subject">Publications:</span> &nbsp;
                  <span class="value">
                    <span v-for="(pub, index) in experience.Publications" :key="index">
                      <a :href="pub.DOI" target="_blank">{{ pub.Title }}</a>
                      <span v-if="index != experience.Publications.length - 1">, </span>
                    </span>
                  </span>
                </li>
                <li>
                  <span class="subject">Author:</span> &nbsp;
                  <span class="value">
                    {{ experience.Researcher }}
                  </span>
                </li>
              </ul>
            </v-col>
            <v-col>
              <h4>TEST CONDITIONS</h4>
              <ul>
                <li>
                  <span class="subject">Temperature:</span> &nbsp;
                  <span class="value">
                    {{ experience.Experiment['Test Conditions'].Temperature }}
                    <template v-if="! isNaN(experience.Experiment['Test Conditions'].Temperature)" >{{ experience['Experiment Units'].Temperature }}</template>
                  </span>
                </li>
                <li>
                  <span class="subject">Relative humidity:</span> &nbsp;
                  <span class="value">
                    {{ experience.Experiment['Test Conditions']['Relative Humidity'] }}
                    <template v-if="! isNaN(experience.Experiment['Test Conditions']['Relative Humidity'])">%</template>
                  </span>
                </li>
                <li>
                  <span class="subject">Grip pressure:</span> &nbsp;
                  <span class="value">
                    {{ experience.Experiment['Test Conditions']['Grip Pressure'] }}
                    <template v-if="! isNaN(experience.Experiment['Test Conditions']['Grip Pressure'])">{{ experience['Experiment Units'].Pressure }}</template>
                  </span>
                </li>
                <li>
                  <span class="subject">Measuring equipment:</span> &nbsp;
                  <span class="value">{{ experience['Measuring Equipment'] }}</span>
                </li>
              </ul>
            </v-col>
            <v-col>
              <h4>LAMINATE AND ASSEMBLIES</h4>
              <ul>
                <li>
                  <span class="subject">Curing time:</span> &nbsp;
                  <span class="value">
                    {{ experience.Experiment['Laminates and Assemblies']['Curing Time'] }}
                    <template v-if="! isNaN(experience.Experiment['Laminates and Assemblies']['Curing Time'])">{{ experience['Experiment Units'].Time }}</template>
                  </span>
                </li>
                <li>
                  <span class="subject">Curing temperature:</span> &nbsp;
                  <span class="value">
                    {{ experience.Experiment['Laminates and Assemblies']['Curing Temperature'] }}
                    <template v-if="! isNaN(experience.Experiment['Laminates and Assemblies']['Curing Temperature'])">{{ experience['Experiment Units'].Temperature }}</template>
                  </span>
                </li>
                <li>
                  <span class="subject">Curing Pressure:</span> &nbsp;
                  <span class="value">
                    {{ experience.Experiment['Laminates and Assemblies']['Curing Pressure'] }}
                    <template v-if="! isNaN(experience.Experiment['Laminates and Assemblies']['Curing Pressure'])">{{ experience['Experiment Units'].Pressure }}</template>
                  </span>
                </li>
                <li>
                  <span class="subject">Stacking sequence:</span> &nbsp;
                  <span class="value">
                    {{ experience.Experiment['Laminates and Assemblies']['Stacking Sequence'] }}
                  </span>
                </li>
                <li>
                  <span class="subject">Fiber content:</span> &nbsp;
                  <span class="value">
                    {{ experience.Experiment['Laminates and Assemblies']['Fiber Content'] }}
                  </span>
                </li>
              </ul>
            </v-col>
            <v-col>
              <h4>MATERIALS</h4>
              <ul>
                <li>
                  <span class="subject">Resin:</span> &nbsp;
                  <span class="value">
                    {{ experience.Experiment['Material Type']['Resin'] }}
                  </span>
                </li>
                <li>
                  <span class="subject">Hardener:</span> &nbsp;
                  <span class="value">
                    {{ experience.Experiment['Material Type']['Hardener'] }}
                  </span>
                </li>
                <li>
                  <span class="subject">Mixing ration:</span> &nbsp;
                  <span class="value">
                    {{ experience.Experiment['Material Type']['Mixing ratio'] }}
                  </span>
                </li>
                <li>
                  <span class="subject">Fiber Material:</span> &nbsp;
                  <span class="value">
                    {{ experience.Experiment['Constituent Materials']['Fiber Material'] }}
                  </span>
                </li>
                <li>
                  <span class="subject">Fiber Geometry:</span> &nbsp;
                  <span class="value">
                    {{ experience.Experiment['Constituent Materials']['Fiber Geometry'] }}
                  </span>
                </li>
                <li>
                  <span class="subject">Area Density:</span> &nbsp;
                  <span class="value">
                    {{ experience.Experiment['Constituent Materials']['Area Density'] }}
                  </span>
                </li>
              </ul>
            </v-col>
          </v-row>
        </v-card-text>
      </v-card>
    </template>

    <v-container fluid>
      <v-row no-gutters>
        <v-col class="col-10">
          <v-container>
            <v-row no-gutters>
              <v-col>
                <iframe id="plot_stress_strain" src="/plot_select_stress_strain.html" frameborder="0"/>
              </v-col>
              <v-col>
                <iframe id="plot_creep" src="/plot_creep.html" frameborder="0"/>
              </v-col>
            </v-row>
            <v-row no-gutters>
              <v-col>
                <iframe id="plot_hystarea" src="/plot_hystarea.html" frameborder="0"/>
              </v-col>
              <v-col>
                <iframe id="plot_stiffness" src="/plot_stiffness.html" frameborder="0"/>
              </v-col>
            </v-row>
          </v-container>
        </v-col>
        <v-col class="col-2 my-auto">
          <v-card>
            <v-card-text>
              <ul>
                <li>
                  <span class="subject">Stress at failure:</span> &nbsp;
                  <span class="value">
                    47.4 MPa (TODO)
                  </span>
                </li>
                <li>
                  <span class="subject">Strain at failure:</span> &nbsp;
                  <span class="value">
                    1.4% (TODO)
                  </span>
                </li>
                <li>
                  <span class="subject">N_cycles:</span> &nbsp;
                  <span class="value">
                    1'198'627 (TODO)
                  </span>
                </li>
                <li>
                  <span class="subject">R:</span> &nbsp;
                  <span class="value">
                    0.1 (TODO)
                  </span>
                </li>
                <li>
                  <span class="subject">Total dissipated energy (TDE):</span> &nbsp;
                  <span class="value">
                    (TODO)
                  </span>
                </li>
              </ul>
            </v-card-text>
          </v-card>
        </v-col>
      </v-row>
    </v-container>
  </div>
</template>

<script>
import { mapState, mapGetters } from 'vuex'
import InfoButton from '@/components/InfoButton.vue'

export default {
  name: 'TestDashboard',
  components: { InfoButton },
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
}
</script>

<style scoped>
iframe {
  height: 600px;
}
span.subject {
  font-weight: bold;
  font-style: italic;
  color: rgb(143, 143, 143);
}
</style>

<template>
  <div class="p-m-3 p-text-left">
    <h1>Fatigue Test Dashboard - Individual test</h1>
    <Skeleton v-if="!dataIsFetched" width="100%" height="10rem" />
    <Panel v-else header="Test specifications">
      <h4 v-for="title in titles" :key="title">{{ title }}</h4>
      <div class="p-grid">
        <div class="p-col">
          <h4>General informations</h4>
          <ul>
            <li>
              <span class="subject">Specimen:</span> &nbsp;
              <span class="value">
                TODO ... not found
              </span>
            </li>
            <li>
              <span class="subject">Researcher:</span> &nbsp;
              <span class="value">
                {{ experience.Researcher }}
              </span>
            </li>
            <li v-for="pub in experience.Publications" :key="pub.doi">
              {{ pub.Title }} <br>
                <a :href="pub.DOI" target="_blank">{{ pub.DOI }}</a>
            </li>
          </ul>
        </div>
        <Divider layout="vertical" />
        <div class="p-col">
          <h4>Test conditions</h4>
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
        </div>
        <Divider layout="vertical" />
        <div class="p-col">
          <h4>Laminate and assemblies</h4>
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
        </div>
        <Divider layout="vertical" />
        <div class="p-col">
          <h4>Materials</h4>
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
        </div>
      </div>
    </Panel>
    <Divider layout="horizontal" />
    <div id="graphs" class="p-grid">
      <div class="p-col-10">
        <div class="p-grid">
          <iframe class="p-col-6" id="plot_stress_strain" src="/plot_select_stress_strain.html" frameborder="0"/>
          <iframe class="p-col-6" id="plot_creep" src="/plot_creep.html" frameborder="0"/>
          <iframe class="p-col-6" id="plot_hystarea" src="/plot_hystarea.html" frameborder="0"/>
          <iframe class="p-col-6" id="plot_stiffness" src="/plot_stiffness.html" frameborder="0"/>
        </div>
      </div>
      <div class="p-col-2">
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
      </div>
    </div>
  </div>
</template>

<script>
import { mapState, mapGetters } from 'vuex'
import Skeleton from 'primevue/skeleton';
import Panel from 'primevue/panel'
import Divider from 'primevue/divider'

export default {
  name: 'TestDashboard',
  components: {
    Panel,
    Divider,
    Skeleton,
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
span.value {
  white-space: nowrap;
}
</style>

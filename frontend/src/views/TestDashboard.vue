<template>
  <div class="this-page">
    <h1>Fatigue Test Dashboard - Individual test</h1>
    <Skeleton v-if="!dataIsFetched" width="100%" height="10rem" />
    <Panel v-else header="Test specifications">
      <div>
        <h4 v-for="title in titles" :key="title">{{ title }}</h4>
        <div class="p-d-flex space-evenly">
          <div class="spec_column">
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
          <div class="spec_column">
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
          <div class="spec_column">
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
          <div class="spec_column">
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
      </div>
    </Panel>
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
div.this-page {
  text-align: left;
}
div.space-evenly {
  justify-content: space-evenly;
}
ul {
  padding-left: 20px;
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

<template>
  <v-card header="Experiment specifications">
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
      <v-row>
        <v-col>
          <h4>GENERAL INFORMATIONS</h4>
          <ul>
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
</template>

<script>
import InfoButton from '@/components/InfoButton'
import ExperimentSV from '@/components/ExperimentSV'

export default {
  name: 'ExperimentSpecifications',
  props: {
    experience: Object,
  },
  components: {
    InfoButton,
    ExperimentSV,
  }
}
</script>

<style scoped lang="scss">
</style>

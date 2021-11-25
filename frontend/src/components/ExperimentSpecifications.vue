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
                :values="[experiment.Publications]"
                valueType="PublicationsLinks"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Author"
                :values="[experiment.Researcher]"
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
                :values="[experiment.Experiment['Test Conditions'].Temperature]"
                :unit="experiment['Experiment Units'].Temperature"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Relative humidity"
                :values="[experiment.Experiment['Test Conditions']['Relative Humidity']]"
                unit="%"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Grip pressure"
                :values="[experiment.Experiment['Test Conditions']['Grip Pressure']]"
                :unit="experiment['Experiment Units'].Pressure"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Measuring equipment"
                :values="[experiment['Measuring Equipment']]"
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
                :values="[experiment.Experiment['Laminates and Assemblies']['Curing Time']]"
                :unit="experiment['Experiment Units'].Time"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Curing temperature"
                :values="[experiment.Experiment['Laminates and Assemblies']['Curing Temperature']]"
                :unit="experiment['Experiment Units'].Temperature"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Curing Pressure"
                :values="[experiment.Experiment['Laminates and Assemblies']['Curing Pressure']]"
                :unit="experiment['Experiment Units'].Pressure"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Stacking sequence"
                :values="[experiment.Experiment['Laminates and Assemblies']['Stacking Sequence']]"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Fiber content"
                :values="[experiment.Experiment['Laminates and Assemblies']['Fiber Content']]"
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
                :values="[experiment.Experiment['Material Type']['Resin']]"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Hardener"
                :values="[experiment.Experiment['Material Type']['Hardener']]"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Mixing ration"
                :values="[experiment.Experiment['Material Type']['Mixing ratio']]"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Fiber Material"
                :values="[experiment.Experiment['Constituent Materials']['Fiber Material']]"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Fiber Geometry"
                :values="[experiment.Experiment['Constituent Materials']['Fiber Geometry']]"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Area Density"
                :values="[experiment.Experiment['Constituent Materials']['Area Density']]"
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
    experiment: Object,
  },
  components: {
    InfoButton,
    ExperimentSV,
  }
}
</script>

<style scoped lang="scss">
</style>

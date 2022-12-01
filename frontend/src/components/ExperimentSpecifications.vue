<template>
  <v-card header="Experiment specifications">
    <v-card-title>
      Experiment specifications (metadata)
      <info-button>
        <template #title> Experiment specifications metadata info </template>
        The metadata are the set of information that define all the test
        parameters, they also allow us to uniquely define each test.
      </info-button>
    </v-card-title>
    <v-card-text>
      <v-skeleton-loader v-if="!readyToDisplay" type="article" />
      <v-row v-else>
        <v-col>
          <h4>GENERAL INFORMATIONS</h4>
          <ul>
            <li>
              <experiment-s-v
                subject="Laboratory"
                :values="[experiment.laboratory]"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Author"
                :values="[experiment.researcher]"
              />
            </li>
            <li>
              <experiment-s-v subject="Date" :values="[experiment.date]" />
            </li>
            <li>
              <experiment-s-v
                subject="Type"
                :values="[experiment.experiment_type]"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Measuring equipment"
                :values="[experiment.measuring_equipment]"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Publications"
                :values="[
                  `${experiment.publication_title} (${experiment.publication_year})`,
                ]"
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
                :values="[experiment.laminates_and_assemblies_curing_time]"
                :unit="units.time"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Curing temperature"
                :values="[
                  experiment.laminates_and_assemblies_curing_temperature,
                ]"
                :unit="units.temperature"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Curing Pressure"
                :values="[experiment.laminates_and_assemblies_curing_pressure]"
                :unit="units.pressure"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Stacking sequence"
                :values="[
                  experiment.laminates_and_assemblies_stacking_sequence,
                ]"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Fiber volume ratio"
                :values="[
                  experiment.laminates_and_assemblies_fiber_volume_ratio,
                ]"
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
                :values="[experiment.material_type_resin]"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Hardener"
                :values="[experiment.material_type_hardener]"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Mixing ratio"
                :values="[experiment.material_type_mixing_ratio]"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Fiber Material"
                :values="[experiment.material_type_fiber_material]"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Fiber form"
                :values="[experiment.material_type_fiber_form]"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Area Density"
                :values="[experiment.material_type_area_density]"
              />
            </li>
          </ul>
        </v-col>
      </v-row>
    </v-card-text>
  </v-card>
</template>

<script>
import { mapState } from "vuex";

import ExperimentSV from "@/components/ExperimentSV";
import InfoButton from "@/components/InfoButton";

export default {
  name: "ExperimentSpecifications",
  components: {
    InfoButton,
    ExperimentSV,
  },
  props: {
    experiment: { type: Object, required: true },
  },
  computed: {
    readyToDisplay() {
      return (
        Object.keys(this.units).length !== 0 &&
        Object.keys(this.experiment).length !== 0
      );
    },
    ...mapState("experiments", {
      units: "units",
    }),
  },
  created() {
    this.$store.dispatch("experiments/fetchUnits");
  },
};
</script>

<style scoped lang="scss"></style>

<template>
  <v-card header="Experiment specifications">
    <v-card-title>
      Experiment specifications (metadata)
      <info-button>
        <template #title>Experiment specifications metadata info</template>
        The metadata are the set of information that define all the test
        parameters, they also allow us to uniquely define each test.
      </info-button>
    </v-card-title>
    <v-card-text>
      <v-skeleton-loader v-if="!readyToDisplay" type="article" />
      <v-row v-else>
        <!-- Column 1: General -->
        <v-col>
          <h4>GENERAL INFORMATIONS</h4>
          <ul>
            <li v-if="experiment.laboratory">
              <experiment-s-v
                subject="Laboratory"
                :values="[experiment.laboratory]"
              />
            </li>
            <li v-if="experiment.researcher">
              <experiment-s-v
                subject="Author"
                :values="[experiment.researcher]"
              />
            </li>
            <li v-if="experiment.date">
              <experiment-s-v subject="Date" :values="[experiment.date]" />
            </li>
            <li v-if="experiment.experiment_type">
              <experiment-s-v
                subject="Type"
                :values="[experiment.experiment_type]"
              />
            </li>
            <li v-if="experiment.qs_experiment_type">
              <experiment-s-v
                subject="QS Experiment Type"
                :values="[experiment.qs_experiment_type]"
              />
            </li>
            <li v-if="experiment.fa_experiment_type">
              <experiment-s-v
                subject="FA Experiment Type"
                :values="[experiment.fa_experiment_type]"
              />
            </li>
            <li v-if="experiment.measuring_equipment">
              <experiment-s-v
                subject="Measuring equipment"
                :values="[experiment.measuring_equipment]"
              />
            </li>
            <li v-if="experiment.publication_doi">
              <experiment-s-v
                subject="Publication DOI"
                :values="[experiment.publication_doi]"
              />
            </li>
          </ul>
        </v-col>

        <!-- Column 2: Materials -->
        <v-col>
          <h4>MATERIALS</h4>
          <ul>
            <li v-if="experiment.material_tested">
              <experiment-s-v
                subject="Materials tested"
                :values="[experiment.material_tested]"
              />
            </li>
            <li v-if="experiment.material_type_sample_type">
              <experiment-s-v
                subject="Sample type"
                :values="[experiment.material_type_sample_type]"
              />
            </li>
            <li v-if="experiment.material_type_fiber_form">
              <experiment-s-v
                subject="Fiber form"
                :values="[experiment.material_type_fiber_form]"
              />
            </li>
            <li v-if="experiment.material_type_resin">
              <experiment-s-v
                subject="Resin"
                :values="[experiment.material_type_resin]"
              />
            </li>
            <li v-if="experiment.laminates_and_assemblies_stacking_sequence">
              <experiment-s-v
                subject="Stacking sequence"
                :values="[
                  experiment.laminates_and_assemblies_stacking_sequence,
                ]"
              />
            </li>
            <li v-if="experiment.curing_time">
              <experiment-s-v
                subject="Curing time"
                :values="[experiment.curing_time]"
                unit="min"
              />
            </li>
            <li v-if="experiment.curing_temperature">
              <experiment-s-v
                subject="Curing temperature"
                :values="[experiment.curing_temperature]"
                unit="°C"
              />
            </li>
            <li v-if="experiment.curing_pressure">
              <experiment-s-v
                subject="Curing pressure"
                :values="[experiment.curing_pressure]"
                unit="bar"
              />
            </li>
            <li v-if="isPostCuringApplied">
              <experiment-s-v
                subject="Post-curing applied"
                :values="['True']"
              />
            </li>
          </ul>
        </v-col>

        <!-- Column 3: Control -->
        <v-col>
          <h4>CONTROL</h4>
          <ul>
            <li v-if="experiment.control_mode">
              <experiment-s-v
                subject="Control mode"
                :values="[experiment.control_mode]"
              />
            </li>
            <li v-if="experiment.loading_rate">
              <experiment-s-v
                subject="Loading rate"
                :values="[experiment.loading_rate]"
                :unit="loadingRateUnit"
              />
            </li>
            <li v-if="isFracture && experiment.fracture_mode_fm">
              <experiment-s-v
                subject="Fracture mode"
                :values="[experiment.fracture_mode_fm]"
              />
            </li>
          </ul>

          <template v-if="experiment.experiment_type === 'FA'">
            <h4>FATIGUE</h4>
            <ul>
              <li v-if="experiment.fatigue_r_ratio">
                <experiment-s-v
                  subject="R ratio"
                  :values="[experiment.fatigue_r_ratio]"
                />
              </li>
              <li v-if="experiment.fatigue_frequency">
                <experiment-s-v
                  subject="Fatigue frequency"
                  :values="[experiment.fatigue_frequency]"
                  unit="Hz"
                />
              </li>
              <li v-if="experiment.fatigue_loading_type_flt">
                <experiment-s-v
                  subject="Fatigue control mode"
                  :values="[experiment.fatigue_loading_type_flt]"
                />
              </li>
            </ul>
          </template>
        </v-col>
      </v-row>
    </v-card-text>
  </v-card>
</template>

<script>
import ExperimentSV from "@/components/ExperimentSV";
import InfoButton from "@/components/InfoButton";
import { mapState } from "vuex";

export default {
  name: "ExperimentSpecifications",
  components: {
    InfoButton,
    ExperimentSV,
  },
  props: {
    experiment: { type: Object, required: true, default: () => ({}) },
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
    isFracture() {
      return (
        this.experiment.qs_experiment_type === "fracture" ||
        this.experiment.fa_experiment_type === "fracture"
      );
    },
    loadingRateUnit() {
      const mode = this.experiment.control_mode;
      if (mode === "Displacement Controlled") return "mm/min";
      if (mode === "Load Controlled") return "kN/s";
      if (mode === "Strain Controlled") return "[-]/s";
      return ""; // fallback
    },
    isPostCuringApplied() {
      const { postcuring_time, postcuring_temperature, postcuring_pressure } =
        this.experiment;
      return (
        postcuring_time != null ||
        postcuring_temperature != null ||
        postcuring_pressure != null
      );
    },
  },
  created() {
    this.$store.dispatch("experiments/fetchUnits");
  },
};
</script>

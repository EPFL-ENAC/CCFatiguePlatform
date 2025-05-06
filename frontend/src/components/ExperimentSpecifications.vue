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
      <!-- <pre v-if="experiment">{{ experiment }}</pre> -->
      <v-row v-else>
        <!-- Column 1: General -->
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
            <li>
              <experiment-s-v
                subject="Materials tested"
                :values="[experiment.material_tested]"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Sample type"
                :values="[experiment.material_type_sample_type]"
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
                subject="Resin"
                :values="[experiment.material_type_resin]"
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
                subject="Curing time"
                :values="[experiment.curing_time]"
                unit="s"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Curing temperature"
                :values="[experiment.curing_temperature]"
                unit="°C"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Curing pressure"
                :values="[experiment.curing_pressure]"
                unit="bar"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Post-curing applied"
                :values="[experiment.postcuring_applied ? 'True' : 'False']"
              />
            </li>
          </ul>
        </v-col>

        <!-- Column 3: Control -->
        <v-col>
          <h4>CONTROL</h4>
          <ul>
            <li>
              <experiment-s-v
                subject="Control mode"
                :values="[experiment.control_mode]"
              />
            </li>
            <li>
              <experiment-s-v
                subject="Loading rate"
                :values="[experiment.loading_rate]"
                unit="mm/s"
              />
            </li>
            <li v-if="isFracture">
              <experiment-s-v
                subject="Fracture mode"
                :values="[experiment.fracture_mode_fm]"
              />
            </li>
          </ul>

          <template v-if="experiment.experiment_type === 'FA'">
            <h4>FATIGUE</h4>
            <ul>
              <li>
                <experiment-s-v
                  subject="R ratio"
                  :values="[experiment.fatigue_r_ratio]"
                />
              </li>
              <li>
                <experiment-s-v
                  subject="Fatigue frequency"
                  :values="[experiment.fatigue_frequency]"
                  unit="Hz"
                />
              </li>
              <li>
                <experiment-s-v
                  subject="Fatigue control mode"
                  :values="[experiment.fatigue_loading_type]"
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
    experimentMetadata() {
      return this.experiment.experiment_metadata || {};
    },
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
  },
  mounted() {
    console.log(
      "📦 ExperimentSpecifications received experiment =",
      this.experiment
    );
    console.log(
      "📩 ExperimentSpecifications received experimentMetadata =",
      this.experimentMetadata
    );
    console.log("✅ experiment =", this.experiment);
    console.log(
      "📂 experiment_metadata =",
      this.experiment?.experiment_metadata
    );
    console.log(
      "🔍 curing_time =",
      this.experiment?.experiment_metadata?.curing_time
    );
    console.log(
      "🔍 curing_temperature =",
      this.experiment?.experiment_metadata?.curing_temperature
    );
    console.log(
      "🔍 loading_rate =",
      this.experiment?.experiment_metadata?.loading_rate
    );
    console.log(
      "🔍 fatigue_frequency =",
      this.experiment?.experiment_metadata?.fatigue_frequency
    );
    console.log(
      "🔍 fatigue_r_ratio =",
      this.experiment?.experiment_metadata?.fatigue_r_ratio
    );
    console.log(
      "🔍 fatigue_loading_type_flt =",
      this.experiment?.experiment_metadata?.fatigue_loading_type_flt
    );
    console.log(
      "🔍 fracture_mode_fm =",
      this.experiment?.experiment_metadata?.fracture_mode_fm
    );
  },
  created() {
    this.$store.dispatch("experiments/fetchUnits");
  },
};
</script>

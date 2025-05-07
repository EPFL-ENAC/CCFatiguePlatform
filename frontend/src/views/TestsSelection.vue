<template>
  <v-container>
    <v-card elevation="0">
      <experiment-specifications :experiment="experiment.experiment" />

      <v-data-table
        v-model="testsSelected"
        :headers="headers"
        :items="numberedTests"
        :options.sync="options"
        :server-items-length="experiment.pagination.total"
        :loading="experiment.loadingTests"
        :footer-props="{ 'items-per-page-options': [5, 10, 15, 20, 40] }"
        item-key="id"
        @click:row="rowClick"
      >
        <template #no-data>No test for this experiment</template>
      </v-data-table>

      <v-container>
        <v-row justify="end">
          <v-btn class="ma-2" @click="goBack"> Back </v-btn>
          <v-btn
            class="ma-2"
            :disabled="testsSelected.length === 0"
            @click="goToTestsDashboard"
          >
            View tests Dashboard
          </v-btn>
        </v-row>
      </v-container>
    </v-card>
  </v-container>
</template>

<script>
import ExperimentSpecifications from "@/components/ExperimentSpecifications.vue";
import { mapState } from "vuex";

export default {
  name: "TestsSelection",
  components: {
    ExperimentSpecifications,
  },
  props: {
    experimentId: { type: Number, required: true },
  },
  data() {
    return {
      testsSelected: [],
      options: {
        page: 1,
        itemsPerPage: 10,
      },
    };
  },
  computed: {
    ...mapState("experiments", {
      experiment: "oneExperiment",
    }),
    numberedTests() {
      const mapped = this.experiment.tests.map((test) => {
        const hasLoadData = test.maximum_load && test.width && test.thickness;
        const calculatedStress = hasLoadData
          ? test.maximum_load / (test.width * test.thickness)
          : null;

        return {
          ...test,
          specimen_number: test.sequential_number,
          maximum_stress:
            calculatedStress !== null
              ? parseFloat(calculatedStress.toFixed(2))
              : null,
        };
      });

      return mapped;
    },
    headers() {
      const baseHeaders = [
        { text: "Specimen Number", value: "specimen_number" },
        { text: "Specimen Name", value: "specimen_name" },
        { text: "Length", value: "length" },
        { text: "Width", value: "width" },
        { text: "Thickness", value: "thickness" },
      ];

      const type = this.experiment?.experiment?.experiment_type;

      if (type !== "QS") {
        baseHeaders.splice(2, 0, {
          text: "Maximum Stress",
          value: "maximum_stress",
        });
        baseHeaders.splice(3, 0, {
          text: "Run Out",
          value: "run_out",
        });
      }

      return baseHeaders;
    },
  },
  watch: {
    options: {
      handler() {
        this.fetchOneExperimentWithTests();
      },
      deep: true,
    },
    "experiment.tests": {
      handler(tests) {
        if (tests.length > 0) {
          console.log("✅ Test caricati:", tests);
          console.log(
            "🧪 Maximum stress:",
            this.numberedTests.map((t) => t.maximum_stress)
          );
        }
      },
      immediate: true,
    },
  },
  created() {
    this.fetchOneExperimentWithTests();
  },
  methods: {
    rowClick(_item, row) {
      row.select(!row.isSelected);
    },
    goBack() {
      this.$router.go(-1);
    },
    goToTestsDashboard() {
      this.$router.push({
        name: "TestsDashboard",
        query: {
          exp: this.experimentId,
          tests: this.testsSelected.map((item) => item.id),
        },
      });
    },
    fetchOneExperimentWithTests() {
      this.$store.dispatch("experiments/fetchOneExperimentWithTests", {
        experimentId: this.experimentId,
        pagination: {
          page: this.options.page,
          size: this.options.itemsPerPage,
        },
      });
    },
  },
};
</script>

<style scoped lang="scss"></style>

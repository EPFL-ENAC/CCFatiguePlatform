<template>
  <v-container>
    <v-card elevation="0">
      <experiment-specifications :experiment="experiment.experiment" />
      <v-row justify="center" class="mt-4 mb-5">
        <v-col cols="5">
          <v-card>
            <v-card-title>
              <v-row align="center" class="w-100">
                <v-col cols="auto" class="d-flex align-center">
                  <span class="text-h6">Test Overview</span>
                </v-col>
                <v-spacer />
                <v-col cols="auto">
                  <v-select
                    v-model="xAxisMode"
                    :items="[
                      { text: 'Cycle count', value: 'normal' },
                      { text: 'Log(Cycle count)', value: 'log' },
                    ]"
                    dense
                    hide-details
                    label="X-Axis scale"
                    style="max-width: 220px"
                  />
                </v-col>
              </v-row>
            </v-card-title>
            <v-card-text>
              <simple-chart
                ref="chartComponent"
                :key="chartKey"
                :show-legend="false"
                :series="chartSeries"
                :aspect-ratio="2"
                :x-axis-name="computedXAxisLabel"
                :x-axis-type="xAxisChartType"
                :y-axis-name="yAxisLabel"
              />
            </v-card-text>
          </v-card>
        </v-col>
      </v-row>
      <v-data-table
        v-model="testsSelected"
        :headers="headers"
        :items="numberedTests"
        :options="options"
        :server-items-length="experiment.pagination.total"
        :loading="experiment.loadingTests"
        :footer-props="{ 'items-per-page-options': [5, 10, 15, 20, 40] }"
        item-key="id"
        @update:options="onOptionsChange"
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
import SimpleChart from "@/components/charts/SimpleChart.vue";
import ExperimentSpecifications from "@/components/ExperimentSpecifications.vue";
import { mapState } from "vuex";

export default {
  name: "TestsSelection",
  components: {
    ExperimentSpecifications,
    SimpleChart,
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
      xAxisMode: "normal",
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
          /* maximum displacement with 2 decimal points */
          maximum_displacement: test.maximum_load
            ? parseFloat(test.maximum_load.toFixed(2))
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
        const controlMode =
          this.experiment?.experiment?.control_mode?.toLowerCase();

        if (controlMode === "displacement controlled") {
          baseHeaders.splice(2, 0, {
            text: "Maximum Displacement [mm]",
            value: "maximum_displacement",
          });
        } else {
          baseHeaders.splice(2, 0, {
            text: "Maximum Stress [MPa]",
            value: "maximum_stress",
          });
        }

        baseHeaders.splice(3, 0, {
          text: "Run Out",
          value: "run_out",
        });

        baseHeaders.splice(3, 0, {
          text: "Cycles",
          value: "number_of_cycles",
        });
      }
      return baseHeaders;
    },
    chartSeries() {
      const controlMode =
        this.experiment?.experiment?.control_mode?.toLowerCase();
      const isDisplacement = controlMode === "displacement controlled";

      return [
        {
          type: "scatter",
          name: isDisplacement
            ? "Maximum Displacement [mm]"
            : "Maximum Stress [MPa]",
          data: this.numberedTests
            .filter((test) => {
              const y = isDisplacement
                ? test.maximum_displacement
                : test.maximum_stress;
              return (
                typeof test.number_of_cycles === "number" &&
                typeof y === "number"
              );
            })
            .map((test) => {
              const selected = this.testsSelected.some((t) => t.id === test.id);
              return {
                value: [
                  test.number_of_cycles,
                  isDisplacement
                    ? test.maximum_displacement
                    : test.maximum_stress,
                ],
                id: test.id,
                itemStyle: {
                  color: selected ? "#1976d2" : "#90caf9",
                },
                symbolSize: selected ? 14 : 8,
              };
            }),
        },
      ];
    },
    chartKey() {
      // Forza il re-render del grafico se cambia la selezione
      return this.testsSelected.map((t) => t.id).join("-");
    },
    yAxisLabel() {
      const controlMode =
        this.experiment?.experiment?.control_mode?.toLowerCase();

      return controlMode === "displacement controlled"
        ? "Maximum Displacement [mm]"
        : "Maximum Stress [MPa]";
    },
    computedXAxisLabel() {
      switch (this.xAxisMode) {
        case "log":
          return "log₁₀(Number of cycles)";
        case "normalized":
          return "Normalized cycles";
        default:
          return "Number of cycles";
      }
    },
    xAxisChartType() {
      return this.xAxisMode === "log" ? "log" : "value";
    },
  },
  watch: {
    "experiment.tests": {
      handler(tests) {
        if (tests.length > 0) {
          /* console.log("✅ Loaded tests:", tests);
          console.log(
            "🧪 Maximum stress:",
            this.numberedTests.map((t) => t.maximum_load)
          );*/
        }
      },
      immediate: true,
    },
    chartKey() {
      this.attachChartClickHandler();
    },
  },
  created() {
    this.fetchOneExperimentWithTests();
  },
  mounted() {
    this.attachChartClickHandler();
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
    onOptionsChange(newOptions) {
      const optionsChanged =
        newOptions.page !== this.options.page ||
        newOptions.itemsPerPage !== this.options.itemsPerPage;

      this.options = newOptions;

      if (optionsChanged) {
        this.fetchOneExperimentWithTests();
      }
    },
    onChartClick(event) {
      const ec = event?.target?.__ecComponent__;
      if (!ec) return;

      const pointInPixel = [event.offsetX, event.offsetY];
      const pointInGrid = ec.convertFromPixel({ seriesIndex: 0 }, pointInPixel);
      const [xVal, yVal] = pointInGrid;

      const controlMode =
        this.experiment?.experiment?.control_mode?.toLowerCase();
      const isDisplacement = controlMode === "displacement controlled";

      const clickedTest = this.numberedTests.find((t) => {
        const y = isDisplacement ? t.maximum_displacement : t.maximum_stress;
        return (
          Math.abs(t.number_of_cycles - xVal) < 1e-1 &&
          Math.abs(y - yVal) < 1e-1
        );
      });

      if (!clickedTest) return;

      const index = this.testsSelected.findIndex(
        (t) => t.id === clickedTest.id
      );
      if (index >= 0) {
        this.testsSelected.splice(index, 1); // Deseleziona
      } else {
        this.testsSelected.push(clickedTest); // Seleziona
      }
    },
    attachChartClickHandler() {
      this.$nextTick(() => {
        const chartInstance =
          this.$refs.chartComponent?.$refs?.chartContainer?.$children?.[0]
            ?.chart;

        if (!chartInstance) return;

        chartInstance.off("click"); // per evitare duplicazioni
        chartInstance.on("click", (params) => {
          if (!params?.data || !Array.isArray(params.data.value)) return;

          const [xVal, yVal] = params.data.value;

          const controlMode =
            this.experiment?.experiment?.control_mode?.toLowerCase();
          const isDisplacement = controlMode === "displacement controlled";

          const clickedTest = this.numberedTests.find((t) => {
            const y = isDisplacement
              ? t.maximum_displacement
              : t.maximum_stress;
            return (
              Math.abs(t.number_of_cycles - xVal) < 1e-1 &&
              Math.abs(y - yVal) < 1e-1
            );
          });

          if (!clickedTest) return;

          const index = this.testsSelected.findIndex(
            (t) => t.id === clickedTest.id
          );
          if (index >= 0) {
            this.testsSelected.splice(index, 1); // Deseleziona
          } else {
            this.testsSelected.push(clickedTest); // Seleziona
          }
        });
      });
    },
  },
};
</script>

<style scoped lang="scss">
:deep(.v-data-table__selected) {
  background-color: #bbdefb !important;
  /* light blue */
}

.chart-wrapper {
  max-height: 250px;
  margin-bottom: 16px;
  overflow: hidden;
}
</style>

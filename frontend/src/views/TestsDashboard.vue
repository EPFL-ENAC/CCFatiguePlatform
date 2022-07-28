<template>
  <v-container>
    <v-skeleton-loader v-if="experiment.loading" type="article" />
    <div v-else>
      <v-row>
        <v-col :cols="12">
          <experiment-specifications :experiment="experiment.experiment" />
        </v-col>

        <v-col :cols="12">
          <v-card flat>
            <v-card-title>
              Test results
              <v-spacer> </v-spacer>
              <v-btn @click="goBack">Add test(s)</v-btn>
            </v-card-title>
            <v-card-text>
              <v-container fluid>
                <v-row>
                  <v-col cols="10">
                    <v-row>
                      <v-col cols="6">
                        <v-row>
                          <info-tooltip>
                            This graph shows a selection of loading/unloading
                            loops. These loops, also known as hysteresis loops
                            are important because they provide a visual
                            representation of what is being plotted in the next
                            three graphs.
                          </info-tooltip>
                          <div
                            ref="echartsPlotStressStrain"
                            class="echarts"
                          ></div>
                        </v-row>
                      </v-col>
                      <v-col cols="6">
                        <v-row>
                          <info-tooltip>
                            On this graph, we show the evolution of the
                            hysteresis area. This value is defined as the area
                            contained within a hysteresis loop. For visual
                            representation, it represents the area defined by
                            each closed loop on the (stress-strain) plane in
                            graph 1.
                          </info-tooltip>
                          <div
                            ref="echartsPlotHysteresisArea"
                            class="echarts"
                          ></div>
                        </v-row>
                      </v-col>
                      <v-col cols="6">
                        <v-row>
                          <info-tooltip>
                            Creep is defined as the average deformation during
                            each loading/unloading cycle. It gives an
                            understanding of how much deformation occurs at each
                            cycle.
                          </info-tooltip>
                          <div ref="echartsPlotCreep" class="echarts"></div>
                        </v-row>
                      </v-col>
                      <v-col cols="6">
                        <v-row>
                          <info-tooltip>
                            Stiffness is representative of the resistance an
                            object opposes to an applied force. On this graph,
                            we show how this capacity evolves over a fatigue
                            life cycle.
                          </info-tooltip>
                          <div ref="echartsPlotStiffness" class="echarts"></div>
                        </v-row>
                      </v-col>
                    </v-row>
                  </v-col>
                  <v-col cols="2">
                    <v-card>
                      <v-card-text>
                        <ul>
                          <li>
                            <experiment-s-v
                              subject="Specimen number"
                              :values="echartsTests.map((t) => t.specimen_id)"
                              valueType="bigNumber"
                              :colors="echartsTests.map((t) => t.color)"
                            />
                          </li>
                          <li>
                            <experiment-s-v
                              subject="Stress at failure"
                              :values="echartsTests.map((t) => '-')"
                              :colors="echartsTests.map((t) => t.color)"
                              :unit="units.stress"
                              tooltip="σ_fail is defined as the stress level that induced failure from the tested specimen and is measured in [MPa]"
                            />
                          </li>
                          <li>
                            <experiment-s-v
                              subject="Strain at failure"
                              :values="echartsTests.map((t) => '-')"
                              :colors="echartsTests.map((t) => t.color)"
                              unit="%"
                              tooltip="ε_fail is defined as the deformation at the time of failure and is measured in [%]"
                            />
                          </li>
                          <li>
                            <experiment-s-v
                              subject="N_cycles"
                              :values="echartsTests.map((t) => '-')"
                              :colors="echartsTests.map((t) => t.color)"
                              valueType="bigNumber"
                              tooltip="defined as the number of cycles to failure [-]"
                            />
                          </li>
                          <li>
                            <experiment-s-v
                              subject="R"
                              :values="echartsTests.map((t) => '-')"
                              :colors="echartsTests.map((t) => t.color)"
                              tooltip="defined as the stress ratio (σ_min/σ_max) [-] and has relevance in the context of constant amplitude experiments."
                            />
                          </li>
                          <li>
                            <experiment-s-v
                              subject="Total dissipated energy (TDE)"
                              :values="
                                echartsTests.map(
                                  (t) => t.total_dissipated_energy
                                )
                              "
                              valueType="bigNumber"
                              :colors="echartsTests.map((t) => t.color)"
                              tooltip="defined as the sum of all the hysteresis areas over the course of an experiment. It gives a good measure of the amount of energy that has been dissipated in deformation and heat over the course of an experiment."
                            />
                          </li>
                        </ul>
                      </v-card-text>
                    </v-card>
                  </v-col>
                </v-row>
              </v-container>
            </v-card-text>
          </v-card>
        </v-col>
      </v-row>
    </div>
  </v-container>
</template>

<script>
import * as echarts from "echarts";

import { mapState } from "vuex";
import InfoTooltip from "@/components/InfoTooltip";
import ExperimentSpecifications from "@/components/ExperimentSpecifications.vue";
import ExperimentSV from "@/components/ExperimentSV";

export default {
  name: "TestsDashboard",
  components: {
    ExperimentSpecifications,
    InfoTooltip,
    ExperimentSV,
  },
  props: {
    experimentId: Number,
    testIds: Array[Number],
  },
  computed: {
    ...mapState("experiments", {
      experiment: "oneExperiment",
      units: "units",
    }),
    ...mapState("echartsPlots", {
      echartsLoading: "loading",
      echartsTests: "tests",
      echartsPlots: "plots",
    }),
  },
  data() {
    return {
      echartsPlotsRendered: false,
    };
  },
  methods: {
    goBack() {
      this.$router.go(-1);
    },
    renderEChartsPlotsIfGoodTime() {
      if (
        !this.experiment.loading &&
        !this.echartsLoading &&
        !this.echartsPlotsRendered
      ) {
        this.plotEcharts(
          this.$refs.echartsPlotStressStrain,
          this.echartsPlots.stress_strain
        );
        this.plotEcharts(
          this.$refs.echartsPlotHysteresisArea,
          this.echartsPlots.hysteresis_area
        );
        this.plotEcharts(this.$refs.echartsPlotCreep, this.echartsPlots.creep);
        this.plotEcharts(
          this.$refs.echartsPlotStiffness,
          this.echartsPlots.stiffness
        );

        this.echartsPlotsRendered = true;
      }
    },
    plotEcharts(div, plot) {
      // Initialize the echarts instance based on the prepared dom
      var myChart = echarts.init(div, null, { renderer: "canvas" });

      const [x_axis_key, x_axis_label] = plot.x_axis,
        [y_axis_key, y_axis_label] = plot.y_axis;

      // Chart stress - strain includes many series of the same test, so we need to create a custom tooltip for this special "3D" chart

      const is3D = plot.tooltips.length > 2,
        id3D = (line) =>
          `Specimen ${line.legend_label} - cycle ${
            line.data[plot.tooltips[2][0]][0]
          }`;

      let htmlDot = (color) =>
        `<span style="height: 10px;width: 10px;background-color: ${color};border-radius: 50%; display: inline-block;"></span>`;

      let tooltip3D = {
        trigger: "axis",
        formatter: (params) =>
          params
            .map(
              ({ seriesId, data, color }) =>
                `${htmlDot(color)} ${seriesId} : <br />
              ${x_axis_label} ${data[0].toFixed(
                  4
                )} - ${y_axis_label} ${data[1].toFixed(4)}<br />`
            )
            .join("<br /><br />"),
      };

      // Normal tooltips
      let tooltip2D = {
        trigger: "axis",
        valueFormatter: (v) => v.toFixed(5),
      };

      let series = plot.lines
        .map((line) => ({
          name: line.legend_label,
          type: "line",
          showSymbol: false,

          // I use id property to create a nice tooltip for 3D charts
          id: is3D ? id3D(line) : line.legend_label,

          data: line.data[x_axis_key].map((x, i) => [
            x,
            line.data[y_axis_key][i],
          ]),
        }))
        // They are some problems in data, especially for stress strain where API returns duplicates values of same test
        .filter(
          (serie, i, a) =>
            serie.data.length > 0 && a.findIndex((s) => s.id === serie.id) === i
        );

      // Specify the configuration items and data for the chart
      var option = {
        title: {
          text: plot.title,
        },
        //Need to specifiy empty legend param so echarts gets values automatically in series
        legend: {
          backgroundColor: "#fff",
          itemGap: 5,
          itemWidth: 20,
          itemHeight: 12,
          orient: "horizontal",
          left: "center",
          top: 40,
        },
        grid: {
          left: "4%",
          right: "3%",
          bottom: "7%",
          containLabel: true,
        },
        tooltip: is3D ? tooltip3D : tooltip2D,
        dataZoom: [],
        toolbox: {
          show: true,
          feature: {
            dataZoom: {},
            dataView: { readOnly: false },
            restore: {},
            saveAsImage: {},
          },
        },
        xAxis: {
          nameLocation: "center",
          name: x_axis_label,
          nameGap: 25,
          axisLabel: {
            formatter: (v) =>
              x_axis_key === "n_cycles" ? v.toExponential() : v,
            align: "center",
          },
          nameTextStyle: {
            fontWeight: "bold",
          },
        },
        yAxis: {
          type: "value",
          name: y_axis_label,
          nameTextStyle: {
            fontWeight: "bold",
          },
        },
        series,
      };

      // Display the chart using the configuration items and data just specified.
      myChart.setOption(option);
    },
  },
  mounted() {
    const storeUnwatch1 = this.$store.watch(
      (state) => state.experiments.oneExperiment.loading,
      () => {
        if (!this.experiment.loading) {
          this.renderEChartsPlotsIfGoodTime();
          storeUnwatch1();
        }
      }
    );
    const storeUnwatch2 = this.$store.watch(
      (state) => state.echartsPlots.loading,
      () => {
        if (!this.echartsLoading) {
          this.renderEChartsPlotsIfGoodTime();
          storeUnwatch2();
        }
      }
    );
  },
  created() {
    this.$store.dispatch("echartsPlots/fetchEchartsPlots", {
      experimentId: this.experimentId,
      testIds: this.testIds,
    });
    this.$store.dispatch("experiments/fetchOneExperimentWithTests", {
      experimentId: this.experimentId,
      pagination: {
        page: 1,
        size: 20,
      },
    });
  },
};
</script>

<style scoped>
div.echarts {
  min-height: 250px;
  width: 600px;
}
</style>

<template>
  <v-responsive ref="chartContainer" :aspect-ratio="aspectRatio">
    <v-chart
      ref="chartInstance"
      autoresize
      :option="actualOption"
      :update-options="updateOptions"
    ></v-chart>
  </v-responsive>
</template>

<script>
import { format3 } from "@/utils/formatters";
import { colorPalette } from "@/utils/style";
import { LineChart, ScatterChart } from "echarts/charts";
import {
  DataZoomComponent,
  GridComponent,
  LegendComponent,
  TitleComponent,
  TooltipComponent,
} from "echarts/components";
import { use } from "echarts/core";
import { CanvasRenderer } from "echarts/renderers";
import { merge } from "lodash";
import VChart from "vue-echarts";

use([
  DataZoomComponent,
  CanvasRenderer,
  LineChart,
  ScatterChart,
  GridComponent,
  LegendComponent,
  TitleComponent,
  TooltipComponent,
]);

export default {
  name: "SimpleChart",
  components: {
    VChart,
  },
  props: {
    aspectRatio: { type: Number, default: 1 },
    series: { type: Array, default: () => [] },
    title: { type: String, default: "" },
    xAxisName: { type: String, default: "" },
    yAxisName: { type: String, default: "" },
    xAxisType: { type: String, default: "value" },
    yAxisType: { type: String, default: "value" },
    dataZoom: { type: String, default: "" },
    color: { type: Array, default: () => colorPalette },
    xAxisMin: { type: [Number, null], default: null },
    xAxisMax: { type: [Number, null], default: null },
    yAxisMax: { type: [Number, null], default: null },
    axisLabelFormatter: {
      type: Function,
      default: format3,
    },
    showLegend: { type: Boolean, default: true },
    tooltipFormatter: {
      type: Function,
      default: format3,
    },
  },
  data() {
    return {
      updateOptions: {
        notMerge: true,
      },
    };
  },
  computed: {
    actualOption() {
      return {
        title: {
          text: this.title,
        },
        legend: this.showLegend ? { type: "scroll" } : { show: false },
        grid: {
          left: 50,
          top: 40,
          right: 50,
          bottom: 20,
          containLabel: true,
        },
        xAxis: {
          type: this.xAxisType,
          name: this.xAxisName,
          nameLocation: "middle",
          nameGap: 26,
          min: this.xAxisMin != null ? this.xAxisMin : "dataMin",
          max: this.xAxisMax != null ? this.xAxisMax : "dataMax",
          axisLabel: {
            formatter: this.axisLabelFormatter,
            hideOverlap: true,
            showMaxLabel: true,
          },
        },
        yAxis: {
          type: this.yAxisType,
          name: this.yAxisName,
          nameLocation: "middle",
          nameGap: 50,
          logBase: this.yAxisType === "log" ? 10 : undefined,
          minorSplitLine: { show: this.yAxisType === "log" },
          min: this.yAxisType === "log" ? "dataMin" : 0,
          max: this.yAxisMax != null ? this.yAxisMax : "dataMax",
          axisLabel: {
            formatter: (val) =>
              this.yAxisType === "log"
                ? `10^${Math.round(Math.log10(val))}`
                : this.axisLabelFormatter(val),
            hideOverlap: true,
            showMaxLabel: true,
          },
        },
        tooltip: {
          trigger: "axis",
          confine: true,
          formatter: (params) => {
            const formatter = this.tooltipFormatter;
            let xLabel;

            if (
              this.xAxisName.includes("Number of cycles") ||
              this.xAxisName.includes("Normalized cycles")
            ) {
              const p = params[0];
              let rawX = null;

              if (p.seriesIndex != null && this.series[p.seriesIndex]?.rawX) {
                const pointIndex = p.dataIndex;
                rawX = this.series[p.seriesIndex].rawX[pointIndex];
              }

              if (rawX != null) {
                xLabel = Number(rawX).toLocaleString(undefined, {
                  minimumFractionDigits: 0,
                  maximumFractionDigits: 0,
                  useGrouping: false,
                });
              } else {
                // fallback if rawX is not available
                const xValue = p.value[0] ?? p.value;
                xLabel = Number(xValue).toLocaleString(undefined, {
                  minimumFractionDigits: 0,
                  maximumFractionDigits: 0,
                  useGrouping: false,
                });
              }
            } else {
              // other graphs: use default value
              xLabel = params[0].axisValueLabel;
            }

            const rows = [`<strong>${xLabel}</strong>`];

            for (const p of params) {
              const rawY = Array.isArray(p.value) ? p.value[1] : p.value;
              const yVal = Number(rawY);
              const formattedY = formatter(yVal);
              rows.push(`${p.marker}${p.seriesName}: ${formattedY}`);
            }

            return rows.join("<br/>");
          },
        },
        dataZoom: [
          {
            id: "dataZoomX",
            xAxisIndex: [0],
            filterMode: "none",
            type: "inside",
          },
          {
            id: "dataZoomY",
            yAxisIndex: [0],
            filterMode: "none",
            type: "inside",
          },
        ],
        series: this.series.map((serie) => merge(serie, { showSymbol: false })),
        color: this.color,
      };
    },
  },
  methods: {
    getChartInstance() {
      return this.$refs.chartInstance?.chart;
    },
  },
};
</script>
